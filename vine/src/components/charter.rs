use std::mem::replace;

use crate::{
  components::loader::Module,
  features::cfg::Config,
  structures::{
    annotations::Annotations,
    ast::{Attr, AttrKind, Flex, Ident, Item, ItemKind, ModKind, Span, Vis, visit::VisitMut},
    chart::Chart,
    diag::{Diag, Diags, ErrorGuaranteed},
  },
};

use crate::structures::chart::*;

pub struct Charter<'a> {
  pub chart: &'a mut Chart,
  pub config: &'a Config,
  pub diags: &'a mut Diags,
  pub annotations: &'a mut Annotations,
}

impl Charter<'_> {
  pub fn chart(&mut self, modules: Vec<Module>) {
    if self.chart.generics.is_empty() {
      self.chart.generics.push_to(
        GenericsId::NONE,
        GenericsDef {
          span: Span::NONE,
          def: DefId::NONE,
          parent: None,
          type_params: Vec::new(),
          impl_params: Vec::new(),
          impl_allowed: true,
          global_flex: Flex::None,
          trait_: None,
        },
      );
    }
    for module in modules {
      let path = format!("#{}", module.name);
      let def = self.new_def(module.name.clone(), path, None);
      self.chart.top_level.insert(module.name.clone(), def);
      if module.main {
        self.chart.main_mod = Some(def);
      }
      self.chart_mod_kind(VisId::Pub, module.kind, def, GenericsId::NONE);
    }
  }

  pub(crate) fn new_def(&mut self, name: Ident, path: String, parent: Option<DefId>) -> DefId {
    let id = self.chart.defs.next_index();
    self.chart.defs.push(Def {
      name,
      path,
      file: None,
      spans: Vec::new(),
      members_lookup: Default::default(),
      named_members: Default::default(),
      implicit_members: Default::default(),
      parent,
      ancestors: parent
        .iter()
        .flat_map(|&p| self.chart.defs[p].ancestors.iter().copied())
        .chain([id])
        .collect(),
      value_kind: None,
      type_kind: None,
      pattern_kind: None,
      trait_kind: None,
      impl_kind: None,
    })
  }

  pub(crate) fn chart_mod_kind(
    &mut self,
    vis: VisId,
    module: ModKind,
    def: DefId,
    generics: GenericsId,
  ) {
    let ModKind::Loaded(_, file, items) = module else { unreachable!("module not yet loaded") };
    if file.is_some() {
      self.chart.defs[def].file = file;
    }
    for item in items {
      self.chart_item(vis, item, def, generics);
    }
  }

  pub fn chart_item(
    &mut self,
    member_vis: VisId,
    mut item: Item,
    parent: DefId,
    parent_generics: GenericsId,
  ) {
    if !self.enabled(&item.attrs) {
      return;
    }

    let subitems = extract_subitems(&mut item);

    let span = item.name_span;
    let vis = self.resolve_vis(parent, item.vis);
    let member_vis = vis.max(member_vis);

    let def = match item.kind {
      ItemKind::OuterMod => {
        self.chart.defs[parent].spans.push(item.name_span);
        Some(parent)
      }

      ItemKind::Mod(mod_item) => {
        Some(self.chart_mod(parent, parent_generics, span, vis, member_vis, mod_item))
      }

      ItemKind::Fn(fn_item) => {
        Some(self.chart_fn(parent, parent_generics, span, vis, member_vis, fn_item))
      }

      ItemKind::Const(const_item) => {
        Some(self.chart_const(parent, parent_generics, span, vis, member_vis, const_item))
      }

      ItemKind::Struct(struct_item) => {
        Some(self.chart_struct(parent, parent_generics, span, vis, member_vis, struct_item))
      }

      ItemKind::Enum(enum_item) => {
        Some(self.chart_enum(parent, parent_generics, span, vis, member_vis, enum_item))
      }

      ItemKind::Type(type_item) => {
        Some(self.chart_type(parent, parent_generics, span, vis, member_vis, type_item))
      }

      ItemKind::Trait(trait_item) => {
        Some(self.chart_trait(parent, parent_generics, span, vis, member_vis, trait_item))
      }

      ItemKind::Impl(impl_item) => {
        Some(self.chart_impl(parent, parent_generics, span, vis, member_vis, impl_item))
      }

      ItemKind::Use(use_item) => {
        self.chart_use(parent, vis, use_item);
        None
      }

      ItemKind::Taken => None,
    };

    if let Some(def) = def {
      for subitem in subitems {
        if !matches!(subitem.vis, Vis::Private) {
          self.diags.error(Diag::VisibleSubitem { span: subitem.name_span });
        }
        self.chart_item(VisId::Def(def), subitem, def, GenericsId::NONE);
      }
    }

    if !item.docs.is_empty() {
      self.annotations.record_docs(span, item.docs);
    }

    self.chart_attrs(def, item.attrs);
  }

  pub(crate) fn chart_attrs(&mut self, def: Option<DefId>, attrs: Vec<Attr>) {
    for attr in attrs {
      let span = attr.span;
      let impl_id = def.and_then(|id| match self.chart.defs[id].impl_kind {
        Some(Binding { kind: DefImplKind::Impl(id), .. }) => Some(id),
        _ => None,
      });
      let concrete_fn_id = def.and_then(|id| match self.chart.defs[id].value_kind {
        Some(Binding { kind: DefValueKind::Fn(FnId::Concrete(id)), .. }) => Some(id),
        _ => None,
      });
      match attr.kind {
        AttrKind::Builtin(builtin) => {
          if !self.chart_builtin(def, builtin) {
            self.diags.error(Diag::BadBuiltin { span });
          }
        }
        AttrKind::Manual => {
          let Some(impl_id) = impl_id else {
            self.diags.error(Diag::BadManualAttr { span });
            continue;
          };
          self.chart.impls[impl_id].manual = true;
        }
        AttrKind::Basic => {
          let Some(impl_id) = impl_id else {
            self.diags.error(Diag::BadBasicAttr { span });
            continue;
          };
          self.chart.impls[impl_id].basic = true;
        }
        AttrKind::Become(path) => {
          let Some(impl_id) = impl_id else {
            self.diags.error(Diag::BadBecomeAttr { span });
            continue;
          };
          let impl_ = &mut self.chart.impls[impl_id];
          if impl_.become_.is_some() {
            self.diags.error(Diag::DuplicateBecomeAttr { span });
            continue;
          }
          impl_.become_ = Some(path);
        }
        AttrKind::Frameless => {
          let Some(concrete_fn_id) = concrete_fn_id else {
            self.diags.error(Diag::BadFramelessAttr { span });
            continue;
          };
          self.chart.concrete_fns[concrete_fn_id].frameless = true;
        }
        AttrKind::Cfg(_) => {}
        AttrKind::Test => {
          let Some(concrete_fn_id) = concrete_fn_id else {
            self.diags.error(Diag::BadTestAttr { span });
            continue;
          };

          let def_id = self.chart.concrete_fns[concrete_fn_id].def;
          if self.chart.main_mod == Some(self.chart.defs[def_id].ancestors[0]) {
            self.chart.tests.push(concrete_fn_id);
          }
        }
      }
    }
  }

  pub(crate) fn chart_child(
    &mut self,
    parent: DefId,
    span: Span,
    name: Ident,
    vis: VisId,
    collapse: bool,
  ) -> DefId {
    let next_def_id = self.chart.defs.next_index();
    let parent_def = &mut self.chart.defs[parent];
    if collapse && parent_def.name == name && parent_def.ancestors.len() > 1 {
      if span != Span::NONE {
        parent_def.spans.push(span);
      }
      return parent;
    }
    let mut new = false;
    let member = parent_def.members_lookup.entry(name.clone()).or_insert_with(|| {
      new = true;
      let member = Binding { span, vis, kind: MemberKind::Child(next_def_id) };
      parent_def.named_members.push(member);
      member
    });
    let child = match member.kind {
      MemberKind::Child(child) => child,
      MemberKind::Import(i) => {
        self
          .diags
          .error(Diag::DuplicateItem { span: self.chart.imports[i].span, name: name.clone() });
        new = true;
        next_def_id
      }
    };
    member.vis = member.vis.min(vis);
    if new {
      let path = format!("{}::{}", parent_def.path, name);
      self.new_def(name, path, Some(parent));
    }
    if span != Span::NONE {
      self.chart.defs[child].spans.push(span);
    }
    child
  }

  pub(crate) fn resolve_vis(&mut self, base: DefId, vis: Vis) -> VisId {
    match vis {
      Vis::Private => VisId::Def(base),
      Vis::Public => VisId::Pub,
      Vis::PublicTo(span, name) => {
        let ancestors = &self.chart.defs[base].ancestors;
        if let Some(&ancestor) = ancestors.iter().rev().find(|&&a| self.chart.defs[a].name == name)
        {
          VisId::Def(ancestor)
        } else {
          self.diags.error(Diag::BadVis { span });
          VisId::Pub
        }
      }
    }
  }

  pub(crate) fn ensure_implemented<T: From<ErrorGuaranteed>>(
    &mut self,
    span: Span,
    option: Option<T>,
  ) -> T {
    option.unwrap_or_else(|| self.diags.error(Diag::MissingImplementation { span }).into())
  }

  pub(crate) fn define_value(&mut self, span: Span, def: DefId, vis: VisId, kind: DefValueKind) {
    let def = &mut self.chart.defs[def];
    if def.value_kind.is_none() {
      def.value_kind = Some(Binding { span, vis, kind });
    } else {
      self.diags.error(Diag::DuplicateItem { span, name: def.name.clone() });
    }
  }

  pub(crate) fn define_type(&mut self, span: Span, def: DefId, vis: VisId, kind: DefTypeKind) {
    let def = &mut self.chart.defs[def];
    if def.type_kind.is_none() {
      def.type_kind = Some(Binding { span, vis, kind });
    } else {
      self.diags.error(Diag::DuplicateItem { span, name: def.name.clone() });
    }
  }

  pub(crate) fn define_pattern(
    &mut self,
    span: Span,
    def: DefId,
    vis: VisId,
    kind: DefPatternKind,
  ) {
    let def = &mut self.chart.defs[def];
    if def.pattern_kind.is_none() {
      def.pattern_kind = Some(Binding { span, vis, kind });
    } else {
      self.diags.error(Diag::DuplicateItem { span, name: def.name.clone() });
    }
  }

  pub(crate) fn define_trait(&mut self, span: Span, def: DefId, vis: VisId, kind: DefTraitKind) {
    let def = &mut self.chart.defs[def];
    if def.trait_kind.is_none() {
      def.trait_kind = Some(Binding { span, vis, kind });
    } else {
      self.diags.error(Diag::DuplicateItem { span, name: def.name.clone() });
    }
  }

  pub(crate) fn define_impl(&mut self, span: Span, def: DefId, vis: VisId, kind: DefImplKind) {
    let def = &mut self.chart.defs[def];
    if def.impl_kind.is_none() {
      def.impl_kind = Some(Binding { span, vis, kind });
    } else {
      self.diags.error(Diag::DuplicateItem { span, name: def.name.clone() });
    }
  }
}

fn extract_subitems(item: &mut Item) -> Vec<Item> {
  let mut visitor = ExtractItems::default();
  if !matches!(item.kind, ItemKind::Mod(_) | ItemKind::Trait(_) | ItemKind::Impl(_)) {
    visitor._visit_item(item);
  }
  visitor.items
}

#[derive(Default)]
pub struct ExtractItems {
  pub items: Vec<Item>,
}

impl VisitMut<'_> for ExtractItems {
  fn visit_item(&mut self, item: &mut Item) {
    self.items.push(replace(
      item,
      Item {
        span: Span::NONE,
        name_span: Span::NONE,
        docs: Vec::new(),
        vis: Vis::Private,
        attrs: vec![],
        kind: ItemKind::Taken,
      },
    ));
  }
}
