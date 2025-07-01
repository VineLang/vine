use std::mem::replace;

use crate::structures::{
  ast::{visit::VisitMut, Attr, AttrKind, Ident, Item, ItemKind, ModKind, Span, Vis},
  chart::Chart,
  core::Core,
  diag::{Diag, ErrorGuaranteed},
};

use crate::structures::chart::*;

pub struct Charter<'core, 'a> {
  pub core: &'core Core<'core>,
  pub chart: &'a mut Chart<'core>,
}

impl<'core> Charter<'core, '_> {
  pub fn chart_root(&mut self, root: ModKind<'core>) {
    if self.chart.generics.is_empty() {
      self.chart.generics.push_to(
        GenericsId::NONE,
        GenericsDef {
          span: Span::NONE,
          def: DefId::ROOT,
          type_params: Vec::new(),
          impl_params: Vec::new(),
        },
      );
    }
    if self.chart.defs.is_empty() {
      self.new_def(self.core.ident("::"), "", None);
    }
    self.chart_mod(DefId::ROOT, root, DefId::ROOT);
  }

  fn new_def(&mut self, name: Ident<'core>, path: &'core str, parent: Option<DefId>) -> DefId {
    let id = self.chart.defs.next_index();
    self.chart.defs.push(Def {
      name,
      path,
      members: Default::default(),
      all_members: Default::default(),
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

  pub(crate) fn chart_mod(&mut self, vis: DefId, module: ModKind<'core>, def: DefId) {
    let ModKind::Loaded(_, items) = module else { unreachable!("module not yet loaded") };
    for item in items {
      self.chart_item(vis, item, def);
    }
  }

  pub fn chart_item(&mut self, member_vis: DefId, mut item: Item<'core>, parent: DefId) {
    let subitems = extract_subitems(&mut item);

    let span = item.span;
    let vis = self.resolve_vis(parent, item.vis);
    let member_vis = vis.max(member_vis);

    let def = match item.kind {
      ItemKind::Fn(fn_item) => Some(self.chart_fn(parent, span, vis, member_vis, fn_item)),

      ItemKind::Const(const_item) => {
        Some(self.chart_const(parent, span, vis, member_vis, const_item))
      }

      ItemKind::Struct(struct_item) => {
        Some(self.chart_struct(parent, span, vis, member_vis, struct_item))
      }

      ItemKind::Enum(enum_item) => Some(self.chart_enum(parent, span, vis, member_vis, enum_item)),

      ItemKind::Type(type_item) => Some(self.chart_type(parent, span, vis, member_vis, type_item)),

      ItemKind::Mod(mod_item) => {
        let def = self.chart_child(parent, mod_item.name, member_vis, true);
        self.chart_mod(vis, mod_item.kind, def);
        Some(def)
      }

      ItemKind::Trait(trait_item) => {
        Some(self.chart_trait(parent, span, vis, member_vis, trait_item))
      }

      ItemKind::Impl(impl_item) => Some(self.chart_impl(parent, span, vis, member_vis, impl_item)),

      ItemKind::Use(use_item) => {
        self.chart_use(parent, vis, use_item);
        None
      }

      ItemKind::Taken => None,
    };

    if let Some(def) = def {
      for subitem in subitems {
        if !matches!(subitem.vis, Vis::Private) {
          self.core.report(Diag::VisibleSubitem { span: item.span });
        }
        self.chart_item(def, subitem, def);
      }
    }

    self.chart_attrs(def, item.attrs);
  }

  pub(crate) fn chart_attrs(&mut self, def: Option<DefId>, attrs: Vec<Attr>) {
    for attr in attrs {
      let span = attr.span;
      let impl_id = def.and_then(|id| match self.chart.defs[id].impl_kind {
        Some(WithVis { vis: _, kind: DefImplKind::Impl(id) }) => Some(id),
        _ => None,
      });
      match attr.kind {
        AttrKind::Builtin(builtin) => {
          if !self.chart_builtin(def, builtin) {
            self.core.report(Diag::BadBuiltin { span });
          }
        }
        AttrKind::Main => {
          self.chart.main_mod = def;
        }
        AttrKind::Manual => {
          let Some(impl_id) = impl_id else {
            self.core.report(Diag::BadManualAttr { span });
            continue;
          };
          self.chart.impls[impl_id].manual = true;
        }
        AttrKind::Duplicate => {
          let Some(impl_id) = impl_id else {
            self.core.report(Diag::BadDuplicateAttr { span });
            continue;
          };
          self.chart.impls[impl_id].duplicate = true;
        }
        AttrKind::Erase => {
          let Some(impl_id) = impl_id else {
            self.core.report(Diag::BadEraseAttr { span });
            continue;
          };
          self.chart.impls[impl_id].erase = true;
        }
      }
    }
  }

  pub(crate) fn chart_child(
    &mut self,
    parent: DefId,
    name: Ident<'core>,
    vis: DefId,
    collapse: bool,
  ) -> DefId {
    let next_def_id = self.chart.defs.next_index();
    let parent_def = &mut self.chart.defs[parent];
    if collapse && parent_def.name == name {
      return parent;
    }
    let mut new = false;
    let member = parent_def.members.entry(name).or_insert_with(|| {
      new = true;
      let member = WithVis { vis, kind: MemberKind::Child(next_def_id) };
      parent_def.all_members.push(member);
      member
    });
    let child = match member.kind {
      MemberKind::Child(child) => child,
      MemberKind::Import(i) => {
        self.core.report(Diag::DuplicateItem { span: self.chart.imports[i].span, name });
        new = true;
        next_def_id
      }
    };
    member.vis = member.vis.min(vis);
    if new {
      let path = format!("{}::{}", parent_def.path, name);
      let path = self.core.alloc_str(&path);
      self.new_def(name, path, Some(parent));
    }
    child
  }

  pub(crate) fn resolve_vis(&mut self, base: DefId, vis: Vis) -> DefId {
    match vis {
      Vis::Private => base,
      Vis::Public => DefId::ROOT,
      Vis::PublicTo(span, name) => {
        let ancestors = &self.chart.defs[base].ancestors;
        if let Some(&ancestor) = ancestors.iter().rev().find(|&&a| self.chart.defs[a].name == name)
        {
          ancestor
        } else {
          self.core.report(Diag::BadVis { span });
          DefId::ROOT
        }
      }
    }
  }

  pub(crate) fn ensure_implemented<T: From<ErrorGuaranteed>>(
    &mut self,
    span: Span,
    option: Option<T>,
  ) -> T {
    option.unwrap_or_else(|| self.core.report(Diag::MissingImplementation { span }).into())
  }

  pub(crate) fn define_value(&mut self, span: Span, def: DefId, vis: DefId, kind: DefValueKind) {
    let def = &mut self.chart.defs[def];
    if def.value_kind.is_none() {
      def.value_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  pub(crate) fn define_type(&mut self, span: Span, def: DefId, vis: DefId, kind: DefTypeKind) {
    let def = &mut self.chart.defs[def];
    if def.type_kind.is_none() {
      def.type_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  pub(crate) fn define_pattern(
    &mut self,
    span: Span,
    def: DefId,
    vis: DefId,
    kind: DefPatternKind,
  ) {
    let def = &mut self.chart.defs[def];
    if def.pattern_kind.is_none() {
      def.pattern_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  pub(crate) fn define_trait(&mut self, span: Span, def: DefId, vis: DefId, kind: DefTraitKind) {
    let def = &mut self.chart.defs[def];
    if def.trait_kind.is_none() {
      def.trait_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  pub(crate) fn define_impl(&mut self, span: Span, def: DefId, vis: DefId, kind: DefImplKind) {
    let def = &mut self.chart.defs[def];
    if def.impl_kind.is_none() {
      def.impl_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }
}

fn extract_subitems<'core>(item: &mut Item<'core>) -> Vec<Item<'core>> {
  let mut visitor = ExtractItems::default();
  if !matches!(item.kind, ItemKind::Mod(_) | ItemKind::Trait(_) | ItemKind::Impl(_)) {
    visitor._visit_item(item);
  }
  visitor.items
}

#[derive(Default)]
pub struct ExtractItems<'core> {
  pub items: Vec<Item<'core>>,
}

impl<'core> VisitMut<'core, '_> for ExtractItems<'core> {
  fn visit_item(&mut self, item: &mut Item<'core>) {
    self.items.push(replace(
      item,
      Item { span: Span::NONE, vis: Vis::Private, attrs: vec![], kind: ItemKind::Taken },
    ));
  }
}
