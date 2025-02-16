use std::{collections::hash_map::Entry, mem::take};

use vine_util::idx::{Counter, IdxVec};

use crate::{
  ast::{
    Attr, AttrKind, Builtin, GenericParams, Generics, Ident, Item, ItemKind, ModKind, Span, Trait,
    TraitKind, Ty, TyKind, UseTree, Vis,
  },
  chart::{Chart, TraitSubitem},
  checker::Type,
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  visit::VisitMut,
};

use crate::chart::*;

pub struct Charter<'core, 'a> {
  pub core: &'core Core<'core>,
  pub chart: &'a mut Chart<'core>,
}

impl<'core> Charter<'core, '_> {
  pub fn chart_root(&mut self, root: ModKind<'core>) {
    if self.chart.generics.is_empty() {
      self.chart.generics.push(GenericsDef::default());
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
      parent,
      ancestors: parent
        .iter()
        .flat_map(|&p| self.chart.defs[p].ancestors.iter().copied())
        .chain([id])
        .collect(),
      value_def: None,
      type_def: None,
      pattern_def: None,
      trait_def: None,
      impl_def: None,
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
      ItemKind::Fn(fn_item) => {
        let def = self.chart_child(parent, fn_item.name, member_vis, true);
        let generics = self.chart_generics(def, fn_item.generics, true);
        let body = self.ensure_implemented(span, fn_item.body);
        let kind = ValueDefKind::Fn { params: fn_item.params, ret: fn_item.ret, body };
        self.define_value(span, def, vis, generics, kind, fn_item.method);
        Some(def)
      }

      ItemKind::Const(const_item) => {
        let def = self.chart_child(parent, const_item.name, member_vis, true);
        let generics = self.chart_generics(def, const_item.generics, true);
        let value = self.ensure_implemented(span, const_item.value);
        let kind = ValueDefKind::Const { ty: const_item.ty, value };
        self.define_value(span, def, vis, generics, kind, false);
        Some(def)
      }

      ItemKind::Struct(struct_item) => {
        let def = self.chart_child(parent, struct_item.name, member_vis, true);
        let generics = self.chart_generics(def, struct_item.generics, false);
        let adt = self.chart.adts.push(AdtDef {
          def,
          generics,
          name: struct_item.name,
          variants: IdxVec::from([AdtVariant {
            def,
            name: struct_item.name,
            fields: struct_item.fields,
            object: struct_item.object,
          }]),
          is_struct: true,
        });
        let variant = VariantId(0);
        self.define_value(span, def, vis, generics, ValueDefKind::Adt(adt, variant), false);
        self.define_pattern(span, def, vis, generics, PatternDefKind::Adt(adt, variant));
        self.define_type(span, def, vis, generics, TypeDefKind::Adt(adt));
        Some(def)
      }

      ItemKind::Enum(enum_item) => {
        let def = self.chart_child(parent, enum_item.name, member_vis, true);
        let generics = self.chart_generics(def, enum_item.generics, false);
        let adt = self.chart.adts.next_index();
        let variants = enum_item
          .variants
          .into_iter()
          .enumerate()
          .map(|(id, variant)| {
            let id = VariantId(id);
            let def = self.chart_child(def, variant.name, vis, true);
            self.define_value(span, def, vis, generics, ValueDefKind::Adt(adt, id), false);
            self.define_pattern(span, def, vis, generics, PatternDefKind::Adt(adt, id));
            AdtVariant { def, name: variant.name, fields: variant.fields, object: false }
          })
          .collect::<Vec<_>>()
          .into();
        self.chart.adts.push(AdtDef {
          def,
          name: enum_item.name,
          generics,
          variants,
          is_struct: false,
        });
        self.define_type(span, def, vis, generics, TypeDefKind::Adt(adt));
        Some(def)
      }

      ItemKind::Type(type_item) => {
        let def = self.chart_child(parent, type_item.name, member_vis, true);
        let generics = self.chart_generics(def, type_item.generics, false);
        self.define_type(span, def, vis, generics, TypeDefKind::Alias(type_item.ty));
        Some(def)
      }

      ItemKind::Mod(mod_item) => {
        let def = self.chart_child(parent, mod_item.name, member_vis, true);
        self.chart_mod(vis, mod_item.kind, def);
        Some(def)
      }

      ItemKind::Trait(trait_item) => {
        let def = self.chart_child(parent, trait_item.name, member_vis, true);
        let generics = self.chart_generics(def, trait_item.generics, false);
        let mut sub_id = Counter::<SubitemId>::default();
        let trait_id = self.chart.traits.next_index();
        let sub_generics = {
          let generics = &self.chart.generics[generics];
          self.chart.generics.push(GenericsDef {
            span: generics.span,
            def,
            type_params: generics.type_params.clone(),
            impl_params: vec![(
              None,
              Trait {
                span: Span::NONE,
                kind: TraitKind::Def(
                  trait_id,
                  Generics {
                    span: Span::NONE,
                    types: (0..generics.type_params.len())
                      .map(|i| Ty { span: Span::NONE, kind: TyKind::Param(i) })
                      .collect(),
                    impls: Vec::new(),
                  },
                ),
              },
            )],
          })
        };
        let subitems = trait_item
          .items
          .into_iter()
          .filter_map(|subitem| {
            let span = subitem.span;
            if !matches!(subitem.vis, Vis::Private) {
              self.core.report(Diag::TraitItemVis { span });
            }
            match subitem.kind {
              ItemKind::Fn(fn_item) => {
                if !fn_item.generics.impls.is_empty() || !fn_item.generics.types.is_empty() {
                  self.core.report(Diag::TraitItemGen { span });
                }
                if fn_item.body.is_some() {
                  self.core.report(Diag::ImplementedTraitItem { span });
                }
                let def = self.chart_child(def, fn_item.name, vis, true);
                let sub_id = sub_id.next();
                let kind = ValueDefKind::TraitSubitem(trait_id, sub_id);
                self.define_value(span, def, vis, sub_generics, kind, fn_item.method);
                self.chart_attrs(span, vis, Some(def), subitem.attrs);
                Some(TraitSubitem {
                  name: fn_item.name,
                  kind: TraitSubitemKind::Fn(fn_item.params, fn_item.ret),
                })
              }
              ItemKind::Const(const_item) => {
                if !const_item.generics.impls.is_empty() || !const_item.generics.types.is_empty() {
                  self.core.report(Diag::TraitItemGen { span });
                }
                if const_item.value.is_some() {
                  self.core.report(Diag::ImplementedTraitItem { span });
                }
                let def = self.chart_child(def, const_item.name, vis, true);
                let sub_id = sub_id.next();
                let kind = ValueDefKind::TraitSubitem(trait_id, sub_id);
                self.define_value(span, def, vis, sub_generics, kind, false);
                self.chart_attrs(span, vis, Some(def), subitem.attrs);
                Some(TraitSubitem {
                  name: const_item.name,
                  kind: TraitSubitemKind::Const(const_item.ty),
                })
              }
              _ => {
                self.core.report(Diag::InvalidTraitItem { span });
                None
              }
            }
          })
          .collect::<Vec<_>>()
          .into();
        self.define_trait(span, def, vis, generics, TraitDefKind::Trait { subitems });
        Some(def)
      }

      ItemKind::Impl(impl_item) => {
        let def = self.chart_child(parent, impl_item.name, member_vis, true);
        let generics = self.chart_generics(def, impl_item.generics, true);
        let subitems = impl_item
          .items
          .into_iter()
          .filter_map(|subitem| {
            let span = subitem.span;
            if !matches!(subitem.vis, Vis::Private) {
              self.core.report(Diag::ImplItemVis { span });
            }
            match subitem.kind {
              ItemKind::Fn(fn_item) => {
                if !fn_item.generics.impls.is_empty() || !fn_item.generics.types.is_empty() {
                  self.core.report(Diag::ImplItemGen { span });
                }
                let def = self.chart_child(def, fn_item.name, vis, false);
                let body = self.ensure_implemented(span, fn_item.body);
                let kind = ValueDefKind::Fn { params: fn_item.params, ret: fn_item.ret, body };
                let value = self.define_value(span, def, vis, generics, kind, fn_item.method);
                self.chart_attrs(span, vis, Some(def), subitem.attrs);
                Some(ImplSubitem { span, name: fn_item.name, value })
              }
              ItemKind::Const(const_item) => {
                if !const_item.generics.impls.is_empty() || !const_item.generics.types.is_empty() {
                  self.core.report(Diag::ImplItemGen { span });
                }
                let def = self.chart_child(def, const_item.name, vis, false);
                let value = self.ensure_implemented(span, const_item.value);
                let kind = ValueDefKind::Const { ty: const_item.ty, value };
                let value = self.define_value(span, def, vis, generics, kind, false);
                self.chart_attrs(span, vis, Some(def), subitem.attrs);
                Some(ImplSubitem { span, name: const_item.name, value })
              }
              _ => {
                self.core.report(Diag::InvalidImplItem { span });
                None
              }
            }
          })
          .collect::<Vec<_>>()
          .into();
        self.define_impl(
          span,
          def,
          vis,
          generics,
          ImplDefKind::Impl { trait_: impl_item.trait_, subitems },
        );
        Some(def)
      }

      ItemKind::Use(use_item) => {
        let import_parent =
          if use_item.absolute { ImportParent::Root } else { ImportParent::Scope };
        for (ident, use_tree) in use_item.tree.children {
          self.chart_use_tree(parent, vis, import_parent, ident, use_tree);
        }
        None
      }

      ItemKind::Ivy(ivy_item) => {
        let def = self.chart_child(parent, ivy_item.name, vis, true);
        let generics = self.chart_generics(def, ivy_item.generics, false);
        self.define_value(
          span,
          def,
          vis,
          generics,
          ValueDefKind::Ivy { ty: ivy_item.ty, net: ivy_item.net },
          ivy_item.method,
        );
        Some(def)
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

    self.chart_attrs(span, vis, def, item.attrs);
  }

  fn chart_attrs(&mut self, span: Span, vis: DefId, def: Option<DefId>, attrs: Vec<Attr>) {
    for attr in attrs {
      match attr.kind {
        AttrKind::Builtin(builtin) => {
          if !self.chart_builtin(span, def, vis, builtin) {
            self.core.report(Diag::BadBuiltin { span: attr.span });
          }
        }
      }
    }
  }

  fn chart_builtin(
    &mut self,
    span: Span,
    def_id: Option<DefId>,
    vis: DefId,
    builtin: Builtin,
  ) -> bool {
    let Some(def_id) = def_id else { return false };
    let def = &mut self.chart.defs[def_id];

    fn set<T>(got: Option<T>, builtin: &mut Option<T>) -> bool {
      if builtin.is_none() && got.is_some() {
        *builtin = got;
        true
      } else {
        false
      }
    }
    let primitive = |self_: &mut Self, ty| {
      self_.define_type(span, def_id, vis, GenericsId::NONE, TypeDefKind::Builtin(ty));
      Some(def_id)
    };
    let adt = || def.type_def.and_then(|id| self.chart.types[id].kind.adt());

    match builtin {
      Builtin::Bool => set(primitive(self, Type::Bool), &mut self.chart.builtins.bool),
      Builtin::N32 => set(primitive(self, Type::N32), &mut self.chart.builtins.n32),
      Builtin::F32 => set(primitive(self, Type::F32), &mut self.chart.builtins.f32),
      Builtin::Char => set(primitive(self, Type::Char), &mut self.chart.builtins.char),
      Builtin::IO => set(primitive(self, Type::IO), &mut self.chart.builtins.io),
      Builtin::Prelude => set(Some(def_id), &mut self.chart.builtins.prelude),
      Builtin::List => set(adt(), &mut self.chart.builtins.list),
      Builtin::String => set(adt(), &mut self.chart.builtins.string),
      Builtin::Concat => set(def.value_def, &mut self.chart.builtins.concat),
      Builtin::ToStringTrait => set(def.trait_def, &mut self.chart.builtins.to_string_trait),
      Builtin::ToStringFn => set(def.value_def, &mut self.chart.builtins.to_string_fn),
    }
  }

  fn chart_generics(
    &mut self,
    def: DefId,
    mut generics: GenericParams<'core>,
    impl_allowed: bool,
  ) -> GenericsId {
    if !impl_allowed && !generics.impls.is_empty() {
      self.core.report(Diag::UnexpectedImplParam { span: generics.span });
      generics.impls.clear();
    }
    self.chart.generics.push(GenericsDef {
      span: generics.span,
      def,
      type_params: generics.types,
      impl_params: generics.impls,
    })
  }

  fn chart_use_tree(
    &mut self,
    def_id: DefId,
    vis: DefId,
    parent: ImportParent,
    ident: Ident<'core>,
    use_tree: UseTree<'core>,
  ) {
    let span = use_tree.span;
    let import = self.chart.imports.push(Import {
      span,
      def: def_id,
      parent,
      ident,
      state: ImportState::Unresolved,
    });
    let def = &mut self.chart.defs[def_id];
    for name in use_tree.aliases {
      if let Entry::Vacant(e) = def.members.entry(name) {
        e.insert(Member { vis, kind: MemberKind::Import(import) });
      } else {
        self.core.report(Diag::DuplicateItem { span, name });
      }
    }
    for (ident, child) in use_tree.children {
      self.chart_use_tree(def_id, vis, ImportParent::Import(import), ident, child);
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
      Member { vis, kind: MemberKind::Child(next_def_id) }
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

  fn resolve_vis(&mut self, base: DefId, vis: Vis) -> DefId {
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

  fn ensure_implemented<T: From<ErrorGuaranteed>>(&mut self, span: Span, option: Option<T>) -> T {
    option.unwrap_or_else(|| self.core.report(Diag::MissingImplementation { span }).into())
  }
}

macro_rules! define_define {
  ($(
    $define_thing:ident,
    $thing_def:ident,
    $things:ident,
    $ThingDefId:ident,
    $ThingDefKind:ty,
    $ThingDef:ident
    $(($($param:tt)*))? $({ $($field:tt)* })?
  );* $(;)?) => {
    impl<'core> Charter<'core, '_> {
      $(fn $define_thing(
        &mut self,
        span: Span,
        def: DefId,
        vis: DefId,
        generics: GenericsId,
        kind: $ThingDefKind,
        $($($param)*)?
      ) -> $ThingDefId {
        let id = self.chart.$things.push($ThingDef { span, def, vis, generics, kind, $($($field)*)? });
        let def = &mut self.chart.defs[def];
        if def.$thing_def.is_none() {
          def.$thing_def = Some(id);
        } else {
          self
            .core
            .report(Diag::DuplicateItem { span, name: def.name });
        }
        id
      })*
    }
  };
}

define_define!(
  define_value,   value_def,   values,   ValueDefId,   ValueDefKind<'core>, ValueDef (method: bool) { locals: Counter::default(), method };
  define_type,    type_def,    types,    TypeDefId,    TypeDefKind<'core>,  TypeDef;
  define_pattern, pattern_def, patterns, PatternDefId, PatternDefKind,      PatternDef;
  define_trait,   trait_def,   traits,   TraitDefId,   TraitDefKind<'core>, TraitDef;
  define_impl,    impl_def,    impls,    ImplDefId,    ImplDefKind<'core>,  ImplDef;
);

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
    self.items.push(take(item));
  }
}
