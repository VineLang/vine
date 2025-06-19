use std::{collections::hash_map::Entry, mem::replace};

use vine_util::idx::{Counter, IdxVec};

use crate::structures::{
  ast::{
    visit::VisitMut, Attr, AttrKind, Builtin, GenericParams, Generics, Ident, ImplParam, Item,
    ItemKind, ModKind, Path, Span, Trait, TraitKind, Ty, TyKind, UseTree, Vis,
  },
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
      self.chart.generics.push(GenericsDef {
        span: Span::NONE,
        def: DefId::ROOT,
        type_params: Vec::new(),
        impl_params: Vec::new(),
      });
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
      ItemKind::Fn(fn_item) => {
        let def = self.chart_child(parent, fn_item.name, member_vis, true);
        let generics = self.chart_generics(def, fn_item.generics, true);
        let body = self.ensure_implemented(span, fn_item.body);
        let fn_id = self.chart.concrete_fns.push(ConcreteFnDef {
          span,
          def,
          generics,
          method: fn_item.method,
          params: fn_item.params,
          ret_ty: fn_item.ret,
          body,
          locals: Counter::default(),
        });
        self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
        Some(def)
      }

      ItemKind::Const(const_item) => {
        let def = self.chart_child(parent, const_item.name, member_vis, true);
        let generics = self.chart_generics(def, const_item.generics, true);
        let value = self.ensure_implemented(span, const_item.value);
        let const_id = self.chart.concrete_consts.push(ConcreteConstDef {
          span,
          def,
          generics,
          ty: const_item.ty,
          value,
        });
        self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
        Some(def)
      }

      ItemKind::Struct(struct_item) => {
        let def = self.chart_child(parent, struct_item.name, member_vis, true);
        let generics = self.chart_generics(def, struct_item.generics, false);
        let data_vis = self.resolve_vis(parent, struct_item.data_vis);
        let struct_id = self.chart.structs.push(StructDef {
          span,
          def,
          generics,
          name: struct_item.name,
          data_vis,
          data: struct_item.data,
        });
        self.define_value(span, def, data_vis, DefValueKind::Struct(struct_id));
        self.define_pattern(span, def, data_vis, DefPatternKind::Struct(struct_id));
        self.define_type(span, def, vis, DefTypeKind::Struct(struct_id));
        Some(def)
      }

      ItemKind::Enum(enum_item) => {
        let def = self.chart_child(parent, enum_item.name, member_vis, true);
        let generics = self.chart_generics(def, enum_item.generics, false);
        let enum_id = self.chart.enums.next_index();
        let variants = enum_item
          .variants
          .into_iter()
          .enumerate()
          .map(|(id, variant)| {
            let variant_id = VariantId(id);
            let def = self.chart_child(def, variant.name, vis, true);
            self.define_value(span, def, vis, DefValueKind::Enum(enum_id, variant_id));
            self.define_pattern(span, def, vis, DefPatternKind::Enum(enum_id, variant_id));
            EnumVariant { span, def, name: variant.name, data: variant.data }
          })
          .collect::<Vec<_>>()
          .into();
        self.chart.enums.push(EnumDef { span, def, name: enum_item.name, generics, variants });
        self.define_type(span, def, vis, DefTypeKind::Enum(enum_id));
        Some(def)
      }

      ItemKind::Type(type_item) => {
        let def = self.chart_child(parent, type_item.name, member_vis, true);
        let generics = self.chart_generics(def, type_item.generics, false);
        let kind = match type_item.ty {
          Some(ty) => {
            let alias_id = self.chart.type_aliases.push(TypeAliasDef { span, def, generics, ty });
            DefTypeKind::Alias(alias_id)
          }
          None => {
            let opaque_id = self.chart.opaque_types.push(OpaqueTypeDef {
              span,
              def,
              generics,
              name: type_item.name,
            });
            DefTypeKind::Opaque(opaque_id)
          }
        };
        self.define_type(span, def, vis, kind);
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
        let subitem_generics = self.chart_trait_subitem_generics(span, trait_item.name, generics);
        let trait_id = self.chart.traits.next_index();
        let mut consts = IdxVec::new();
        let mut fns = IdxVec::new();
        for subitem in trait_item.items {
          let span = subitem.span;
          if !matches!(subitem.vis, Vis::Private) {
            self.core.report(Diag::TraitItemVis { span });
          }
          match subitem.kind {
            ItemKind::Const(const_item) => {
              if !const_item.generics.impls.is_empty() || !const_item.generics.types.is_empty() {
                self.core.report(Diag::TraitItemGen { span });
              }
              if const_item.value.is_some() {
                self.core.report(Diag::ImplementedTraitItem { span });
              }
              let trait_const_id =
                consts.push(TraitConst { name: const_item.name, ty: const_item.ty });
              let def = self.chart_child(def, const_item.name, vis, true);
              let kind = DefValueKind::Const(ConstId::Abstract(trait_id, trait_const_id));
              self.define_value(span, def, vis, kind);
              self.chart_attrs(Some(def), subitem.attrs);
            }
            ItemKind::Fn(fn_item) => {
              if !fn_item.generics.impls.is_empty() || !fn_item.generics.types.is_empty() {
                self.core.report(Diag::TraitItemGen { span });
              }
              if fn_item.body.is_some() {
                self.core.report(Diag::ImplementedTraitItem { span });
              }
              let trait_fn_id = fns.push(TraitFn {
                name: fn_item.name,
                method: fn_item.method,
                params: fn_item.params,
                ret_ty: fn_item.ret,
              });
              let def = self.chart_child(def, fn_item.name, vis, true);
              let kind = DefValueKind::Fn(FnId::Abstract(trait_id, trait_fn_id));
              self.define_value(span, def, vis, kind);
              self.chart_attrs(Some(def), subitem.attrs);
            }
            _ => {
              self.core.report(Diag::InvalidTraitItem { span });
            }
          }
        }
        let _trait_id = self.chart.traits.push(TraitDef {
          span,
          def,
          name: trait_item.name,
          generics,
          subitem_generics,
          consts,
          fns,
        });
        assert_eq!(_trait_id, trait_id);
        self.define_trait(span, def, vis, DefTraitKind::Trait(trait_id));
        Some(def)
      }

      ItemKind::Impl(impl_item) => {
        let def = self.chart_child(parent, impl_item.name, member_vis, true);
        let generics = self.chart_generics(def, impl_item.generics, true);
        let mut subitems = Vec::new();
        for subitem in impl_item.items {
          let span = subitem.span;
          if !matches!(subitem.vis, Vis::Private) {
            self.core.report(Diag::ImplItemVis { span });
          }
          match subitem.kind {
            ItemKind::Const(const_item) => {
              if !const_item.generics.impls.is_empty() || !const_item.generics.types.is_empty() {
                self.core.report(Diag::ImplItemGen { span });
              }
              let def = self.chart_child(def, const_item.name, vis, false);
              let value = self.ensure_implemented(span, const_item.value);
              let const_id = self.chart.concrete_consts.push(ConcreteConstDef {
                span,
                def,
                generics,
                ty: const_item.ty,
                value,
              });
              self.define_value(span, def, vis, DefValueKind::Const(ConstId::Concrete(const_id)));
              self.chart_attrs(Some(def), subitem.attrs);
              subitems.push(ImplSubitem {
                span,
                name: const_item.name,
                kind: ImplSubitemKind::Const(const_id),
              })
            }
            ItemKind::Fn(fn_item) => {
              if !fn_item.generics.impls.is_empty() || !fn_item.generics.types.is_empty() {
                self.core.report(Diag::ImplItemGen { span });
              }
              if fn_item.method {
                self.core.report(Diag::ImplItemMethod { span });
              }
              let def = self.chart_child(def, fn_item.name, vis, false);
              let body = self.ensure_implemented(span, fn_item.body);
              let fn_id = self.chart.concrete_fns.push(ConcreteFnDef {
                span,
                def,
                generics,
                method: fn_item.method,
                params: fn_item.params,
                ret_ty: fn_item.ret,
                body,
                locals: Counter::default(),
              });
              self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
              self.chart_attrs(Some(def), subitem.attrs);
              subitems.push(ImplSubitem {
                span,
                name: fn_item.name,
                kind: ImplSubitemKind::Fn(fn_id),
              });
            }
            _ => {
              self.core.report(Diag::InvalidImplItem { span });
            }
          }
        }
        let impl_id = self.chart.impls.push(ImplDef {
          span,
          def,
          generics,
          trait_: impl_item.trait_,
          subitems,
        });
        self.define_impl(span, def, vis, DefImplKind::Impl(impl_id));
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

  fn chart_attrs(&mut self, def: Option<DefId>, attrs: Vec<Attr>) {
    for attr in attrs {
      match attr.kind {
        AttrKind::Builtin(builtin) => {
          if !self.chart_builtin(def, builtin) {
            self.core.report(Diag::BadBuiltin { span: attr.span });
          }
        }
        AttrKind::Main => {
          self.chart.main_mod = def;
        }
      }
    }
  }

  fn chart_builtin(&mut self, def_id: Option<DefId>, builtin: Builtin) -> bool {
    let Some(def_id) = def_id else { return false };
    let def = &mut self.chart.defs[def_id];

    fn set<T>(builtin: &mut Option<T>, got: Option<T>) -> bool {
      if builtin.is_none() && got.is_some() {
        *builtin = got;
        true
      } else {
        false
      }
    }
    let opaque_type_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Opaque(id), .. }) => Some(id),
      _ => None,
    };
    let struct_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Struct(id), .. }) => Some(id),
      _ => None,
    };
    let enum_id = match def.type_kind {
      Some(WithVis { kind: DefTypeKind::Enum(id), .. }) => Some(id),
      _ => None,
    };
    let fn_id = match def.value_kind {
      Some(WithVis { kind: DefValueKind::Fn(kind), .. }) => Some(kind),
      _ => None,
    };
    let trait_id = match def.trait_kind {
      Some(WithVis { kind: DefTraitKind::Trait(id), .. }) => Some(id),
      _ => None,
    };
    let impl_id = match def.impl_kind {
      Some(WithVis { kind: DefImplKind::Impl(id), .. }) => Some(id),
      _ => None,
    };

    let builtins = &mut self.chart.builtins;
    match builtin {
      Builtin::Prelude => set(&mut builtins.prelude, Some(def_id)),
      Builtin::Bool => set(&mut builtins.bool, opaque_type_id),
      Builtin::N32 => set(&mut builtins.n32, opaque_type_id),
      Builtin::I32 => set(&mut builtins.i32, opaque_type_id),
      Builtin::F32 => set(&mut builtins.f32, opaque_type_id),
      Builtin::Char => set(&mut builtins.char, opaque_type_id),
      Builtin::IO => set(&mut builtins.io, opaque_type_id),
      Builtin::List => set(&mut builtins.list, struct_id),
      Builtin::String => set(&mut builtins.string, struct_id),
      Builtin::Result => set(&mut builtins.result, enum_id),
      Builtin::Neg => set(&mut builtins.neg, fn_id),
      Builtin::Not => set(&mut builtins.not, fn_id),
      Builtin::Cast => set(&mut builtins.cast, fn_id),
      Builtin::Fork => set(&mut builtins.fork, trait_id),
      Builtin::Drop => set(&mut builtins.drop, trait_id),
      Builtin::Duplicate => set(&mut builtins.duplicate, impl_id),
      Builtin::Erase => set(&mut builtins.erase, impl_id),
      Builtin::BoolNot => set(&mut builtins.bool_not, impl_id),
      Builtin::BinaryOp(op) => set(builtins.binary_ops.entry(op).or_default(), fn_id),
      Builtin::ComparisonOp(op) => set(builtins.comparison_ops.entry(op).or_default(), fn_id),
      Builtin::Range => set(&mut builtins.range, struct_id),
      Builtin::BoundUnbounded => set(&mut builtins.bound_unbounded, struct_id),
      Builtin::BoundExclusive => set(&mut builtins.bound_exclusive, struct_id),
      Builtin::BoundInclusive => set(&mut builtins.bound_inclusive, struct_id),
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

  fn chart_trait_subitem_generics(
    &mut self,
    span: Span,
    trait_name: Ident<'core>,
    generics: GenericsId,
  ) -> GenericsId {
    let generics = self.chart.generics[generics].clone();
    self.chart.generics.push(GenericsDef {
      impl_params: vec![ImplParam {
        span,
        name: None,
        trait_: Trait {
          span,
          kind: TraitKind::Path(Path {
            span,
            absolute: false,
            segments: vec![trait_name],
            generics: Some(Generics {
              span,
              types: generics
                .type_params
                .iter()
                .map(|param| Ty {
                  span,
                  kind: TyKind::Path(Path {
                    span,
                    absolute: false,
                    segments: vec![param.name],
                    generics: None,
                  }),
                })
                .collect(),
              impls: vec![],
            }),
          }),
        },
      }],
      ..generics
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
    let import = self.chart.imports.push(ImportDef { span, def: def_id, parent, ident });
    let def = &mut self.chart.defs[def_id];
    for name in use_tree.aliases {
      if let Entry::Vacant(e) = def.members.entry(name) {
        e.insert(WithVis { vis, kind: MemberKind::Import(import) });
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
      WithVis { vis, kind: MemberKind::Child(next_def_id) }
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

  fn define_value(&mut self, span: Span, def: DefId, vis: DefId, kind: DefValueKind) {
    let def = &mut self.chart.defs[def];
    if def.value_kind.is_none() {
      def.value_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  fn define_type(&mut self, span: Span, def: DefId, vis: DefId, kind: DefTypeKind) {
    let def = &mut self.chart.defs[def];
    if def.type_kind.is_none() {
      def.type_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  fn define_pattern(&mut self, span: Span, def: DefId, vis: DefId, kind: DefPatternKind) {
    let def = &mut self.chart.defs[def];
    if def.pattern_kind.is_none() {
      def.pattern_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  fn define_trait(&mut self, span: Span, def: DefId, vis: DefId, kind: DefTraitKind) {
    let def = &mut self.chart.defs[def];
    if def.trait_kind.is_none() {
      def.trait_kind = Some(WithVis { vis, kind });
    } else {
      self.core.report(Diag::DuplicateItem { span, name: def.name });
    }
  }

  fn define_impl(&mut self, span: Span, def: DefId, vis: DefId, kind: DefImplKind) {
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
