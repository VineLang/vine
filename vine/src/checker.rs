use std::{
  collections::BTreeMap,
  mem::{replace, swap, take},
};

use vine_util::idx::{IdxVec, IntMap, RangeExt};

use crate::{
  ast::{
    Block, GenericArgs, Impl, ImplKind, Key, LabelId, LetFnId, Local, Pat, PatKind, Span, StmtKind,
    Trait, TraitKind, Ty, TyKind,
  },
  chart::{
    Chart, ChartCheckpoint, DefId, EnumDef, EnumId, GenericsDef, GenericsId, ImplDef, ImplDefId,
    ImplDefKind, StructDef, StructId, TraitDef, TraitDefId, TraitDefKind, TraitSubitemKind,
    TypeDefId, TypeDefKind, ValueDef, ValueDefId, ValueDefKind,
  },
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  finder::Finder,
  signatures::{EnumSig, GenericsSig, ImplSig, Signatures, StructSig, TraitSig, TypeSig, ValueSig},
  types::{ImplType, Type, TypeCtx, TypeKind, Types},
};

mod check_expr;
mod check_pat;

#[derive(Debug)]
pub struct Checker<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a mut Chart<'core>,
  sigs: &'a mut Signatures<'core>,
  types: Types<'core>,
  locals: IntMap<Local, Type>,
  let_fns: IntMap<LetFnId, Type>,
  return_ty: Option<Type>,
  labels: IdxVec<LabelId, Option<Type>>,
  cur_def: DefId,
  cur_generics: GenericsId,
}

impl<'core, 'a> Checker<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a mut Chart<'core>,
    types: &'a mut Signatures<'core>,
  ) -> Self {
    Checker {
      core,
      chart,
      sigs: types,
      types: Types::default(),
      locals: Default::default(),
      let_fns: Default::default(),
      return_ty: None,
      labels: Default::default(),
      cur_def: DefId::ROOT,
      cur_generics: GenericsId::NONE,
    }
  }

  pub fn check_all(&mut self) {
    self.check_since(&ChartCheckpoint::default());
  }

  pub(crate) fn check_since(&mut self, checkpoint: &ChartCheckpoint) {
    for id in self.chart.types.keys_from(checkpoint.types) {
      self.assess_type_def(id);
    }
    for id in self.chart.structs.keys_from(checkpoint.structs) {
      self.assess_struct_type(id);
    }
    for id in self.chart.enums.keys_from(checkpoint.enums) {
      self.assess_enum_type(id);
    }
    for id in self.chart.traits.keys_from(checkpoint.traits) {
      self.assess_trait_type(id);
    }
    for id in self.chart.generics.keys_from(checkpoint.generics) {
      self.assess_impl_params(id);
    }
    for id in self.chart.values.keys_from(checkpoint.values) {
      self.assess_value_type(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.assess_impl_type(id);
    }
    for id in self.chart.values.keys_from(checkpoint.values) {
      self.check_value_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.check_impl_def(id);
    }
  }

  fn initialize(&mut self, def_id: DefId, generics_id: GenericsId) {
    assert!(self.return_ty.is_none());
    self.labels.clear();
    self.types.reset();
    self.locals.clear();
    self.let_fns.clear();

    self.cur_def = def_id;
    self.cur_generics = generics_id;
  }

  fn assess_type_def(&mut self, type_id: TypeDefId) {
    if let Some(Some(_)) = self.sigs.types.get(type_id) {
      return;
    }
    let type_def = &mut self.chart.types[type_id];
    let span = type_def.span;
    let prev_types = take(&mut self.types);
    let prev_def = replace(&mut self.cur_def, type_def.def);
    let prev_generics = replace(&mut self.cur_generics, type_def.generics);
    let mut kind = take(&mut type_def.kind);
    let params = self.all_params(self.chart.types[type_id].generics);
    let ty = match &mut kind {
      TypeDefKind::Taken => self.types.error(self.core.report(Diag::RecursiveTypeAlias { span })),
      TypeDefKind::Alias(ty) => self.hydrate_type(ty, false),
      TypeDefKind::Opaque => self.types.new(TypeKind::Opaque(type_id, params)),
      TypeDefKind::Struct(struct_id) => self.types.new(TypeKind::Struct(*struct_id, params)),
      TypeDefKind::Enum(enum_id) => self.types.new(TypeKind::Enum(*enum_id, params)),
    };
    self.chart.types[type_id].kind = kind;
    let slot = self.sigs.types.get_or_extend(type_id);
    if slot.is_none() {
      *slot = Some(self.types.export(|t| TypeSig { ty: t.transfer(ty) }));
    }
    self.cur_def = prev_def;
    self.cur_generics = prev_generics;
    self.types = prev_types;
  }

  fn check_value_def(&mut self, value_id: ValueDefId) {
    let ValueDef { def, generics, ref mut kind, .. } = self.chart.values[value_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    match &mut kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { value, .. } => {
        let ty = self.types.import(&self.sigs.values[value_id], None, |t, sig| t.transfer(sig.ty));
        self.check_expr_form_type(value, Form::Value, ty)
      }
      ValueDefKind::Fn { params, body, .. } => {
        for pat in params {
          self.check_pat(pat, Form::Value, false);
        }
        let ty = self.types.import(&self.sigs.values[value_id], None, |t, sig| t.transfer(sig.ty));
        let Some((_, &TypeKind::Fn(_, ret))) = self.types.kind(ty) else { unreachable!() };
        let old = self.return_ty.replace(ret);
        self.check_block_type(body, ret);
        self.return_ty = old;
      }
      ValueDefKind::Struct(..) | ValueDefKind::Enum(..) | ValueDefKind::TraitSubitem(..) => {}
    }
    self.chart.values[value_id].kind = kind;
  }

  pub(crate) fn _check_custom(
    &mut self,
    def_id: DefId,
    types: &mut Types<'core>,
    locals: &mut IntMap<Local, Type>,
    block: &mut Block<'core>,
  ) -> Type {
    self.cur_def = def_id;
    swap(types, &mut self.types);
    swap(locals, &mut self.locals);
    let ty = self.check_block(block);
    swap(types, &mut self.types);
    swap(locals, &mut self.locals);
    ty
  }

  fn check_block(&mut self, block: &mut Block<'core>) -> Type {
    let mut ty = self.types.nil();
    for stmt in block.stmts.iter_mut() {
      ty = self.types.nil();
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let refutable = l.else_block.is_some();
          let let_ty = self.check_pat(&mut l.bind, Form::Value, refutable);
          if let Some(value) = &mut l.init {
            self.check_expr_form_type(value, Form::Value, let_ty);
          }
          if let Some(block) = &mut l.else_block {
            let never = self.types.new(TypeKind::Never);
            self.check_block_type(block, never);
          }
        }
        StmtKind::LetFn(d) => {
          let params = d.params.iter_mut().map(|p| self.check_pat(p, Form::Value, false)).collect();
          let ret = d
            .ret
            .as_mut()
            .map(|t| self.hydrate_type(t, true))
            .unwrap_or_else(|| self.types.new_var(d.body.span));
          let old = self.return_ty.replace(ret);
          self.check_block_type(&mut d.body, ret);
          self.return_ty = old;
          let ty = self.types.new(TypeKind::Fn(params, ret));
          self.let_fns.insert(d.id.unwrap(), ty);
        }
        StmtKind::Expr(e, semi) => {
          ty = self.check_expr_form(e, Form::Value);
          if *semi {
            ty = self.types.nil();
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => {}
      }
    }
    ty
  }

  fn check_block_type(&mut self, block: &mut Block<'core>, ty: Type) {
    let found = self.check_block(block);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: block.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  fn hydrate_type(&mut self, ty: &mut Ty<'core>, inference: bool) -> Type {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.types.new_var(span),
      TyKind::Paren(t) => self.hydrate_type(t, inference),
      TyKind::Hole => self.types.error(self.core.report(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => {
        let params = args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect();
        let ret =
          ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(self.types.nil());
        self.types.new(TypeKind::Fn(params, ret))
      }
      TyKind::Tuple(tys) => {
        let tys = tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect();
        self.types.new(TypeKind::Tuple(tys))
      }
      TyKind::Object(entries) => {
        self.build_object_type(entries, |self_, t| self_.hydrate_type(t, inference))
      }
      TyKind::Ref(inner) => {
        let inner = self.hydrate_type(inner, inference);
        self.types.new(TypeKind::Ref(inner))
      }
      TyKind::Inverse(inner) => {
        let inner = self.hydrate_type(inner, inference);
        inner.inverse()
      }
      TyKind::Path(_) => unreachable!(),
      TyKind::Def(type_def_id, generics) => {
        let type_params =
          self.check_generics(generics, self.chart.types[*type_def_id].generics, inference);
        self.assess_type_def(*type_def_id);
        self.types.import(
          self.sigs.types[*type_def_id].as_ref().unwrap(),
          Some(&type_params),
          |t, sig| t.transfer(sig.ty),
        )
      }
      TyKind::Param(index) => {
        let name = self.chart.generics[self.cur_generics].type_params[*index].name;
        self.types.new(TypeKind::Param(*index, name))
      }
      TyKind::Error(e) => self.types.error(*e),
    }
  }

  fn hydrate_param(&mut self, pat: &mut Pat<'core>) -> Type {
    let span = pat.span;
    match &mut pat.kind {
      PatKind::Path(..) => unreachable!(),
      PatKind::Paren(inner) => self.hydrate_param(inner),
      PatKind::Annotation(_, ty) => self.hydrate_type(ty, false),
      PatKind::Struct(struct_id, generics, _) => {
        let type_params =
          self.check_generics(generics, self.chart.structs[*struct_id].generics, false);
        self.types.new(TypeKind::Struct(*struct_id, type_params))
      }
      PatKind::Enum(enum_id, _, generics, _) => {
        let type_params = self.check_generics(generics, self.chart.enums[*enum_id].generics, false);
        self.types.new(TypeKind::Enum(*enum_id, type_params))
      }
      PatKind::Ref(inner) => {
        let inner = self.hydrate_param(inner);
        self.types.new(TypeKind::Ref(inner))
      }
      PatKind::Inverse(inner) => {
        let inner = self.hydrate_param(inner);
        inner.inverse()
      }
      PatKind::Tuple(els) => {
        let els = els.iter_mut().map(|p| self.hydrate_param(p)).collect();
        self.types.new(TypeKind::Tuple(els))
      }
      PatKind::Object(entries) => self.build_object_type(entries, Self::hydrate_param),
      PatKind::Hole | PatKind::Local(_) | PatKind::Deref(_) => {
        self.types.error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => self.types.error(*e),
    }
  }

  fn check_impl_def(&mut self, impl_id: ImplDefId) {
    let ImplDef { span, def, generics, ref mut kind, .. } = self.chart.impls[impl_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    match &mut kind {
      ImplDefKind::Taken => unreachable!(),
      ImplDefKind::Impl { subitems, .. } => {
        let ty = self
          .types
          .import(&self.sigs.impls[impl_id], None, |t, sig| t.transfer_impl_type(&sig.ty));
        if let ImplType::Trait(trait_id, trait_type_params) = ty {
          let trait_sig = &self.sigs.traits[trait_id];
          subitems.vec.sort_by_key(|s| trait_sig.inner.lookup.get(&s.name));
          if trait_sig.inner.lookup.len() > subitems.len() {
            self.core.report(Diag::IncompleteImpl { span });
          }
          for subitem in subitems.values_mut() {
            let trait_sig = &self.sigs.traits[trait_id];
            if let Some(&subitem_id) = trait_sig.inner.lookup.get(&subitem.name) {
              let trait_ty = self.types.import(trait_sig, Some(&trait_type_params), |t, sig| {
                t.transfer(sig.subitem_types[subitem_id])
              });
              let ty = self
                .types
                .import(&self.sigs.values[subitem.value], None, |t, sig| t.transfer(sig.ty));
              if self.types.unify(ty, trait_ty).is_failure() {
                self.core.report(Diag::ExpectedTypeFound {
                  span: subitem.span,
                  expected: self.types.show(self.chart, trait_ty),
                  found: self.types.show(self.chart, ty),
                });
              }
            } else {
              self.core.report(Diag::ExtraneousImplItem { span: subitem.span, name: subitem.name });
            }
          }
        }
      }
    }
    self.chart.impls[impl_id].kind = kind;
  }

  fn build_object_type<T>(
    &mut self,
    entries: &mut Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut T) -> Type,
  ) -> Type {
    let mut fields = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = fields.insert(key.ident, f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    if let Err(err) = duplicate {
      self.types.error(err)
    } else {
      self.types.new(TypeKind::Object(fields))
    }
  }

  fn assess_struct_type(&mut self, struct_id: StructId) {
    assert_eq!(self.sigs.structs.next_index(), struct_id);
    let StructDef { def, generics, ref mut data, .. } = self.chart.structs[struct_id];
    let mut data = take(data);
    self.initialize(def, generics);
    let data_ty = self.hydrate_type(&mut data, false);
    self
      .sigs
      .structs
      .push(TypeCtx { types: take(&mut self.types), inner: StructSig { data: data_ty } });
    self.chart.structs[struct_id].data = data;
  }

  fn assess_enum_type(&mut self, enum_id: EnumId) {
    assert_eq!(self.sigs.enums.next_index(), enum_id);
    let EnumDef { def, generics, ref mut variants, .. } = self.chart.enums[enum_id];
    let mut variants = take(variants);
    self.initialize(def, generics);
    let variant_data = variants
      .values_mut()
      .map(|variant| variant.data.as_mut().map(|ty| self.hydrate_type(ty, false)))
      .collect::<Vec<_>>()
      .into();
    self.sigs.enums.push(TypeCtx { types: take(&mut self.types), inner: EnumSig { variant_data } });
    self.chart.enums[enum_id].variants = variants;
  }

  fn assess_trait_type(&mut self, trait_id: TraitDefId) {
    assert_eq!(self.sigs.traits.next_index(), trait_id);
    let TraitDef { def, generics, ref mut kind, .. } = self.chart.traits[trait_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let sig = match &mut kind {
      TraitDefKind::Taken => unreachable!(),
      TraitDefKind::Trait { subitems } => TraitSig {
        lookup: subitems.iter_mut().map(|(id, subitem)| (subitem.name, id)).collect(),
        subitem_types: subitems
          .values_mut()
          .map(|subitem| match &mut subitem.kind {
            TraitSubitemKind::Fn(params, ret) => self.assess_fn_sig(params, ret),
            TraitSubitemKind::Const(ty) => self.hydrate_type(ty, false),
          })
          .collect::<Vec<_>>()
          .into(),
      },
    };
    self.sigs.traits.push(TypeCtx { types: take(&mut self.types), inner: sig });
    self.chart.traits[trait_id].kind = kind;
  }

  fn assess_impl_params(&mut self, generics_id: GenericsId) {
    assert_eq!(self.sigs.generics.next_index(), generics_id);
    let GenericsDef { def, ref mut type_params, ref mut impl_params, .. } =
      self.chart.generics[generics_id];
    let type_params = take(type_params);
    let mut impl_params = take(impl_params);
    self.initialize(def, generics_id);
    let mut impl_param_types = vec![];
    impl_param_types.extend(
      type_params
        .iter()
        .enumerate()
        .flat_map(|(i, param)| {
          let span = param.span;
          let ty = self.types.new(TypeKind::Param(i, param.name));
          [
            param.flex.fork().then(|| {
              if let Some(fork) = self.chart.builtins.fork {
                ImplType::Trait(fork, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Fork" }))
              }
            }),
            param.flex.drop().then(|| {
              if let Some(drop) = self.chart.builtins.drop {
                ImplType::Trait(drop, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Drop" }))
              }
            }),
          ]
        })
        .flatten(),
    );
    self.chart.generics[generics_id].type_params = type_params;
    impl_param_types.extend(impl_params.iter_mut().map(|p| self.assess_trait(&mut p.trait_)));
    self.sigs.generics.push(TypeCtx {
      types: take(&mut self.types),
      inner: GenericsSig { impl_params: impl_param_types },
    });
    self.chart.generics[generics_id].impl_params = impl_params;
  }

  fn assess_value_type(&mut self, value_id: ValueDefId) {
    assert_eq!(self.sigs.values.next_index(), value_id);
    let ValueDef { def, generics, ref mut kind, .. } = self.chart.values[value_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let ty = match &mut kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { ty, .. } => self.hydrate_type(ty, false),
      ValueDefKind::Fn { params, ret, .. } => self.assess_fn_sig(params, ret),
      ValueDefKind::Struct(struct_id) => {
        let struct_def = &self.chart.structs[*struct_id];
        let data =
          self.types.import(&self.sigs.structs[*struct_id], None, |t, sig| t.transfer(sig.data));
        let params = self.all_params(struct_def.generics);
        let struct_ty = self.types.new(TypeKind::Struct(*struct_id, params));
        self.types.new(TypeKind::Fn(vec![data], struct_ty))
      }
      ValueDefKind::Enum(enum_id, variant_id) => {
        let enum_def = &self.chart.enums[*enum_id];
        let data = self.types.import(&self.sigs.enums[*enum_id], None, |t, sig| {
          Some(t.transfer(sig.variant_data[*variant_id]?))
        });
        let params = self.all_params(enum_def.generics);
        let enum_ty = self.types.new(TypeKind::Enum(*enum_id, params));
        if let Some(data) = data {
          self.types.new(TypeKind::Fn(vec![data], enum_ty))
        } else {
          enum_ty
        }
      }
      ValueDefKind::TraitSubitem(trait_def_id, subitem_id) => {
        self.types.import(&self.sigs.traits[*trait_def_id], None, |t, sig| {
          t.transfer(sig.subitem_types[*subitem_id])
        })
      }
    };
    self.sigs.values.push(TypeCtx { types: take(&mut self.types), inner: ValueSig { ty } });
    self.chart.values[value_id].kind = kind;
  }

  fn assess_impl_type(&mut self, impl_id: ImplDefId) {
    assert_eq!(self.sigs.impls.next_index(), impl_id);
    let ImplDef { def, generics, ref mut kind, .. } = self.chart.impls[impl_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let ty = match &mut kind {
      ImplDefKind::Taken => unreachable!(),
      ImplDefKind::Impl { trait_, .. } => self.assess_trait(trait_),
    };
    self.sigs.impls.push(TypeCtx { types: take(&mut self.types), inner: ImplSig { ty } });
    self.chart.impls[impl_id].kind = kind;
  }

  fn assess_fn_sig(&mut self, params: &mut [Pat<'core>], ret: &mut Option<Ty<'core>>) -> Type {
    let params = params.iter_mut().map(|p| self.hydrate_param(p)).collect();
    let ret = ret.as_mut().map(|t| self.hydrate_type(t, false)).unwrap_or(self.types.nil());
    self.types.new(TypeKind::Fn(params, ret))
  }

  fn assess_trait(&mut self, trait_: &mut Trait<'core>) -> ImplType {
    match &mut trait_.kind {
      TraitKind::Path(_) => unreachable!(),
      TraitKind::Def(id, generics) => {
        let type_params = self.check_generics(generics, self.chart.traits[*id].generics, false);
        ImplType::Trait(*id, type_params)
      }
      TraitKind::Error(e) => ImplType::Error(*e),
    }
  }

  fn check_impl_type(&mut self, impl_: &mut Impl<'core>, ty: &ImplType) {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Hole => {
        *impl_ = self.find_impl(span, ty);
      }
      _ => {
        let found = self.check_impl(impl_);
        if self.types.unify_impl_type(&found, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: impl_.span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &found),
          });
        }
      }
    }
  }

  fn check_impl(&mut self, impl_: &mut Impl<'core>) -> ImplType {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Path(_) => unreachable!(),
      ImplKind::Hole => ImplType::Error(self.core.report(Diag::UnspecifiedImpl { span })),
      ImplKind::Param(n) => {
        self.types.import(&self.sigs.generics[self.cur_generics], None, |t, sig| {
          t.transfer_impl_type(&sig.impl_params[*n])
        })
      }
      ImplKind::Def(id, generics) => {
        let type_params = self.check_generics(generics, self.chart.impls[*id].generics, true);
        self
          .types
          .import(&self.sigs.impls[*id], Some(&type_params), |t, sig| t.transfer_impl_type(&sig.ty))
      }
      ImplKind::Error(e) => ImplType::Error(*e),
    }
  }

  pub fn check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
  ) -> Vec<Type> {
    self._check_generics(args, params_id, inference, None)
  }

  fn _check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
    type_params: Option<Vec<Type>>,
  ) -> Vec<Type> {
    let params = &self.chart.generics[params_id];
    let check_count = |got, expected, kind| {
      if got != expected {
        self.core.report(Diag::BadGenericCount {
          span: args.span,
          path: self.chart.defs[params.def].path,
          expected,
          got,
          kind,
        });
      }
    };
    if !inference || !args.types.is_empty() {
      check_count(args.types.len(), params.type_params.len(), "type");
    }
    if !args.impls.is_empty() {
      check_count(args.impls.len(), params.impl_params.len(), "impl");
    }
    let has_impl_params = !params.impl_params.is_empty();
    let type_param_count = params.type_params.len();
    let type_params = if let Some(type_params) = type_params {
      for (a, b) in type_params.iter().zip(args.types.iter_mut()) {
        let b = self.hydrate_type(b, inference);
        _ = self.types.unify(*a, b);
      }
      type_params
    } else {
      (0..type_param_count)
        .iter()
        .map(|i| {
          args.types.get_mut(i).map(|t| self.hydrate_type(t, inference)).unwrap_or_else(|| {
            if inference {
              self.types.new_var(args.span)
            } else {
              self.types.error(ErrorGuaranteed::new_unchecked())
            }
          })
        })
        .collect::<Vec<_>>()
    };
    if has_impl_params {
      let impl_params_types =
        self.types.import(&self.sigs.generics[params_id], Some(&type_params), |t, sig| {
          sig.impl_params.iter().map(|ty| t.transfer_impl_type(ty)).collect::<Vec<_>>()
        });
      if args.impls.is_empty() {
        args.impls =
          impl_params_types.into_iter().map(|ty| self.find_impl(args.span, &ty)).collect();
      } else {
        for (impl_, param_type) in args.impls.iter_mut().zip(impl_params_types.into_iter()) {
          self.check_impl_type(impl_, &param_type);
        }
      }
    }
    type_params
  }

  fn finder(&self, span: Span) -> Finder<'core, '_> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
  }

  fn find_impl(&mut self, span: Span, ty: &ImplType) -> Impl<'core> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
      .find_impl(&mut self.types, ty)
  }

  fn all_params(&mut self, generics: GenericsId) -> Vec<Type> {
    self.chart.generics[generics]
      .type_params
      .iter()
      .enumerate()
      .map(|(i, p)| self.types.new(TypeKind::Param(i, p.name)))
      .collect::<Vec<_>>()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
  Value,
  Place,
  Space,
  Error(ErrorGuaranteed),
}

impl Form {
  fn inverse(self) -> Self {
    match self {
      Form::Value => Form::Space,
      Form::Place => Form::Place,
      Form::Space => Form::Value,
      Form::Error(e) => Form::Error(e),
    }
  }
}
