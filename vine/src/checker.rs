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
    checkpoint::ChartCheckpoint, Chart, ConcreteConstId, ConcreteFnId, DefId, EnumId, GenericsDef,
    GenericsId, ImplId, ImplSubitemKind, StructId, TraitId, TypeAliasId,
  },
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  finder::Finder,
  signatures::{
    ConstSig, EnumSig, FnSig, GenericsSig, ImplSig, Signatures, StructSig, TraitSig, TypeAliasSig,
  },
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
    for id in self.chart.type_aliases.keys_from(checkpoint.type_aliases) {
      self.resolve_type_alias(id);
    }
    for id in self.chart.structs.keys_from(checkpoint.structs) {
      self.resolve_struct_sig(id);
    }
    for id in self.chart.enums.keys_from(checkpoint.enums) {
      self.resolve_enum_sig(id);
    }
    for id in self.chart.traits.keys_from(checkpoint.traits) {
      self.resolve_trait_sig(id);
    }
    for id in self.chart.generics.keys_from(checkpoint.generics) {
      self.resolve_generics_sig(id);
    }
    for id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      self.resolve_const_sig(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.resolve_fn_sig(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.resolve_impl_sig(id);
    }
    for id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      self.check_const_def(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.check_fn_def(id);
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

  fn resolve_type_alias(&mut self, alias_id: TypeAliasId) {
    if let Some(Some(_)) = self.sigs.type_aliases.get(alias_id) {
      return;
    }
    let mut alias_def = self.chart.type_aliases[alias_id].clone();
    let prev_types = take(&mut self.types);
    let prev_def = replace(&mut self.cur_def, alias_def.def);
    let prev_generics = replace(&mut self.cur_generics, alias_def.generics);
    let ty = self.hydrate_type(&mut alias_def.ty, false);
    self.chart.type_aliases[alias_id] = alias_def;
    let slot = self.sigs.type_aliases.get_or_extend(alias_id);
    if slot.is_none() {
      *slot = Some(self.types.export(|t| TypeAliasSig { ty: t.transfer(&ty) }));
    }
    self.cur_def = prev_def;
    self.cur_generics = prev_generics;
    self.types = prev_types;
  }

  fn check_const_def(&mut self, const_id: ConcreteConstId) {
    let mut const_def = self.chart.concrete_consts[const_id].clone();
    self.initialize(const_def.def, const_def.generics);
    let ty = self.types.import(&self.sigs.concrete_consts[const_id], None).ty;
    self.check_expr_form_type(&mut const_def.value, Form::Value, ty);
    self.chart.concrete_consts[const_id] = const_def;
  }

  fn check_fn_def(&mut self, fn_id: ConcreteFnId) {
    let mut fn_def = self.chart.concrete_fns[fn_id].clone();
    self.initialize(fn_def.def, fn_def.generics);
    let ret_ty = self
      .types
      .import_with(&self.sigs.concrete_fns[fn_id], None, |t, sig| t.transfer(&sig.ret_ty));
    for pat in fn_def.params.iter_mut() {
      self.check_pat(pat, Form::Value, false);
    }
    self.return_ty = Some(ret_ty);
    self.check_block_type(&mut fn_def.body, ret_ty);
    self.return_ty = None;
    self.chart.concrete_fns[fn_id] = fn_def;
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
      TyKind::Opaque(opaque_id, generics) => {
        let generics_id = self.chart.opaque_types[*opaque_id].generics;
        let type_params = self.check_generics(generics, generics_id, inference);
        self.types.new(TypeKind::Opaque(*opaque_id, type_params))
      }
      TyKind::Struct(struct_id, generics) => {
        let generics_id = self.chart.structs[*struct_id].generics;
        let type_params = self.check_generics(generics, generics_id, inference);
        self.types.new(TypeKind::Struct(*struct_id, type_params))
      }
      TyKind::Enum(enum_id, generics) => {
        let generics_id = self.chart.enums[*enum_id].generics;
        let type_params = self.check_generics(generics, generics_id, inference);
        self.types.new(TypeKind::Enum(*enum_id, type_params))
      }
      TyKind::Alias(type_alias_id, generics) => {
        let generics_id = self.chart.type_aliases[*type_alias_id].generics;
        let type_params = self.check_generics(generics, generics_id, inference);
        self.resolve_type_alias(*type_alias_id);
        self
          .types
          .import(self.sigs.type_aliases[*type_alias_id].as_ref().unwrap(), Some(&type_params))
          .ty
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

  fn check_impl_def(&mut self, impl_id: ImplId) {
    let mut impl_def = self.chart.impls[impl_id].clone();
    self.initialize(impl_def.def, impl_def.generics);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    if let ImplType::Trait(trait_id, type_params) = ty {
      let trait_def = &self.chart.traits[trait_id];
      let trait_sig = &self.sigs.traits[trait_id];
      let consts = IdxVec::from(Vec::from_iter(trait_sig.consts.iter().map(|(id, sig)| {
        let name = trait_def.consts[id].name;
        let Some(subitem) = impl_def.subitems.iter().find(|i| i.name == name) else {
          return Err(self.core.report(Diag::IncompleteImpl { span, name }));
        };
        let span = subitem.span;
        let ImplSubitemKind::Const(const_id) = subitem.kind else {
          return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "const" }));
        };
        let expected_ty = self.types.import(sig, Some(&type_params)).ty;
        let const_sig = &self.sigs.concrete_consts[const_id];
        let found_ty = self.types.import(const_sig, None).ty;
        if self.types.unify(expected_ty, found_ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show(self.chart, expected_ty),
            found: self.types.show(self.chart, found_ty),
          });
        }
        Ok(const_id)
      })));
      let fns = IdxVec::from(Vec::from_iter(trait_sig.fns.iter().map(|(id, sig)| {
        let name = trait_def.fns[id].name;
        let Some(subitem) = impl_def.subitems.iter().find(|i| i.name == name) else {
          return Err(self.core.report(Diag::IncompleteImpl { span, name }));
        };
        let span = subitem.span;
        let ImplSubitemKind::Fn(fn_id) = subitem.kind else {
          return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "fn" }));
        };
        let expected_ty = {
          let sig = self.types.import(sig, Some(&type_params));
          self.types.new(TypeKind::Fn(sig.params, sig.ret_ty))
        };
        let fn_sig = &self.sigs.concrete_fns[fn_id];
        let found_ty = {
          let sig = self.types.import(fn_sig, None);
          self.types.new(TypeKind::Fn(sig.params, sig.ret_ty))
        };
        if self.types.unify(expected_ty, found_ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show(self.chart, expected_ty),
            found: self.types.show(self.chart, found_ty),
          });
        }
        Ok(fn_id)
      })));
      for item in impl_def.subitems.iter() {
        if trait_def.consts.values().all(|x| x.name != item.name)
          && trait_def.fns.values().all(|x| x.name != item.name)
        {
          self.core.report(Diag::ExtraneousImplItem { span: item.span, name: item.name });
        }
      }
      impl_def.consts = consts;
      impl_def.fns = fns;
    }
    self.chart.impls[impl_id] = impl_def;
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

  fn resolve_struct_sig(&mut self, struct_id: StructId) {
    assert_eq!(self.sigs.structs.next_index(), struct_id);
    let mut struct_def = self.chart.structs[struct_id].clone();
    self.initialize(struct_def.def, struct_def.generics);
    let data = self.hydrate_type(&mut struct_def.data, false);
    self.sigs.structs.push(TypeCtx { types: take(&mut self.types), inner: StructSig { data } });
    self.chart.structs[struct_id] = struct_def;
  }

  fn resolve_enum_sig(&mut self, enum_id: EnumId) {
    assert_eq!(self.sigs.enums.next_index(), enum_id);
    let mut enum_def = self.chart.enums[enum_id].clone();
    self.initialize(enum_def.def, enum_def.generics);
    let variant_data = enum_def
      .variants
      .values_mut()
      .map(|variant| variant.data.as_mut().map(|ty| self.hydrate_type(ty, false)))
      .collect::<Vec<_>>()
      .into();
    self.sigs.enums.push(TypeCtx { types: take(&mut self.types), inner: EnumSig { variant_data } });
    self.chart.enums[enum_id] = enum_def;
  }

  fn resolve_trait_sig(&mut self, trait_id: TraitId) {
    assert_eq!(self.sigs.traits.next_index(), trait_id);
    let mut trait_def = self.chart.traits[trait_id].clone();
    self.initialize(trait_def.def, trait_def.generics);
    let sig = TraitSig {
      consts: IdxVec::from(Vec::from_iter(trait_def.consts.values_mut().map(|trait_const| {
        let ty = self.hydrate_type(&mut trait_const.ty, false);
        TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } }
      }))),
      fns: IdxVec::from(Vec::from_iter(trait_def.fns.values_mut().map(|trait_fn| {
        let (params, ret_ty) = self.assess_fn_sig(&mut trait_fn.params, &mut trait_fn.ret_ty);
        TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } }
      }))),
    };
    self.sigs.traits.push(sig);
    self.chart.traits[trait_id] = trait_def;
  }

  fn resolve_generics_sig(&mut self, generics_id: GenericsId) {
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

  fn resolve_const_sig(&mut self, const_id: ConcreteConstId) {
    assert_eq!(self.sigs.concrete_consts.next_index(), const_id);
    let mut const_def = self.chart.concrete_consts[const_id].clone();
    self.initialize(const_def.def, const_def.generics);
    let ty = self.hydrate_type(&mut const_def.ty, false);
    self
      .sigs
      .concrete_consts
      .push(TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } });
    self.chart.concrete_consts[const_id] = const_def;
  }

  fn resolve_fn_sig(&mut self, fn_id: ConcreteFnId) {
    assert_eq!(self.sigs.concrete_fns.next_index(), fn_id);
    let mut fn_def = self.chart.concrete_fns[fn_id].clone();
    self.initialize(fn_def.def, fn_def.generics);
    let (params, ret_ty) = self.assess_fn_sig(&mut fn_def.params, &mut fn_def.ret_ty);
    self
      .sigs
      .concrete_fns
      .push(TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } });
    self.chart.concrete_fns[fn_id] = fn_def;
  }

  fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    assert_eq!(self.sigs.impls.next_index(), impl_id);
    let mut impl_def = self.chart.impls[impl_id].clone();
    self.initialize(impl_def.def, impl_def.generics);
    let ty = self.assess_trait(&mut impl_def.trait_);
    self.sigs.impls.push(TypeCtx { types: take(&mut self.types), inner: ImplSig { ty } });
    self.chart.impls[impl_id] = impl_def;
  }

  fn assess_fn_sig(
    &mut self,
    params: &mut [Pat<'core>],
    ret: &mut Option<Ty<'core>>,
  ) -> (Vec<Type>, Type) {
    let params = params.iter_mut().map(|p| self.hydrate_param(p)).collect();
    let ret = ret.as_mut().map(|t| self.hydrate_type(t, false)).unwrap_or(self.types.nil());
    (params, ret)
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
        self.types.import_with(&self.sigs.generics[self.cur_generics], None, |t, sig| {
          t.transfer(&sig.impl_params[*n])
        })
      }
      ImplKind::Def(id, generics) => {
        let type_params = self.check_generics(generics, self.chart.impls[*id].generics, true);
        self.types.import(&self.sigs.impls[*id], Some(&type_params)).ty
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
    let impl_param_count =
      // Things with no inference cannot have implementation parameters; skip
      // checking the `GenericsSig` as this may not have been resolved yet.
      if !inference { 0 } else { self.sigs.generics[params_id].inner.impl_params.len() };
    if !args.impls.is_empty() {
      check_count(args.impls.len(), impl_param_count, "impl");
    }
    let has_impl_params = impl_param_count != 0;
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
        self.types.import(&self.sigs.generics[params_id], Some(&type_params)).impl_params;
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
