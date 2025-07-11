use std::{collections::HashMap, mem::take};

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::{
  components::finder::Finder,
  structures::{
    ast::{
      Block, Expr, ExprKind, Ident, Impl, ImplKind, Pat, PatKind, Path, Span, Target, Trait,
      TraitKind, Ty, TyKind,
    },
    chart::{Chart, ConcreteFnId, DefId, DefTraitKind, FnId, GenericsId, OpaqueTypeId},
    checkpoint::Checkpoint,
    core::Core,
    diag::{Diag, ErrorGuaranteed},
    resolutions::{FnRel, FnRelId, Fragment, FragmentId, Rels, Resolutions},
    signatures::{FnSig, Signatures},
    tir::{
      ClosureId, Local, TargetId, Tir, TirClosure, TirExpr, TirExprKind, TirImpl, TirLocal, TirPat,
      TirPatKind,
    },
    types::{ImplType, Type, TypeKind, Types},
  },
};

#[derive(Debug)]
pub struct Resolver<'core, 'a> {
  pub(crate) core: &'core Core<'core>,
  pub(crate) chart: &'a Chart<'core>,
  pub(crate) sigs: &'a mut Signatures<'core>,
  pub(crate) resolutions: &'a mut Resolutions<'core>,
  pub(crate) fragments: &'a mut IdxVec<FragmentId, Fragment<'core>>,
  pub(crate) types: Types<'core>,
  pub(crate) return_ty: Option<Type>,
  pub(crate) cur_def: DefId,
  pub(crate) cur_generics: GenericsId,

  pub(crate) scope: HashMap<Ident<'core>, Vec<ScopeEntry>>,
  pub(crate) scope_depth: usize,
  pub(crate) locals: IdxVec<Local, TirLocal>,
  pub(crate) targets: HashMap<Target<'core>, Vec<TargetInfo>>,
  pub(crate) target_id: Counter<TargetId>,

  pub(crate) rels: Rels<'core>,
  pub(crate) closures: IdxVec<ClosureId, Option<TirClosure>>,
}

#[derive(Debug)]
pub(crate) struct ScopeEntry {
  pub(crate) depth: usize,
  pub(crate) binding: Binding,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct TargetInfo {
  pub(crate) id: TargetId,
  pub(crate) break_ty: Type,
  pub(crate) continue_: bool,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Binding {
  Local(Local, Span, Type),
  Closure(ClosureId, Type),
}

impl<'core, 'a> Resolver<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a Chart<'core>,
    sigs: &'a mut Signatures<'core>,
    resolutions: &'a mut Resolutions<'core>,
    fragments: &'a mut IdxVec<FragmentId, Fragment<'core>>,
  ) -> Self {
    Resolver {
      core,
      chart,
      sigs,
      resolutions,
      fragments,
      types: Types::default(),
      locals: Default::default(),
      return_ty: None,
      targets: Default::default(),
      cur_def: DefId::ROOT,
      cur_generics: GenericsId::NONE,
      scope: Default::default(),
      scope_depth: Default::default(),
      target_id: Default::default(),
      rels: Default::default(),
      closures: Default::default(),
    }
  }

  pub fn resolve_all(&mut self) {
    self.resolve_since(&Checkpoint::default());
  }

  pub(crate) fn resolve_since(&mut self, checkpoint: &Checkpoint) {
    for id in self.chart.imports.keys_from(checkpoint.imports) {
      _ = self.resolve_import(id);
    }
    for id in self.chart.generics.keys_from(checkpoint.generics) {
      self.resolve_type_params(id);
    }
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
      self.resolve_impl_params(id);
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
      self.resolve_const_def(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.resolve_fn_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.resolve_impl_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.resolve_impl_become(id);
    }
    if let Some(main_mod) = self.chart.main_mod {
      if main_mod >= checkpoint.defs {
        self.resolve_main(main_mod);
      }
    }
  }

  fn resolve_main(&mut self, main_mod: DefId) {
    match self._resolve_main(main_mod) {
      Ok(main_mod) => {
        self.resolutions.main = Some(self.resolutions.fns[main_mod]);
      }
      Err(diag) => {
        self.core.report(diag);
      }
    }
  }

  fn _resolve_main(&mut self, main_mod: DefId) -> Result<ConcreteFnId, Diag<'core>> {
    self.types.reset();
    let span = Span::NONE;
    let main_mod_name = self.chart.defs[main_mod].name;
    let main_ident = self.core.ident("main");
    let path = Path {
      span,
      absolute: true,
      segments: if main_mod_name == main_ident {
        vec![main_ident]
      } else {
        vec![main_mod_name, main_ident]
      },
      generics: None,
    };
    let fn_id = self.resolve_path(DefId::ROOT, &path, "fn", |def| def.fn_id())?;
    let generics = &self.chart.generics[self.chart.fn_generics(fn_id)];
    if !generics.type_params.is_empty()
      || !generics.impl_params.is_empty()
      || generics.parent.is_some()
    {
      Err(Diag::GenericMain { span })?
    }
    let FnId::Concrete(fn_id) = fn_id else { unreachable!() };
    let fn_def = &self.chart.concrete_fns[fn_id];
    let span = fn_def.span;
    let io = self.builtin_ty(span, "IO", self.chart.builtins.io);
    let io_ref = self.types.new(TypeKind::Ref(io));
    let nil = self.types.nil();
    let expected = FnSig { params: vec![io_ref], ret_ty: nil };
    let found = self.types.import(&self.sigs.concrete_fns[fn_id], None);
    Self::expect_fn_sig(self.core, self.chart, &mut self.types, span, main_ident, expected, found);
    Ok(fn_id)
  }

  pub(crate) fn initialize(&mut self, def_id: DefId, generics_id: GenericsId) {
    assert!(self.return_ty.is_none());
    self.targets.clear();
    self.types.reset();

    self.scope.clear();
    debug_assert_eq!(self.scope_depth, 0);
    self.locals.clear();
    self.targets.clear();
    self.target_id.reset();

    self.cur_def = def_id;
    self.cur_generics = generics_id;
  }

  #[allow(clippy::type_complexity)]
  pub(crate) fn _resolve_repl<'l>(
    &mut self,
    span: Span,
    path: &'core str,
    def_id: DefId,
    types: Types<'core>,
    locals: impl Iterator<Item = (Ident<'core>, Span, Type)>,
    block: &Block<'core>,
  ) -> (FragmentId, Type, Vec<(Local, Ident<'core>, Span, Type)>) {
    self.initialize(def_id, GenericsId::NONE);
    self.types = types;
    for (name, span, ty) in locals {
      let local = self.locals.push(TirLocal { span, ty });
      let binding = Binding::Local(local, span, ty);
      self.scope.insert(name, vec![ScopeEntry { depth: 0, binding }]);
    }
    let ty = self.types.new_var(block.span);
    let root = self.resolve_stmts_type(block.span, &block.stmts, ty);
    let root = TirExpr {
      span,
      ty: self.types.new(TypeKind::Ref(ty)),
      kind: Box::new(TirExprKind::Ref(root)),
    };
    let fragment = self.finish_fragment(span, path, root);
    let fragment_id = self.fragments.push(fragment);
    let mut bindings =
      Vec::from_iter(take(&mut self.scope).into_iter().filter_map(|(name, entries)| {
        let entry = entries.first()?;
        if entry.depth == 0 {
          if let Binding::Local(local, span, ty) = entry.binding {
            Some((local, name, span, ty))
          } else {
            None
          }
        } else {
          None
        }
      }));
    bindings.sort_by_key(|b| b.0);
    (fragment_id, ty, bindings)
  }

  pub(crate) fn expect_fn_sig(
    core: &Core<'core>,
    chart: &Chart<'core>,
    types: &mut Types<'core>,
    span: Span,
    name: Ident<'core>,
    expected_sig: FnSig,
    found_sig: FnSig,
  ) {
    let fake_receiver = types.new(TypeKind::Param(usize::MAX, name)); // for nicer printing
    let expected_ty = ImplType::Fn(fake_receiver, expected_sig.params, expected_sig.ret_ty);
    let found_ty = ImplType::Fn(fake_receiver, found_sig.params, found_sig.ret_ty);
    if types.unify_impl_type(&expected_ty, &found_ty).is_failure() {
      core.report(Diag::ExpectedTypeFound {
        span,
        expected: types.show_impl_type(chart, &expected_ty),
        found: types.show_impl_type(chart, &found_ty),
      });
    }
  }

  pub(crate) fn resolve_trait(&mut self, trait_: &Trait<'core>) -> ImplType {
    match &*trait_.kind {
      TraitKind::Path(path) => {
        match self.resolve_path(self.cur_def, path, "trait", |d| d.trait_kind) {
          Ok(DefTraitKind::Trait(trait_id)) => {
            let (type_params, _) =
              self.resolve_generics(path, self.chart.traits[trait_id].generics, false);
            ImplType::Trait(trait_id, type_params)
          }
          Err(diag) => ImplType::Error(self.core.report(diag)),
        }
      }
      TraitKind::Fn(receiver, params, ret) => self.resolve_trait_fn(receiver, params, ret),
      TraitKind::Error(e) => ImplType::Error(*e),
    }
  }

  pub(crate) fn resolve_impl_type(&mut self, impl_: &Impl<'core>, ty: &ImplType) -> TirImpl<'core> {
    let span = impl_.span;
    match &*impl_.kind {
      ImplKind::Hole => self.find_impl(span, ty),
      _ => {
        let (found_ty, impl_) = self.resolve_impl(impl_);
        if self.types.unify_impl_type(&found_ty, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &found_ty),
          });
        }
        impl_
      }
    }
  }

  fn resolve_impl(&mut self, impl_: &Impl<'core>) -> (ImplType, TirImpl<'core>) {
    let span = impl_.span;
    match &*impl_.kind {
      ImplKind::Path(path) => self.resolve_impl_path(path),
      ImplKind::Hole => {
        let err = self.core.report(Diag::UnspecifiedImpl { span });
        (ImplType::Error(err), TirImpl::Error(err))
      }
      ImplKind::Fn(path) => self.resolve_impl_fn(path),
      ImplKind::Error(err) => (ImplType::Error(*err), TirImpl::Error(*err)),
    }
  }

  pub(crate) fn finder(&self, span: Span) -> Finder<'core, '_> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
  }

  pub(crate) fn find_impl(&mut self, span: Span, ty: &ImplType) -> TirImpl<'core> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
      .find_impl(&mut self.types, ty)
  }

  pub(crate) fn bind(&mut self, ident: Ident<'core>, binding: Binding) {
    let stack = self.scope.entry(ident).or_default();
    let top = stack.last_mut();
    if top.as_ref().is_some_and(|x| x.depth == self.scope_depth) {
      top.unwrap().binding = binding;
    } else {
      stack.push(ScopeEntry { depth: self.scope_depth, binding })
    }
  }

  pub(crate) fn enter_scope(&mut self) {
    self.scope_depth += 1;
  }

  pub(crate) fn exit_scope(&mut self) {
    self.scope_depth -= 1;
    for stack in self.scope.values_mut() {
      if stack.last().is_some_and(|x| x.depth > self.scope_depth) {
        stack.pop();
      }
    }
  }

  pub(crate) fn error_expr(&mut self, span: Span, diag: Diag<'core>) -> TirExpr {
    let err = self.core.report(diag);
    TirExpr { span, ty: self.types.error(err), kind: Box::new(TirExprKind::Error(err)) }
  }

  pub(crate) fn error_pat(&mut self, span: Span, diag: Diag<'core>) -> TirPat {
    let err = self.core.report(diag);
    TirPat { span, ty: self.types.error(err), kind: Box::new(TirPatKind::Error(err)) }
  }

  pub(crate) fn expect_type(
    &mut self,
    span: Span,
    found: Type,
    expected: Type,
  ) -> Option<ErrorGuaranteed> {
    if self.types.unify(found, expected).is_failure() {
      Some(self.core.report(Diag::ExpectedTypeFound {
        span,
        expected: self.types.show(self.chart, expected),
        found: self.types.show(self.chart, found),
      }))
    } else {
      None
    }
  }

  pub(crate) fn finish_fragment(
    &mut self,
    span: Span,
    path: &'core str,
    root: TirExpr,
  ) -> Fragment<'core> {
    Fragment {
      def: self.cur_def,
      generics: self.cur_generics,
      path,
      impl_params: self.sigs.impl_params[self.cur_generics].types.inner.len(),
      tir: Tir {
        span,
        types: take(&mut self.types),
        locals: take(&mut self.locals),
        closures: unwrap_idx_vec(take(&mut self.closures)),
        rels: take(&mut self.rels),
        root,
      },
    }
  }

  pub(crate) fn builtin_ty(
    &mut self,
    span: Span,
    name: &'static str,
    builtin: Option<OpaqueTypeId>,
  ) -> Type {
    if let Some(id) = builtin {
      self.types.new(TypeKind::Opaque(id, vec![]))
    } else {
      self.types.error(self.core.report(Diag::MissingBuiltin { span, builtin: name }))
    }
  }

  pub(crate) fn bool(&mut self, span: Span) -> Type {
    self.builtin_ty(span, "Bool", self.chart.builtins.bool)
  }

  pub(crate) fn builtin_fn<const N: usize>(
    &mut self,
    span: Span,
    fn_id: Option<FnId>,
    name: &'static str,
    type_params: [Type; N],
  ) -> Result<FnRelId, Diag<'core>> {
    if let Some(fn_id) = fn_id {
      let (_, impl_params) = self._resolve_generics(
        span,
        None,
        self.chart.fn_generics(fn_id),
        true,
        Some(Vec::from(type_params)),
      );
      Ok(self.rels.fns.push(FnRel::Item(fn_id, impl_params)))
    } else {
      Err(Diag::MissingBuiltin { span, builtin: name })?
    }
  }

  pub(crate) fn resolve_expr_type(&mut self, expr: &Expr<'core>, ty: Type) -> TirExpr {
    let expr = self.resolve_expr(expr);
    self.expect_type(expr.span, expr.ty, ty);
    expr
  }

  pub(crate) fn resolve_expr(&mut self, expr: &Expr<'core>) -> TirExpr {
    self._resolve_expr(expr).unwrap_or_else(|diag| self.error_expr(expr.span, diag))
  }

  pub(crate) fn _resolve_expr(&mut self, expr: &Expr<'core>) -> Result<TirExpr, Diag<'core>> {
    let span = expr.span;
    match &*expr.kind {
      ExprKind::Error(e) => Err(*e)?,
      ExprKind::Paren(e) => self._resolve_expr(e),
      ExprKind::Do(label, ty, block) => self.resolve_expr_do(span, *label, ty, block),
      ExprKind::Assign(dir, space, value) => self.resolve_expr_assign(span, *dir, space, value),
      ExprKind::Match(scrutinee, ty, arms) => self.resolve_match(span, scrutinee, ty, arms),
      ExprKind::If(cond, ty, then, else_) => self.resolve_expr_if(span, cond, ty, then, else_),
      ExprKind::When(label, ty, arms, leg) => self.resolve_expr_when(span, *label, ty, arms, leg),
      ExprKind::While(label, cond, ty, block, else_) => {
        self.resolve_expr_while(span, *label, cond, ty, block, else_)
      }
      ExprKind::For(label, pat, iter, ty, block, else_) => {
        self.resolve_expr_for(span, *label, pat, iter, ty, block, else_)
      }
      ExprKind::Loop(label, ty, block) => self.resolve_expr_loop(span, *label, ty, block),
      ExprKind::Fn(flex, params, ret, body) => self.resolve_expr_fn(span, flex, params, ret, body),
      ExprKind::Ref(inner, _) => self.resolve_expr_ref(span, inner),
      ExprKind::List(elements) => self.resolve_expr_list(span, elements),
      ExprKind::Method(receiver, name, generics, args) => {
        self.resolve_method(span, receiver, *name, generics, args)
      }
      ExprKind::Call(func, args) => self.resolve_expr_call(span, func, args),
      ExprKind::Not(inner) => self.resolve_expr_not(span, inner),
      ExprKind::Neg(inner) => self.resolve_expr_neg(span, inner),
      ExprKind::BinaryOp(op, lhs, rhs) => self.resolve_expr_binary_op(span, *op, lhs, rhs),
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        self.resolve_expr_binary_op_assign(span, *op, lhs, rhs)
      }
      ExprKind::ComparisonOp(init, cmps) => self.resolve_expr_comparison_op(span, init, cmps),
      ExprKind::Cast(inner, to, _) => self.resolve_expr_cast(span, inner, to),
      ExprKind::RangeExclusive(start, end) => {
        self.resolve_expr_range(span, start.as_ref(), end.as_ref(), false)
      }
      ExprKind::RangeInclusive(start, end) => {
        self.resolve_expr_range(span, start.as_ref(), Some(end), true)
      }
      ExprKind::N32(value) => self.resolve_expr_n32(span, *value),
      ExprKind::I32(value) => self.resolve_expr_i32(span, *value),
      ExprKind::F32(value) => self.resolve_expr_f32(span, *value),
      ExprKind::Char(char) => self.resolve_expr_char(span, *char),
      ExprKind::String(init, rest) => self.resolve_expr_string(span, init, rest),
      ExprKind::InlineIvy(binds, ty, _, net) => self.resolve_inline_ivy(span, binds, ty, net),
      ExprKind::Try(result) => self.resolve_expr_try(span, result),
      ExprKind::Bool(..) | ExprKind::Is(..) | ExprKind::LogicalOp(..) => {
        Ok(self.resolve_cond(expr))
      }
      ExprKind::Hole => self.resolve_expr_hole(span),
      ExprKind::Deref(inner, _) => self.resolve_expr_deref(span, inner),
      ExprKind::Inverse(inner, _) => self.resolve_expr_inverse(span, inner),
      ExprKind::Place(value, space) => self.resolve_expr_place(span, value, space),
      ExprKind::TupleField(tuple, index) => self.resolve_expr_tuple_field(span, tuple, *index),
      ExprKind::ObjectField(object, key) => self.resolve_expr_object_field(span, object, key),
      ExprKind::Tuple(elements) => self.resolve_expr_tuple(span, elements),
      ExprKind::Object(entries) => self.resolve_expr_object(span, entries),
      ExprKind::Unwrap(inner) => self.resolve_expr_unwrap(span, inner),
      ExprKind::Path(path, args) => self.resolve_expr_path(expr, span, path, args),
    }
  }

  pub(crate) fn resolve_pat_type(&mut self, pat: &Pat<'core>, ty: Type) -> TirPat {
    let span = pat.span;
    let pat = self.resolve_pat(pat);
    match self.expect_type(pat.span, pat.ty, ty) {
      Some(err) => self.error_pat(span, err.into()),
      None => pat,
    }
  }

  pub(crate) fn resolve_pat(&mut self, pat: &Pat<'core>) -> TirPat {
    self._resolve_pat(pat).unwrap_or_else(|diag| self.error_pat(pat.span, diag))
  }

  pub(crate) fn _resolve_pat(&mut self, pat: &Pat<'core>) -> Result<TirPat, Diag<'core>> {
    let span = pat.span;
    match &*pat.kind {
      PatKind::Error(e) => Err(*e)?,
      PatKind::Paren(p) => self._resolve_pat(p),
      PatKind::Annotation(pat, ty) => self.resolve_pat_annotation(span, pat, ty),
      PatKind::Path(path, data) => self.resolve_pat_path(span, path, data),
      PatKind::Hole => self.resolve_pat_hole(span),
      PatKind::Inverse(inner) => self.resolve_pat_inverse(span, inner),
      PatKind::Tuple(elements) => self.resolve_pat_tuple(span, elements),
      PatKind::Object(entries) => self.resolve_pat_object(span, entries),
      PatKind::Deref(inner) => self.resolve_pat_deref(span, inner),
      PatKind::Ref(inner) => self.resolve_pat_ref(span, inner),
    }
  }

  pub(crate) fn resolve_pat_sig(&mut self, pat: &Pat<'core>, inference: bool) -> Type {
    let span = pat.span;
    match &*pat.kind {
      PatKind::Paren(inner) => self.resolve_pat_sig(inner, inference),
      PatKind::Annotation(_, ty) => self.resolve_ty(ty, inference),
      PatKind::Path(path, _) => self.resolve_pat_sig_path(span, path, inference),
      PatKind::Ref(inner) => self.resolve_pat_sig_ref(inner, inference),
      PatKind::Inverse(inner) => self.resolve_pat_sig_inverse(inner, inference),
      PatKind::Tuple(elements) => self.resolve_pat_sig_tuple(elements, inference),
      PatKind::Object(entries) => self.resolve_pat_sig_object(entries, inference),
      PatKind::Hole | PatKind::Deref(_) => {
        self.types.error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => self.types.error(*e),
    }
  }

  pub(crate) fn resolve_ty(&mut self, ty: &Ty<'core>, inference: bool) -> Type {
    let span = ty.span;
    match &*ty.kind {
      TyKind::Error(e) => self.types.error(*e),
      TyKind::Hole => self.resolve_ty_hole(span, inference),
      TyKind::Paren(t) => self.resolve_ty(t, inference),
      TyKind::Tuple(tys) => self.resolve_ty_tuple(tys, inference),
      TyKind::Object(entries) => self.resolve_ty_object(entries, inference),
      TyKind::Ref(inner) => self.resolve_ty_ref(inner, inference),
      TyKind::Inverse(inner) => self.resolve_ty_inverse(inner, inference),
      TyKind::Path(path) => self.resolve_ty_path(path, inference),
      TyKind::Fn(path) => self.resolve_ty_fn(path),
    }
  }

  pub(crate) fn resolve_arrow_ty(
    &mut self,
    span: Span,
    ty: &Option<Ty<'core>>,
    inference: bool,
  ) -> Type {
    match ty {
      Some(ty) => self.resolve_ty(ty, inference),
      None => {
        if inference {
          self.types.new_var(span)
        } else {
          self.types.nil()
        }
      }
    }
  }
}
