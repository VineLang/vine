use std::mem::take;

use ivy::ast::Tree;
use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    distiller::{Distiller, Return},
    emitter::Emitter,
    lexer::Token,
    parser::{VineParser, BP},
    resolver::{Binding, Resolver},
  },
  structures::{
    ast::{Block, Expr, ExprKind, Flex, FnItem, LetFnStmt, Pat, Path, Span, Stmt, StmtKind, Ty},
    chart::{ConcreteFnDef, ConcreteFnId, DefId, DefValueKind, FnId, GenericsId},
    diag::Diag,
    resolutions::{FnRel, FnRelId, Fragment},
    signatures::FnSig,
    tir::{ClosureId, TirClosure, TirExpr, TirExprKind, TirImpl},
    types::{ImplType, Type, TypeCtx, TypeKind},
    vir::{Header, Interface, InterfaceKind, Port, PortKind, Stage, Step, Transfer},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_fn_item(&mut self) -> Result<FnItem<'core>, Diag<'core>> {
    self.expect(Token::Fn)?;
    let method = self.eat(Token::Dot)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let params = self.parse_pats()?;
    let ret = self.parse_arrow_ty()?;
    let body = (!self.eat(Token::Semi)?).then(|| self.parse_block()).transpose()?;
    Ok(FnItem { method, name, generics, params, ret, body })
  }

  pub(crate) fn parse_expr_fn(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    self.expect(Token::Fn)?;
    let flex = self.parse_flex()?;
    let params = self.parse_pats()?;
    let ty = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    Ok(ExprKind::Fn(flex, params, ty, body))
  }

  pub(crate) fn parse_stmt_return(&mut self) -> Result<StmtKind<'core>, Diag<'core>> {
    let expr = self.maybe_parse_expr_bp(BP::Min)?;
    self.eat(Token::Semi)?;
    Ok(StmtKind::Return(expr))
  }

  pub(crate) fn _parse_stmt_let_fn(&mut self) -> Result<StmtKind<'core>, Diag<'core>> {
    self.expect(Token::Fn)?;
    let flex = self.parse_flex()?;
    let name = self.parse_ident()?;
    let params = self.parse_pats()?;
    let ret = self.parse_arrow_ty()?;
    let body = self.parse_block()?;
    Ok(StmtKind::LetFn(LetFnStmt { flex, name, params, ret, body }))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_fn_item(&self, f: &FnItem<'core>) -> Doc<'src> {
    let params = &f.params;
    Doc::concat([
      Doc("fn "),
      Doc(if f.method { "." } else { "" }),
      Doc(f.name),
      self.fmt_generic_params(&f.generics),
      Doc::paren_comma(params.iter().map(|p| self.fmt_pat(p))),
      self.fmt_return_ty(f.ret.as_ref()),
      match &f.body {
        Some(b) => Doc::concat([Doc(" "), self.fmt_block(b, true)]),
        None => Doc(";"),
      },
    ])
  }

  pub(crate) fn fmt_stmt_let_fn(&self, d: &LetFnStmt<'core>) -> Doc<'src> {
    Doc::concat([
      Doc("let fn"),
      self.fmt_flex(d.flex),
      Doc(" "),
      Doc(d.name),
      Doc::paren_comma(d.params.iter().map(|p| self.fmt_pat(p))),
      self.fmt_return_ty(d.ret.as_ref()),
      Doc(" "),
      self.fmt_block(&d.body, true),
    ])
  }

  pub(crate) fn fmt_expr_fn(
    &self,
    flex: &Flex,
    params: &Vec<Pat<'core>>,
    ty: &Option<Ty<'core>>,
    body: &Block<'core>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("fn"),
      self.fmt_flex(*flex),
      Doc(" "),
      Doc::paren_comma(params.iter().map(|p| self.fmt_pat(p))),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      self.fmt_block(body, false),
    ])
  }

  pub(crate) fn fmt_expr_call(&self, func: &Expr<'core>, args: &Vec<Expr<'core>>) -> Doc<'src> {
    Doc::concat([self.fmt_expr(func), Doc::paren_comma(args.iter().map(|x| self.fmt_expr(x)))])
  }

  pub(crate) fn fmt_stmt_return(&self, expr: &Option<Expr<'core>>) -> Doc<'src> {
    match expr {
      Some(expr) => Doc::concat([Doc("return "), self.fmt_expr(expr), Doc(";")]),
      None => Doc("return;"),
    }
  }

  fn fmt_return_ty(&self, r: Option<&Ty<'core>>) -> Doc<'src> {
    match r {
      Some(t) => Doc::concat([Doc(" -> "), self.fmt_ty(t)]),
      None => Doc::EMPTY,
    }
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_fn(
    &mut self,
    parent: DefId,
    parent_generics: GenericsId,
    span: Span,
    vis: DefId,
    member_vis: DefId,
    fn_item: FnItem<'core>,
  ) -> DefId {
    let def = self.chart_child(parent, fn_item.name, member_vis, true);
    let generics = self.chart_generics(def, parent_generics, fn_item.generics, true);
    let body = self.ensure_implemented(span, fn_item.body);
    let fn_id = self.chart.concrete_fns.push(ConcreteFnDef {
      span,
      def,
      generics,
      method: fn_item.method,
      params: fn_item.params,
      ret_ty: fn_item.ret,
      body,
    });
    self.define_value(span, def, vis, DefValueKind::Fn(FnId::Concrete(fn_id)));
    def
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_fn_sig(&mut self, fn_id: ConcreteFnId) {
    let fn_def = &self.chart.concrete_fns[fn_id];
    self.initialize(fn_def.def, fn_def.generics);
    let (params, ret_ty) = self._resolve_fn_sig(&fn_def.params, &fn_def.ret_ty);
    let types = take(&mut self.types);
    self.sigs.concrete_fns.push_to(fn_id, TypeCtx { types, inner: FnSig { params, ret_ty } });
  }

  pub(crate) fn _resolve_fn_sig(
    &mut self,
    params: &[Pat<'core>],
    ret: &Option<Ty<'core>>,
  ) -> (Vec<Type>, Type) {
    let params = params.iter().map(|p| self.resolve_pat_sig(p, false)).collect();
    let ret = ret.as_ref().map(|t| self.resolve_ty(t, false)).unwrap_or(self.types.nil());
    (params, ret)
  }

  pub(crate) fn resolve_fn_def(&mut self, fn_id: ConcreteFnId) {
    let fn_def = &self.chart.concrete_fns[fn_id];
    let span = fn_def.span;
    self.initialize(fn_def.def, fn_def.generics);
    let (ty, closure_id) =
      self.resolve_closure(span, Flex::None, &fn_def.params, &fn_def.ret_ty, &fn_def.body, false);
    let root = TirExpr { span, ty, kind: Box::new(TirExprKind::Closure(closure_id)) };
    let fragment = self.finish_fragment(span, self.chart.defs[fn_def.def].path, root);
    let fragment_id = self.fragments.push(fragment);
    self.resolutions.fns.push_to(fn_id, fragment_id);
  }

  pub(crate) fn resolve_expr_fn(
    &mut self,
    span: Span,
    flex: &Flex,
    params: &Vec<Pat<'core>>,
    ret: &Option<Ty<'core>>,
    body: &Block<'core>,
  ) -> Result<TirExpr, Diag<'core>> {
    let (ty, closure_id) = self.resolve_closure(span, *flex, params, ret, body, true);
    Ok(TirExpr::new(span, ty, TirExprKind::Closure(closure_id)))
  }

  pub(crate) fn resolve_stmts_let_fn_group<'s>(
    &mut self,
    mut stmts: &'s [Stmt<'core>],
  ) -> &'s [Stmt<'core>] {
    let mut let_fns = Vec::new();
    while let [stmt, rest @ ..] = stmts {
      match &stmt.kind {
        StmtKind::LetFn(let_fn) => {
          let span = stmt.span;
          let id = self.closures.push(None);
          let param_tys =
            Vec::from_iter(let_fn.params.iter().map(|p| self.resolve_pat_sig(p, true)));
          let ret = self.resolve_arrow_ty(span, &let_fn.ret, true);
          let ty = self.types.new(TypeKind::Closure(id, let_fn.flex, param_tys.clone(), ret));
          self.bind(let_fn.name, Binding::Closure(id, ty));
          let_fns.push((span, id, param_tys, ret, let_fn));
        }
        StmtKind::Empty | StmtKind::Item(_) => {}
        _ => break,
      }
      stmts = rest;
    }
    for (span, id, param_tys, ret, let_fn) in let_fns {
      let old_targets = take(&mut self.targets);
      let old_return_ty = self.return_ty.replace(ret);
      self.enter_scope();
      let params = Vec::from_iter(
        let_fn.params.iter().zip(&param_tys).map(|(p, &ty)| self.resolve_pat_type(p, ty)),
      );
      let body = self.resolve_block_type(&let_fn.body, ret);
      self.exit_scope();
      self.targets = old_targets;
      self.return_ty = old_return_ty;
      let param_tys = params.iter().map(|x| x.ty).collect();
      let ty = self.types.new(TypeKind::Closure(id, let_fn.flex, param_tys, ret));
      let closure = TirClosure { span, ty, flex: let_fn.flex, params, body };
      self.closures[id] = Some(closure);
    }
    stmts
  }

  pub(crate) fn resolve_closure(
    &mut self,
    span: Span,
    flex: Flex,
    params: &[Pat<'core>],
    ret: &Option<Ty<'core>>,
    body: &Block<'core>,
    inferred_ret: bool,
  ) -> (Type, ClosureId) {
    let id = self.closures.push(None);
    let old_targets = take(&mut self.targets);
    self.enter_scope();
    let params = params.iter().map(|p| self.resolve_pat(p)).collect::<Vec<_>>();
    let ret_ty = ret.as_ref().map(|t| self.resolve_ty(t, true)).unwrap_or_else(|| {
      if inferred_ret {
        self.types.new_var(body.span)
      } else {
        self.types.nil()
      }
    });
    let old_return_ty = self.return_ty.replace(ret_ty);
    let body = self.resolve_block_type(body, ret_ty);
    self.exit_scope();
    self.targets = old_targets;
    self.return_ty = old_return_ty;
    let param_tys = params.iter().map(|x| x.ty).collect();
    let ty = self.types.new(TypeKind::Closure(id, flex, param_tys, ret_ty));
    let closure = TirClosure { span, ty, flex, params, body };
    self.closures[id] = Some(closure);
    (ty, id)
  }

  pub(crate) fn resolve_expr_path_fn(
    &mut self,
    span: Span,
    path: &Path<'core>,
    fn_id: FnId,
    args: &Option<Vec<Expr<'core>>>,
  ) -> Result<TirExpr, Diag<'core>> {
    if let Some(args) = args {
      let generics_id = self.chart.fn_generics(fn_id);
      let type_params_len = self.sigs.type_params[generics_id].params.len();
      let type_params = self.types.new_vars(path.span, type_params_len);
      let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
      if sig.params.len() != args.len() {
        for arg in args {
          _ = self.resolve_expr(arg);
        }
        Err(Diag::BadArgCount { span, expected: sig.params.len(), got: args.len() })?
      }
      let args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Vec<_>>();
      for (arg, ty) in args.iter().zip(sig.params) {
        // just need inference; errors will be reported later
        _ = self.types.unify(arg.ty, ty);
      }
      let generics = path.generics.as_ref();
      let (type_params, impl_params) =
        self._resolve_generics(path.span, generics, generics_id, true, Some(type_params));
      let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
      for (arg, ty) in args.iter().zip(sig.params) {
        self.expect_type(arg.span, arg.ty, ty);
      }
      let rel = self.rels.fns.push(FnRel::Item(fn_id, impl_params));
      Ok(TirExpr::new(span, sig.ret_ty, TirExprKind::Call(rel, None, args)))
    } else {
      _ = self.resolve_generics(path, GenericsId::NONE, true);
      Ok(TirExpr::new(span, self.types.new(TypeKind::Fn(fn_id)), TirExprKind::Fn))
    }
  }

  pub(crate) fn resolve_ty_fn(&mut self, path: &Path<'core>) -> Type {
    match self.resolve_path(self.cur_def, path, "fn", |d| d.fn_id()) {
      Ok(fn_id) => self.types.new(TypeKind::Fn(fn_id)),
      Err(diag) => self.types.error(self.core.report(diag)),
    }
  }

  pub(crate) fn resolve_trait_fn(
    &mut self,
    receiver: &Ty<'core>,
    params: &Vec<Ty<'core>>,
    ret: &Option<Ty<'core>>,
  ) -> ImplType {
    ImplType::Fn(
      self.resolve_ty(receiver, false),
      params.iter().map(|p| self.resolve_ty(p, false)).collect(),
      match ret {
        Some(ret) => self.resolve_ty(ret, false),
        None => self.types.nil(),
      },
    )
  }

  pub(crate) fn resolve_impl_fn(&mut self, path: &Path<'core>, ty: &ImplType) -> TirImpl<'core> {
    match self.resolve_path(self.cur_def, path, "fn", |d| d.fn_id()) {
      Ok(fn_id) => {
        let (type_params, impl_params) =
          self.resolve_generics(path, self.chart.fn_generics(fn_id), true);
        let sig = self.types.import(self.sigs.fn_sig(fn_id), Some(&type_params));
        let actual_ty = ImplType::Fn(self.types.new(TypeKind::Fn(fn_id)), sig.params, sig.ret_ty);
        if self.types.unify_impl_type(&actual_ty, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: path.span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &actual_ty),
          });
        }
        TirImpl::Fn(fn_id, impl_params)
      }
      Err(diag) => TirImpl::Error(self.core.report(diag)),
    }
  }

  pub(crate) fn resolve_expr_call(
    &mut self,
    span: Span,
    func: &Expr<'core>,
    args: &[Expr<'core>],
  ) -> Result<TirExpr, Diag<'core>> {
    let func = self.resolve_expr(func);
    self._resolve_expr_call(span, func, args)
  }

  pub(crate) fn _resolve_expr_call(
    &mut self,
    span: Span,
    func: TirExpr,
    args: &[Expr<'core>],
  ) -> Result<TirExpr, Diag<'core>> {
    let args = args.iter().map(|arg| self.resolve_expr(arg)).collect::<Vec<_>>();
    let ret_ty = self.types.new_var(span);
    let impl_type = ImplType::Fn(func.ty, args.iter().map(|x| x.ty).collect(), ret_ty);
    let impl_ = self.find_impl(span, &impl_type, false);
    let rel = self.rels.fns.push(FnRel::Impl(impl_));
    Ok(TirExpr::new(span, ret_ty, TirExprKind::Call(rel, Some(func), args)))
  }

  pub(crate) fn resolve_expr_return(
    &mut self,
    span: Span,
    value: &Option<Expr<'core>>,
  ) -> Result<TirExpr, Diag<'core>> {
    let nil = self.types.nil();
    if let Some(ty) = &self.return_ty {
      let ty = *ty;
      let value = match value {
        Some(value) => Some(self.resolve_expr_type(value, ty)),
        None => {
          if self.types.unify(ty, nil).is_failure() {
            self.core.report(Diag::MissingReturnExpr { span, ty: self.types.show(self.chart, ty) });
          }
          None
        }
      };
      Ok(TirExpr::new(span, self.types.new_var(span), TirExprKind::Return(value)))
    } else {
      Err(Diag::NoReturn { span })?
    }
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_closures(&mut self, fragment: &Fragment<'core>) {
    for id in fragment.tir.closures.keys() {
      self.closures.push_to(id, self.interfaces.push(None));
    }
    for (id, closure) in &fragment.tir.closures {
      self.distill_closure(id, closure);
    }
  }

  fn distill_closure(&mut self, id: ClosureId, closure: &TirClosure) {
    let span = closure.span;
    let mut layer = self.new_layer();
    let interface = self.closures[id];

    let call = {
      let mut stage = self.new_stage(&mut layer, span, interface);

      let return_local = self.new_local(&mut stage, span, closure.body.ty);
      let params =
        closure.params.iter().map(|p| self.distill_pat_value(&mut stage, p)).collect::<Vec<_>>();
      let result = stage.local_read_barrier(return_local, span, closure.body.ty);
      stage.header = Header::Fn(params, result);

      self.returns.push(Return { ty: closure.body.ty, layer: stage.layer, local: return_local });
      let result = self.distill_expr_value(&mut stage, &closure.body);
      stage.local_barrier_write_to(return_local, result);
      self.returns.pop();

      self.finish_stage(stage)
    };

    let fork = closure.flex.fork().then(|| {
      let mut stage = self.new_stage(&mut layer, span, interface);
      let former = stage.new_wire(span, closure.ty);
      let latter = stage.new_wire(span, closure.ty);
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(former.neg) }));
      stage.steps.push(Step::Transfer(Transfer { interface, data: Some(latter.neg) }));
      stage.header = Header::Fork(former.pos, latter.pos);
      self.finish_stage(stage)
    });

    let drop = closure.flex.drop().then(|| {
      let mut stage = self.new_stage(&mut layer, span, interface);
      stage.header = Header::Drop;
      self.finish_stage(stage)
    });

    self.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Fn { call, fork, drop }));

    self.finish_layer(layer);
  }

  pub(crate) fn distill_expr_value_fn(&mut self, ty: Type) -> Port {
    Port { ty, kind: PortKind::Nil }
  }

  pub(crate) fn distill_expr_value_closure(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    closure_id: ClosureId,
  ) -> Port {
    let interface = self.closures[closure_id];
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Transfer(Transfer { interface, data: Some(wire.neg) }));
    wire.pos
  }

  pub(crate) fn distill_expr_value_call(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    rel: FnRelId,
    receiver: &Option<TirExpr>,
    args: &[TirExpr],
  ) -> Port {
    let receiver = receiver.as_ref().map(|r| self.distill_expr_value(stage, r));
    let args = args.iter().map(|s| self.distill_expr_value(stage, s)).collect::<Vec<_>>();
    let wire = stage.new_wire(span, ty);
    stage.steps.push(Step::Call(rel, receiver, args, wire.neg));
    wire.pos
  }

  pub(crate) fn distill_return(&mut self, stage: &mut Stage, value: &Option<TirExpr>) {
    let value = match value {
      Some(v) => self.distill_expr_value(stage, v),
      None => Port { ty: self.types.nil(), kind: PortKind::Nil },
    };
    let return_ = self.returns.last().unwrap();

    stage.local_barrier_write_to(return_.local, value);
    stage.steps.push(Step::Diverge(return_.layer, None));
  }
}

impl<'core> Emitter<'core, '_> {
  pub(crate) fn emit_call(&mut self, rel: FnRelId, recv: &Option<Port>, args: &[Port], ret: &Port) {
    let func = self.emit_fn_rel(rel);
    let recv = recv.as_ref().map(|p| self.emit_port(p)).unwrap_or(Tree::Erase);
    let pair = (
      func,
      Tree::n_ary(
        "fn",
        [recv].into_iter().chain(args.iter().chain([ret]).map(|p| self.emit_port(p))),
      ),
    );
    self.pairs.push(pair)
  }
}
