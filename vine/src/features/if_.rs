use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::Distiller, finder::Finder, lexer::Token, parser::Parser, resolver::Resolver,
    synthesizer::SyntheticImpl,
  },
  structures::{
    ast::{Block, Expr, ExprKind, Path, Span, Ty, TyKind},
    chart::{Binding, ConstId, DefValueKind},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirImpl},
    types::{Type, TypeCtx, TypeKind, Types},
    vir::{Port, Stage},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_expr_if(&mut self) -> Result<ExprKind, Diag> {
    self.expect(Token::If)?;
    if self.eat(Token::Const)? {
      let cond = self.parse_path()?;
      let then = self.parse_block()?;
      let else_ = self.eat_then(Token::Else, Self::parse_block)?;
      Ok(ExprKind::IfConst(cond, then, else_))
    } else {
      let cond = self.parse_expr()?;
      let ty = self.parse_arrow_ty()?;
      let then = self.parse_block()?;
      let else_ = self.eat_then(Token::Else, Self::parse_block)?;
      Ok(ExprKind::If(cond, ty, then, else_))
    }
  }

  pub(crate) fn parse_ty_if(&mut self) -> Result<TyKind, Diag> {
    self.expect(Token::If)?;
    self.expect(Token::Const)?;
    let cond = self.parse_path()?;
    self.expect(Token::OpenBrace)?;
    let then = self.parse_ty()?;
    self.expect(Token::CloseBrace)?;
    let else_ = self.eat_then(Token::Else, |self_| {
      self_.expect(Token::OpenBrace)?;
      let else_ = self_.parse_ty()?;
      self_.expect(Token::CloseBrace)?;
      Ok(else_)
    })?;
    Ok(TyKind::IfConst(cond, then, else_))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_if(
    &self,
    cond: &Expr,
    ty: &Option<Ty>,
    then: &Block,
    else_: &Option<Block>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("if "),
      self.fmt_expr(cond),
      self.fmt_arrow_ty(ty),
      Doc(" "),
      self.fmt_block(then, true),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else "), self.fmt_block(else_, true)]),
        None => Doc(""),
      },
    ])
  }

  pub(crate) fn fmt_expr_if_const(
    &self,
    cond: &Path,
    then: &Block,
    else_: &Option<Block>,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("if const "),
      self.fmt_path(cond),
      Doc(" "),
      self.fmt_block(then, true),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else "), self.fmt_block(else_, true)]),
        None => Doc(""),
      },
    ])
  }

  pub(crate) fn fmt_ty_if_const(&self, cond: &Path, then: &Ty, else_: &Option<Ty>) -> Doc<'src> {
    Doc::concat([
      Doc("if const "),
      self.fmt_path(cond),
      Doc(" { "),
      self.fmt_ty(then),
      Doc(" }"),
      match else_ {
        Some(else_) => Doc::concat([Doc(" else { "), self.fmt_ty(else_), Doc(" }")]),
        None => Doc(""),
      },
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_if(
    &mut self,
    span: Span,
    cond: &Expr,
    ty: &Option<Ty>,
    then: &Block,
    else_: &Option<Block>,
  ) -> Result<TirExpr, Diag> {
    let ty = self.resolve_arrow_ty(span, ty, true);
    self.enter_scope();
    let cond = self.resolve_scoped_cond(cond);
    let then = self.resolve_block_type(then, ty);
    self.exit_scope();
    let else_ = else_.as_ref().map(|leg| self.resolve_block_type(leg, ty));
    let nil = self.types.nil();
    if else_.is_none() && self.types.unify(ty, nil).is_failure() {
      self.diags.error(Diag::MissingElse { span });
    }
    Ok(TirExpr::new(span, ty, TirExprKind::If(cond, then, else_)))
  }

  pub(crate) fn resolve_expr_if_const(
    &mut self,
    span: Span,
    cond: &Path,
    then: &Block,
    else_: &Option<Block>,
  ) -> Result<TirExpr, Diag> {
    let const_id = self.resolve_path(self.cur_def, cond, "const", |d| match d.value_kind {
      Some(Binding { span, vis, kind: DefValueKind::Const(ConstId::Concrete(id)) }) => {
        Some(Binding { span, vis, kind: id })
      }
      _ => None,
    })?;

    let const_ty = self.types.import(&self.sigs.concrete_consts[const_id], Some(&[])).ty;
    let bool = self.builtin_ty(span, "Bool", self.chart.builtins.bool);
    self.expect_type(cond.span, const_ty, bool);

    let generics = self.chart.concrete_consts[const_id].generics;
    if cond.generics.is_some()
      || !self.sigs.type_params[generics].params.is_empty()
      || !self.sigs.impl_params[generics].types.inner.is_empty()
    {
      Err(Diag::IfConstGeneric { span })?
    }

    let then_ty = self.types.new_var(span);
    let then = self.resolve_block_type(then, then_ty);

    let else_ty = if else_.is_some() { self.types.new_var(span) } else { self.types.nil() };
    let else_ = else_.as_ref().map(|leg| self.resolve_block_type(leg, else_ty));

    let ty = self.types.new(TypeKind::IfConst(const_id, then_ty, else_ty));
    let const_rel = self.rels.consts.push((ConstId::Concrete(const_id), Vec::new()));
    Ok(TirExpr::new(
      span,
      ty,
      TirExprKind::If(TirExpr::new(cond.span, bool, TirExprKind::Const(const_rel)), then, else_),
    ))
  }

  pub(crate) fn resolve_ty_if_const(
    &mut self,
    span: Span,
    cond: &Path,
    then: &Ty,
    else_: &Option<Ty>,
    inference: bool,
  ) -> Type {
    let const_id = match self.resolve_path(self.cur_def, cond, "const", |d| match d.value_kind {
      Some(Binding { span, vis, kind: DefValueKind::Const(ConstId::Concrete(id)) }) => {
        Some(Binding { span, vis, kind: id })
      }
      _ => None,
    }) {
      Ok(const_id) => const_id,
      Err(diag) => return self.types.error(self.diags.error(diag)),
    };

    let generics = self.chart.concrete_consts[const_id].generics;
    if cond.generics.is_some() || !self.sigs.type_params[generics].params.is_empty() {
      return self.types.error(self.diags.error(Diag::IfConstGeneric { span }));
    }

    let then = self.resolve_ty(then, inference);
    let else_ = match else_ {
      Some(else_) => self.resolve_ty(else_, inference),
      None => self.types.nil(),
    };

    self.types.new(TypeKind::IfConst(const_id, then, else_))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_if(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    cond: &TirExpr,
    then: &TirExpr,
    else_: &Option<TirExpr>,
  ) -> Port {
    let local = self.new_local(stage, span, ty);
    let (mut layer, mut init_stage) = self.child_layer(stage, span);

    let (mut then_stage, mut else_stage) =
      self.distill_cond(&mut layer, &mut init_stage, span, cond);
    let result = self.distill_expr_value(&mut then_stage, then);
    then_stage.local_barrier_write_to(local, result);
    self.finish_stage(init_stage);
    self.finish_stage(then_stage);

    if let Some(leg) = else_ {
      let result = self.distill_expr_value(&mut else_stage, leg);
      else_stage.local_barrier_write_to(local, result);
    }

    self.finish_stage(else_stage);
    self.finish_layer(layer);

    stage.local_read_barrier(local, span, ty)
  }
}

impl Finder<'_> {
  pub(crate) fn find_auto_impls_if_const(
    &mut self,
    type_params: &[Type],
    types: &Types,
    found: &mut Vec<TypeCtx<TirImpl>>,
  ) {
    if let Some((inv, TypeKind::IfConst(const_id, then, else_))) = types.kind(type_params[0]) {
      let mut types = types.clone();
      if types.unify(then.invert_if(inv), type_params[1]).is_success()
        && types.unify(else_.invert_if(inv), type_params[2]).is_success()
      {
        found.push(TypeCtx { types, inner: TirImpl::Synthetic(SyntheticImpl::IfConst(*const_id)) });
      }
    }
  }
}
