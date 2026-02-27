use vine_util::parser::Parse;

use crate::{
  components::{distiller::Distiller, lexer::Token, parser::Parser, resolver::Resolver},
  structures::{
    ast::{ExprKind, Span, Ty},
    chart::{ConstId, TraitConstId},
    diag::Diag,
    tir::{TirExpr, TirExprKind, TirPat, TirPatKind},
    types::{ImplType, Type},
    vir::{Port, Stage},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Parser<'src> {
  pub(crate) fn parse_expr_hole(&mut self) -> Result<ExprKind, Diag> {
    let ty = self.eat_then(Token::OpenBracket, |self_| {
      let ty = self_.parse_ty()?;
      self_.expect(Token::CloseBracket)?;
      Ok(ty)
    })?;
    Ok(ExprKind::Hole(ty))
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_hole(&mut self, span: Span, ty: Option<&Ty>) -> Result<TirExpr, Diag> {
    let ty = ty.map(|ty| self.resolve_ty(ty, true)).unwrap_or_else(|| self.types.new_var(span));
    Ok(TirExpr::new(span, ty, TirExprKind::Hole))
  }

  pub(crate) fn resolve_pat_hole(&mut self, span: Span) -> Result<TirPat, Diag> {
    Ok(TirPat::new(span, self.types.new_var(span), TirPatKind::Hole))
  }

  pub(crate) fn resolve_ty_hole(&mut self, span: Span, inference: bool) -> Type {
    if inference {
      self.types.new_var(span)
    } else {
      self.types.error(self.diags.error(Diag::ItemTypeHole { span }))
    }
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> Port {
    let Some(default_trait) = self.chart.builtins.default else {
      return Port::error(ty, self.diags.error(Diag::MissingBuiltin { span, builtin: "Default" }));
    };
    let default_impl_type = ImplType::Trait(default_trait, vec![ty]);
    let default_impl = self.find_impl(span, &default_impl_type, false);
    let default_const = ConstId::Abstract(default_trait, TraitConstId(0));
    let default_const_rel = self.rels.consts.push((default_const, vec![default_impl]));

    self.distill_expr_value_const(stage, span, ty, default_const_rel)
  }
  pub(crate) fn distill_expr_space_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> Port {
    self.drop_space(span, stage, ty)
  }

  pub(crate) fn distill_expr_place_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> (Port, Port) {
    (self.distill_expr_value_hole(stage, span, ty), self.drop_space(span, stage, ty))
  }

  pub(crate) fn distill_pat_value_hole(&mut self, stage: &mut Stage, span: Span, ty: Type) -> Port {
    self.drop_space(span, stage, ty)
  }

  pub(crate) fn distill_pat_space_hole(&mut self, stage: &mut Stage, span: Span, ty: Type) -> Port {
    self.drop_space(span, stage, ty.inverse())
  }

  pub(crate) fn distill_pat_place_hole(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
  ) -> (Port, Port) {
    let wire = stage.new_wire(span, ty);
    (wire.neg, wire.pos)
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_hole(&self, ty: &Option<Ty>) -> Doc<'src> {
    Doc::concat([
      Doc("_"),
      if let Some(ty) = ty {
        Doc::concat([Doc("["), self.fmt_ty(ty), Doc("]")])
      } else {
        Doc::EMPTY
      },
    ])
  }
}
