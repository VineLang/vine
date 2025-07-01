use vine_util::parser::Parser;

use crate::{
  components::{
    lexer::Token,
    parser::{VineParser, BP},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeKind},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn _parse_expr_range(
    &mut self,
    left_bound: Option<Expr<'core>>,
  ) -> Result<ExprKind<'core>, Diag<'core>> {
    if self.eat(Token::DotDot)? {
      let right_bound = self.maybe_parse_expr_bp(BP::Range)?;
      return Ok(ExprKind::RangeExclusive(left_bound, right_bound));
    }
    if self.eat(Token::DotDotEq)? {
      let right_bound = self.parse_expr_bp(BP::Range)?;
      return Ok(ExprKind::RangeInclusive(left_bound, right_bound));
    }
    self.unexpected()
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_range_inclusive(
    &self,
    start: &Option<Expr<'core>>,
    end: &Expr<'core>,
  ) -> Doc<'src> {
    let mut docs = vec![];
    if let Some(start) = start {
      docs.push(self.fmt_expr(start));
    }
    docs.push(Doc("..="));
    docs.push(self.fmt_expr(end));
    Doc::concat_vec(docs)
  }

  pub(crate) fn fmt_expr_range_exclusive(
    &self,
    start: &Option<Expr<'core>>,
    end: &Option<Expr<'core>>,
  ) -> Doc<'src> {
    let mut docs = vec![];
    if let Some(start) = start {
      docs.push(self.fmt_expr(start));
    }
    docs.push(Doc(".."));
    if let Some(end) = end {
      docs.push(self.fmt_expr(end));
    }
    Doc::concat_vec(docs)
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_expr_range(
    &mut self,
    span: Span,
    start: Option<&Expr<'core>>,
    end: Option<&Expr<'core>>,
    end_inclusive: bool,
  ) -> Result<TirExpr, Diag<'core>> {
    let bound_value_ty = self.types.new_var(span);
    let left_bound = self.resolve_range_bound(span, bound_value_ty, start, true)?;
    let right_bound = self.resolve_range_bound(span, bound_value_ty, end, end_inclusive)?;
    let Some(range_id) = self.chart.builtins.range else {
      Err(Diag::MissingBuiltin { span, builtin: "Range" })?
    };
    let ty = self.types.new(TypeKind::Struct(range_id, vec![left_bound.ty, right_bound.ty]));
    Ok(TirExpr::new(
      span,
      ty,
      TirExprKind::Struct(
        range_id,
        TirExpr {
          span,
          ty: self.types.new(TypeKind::Tuple(vec![left_bound.ty, right_bound.ty])),
          kind: Box::new(TirExprKind::Composite(vec![left_bound, right_bound])),
        },
      ),
    ))
  }

  fn resolve_range_bound(
    &mut self,
    span: Span,
    bound_value_ty: Type,
    bound_value: Option<&Expr<'core>>,
    inclusive: bool,
  ) -> Result<TirExpr, Diag<'core>> {
    match bound_value {
      Some(bound) => {
        let bound = self.resolve_expr_type(bound, bound_value_ty);
        let (bound_id, bound_name) = if inclusive {
          (self.chart.builtins.bound_inclusive, "BoundInclusive")
        } else {
          (self.chart.builtins.bound_exclusive, "BoundExclusive")
        };
        let Some(bound_id) = bound_id else {
          Err(Diag::MissingBuiltin { span, builtin: bound_name })?
        };
        Ok(TirExpr {
          span: bound.span,
          ty: self.types.new(TypeKind::Struct(bound_id, vec![bound_value_ty])),
          kind: Box::new(TirExprKind::Struct(bound_id, bound)),
        })
      }
      None => {
        let Some(unbounded_id) = self.chart.builtins.bound_unbounded else {
          Err(Diag::MissingBuiltin { span, builtin: "Unbounded" })?
        };
        Ok(TirExpr {
          span,
          ty: self.types.new(TypeKind::Struct(unbounded_id, vec![])),
          kind: Box::new(TirExprKind::Struct(
            unbounded_id,
            TirExpr { span, ty: self.types.nil(), kind: Box::new(TirExprKind::Composite(vec![])) },
          )),
        })
      }
    }
  }
}
