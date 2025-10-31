use ivy::ast::Tree;
use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    parser::{VineParser, BRACKET_COMMA},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeKind},
    vir::{Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl VineParser<'_> {
  pub(crate) fn parse_expr_list(&mut self) -> Result<ExprKind, Diag> {
    let elements = self.parse_delimited(BRACKET_COMMA, Self::parse_expr)?;
    Ok(ExprKind::List(elements))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_list(&self, elements: &[Expr]) -> Doc<'src> {
    Doc::bracket_comma(elements.iter().map(|x| self.fmt_expr(x)))
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_list(
    &mut self,
    span: Span,
    elements: &[Expr],
  ) -> Result<TirExpr, Diag> {
    let ty = self.types.new_var(span);
    let elements =
      Vec::from_iter(elements.iter().map(|element| self.resolve_expr_type(element, ty)));
    let Some(list) = self.chart.builtins.list else {
      Err(Diag::MissingBuiltin { span, builtin: "List" })?
    };
    Ok(TirExpr::new(
      span,
      self.types.new(TypeKind::Struct(list, vec![ty])),
      TirExprKind::List(elements),
    ))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_list(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    list: &[TirExpr],
  ) -> Port {
    self.distill_vec(stage, span, ty, list, Self::distill_expr_value, Step::List)
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_list(&mut self, port: &Port, list: &[Port]) {
    let end = self.new_wire();
    let buf = Tree::n_ary("tup", list.iter().map(|p| self.emit_port(p)).chain([end.0]));
    let list = Tree::n_ary("tup", [Tree::N32(list.len() as u32), buf, end.1]);
    let pair = (self.emit_port(port), list);
    self.pairs.push(pair)
  }
}
