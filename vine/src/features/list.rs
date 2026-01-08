use ivy::ast::Tree;
use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    parser::{BRACKET_COMMA, Parser},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
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
      self.types.new_struct(self.chart, list, vec![ty]),
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
    let port = self.emit_port(port);
    let list = self.build_list(list, Self::emit_port);
    self.pairs.push((port, list))
  }

  pub(crate) fn build_list<T>(
    &mut self,
    iter: impl IntoIterator<Item = T, IntoIter: DoubleEndedIterator>,
    f: fn(&mut Self, T) -> Tree,
  ) -> Tree {
    let mut len = 0;
    let end = self.new_wire();
    let buf = Tree::n_ary(
      "tup",
      iter
        .into_iter()
        .map(|t| {
          len += 1;
          f(self, t)
        })
        .chain([end.0]),
    );
    Tree::n_ary("tup", [Tree::N32(len), buf, end.1])
  }
}
