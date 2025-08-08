use ivy::{
  ast::{Net, Tree},
  parser::IvyParser,
};
use vine_util::parser::Parser;

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    lexer::Token,
    parser::{VineParser, PAREN_COMMA},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Ident, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_inline_ivy(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    let span = self.span();
    self.bump()?;
    let binds = self.parse_delimited(PAREN_COMMA, |self_| {
      let var = self_.parse_ident()?;
      let value = self_.eat(Token::ThinLeftArrow)?;
      if !value {
        self_.expect(Token::ThinArrow)?;
      }
      let expr = self_.parse_expr()?;
      Ok((var, value, expr))
    })?;
    self.expect(Token::ThinArrow)?;
    let ty = self.parse_ty()?;
    if !self.check(Token::OpenBrace) {
      self.unexpected()?;
    }
    let net_span = self.start_span();
    let net = self.switch(
      |state| IvyParser { state },
      IvyParser::parse_net_inner,
      |_| Diag::InvalidIvy { span },
    )?;
    let net_span = self.end_span(net_span);
    Ok(ExprKind::InlineIvy(binds, ty, net_span, net))
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_expr_inline_ivy(
    &self,
    binds: &Vec<(Ident<'core>, bool, Expr<'core>)>,
    ty: &Ty<'core>,
    net_span: &Span,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("inline_ivy! "),
      Doc::paren_comma(binds.iter().map(|(var, value, expr)| {
        Doc::concat([Doc(*var), Doc(if *value { " <- " } else { " -> " }), self.fmt_expr(expr)])
      })),
      Doc(" -> "),
      self.fmt_ty(ty),
      Doc(" "),
      self.fmt_verbatim(*net_span),
    ])
  }
}

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_inline_ivy(
    &mut self,
    span: Span,
    binds: &Vec<(Ident<'core>, bool, Expr<'core>)>,
    ty: &Ty<'core>,
    net: &Net,
  ) -> Result<TirExpr, Diag<'core>> {
    let binds = Vec::from_iter(
      binds.iter().map(|(name, value, expr)| (name.0 .0.into(), *value, self.resolve_expr(expr))),
    );
    let ty = self.resolve_ty(ty, true);
    Ok(TirExpr::new(span, ty, TirExprKind::InlineIvy(binds, net.clone())))
  }
}

impl<'core> Distiller<'core, '_> {
  pub(crate) fn distill_expr_value_inline_ivy(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    binds: &[(String, bool, TirExpr)],
    net: &Net,
  ) -> Port {
    let out = stage.new_wire(span, ty);
    let binds = binds
      .iter()
      .map(|(var, value, expr)| {
        (
          var.clone(),
          if *value {
            self.distill_expr_value(stage, expr)
          } else {
            self.distill_expr_space(stage, expr)
          },
        )
      })
      .collect();
    stage.steps.push(Step::InlineIvy(binds, out.neg, net.clone()));
    out.pos
  }
}

impl<'core> Emitter<'core, '_> {
  pub(crate) fn emit_inline_ivy(&mut self, binds: &Vec<(String, Port)>, out: &Port, net: &Net) {
    if net.pairs.is_empty() && matches!(&net.root, Tree::Var(var) if var == "debug") {
      let pair = (self.emit_port(out), self.tap_debug());
      self.pairs.push(pair);
      return;
    }
    for (var, port) in binds {
      let port = self.emit_port(port);
      self.pairs.push((Tree::Var(var.clone()), port));
    }
    let out = self.emit_port(out);
    self.pairs.push((out, net.root.clone()));
    self.pairs.extend_from_slice(&net.pairs);
  }
}
