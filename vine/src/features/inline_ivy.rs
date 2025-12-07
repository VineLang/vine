use ivy::{
  ast::{Net, Tree},
  parser::Parser as IvyParser,
};
use vine_util::parser::Parse;

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    lexer::Token,
    parser::{PAREN_COMMA, Parser},
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Ident, Span, Ty},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl Parser<'_> {
  pub(crate) fn parse_inline_ivy(&mut self) -> Result<ExprKind, Diag> {
    let span = self.span();
    self.bump()?;
    let binds = self.parse_delimited(PAREN_COMMA, |self_| {
      let var = self_.parse_ident()?;
      let value = self_.eat(Token::LeftArrow)?;
      if !value {
        self_.expect(Token::RightArrow)?;
      }
      let expr = self_.parse_expr()?;
      Ok((var, value, expr))
    })?;
    self.expect(Token::RightArrow)?;
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

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_inline_ivy(
    &self,
    binds: &[(Ident, bool, Expr)],
    ty: &Ty,
    net_span: &Span,
  ) -> Doc<'src> {
    Doc::concat([
      Doc("inline_ivy! "),
      Doc::paren_comma(binds.iter().map(|(var, value, expr)| {
        Doc::concat([
          Doc(var.clone()),
          Doc(if *value { " <- " } else { " -> " }),
          self.fmt_expr(expr),
        ])
      })),
      Doc(" -> "),
      self.fmt_ty(ty),
      Doc(" "),
      self.fmt_verbatim(*net_span),
    ])
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_inline_ivy(
    &mut self,
    span: Span,
    binds: &[(Ident, bool, Expr)],
    ty: &Ty,
    net: &Net,
  ) -> Result<TirExpr, Diag> {
    let binds = Vec::from_iter(
      binds.iter().map(|(name, value, expr)| (name.0.clone(), *value, self.resolve_expr(expr))),
    );
    let ty = self.resolve_ty(ty, true);
    Ok(TirExpr::new(span, ty, TirExprKind::InlineIvy(binds, net.clone())))
  }
}

impl Distiller<'_> {
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

impl Emitter<'_> {
  pub(crate) fn emit_inline_ivy(&mut self, binds: &Vec<(String, Port)>, out: &Port, net: &Net) {
    for (var, port) in binds {
      let port = self.emit_port(port);
      self.pairs.push((Tree::Var(var.clone()), port));
    }
    let out = self.emit_port(out);
    self.pairs.push((out, net.root.clone()));
    self.pairs.extend_from_slice(&net.pairs);
  }
}
