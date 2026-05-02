use ivy::{
  name::{Name, Table},
  net::{FlatNet, FlatNode, Wire},
  text::{
    ast::{Diag as IvyDiag, Expr as IvyExpr, Net, Stmt as IvyStmt},
    lexer::{Lexer as IvyLexer, Token as IvyToken},
    parser::Parser as IvyParser,
  },
};
use vine_util::{lexer::Lex, parser::Parse};

use crate::{
  components::{
    distiller::Distiller, emitter::Emitter, lexer::Token, parser::Parser, resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span},
    content::{Color, Colored, Content, Delimited, Delims, Indent, Keyword, Punct, Space},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeKind},
    vir::{Port, Stage, Step},
  },
  tools::fmt::Formatter,
};

impl Parser<'_> {
  pub(crate) fn parse_inline_ivy(&mut self, span: usize) -> Result<ExprKind, Diag> {
    let span = self.end_span(span);
    if !self.check(Token::OpenBrace) {
      self.unexpected()?;
    }
    let mut ivy_lexer = IvyLexer::new(self.lexer().src());
    ivy_lexer.teleport(self.lexer().offset());
    let mut table = Table::default();
    let mut captured_diag = None;
    let (offset, net) = (|| {
      let mut ivy_parser = IvyParser::with_interpolations(&mut table, ivy_lexer, |state| {
        let expr = (|| {
          self.teleport(state.lexer.offset())?;
          let expr = self.parse_expr()?;
          if !self.check(Token::CloseBrace) {
            self.unexpected()?;
          }
          Ok(expr)
        })()
        .map_err(|diag| {
          captured_diag = Some(diag);
          IvyDiag::InvalidInterpolation
        })?;
        state.lexer.teleport(self.lexer().offset());
        state.bump()?;
        Ok(expr)
      })?;
      let mut net = Net { stmts: Vec::new() };
      while !ivy_parser.check(IvyToken::CloseBrace) {
        net.stmts.push(ivy_parser.parse_stmt()?);
      }
      let offset = ivy_parser.lexer().offset();
      Ok((offset, net))
    })()
    .map_err(|diag| match diag {
      IvyDiag::InvalidInterpolation => captured_diag.unwrap(),
      _ => Diag::InvalidIvy { span, diag },
    })?;
    self.teleport(offset)?;
    Ok(ExprKind::InlineIvy(table, net))
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_inline_ivy(&self, table: &Table, net: &Net<Expr>) -> Content {
    Content::even((Keyword("$ivy"), Space, self.fmt_ivy_stmts(table, &net.stmts)))
  }

  fn fmt_ivy_stmts(&self, table: &Table, stmts: &[(IvyExpr<Expr>, IvyExpr<Expr>)]) -> Content {
    Content::even(
      Delimited::new(Delims::BRACE, stmts.iter().map(|s| self.fmt_ivy_stmt(table, s)))
        .force_multi(stmts.len() > 1),
    )
  }

  fn fmt_ivy_stmt(&self, table: &Table, (a, b): &IvyStmt<Expr>) -> Content {
    Content::smart((
      self.fmt_ivy_expr(table, a),
      (Space, Punct("="), Space),
      self.fmt_ivy_expr(table, b),
    ))
  }

  fn fmt_ivy_expr(&self, table: &Table, expr: &IvyExpr<Expr>) -> Content {
    match expr {
      IvyExpr::Node(name, exprs) => Content::smart((
        self.fmt_ivy_name(table, name),
        Delimited::new(
          &Delims {
            empty: "",
            single: ("(", " ", ")"),
            multi: ("(", "", "", ")"),
            color: Color::VAGUE,
          },
          exprs.iter().map(|e| self.fmt_ivy_expr(table, e)),
        ),
      )),
      IvyExpr::Wire(wire) => Content::even(Colored(Color::WHITE, wire.clone())),
      IvyExpr::Free(None) => Content::even(Colored(Color::KEYWORD, "^")),
      IvyExpr::Free(Some(n)) => Content::even(Colored(Color::KEYWORD, format!("^{n}"))),
      IvyExpr::Interpolation(expr) => {
        Content::even((Punct("$"), Punct("{"), Indent::eager(self.fmt_expr(expr)), Punct("}")))
      }
      IvyExpr::Subnet(expr, stmts) => {
        Content::right((self.fmt_ivy_expr(table, expr), Space, self.fmt_ivy_stmts(table, stmts)))
      }
    }
  }

  fn fmt_ivy_name(&self, table: &Table, name: &Name) -> Content {
    let path = table.path(name.path);
    Content::even((
      Colored(if path.starts_with(":") { Color::STRING } else { Color::SPECIAL }, path.to_owned()),
      (!name.payload.is_zero())
        .then(|| (Punct("#"), Colored(Color::KEYWORD, format!("{}", name.payload)))),
      (!name.children.is_empty()).then(|| {
        Delimited::new(
          Delims::BRACKET_COMMA,
          name.children.iter().map(|&n| self.fmt_ivy_name(table, table.name(n))),
        )
      }),
    ))
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_inline_ivy(
    &mut self,
    span: Span,
    table: &Table,
    net: &Net<Expr>,
  ) -> Result<TirExpr, Diag> {
    self.unsafe_(span);
    let mut interpolations = Vec::new();
    let net = net
      .clone()
      .to_flat_with_interpolations(|_, expr, wire| {
        interpolations.push((wire, self.resolve_expr(&expr)));
      })
      .map_err(|diag| Diag::InvalidIvy { span, diag })?;
    let ty = if net.free.len() == 1 {
      self.types.new_var(span)
    } else {
      let tys = self.types.new_vars(span, net.free.len());
      self.types.new(TypeKind::Tuple(tys))
    };
    Ok(TirExpr::new(span, ty, TirExprKind::InlineIvy(table.clone(), net.clone(), interpolations)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_inline_ivy(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    table: &Table,
    net: &FlatNet,
    interpolations: &[(Wire, TirExpr)],
  ) -> Port {
    let out = stage.new_wire(span, ty);
    let interpolations = interpolations
      .iter()
      .map(|(wire, expr)| (*wire, self.distill_expr_value(stage, expr)))
      .collect();
    stage.steps.push(Step::InlineIvy(out.neg, table.clone(), net.clone(), interpolations));
    out.pos
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_inline_ivy(
    &mut self,
    root: &Port,
    table: &Table,
    net: &FlatNet,
    interpolations: &[(Wire, Port)],
  ) {
    let chunk = self.net.wires.chunk(net.wires.count());
    let map = |&w| Wire(chunk.start.0 + net.links.canonicalize_ref(w).0);
    let importer = self.table.import(table);
    for node in &net.nodes {
      self.net.push(FlatNode {
        name: importer.import_name(&node.name),
        pri: map(&node.pri),
        aux: node.aux.iter().map(map).collect(),
      });
    }
    let root = self.emit_port(root);
    if net.free.len() == 1 {
      self.net.link(root, map(&net.free[0]));
    } else {
      self.net.add(self.guide.tuple, root, net.free.iter().map(map));
    }
    for (wire, port) in interpolations {
      let port = self.emit_port(port);
      self.net.link(map(wire), port);
    }
  }
}
