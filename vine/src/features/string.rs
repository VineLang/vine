use ivy::ast::Tree;

use vine_util::parser::{Parser, ParserState};

use crate::{
  components::{
    distiller::Distiller,
    emitter::Emitter,
    lexer::{StrToken, Token},
    loader::FileId,
    parser::VineParser,
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span, StringSegment},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::{Type, TypeKind},
    vir::{Port, PortKind, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> VineParser<'src> {
  pub(crate) fn parse_string(&mut self) -> Result<String, Diag> {
    if !self.check(Token::DoubleQuote) {
      self.unexpected()?;
    }
    let (segment, interpolation) = self.parse_string_segment()?;
    if interpolation {
      Err(Diag::UnexpectedInterpolation { span: segment.span })
    } else {
      Ok(segment.content)
    }
  }

  pub(crate) fn parse_expr_string(&mut self) -> Result<ExprKind, Diag> {
    if !self.check(Token::DoubleQuote) {
      self.unexpected()?;
    }
    let (init, mut interpolation) = self.parse_string_segment()?;
    let mut rest = Vec::new();
    while interpolation {
      let expr = self.parse_expr()?;
      if !self.check(Token::CloseBrace) {
        self.unexpected()?;
      }
      let segment;
      (segment, interpolation) = self.parse_string_segment()?;
      rest.push((expr, segment));
    }
    Ok(ExprKind::String(init, rest))
  }

  fn parse_string_segment(&mut self) -> Result<(StringSegment, bool), Diag> {
    let span = self.start_span();
    let file = self.file;
    let (content, interpolation) = self.switch(
      |state| StringParser { state, file },
      StringParser::parse_string_segment,
      |e| e,
    )?;
    let span = self.end_span(span);
    Ok((StringSegment { content, span }, interpolation))
  }

  pub(crate) fn parse_expr_char(&mut self) -> Result<ExprKind, Diag> {
    let file = self.file;
    let char =
      self.switch(|state| StringParser { state, file }, StringParser::parse_char, |e| e)?;
    Ok(ExprKind::Char(char))
  }
}

struct StringParser<'src> {
  state: ParserState<'src, StrToken>,
  file: FileId,
}

impl<'src> Parser<'src> for StringParser<'src> {
  type Token = StrToken;
  type Error = Diag;

  fn state(&mut self) -> &mut ParserState<'src, Self::Token> {
    &mut self.state
  }

  fn lex_error(&self) -> Self::Error {
    Diag::LexError { span: self.span() }
  }

  fn unexpected_error(&self) -> Self::Error {
    Diag::UnexpectedStringToken { span: self.span(), found: self.state.token }
  }
}

impl<'src> StringParser<'src> {
  fn parse_string_segment(&mut self) -> Result<(String, bool), Diag> {
    let mut segment = String::new();
    while !matches!(self.state.token, None | Some(StrToken::DoubleQuote | StrToken::OpenBrace)) {
      segment.push(self.cur_char()?);
      self.bump()?;
    }
    if self.state.token.is_none() {
      self.unexpected()?;
    }
    Ok((segment, matches!(self.state.token, Some(StrToken::OpenBrace))))
  }

  fn parse_char(&mut self) -> Result<char, Diag> {
    if matches!(self.state.token, None | Some(StrToken::SingleQuote)) {
      self.unexpected()?;
    }
    let char = self.cur_char().unwrap();
    self.bump()?;
    if !matches!(self.state.token, Some(StrToken::SingleQuote)) {
      self.unexpected()?;
    }
    Ok(char)
  }

  fn cur_char(&self) -> Result<char, Diag> {
    let str = self.state.lexer.slice();
    Ok(match self.state.token.unwrap() {
      StrToken::DoubleQuote => '"',
      StrToken::SingleQuote => '\'',
      StrToken::Literal => str.chars().next_back().unwrap(),
      StrToken::Newline => '\n',
      StrToken::Tab => '\t',
      StrToken::CarriageReturn => '\r',
      StrToken::Null => '\0',
      StrToken::Ascii => char::from_u32(decode_hex(&str[2..])).unwrap(),
      StrToken::Unicode => char::from_u32(decode_hex(&str[3..str.len() - 1]))
        .ok_or(Diag::InvalidUnicode { span: self.span() })?,
      StrToken::OpenBrace => '{',
      StrToken::Char => str.chars().next().unwrap(),
    })
  }

  fn span(&self) -> Span {
    let span = self.state.lexer.span();
    Span { file: self.file, start: span.start, end: span.end }
  }
}

fn decode_hex(hex: &str) -> u32 {
  hex.chars().fold(0, |n, c| n * 16 + c.to_digit(16).unwrap())
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_expr_string(
    &self,
    init: &StringSegment,
    rest: &[(Expr, StringSegment)],
  ) -> Doc<'src> {
    Doc::concat(
      [self.fmt_verbatim(init.span)].into_iter().chain(
        rest
          .iter()
          .flat_map(|(expr, seg)| [Doc::group([self.fmt_expr(expr)]), self.fmt_verbatim(seg.span)]),
      ),
    )
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_expr_char(&mut self, span: Span, char: char) -> Result<TirExpr, Diag> {
    let ty = self.builtin_ty(span, "Char", self.chart.builtins.char);
    Ok(TirExpr::new(span, ty, TirExprKind::Char(char)))
  }

  pub(crate) fn resolve_expr_string(
    &mut self,
    span: Span,
    init: &StringSegment,
    rest: &[(Expr, StringSegment)],
  ) -> Result<TirExpr, Diag> {
    let string_ty = if let Some(string) = self.chart.builtins.string {
      self.types.new(TypeKind::Struct(string, Vec::new()))
    } else {
      self.types.error(self.diags.error(Diag::MissingBuiltin { span, builtin: "String" }))
    };
    let rest = rest
      .iter()
      .map(|(expr, str)| {
        let span = expr.span;
        let expr = self.resolve_expr(expr);
        let rel = self.builtin_fn(span, self.chart.builtins.cast, "cast", [expr.ty, string_ty]);
        let expr = if let Ok(rel) = rel {
          TirExpr { span, ty: string_ty, kind: Box::new(TirExprKind::Call(rel, None, vec![expr])) }
        } else {
          self.expect_type(span, expr.ty, string_ty);
          expr
        };
        (expr, str.content.clone())
      })
      .collect();
    Ok(TirExpr::new(span, string_ty, TirExprKind::String(init.content.clone(), rest)))
  }
}

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_char(&mut self, ty: Type, char: char) -> Port {
    Port { ty, kind: PortKind::N32(char as u32) }
  }

  pub(crate) fn distill_expr_value_string(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    init: &str,
    rest: &[(TirExpr, String)],
  ) -> Port {
    let wire = stage.new_wire(span, ty);
    let rest =
      rest.iter().map(|(expr, seg)| (self.distill_expr_value(stage, expr), seg.clone())).collect();
    stage.steps.push(Step::String(wire.neg, init.into(), rest));
    wire.pos
  }
}

impl Emitter<'_> {
  pub(crate) fn emit_string(&mut self, port: &Port, init: &str, rest: &Vec<(Port, String)>) {
    let const_len = init.chars().count() + rest.iter().map(|x| x.1.chars().count()).sum::<usize>();
    let len = self.new_wire();
    let start = self.new_wire();
    let end = self.new_wire();
    let port = self.emit_port(port);
    self.pairs.push((
      port,
      Tree::n_ary(
        "tup",
        [
          len.0,
          Tree::n_ary("tup", init.chars().map(|c| Tree::N32(c as u32)).chain([start.0])),
          end.0,
        ],
      ),
    ));
    let mut cur_len = Tree::N32(const_len as u32);
    let mut cur_buf = start.1;
    for (port, seg) in rest {
      let next_len = self.new_wire();
      let next_buf = self.new_wire();
      let port = self.emit_port(port);
      self.pairs.push((
        port,
        Tree::n_ary(
          "tup",
          [
            Tree::ExtFn("n32_add".into(), false, Box::new(cur_len), Box::new(next_len.0)),
            cur_buf,
            Tree::n_ary("tup", seg.chars().map(|c| Tree::N32(c as u32)).chain([next_buf.0])),
          ],
        ),
      ));
      cur_len = next_len.1;
      cur_buf = next_buf.1;
    }
    self.pairs.push((cur_len, len.1));
    self.pairs.push((cur_buf, end.1));
  }
}
