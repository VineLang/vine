use hedera::net::FlatNode;

use vine_util::{
  lexer::{Lex, LexerState},
  parser::Parse,
};

use crate::{
  components::{
    distiller::Distiller, emitter::Emitter, lexer::Token, loader::FileId, parser::Parser,
    resolver::Resolver,
  },
  structures::{
    ast::{Expr, ExprKind, Span, StringSegment},
    diag::Diag,
    tir::{TirExpr, TirExprKind},
    types::Type,
    vir::{Port, PortKind, Stage, Step},
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl<'src> Parser<'src> {
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
    let mut lexer = StrLexer::new(self.file, self.lexer().src());
    lexer.teleport(self.lexer().range().end);
    let mut content = String::new();
    let interpolation = loop {
      match lexer.lex()? {
        StrToken::DoubleQuote => break false,
        StrToken::OpenBrace => break true,
        StrToken::SingleQuote => content.push('\''),
        StrToken::Char(char) => content.push(char),
        StrToken::Eof => Err(Diag::UnexpectedEofString { span: lexer.span() })?,
      }
    };
    self.teleport(lexer.range().end)?;
    let span = self.end_span(span);
    Ok((StringSegment { content, span }, interpolation))
  }

  pub(crate) fn parse_expr_char(&mut self) -> Result<ExprKind, Diag> {
    if !self.check(Token::SingleQuote) {
      self.unexpected()?;
    }
    let span = self.start_span();
    let mut lexer = StrLexer::new(self.file, self.lexer().src());
    lexer.teleport(self.lexer().offset());
    let mut content = String::new();
    loop {
      match lexer.lex()? {
        StrToken::SingleQuote => break,
        StrToken::OpenBrace => content.push('{'),
        StrToken::DoubleQuote => content.push('"'),
        StrToken::Char(char) => content.push(char),
        StrToken::Eof => Err(Diag::UnexpectedEofString { span: lexer.span() })?,
      }
    }
    self.teleport(lexer.offset())?;
    let span = self.end_span(span);
    let mut chars = content.chars();
    let (Some(char), None) = (chars.next(), chars.next()) else {
      Err(Diag::MultiCharLiteral { span })?
    };
    Ok(ExprKind::Char(char))
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum StrToken {
  DoubleQuote,
  SingleQuote,
  OpenBrace,
  Char(char),
  Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct StrLexer<'src> {
  file: FileId,
  state: LexerState<'src>,
}

impl<'src> StrLexer<'src> {
  fn new(file: FileId, src: &'src str) -> Self {
    StrLexer { file, state: LexerState::new(src) }
  }

  fn span(&self) -> Span {
    let range = self.range();
    Span { file: self.file, start: range.start, end: range.end }
  }

  fn invalid_escape(&self) -> Diag {
    Diag::InvalidEscape { span: self.span() }
  }
}

impl<'src> Lex<'src> for StrLexer<'src> {
  type Token = StrToken;
  type Error = Diag;

  fn state(&self) -> &LexerState<'src> {
    &self.state
  }

  fn state_mut(&mut self) -> &mut LexerState<'src> {
    &mut self.state
  }

  fn lex(&mut self) -> Result<Self::Token, Self::Error> {
    self.start_token();
    match self.char() {
      Some('"') => self.bump_ok(StrToken::DoubleQuote),
      Some('\'') => self.bump_ok(StrToken::SingleQuote),
      Some('{') => self.bump_ok(StrToken::OpenBrace),
      Some('\\') => match self.bump() {
        Some('"') => self.bump_ok(StrToken::Char('"')),
        Some('\'') => self.bump_ok(StrToken::Char('\'')),
        Some('{') => self.bump_ok(StrToken::Char('{')),
        Some('\\') => self.bump_ok(StrToken::Char('\\')),
        Some('n') => self.bump_ok(StrToken::Char('\n')),
        Some('t') => self.bump_ok(StrToken::Char('\t')),
        Some('r') => self.bump_ok(StrToken::Char('\r')),
        Some('0') => self.bump_ok(StrToken::Char('\0')),
        Some('x') => {
          let Some(digit_0) = self.bump().and_then(|c| c.to_digit(8)) else {
            return Err(self.invalid_escape());
          };
          let Some(digit_1) = self.bump().and_then(|c| c.to_digit(16)) else {
            return Err(self.invalid_escape());
          };
          self.bump_ok(StrToken::Char(char::from_u32(digit_0 << 4 | digit_1).unwrap()))
        }
        Some('u') => {
          self.bump();
          if !self.eat('{') {
            return Err(self.invalid_escape());
          }
          let start = self.offset();
          self.bump_while(|c| c.is_ascii_hexdigit());
          let end = self.offset();
          if !self.eat('}') {
            return Err(self.invalid_escape());
          }
          let Some(char) =
            u32::from_str_radix(&self.src()[start..end], 16).ok().and_then(char::from_u32)
          else {
            return Err(self.invalid_escape());
          };
          Ok(StrToken::Char(char))
        }
        _ => Err(self.invalid_escape())?,
      },
      Some(c) => self.bump_ok(StrToken::Char(c)),
      None => Ok(StrToken::Eof),
    }
  }
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
      self.types.new_struct(self.chart, string, Vec::new())
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
    let [len, buf] = self.net.wires();
    let port = self.emit_port(port);

    let mut static_len = 0u32;
    let mut dyn_len = Vec::new();

    let mut cur = buf;

    for char in init.chars() {
      static_len += 1;
      let next = self.net.wire();
      let char = self.net.insert(self.guide.n32.with_data(char as u32), []);
      self.net.push(FlatNode(self.guide.tuple, [cur, char, next]));
      cur = next;
    }

    for (port, seg) in rest {
      let port = self.emit_port(port);
      let [port_len, next] = self.net.wires();
      self.net.push(FlatNode(self.guide.tuple, [port, port_len, cur, next]));
      cur = next;
      dyn_len.push(port_len);

      for char in seg.chars() {
        static_len += 1;
        let next = self.net.wire();
        let char = self.net.insert(self.guide.n32.with_data(char as u32), []);
        self.net.push(FlatNode(self.guide.tuple, [cur, char, next]));
        cur = next;
      }
    }

    let end = cur;

    dyn_len.push(len);
    self.net.push(FlatNode(self.guide.n32_add.with_data(static_len), dyn_len));

    self.net.push(FlatNode(self.guide.tuple, [port, len, buf, end]));
  }
}
