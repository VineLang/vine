use vine_util::parser::{Parser, ParserState};

use crate::{
  ast::{ExprKind, Span, StringSegment},
  diag::Diag,
  lexer::{StrToken, Token},
};

use super::VineParser;

impl<'core, 'src> VineParser<'core, 'src> {
  pub(super) fn parse_string(&mut self) -> Result<String, Diag<'core>> {
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

  pub(super) fn parse_string_expr(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
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

  fn parse_string_segment(&mut self) -> Result<(StringSegment, bool), Diag<'core>> {
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

  pub(super) fn parse_char_expr(&mut self) -> Result<ExprKind<'core>, Diag<'core>> {
    let file = self.file;
    let char =
      self.switch(|state| StringParser { state, file }, StringParser::parse_char, |e| e)?;
    Ok(ExprKind::Char(char))
  }
}

struct StringParser<'src> {
  state: ParserState<'src, StrToken>,
  file: usize,
}

impl<'src> Parser<'src> for StringParser<'src> {
  type Token = StrToken;
  type Error = Diag<'static>;

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
  fn parse_string_segment(&mut self) -> Result<(String, bool), Diag<'static>> {
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

  fn parse_char(&mut self) -> Result<char, Diag<'static>> {
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

  fn cur_char(&self) -> Result<char, Diag<'static>> {
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
