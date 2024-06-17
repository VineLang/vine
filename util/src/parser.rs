use logos::Lexer;

use crate::lexer::{Token, TokenSet};

pub struct ParserState<'src, T: Token> {
  pub lexer: Lexer<'src, T>,
  pub token: Option<T>,
  pub expected: TokenSet<T>,
}

impl<'src, T: Token> ParserState<'src, T> {
  pub fn new(src: &'src str) -> Self {
    let lexer = Lexer::new(src);
    ParserState { lexer, token: None, expected: TokenSet::default() }
  }
}

pub trait Parser<'src> {
  type Token: Token;
  type Error;

  fn state(&mut self) -> &mut ParserState<'src, Self::Token>;

  fn lex_error(&self) -> Self::Error;
  fn unexpected_error(&self) -> Self::Error;

  fn bump(&mut self) -> Result<(), Self::Error> {
    self.state().expected.reset();
    self.state().token = self.state().lexer.next().transpose().map_err(|_| self.lex_error())?;
    Ok(())
  }

  fn check(&mut self, kind: Self::Token) -> bool {
    self.state().expected.add(kind);
    self.state().token == Some(kind)
  }

  fn eat(&mut self, kind: Self::Token) -> Result<bool, Self::Error> {
    let matches = self.check(kind);
    if matches {
      self.bump()?;
    }
    Ok(matches)
  }

  fn expect(&mut self, kind: Self::Token) -> Result<&'src str, Self::Error> {
    if self.check(kind) {
      let token = self.state().lexer.slice();
      self.bump()?;
      Ok(token)
    } else {
      self.unexpected()
    }
  }

  fn unexpected<T>(&self) -> Result<T, Self::Error> {
    Err(self.unexpected_error())
  }

  fn parse_delimited<T>(
    &mut self,
    delims: Delimiters<Self::Token>,
    mut parse_el: impl FnMut(&mut Self) -> Result<T, Self::Error>,
  ) -> Result<Vec<T>, Self::Error> {
    if let Some(open) = delims.open {
      self.expect(open)?;
    }
    let mut items = Vec::new();
    loop {
      if let Some(close) = delims.close {
        if self.check(close) {
          break;
        }
      }
      items.push(parse_el(self)?);
      if let Some(separator) = delims.separator {
        if !self.eat(separator)? {
          break;
        }
      }
    }
    if let Some(close) = delims.close {
      self.expect(close)?;
    }
    Ok(items)
  }

  fn parse_num_like(
    &mut self,
    token: &'src str,
    err: fn(&'src str) -> Self::Error,
  ) -> Result<u32, Self::Error> {
    let bytes = token.as_bytes(); // all valid ascii
    let (radix, bytes) = match bytes {
      [b'0', b'b', b @ ..] => (2, b),
      [b'0', b'o', b @ ..] => (8, b),
      [b'0', b'x', b @ ..] => (16, b),
      b => (10, b),
    };
    let mut num = 0u32;
    let err = || err(token);
    for &byte in bytes {
      if byte == b'_' {
        continue;
      }
      let digit = (byte as char).to_digit(radix).ok_or_else(err)?;
      num = num.checked_mul(radix).and_then(|x| x.checked_add(digit)).ok_or_else(err)?;
    }
    Ok(num)
  }

  fn parse_string_like(
    &mut self,
    kind: Self::Token,
    err: fn(&'src str) -> Self::Error,
  ) -> Result<String, Self::Error> {
    let token = self.expect(kind)?;
    let inner = &token[1..token.len() - 1];
    let mut str = String::new();
    let mut chars = inner.chars();
    while let Some(char) = chars.next() {
      if char != '\\' {
        str.push(char);
        continue;
      }
      match chars.next() {
        Some(c @ ('\'' | '"' | '\\')) => str.push(c),
        Some('n') => str.push('\n'),
        Some('r') => str.push('\r'),
        Some('t') => str.push('\t'),
        Some('0') => str.push('\0'),
        _ => Err(err(token))?,
      }
    }
    Ok(str)
  }
}

pub struct Delimiters<T: Token> {
  pub open: Option<T>,
  pub close: Option<T>,
  pub separator: Option<T>,
}
