use crate::{
  lexer::{Lex, Token, TokenSet},
  nat::Nat,
};

#[derive(Debug)]
pub struct ParserState<'src, L: Lex<'src, Token: Token>> {
  pub lexer: L,
  pub token: L::Token,
  pub last_token_end: usize,
  pub expected: TokenSet<L::Token>,
}

impl<'src, L: Lex<'src, Token: Token>> ParserState<'src, L> {
  pub fn new(mut lexer: L) -> Result<Self, L::Error> {
    Ok(ParserState { token: lexer.lex()?, lexer, last_token_end: 0, expected: TokenSet::default() })
  }
}

pub trait Parse<'src> {
  type Token: Token;
  type Lexer: Lex<'src, Token = Self::Token, Error = Self::Error>;
  type Error;

  fn state(&mut self) -> &mut ParserState<'src, Self::Lexer>;

  fn unexpected_error(&self) -> Self::Error;

  fn lexer<'a>(&'a mut self) -> &'a mut Self::Lexer
  where
    'src: 'a,
  {
    &mut self.state().lexer
  }

  fn bump(&mut self) -> Result<(), Self::Error> {
    self.state().expected.reset();
    self.state().last_token_end = self.lexer().range().end;
    self.state().token = self.lexer().lex()?;
    Ok(())
  }

  fn check(&mut self, kind: Self::Token) -> bool {
    self.state().expected.add(kind);
    self.state().token == kind
  }

  fn eat(&mut self, kind: Self::Token) -> Result<bool, Self::Error> {
    let matches = self.check(kind);
    if matches {
      self.bump()?;
    }
    Ok(matches)
  }

  fn check_then<T>(
    &mut self,
    kind: Self::Token,
    f: impl FnOnce(&mut Self) -> Result<T, Self::Error>,
  ) -> Result<Option<T>, Self::Error> {
    self.check(kind).then(|| f(self)).transpose()
  }

  fn eat_then<T>(
    &mut self,
    kind: Self::Token,
    f: impl FnOnce(&mut Self) -> Result<T, Self::Error>,
  ) -> Result<Option<T>, Self::Error> {
    self.eat(kind)?.then(|| f(self)).transpose()
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

  fn teleport(&mut self, offset: usize) -> Result<(), Self::Error> {
    self.lexer().teleport(offset);
    self.bump()
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
      if let Some(close) = delims.close
        && self.check(close)
      {
        break;
      }
      items.push(parse_el(self)?);
      if let Some(separator) = delims.separator
        && !self.eat(separator)?
      {
        break;
      }
      if delims.separator.is_none() && delims.close.is_none() {
        break;
      }
    }
    if let Some(close) = delims.close {
      self.expect(close)?;
    }
    Ok(items)
  }

  fn parse_u32_like(
    &mut self,
    token: &'src str,
    err: impl Fn(&'src str) -> Self::Error,
  ) -> Result<u32, Self::Error> {
    let bytes = token.as_bytes(); // all valid ascii
    let (radix, bytes) = Self::extract_radix(bytes);
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

  fn parse_nat_like(
    &mut self,
    token: &'src str,
    err: impl Fn(&'src str) -> Self::Error,
  ) -> Result<Nat, Self::Error> {
    let bytes = token.as_bytes(); // all valid ascii
    let (radix, bytes) = Self::extract_radix(bytes);
    let mut num = Nat::ZERO;
    let err = || err(token);
    for &byte in bytes {
      if byte == b'_' {
        continue;
      }
      let digit = (byte as char).to_digit(radix).ok_or_else(err)?;
      num.mul_u32(radix);
      num.add_u32(digit);
    }
    Ok(num)
  }

  fn extract_radix(bytes: &[u8]) -> (u32, &[u8]) {
    let (radix, bytes) = match bytes {
      [b'0', b'b', b @ ..] => (2, b),
      [b'0', b'o', b @ ..] => (8, b),
      [b'0', b'x', b @ ..] => (16, b),
      b => (10, b),
    };
    (radix, bytes)
  }

  fn parse_f32_like(
    &mut self,
    token: &'src str,
    err: impl Fn(&'src str) -> Self::Error,
  ) -> Result<f32, Self::Error> {
    token.parse::<f32>().map_err(|_| err(token))
  }
}

pub struct Delimiters<T: Token> {
  pub open: Option<T>,
  pub close: Option<T>,
  pub separator: Option<T>,
}
