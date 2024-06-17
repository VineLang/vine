use std::{
  fmt::{self, Debug},
  marker::PhantomData,
};

use logos::{Lexer, Logos};

pub trait Token: for<'src> Logos<'src, Source = str, Extras = ()> + Copy + Eq + Debug {
  fn into_u8(self) -> u8;
  /// ## Safety
  /// `value` was returned by `Self::into_u8`
  unsafe fn from_u8(value: u8) -> Self;
}

#[derive(Clone, Copy)]
pub struct TokenSet<T: Token>(u128, PhantomData<T>);

impl<T: Token> Default for TokenSet<T> {
  fn default() -> Self {
    Self(0, PhantomData)
  }
}

impl<T: Token> TokenSet<T> {
  pub fn reset(&mut self) {
    self.0 = 0;
  }

  pub fn add(&mut self, kind: T) {
    self.0 |= 1 << kind.into_u8();
  }
}

impl<T: Token> IntoIterator for TokenSet<T> {
  type Item = T;
  type IntoIter = TokenSetIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    TokenSetIter(self.0, PhantomData)
  }
}

#[derive(Clone, Copy)]
pub struct TokenSetIter<T: Token>(u128, PhantomData<T>);

impl<T: Token> Iterator for TokenSetIter<T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    if self.0 == 0 {
      None
    } else {
      let x = self.0.trailing_zeros() as u8;
      self.0 ^= 1 << x;
      Some(unsafe { T::from_u8(x) })
    }
  }
}

impl<T: Token> Debug for TokenSet<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_set().entries(*self).finish()
  }
}

#[derive(Logos)]
#[logos(skip "(?s).")]
enum BlockCommentToken {
  #[token("/*")]
  Open,
  #[token("*/")]
  Close,
}

pub fn lex_block_comment<T: Token>(lexer: &mut Lexer<'_, T>) -> logos::Skip {
  let mut comment_lexer = BlockCommentToken::lexer(lexer.source());
  comment_lexer.bump(lexer.span().end);
  let mut depth = 1;
  for token in &mut comment_lexer {
    match token {
      Ok(BlockCommentToken::Open) => depth += 1,
      Ok(BlockCommentToken::Close) => depth -= 1,
      Err(_) => unreachable!(),
    }
    if depth == 0 {
      break;
    }
  }
  lexer.bump(comment_lexer.span().end - lexer.span().end);
  logos::Skip
}
