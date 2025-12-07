use std::{
  fmt::{self, Debug},
  marker::PhantomData,
  ops::Range,
};

#[derive(Debug, Clone, Copy)]
pub struct LexerState<'src> {
  src: &'src str,
  offset: usize,
  char: Option<char>,
  next_offset: usize,

  token_start: usize,
}

impl<'src> LexerState<'src> {
  pub fn new(src: &'src str) -> Self {
    let mut state = LexerState { src, offset: 0, char: None, next_offset: 0, token_start: 0 };
    state.bump();
    state
  }

  pub fn bump(&mut self) {
    self.offset = self.next_offset;
    let mut iter = self.src[self.offset..].chars();
    self.char = iter.next();
    self.next_offset = self.src.len() - iter.as_str().len();
  }

  pub fn teleport(&mut self, offset: usize) {
    self.next_offset = offset;
    self.bump();
  }
}

pub trait Lex<'src> {
  type Token;
  type Error;

  fn state(&self) -> &LexerState<'src>;
  fn state_mut(&mut self) -> &mut LexerState<'src>;

  fn lex(&mut self) -> Result<Self::Token, Self::Error>;

  fn src(&self) -> &'src str {
    self.state().src
  }

  fn range(&self) -> Range<usize> {
    let state = self.state();
    state.token_start..state.offset
  }

  fn slice(&self) -> &'src str {
    &self.src()[self.range()]
  }

  fn offset(&self) -> usize {
    self.state().offset
  }

  fn bump(&mut self) -> Option<char> {
    self.state_mut().bump();
    self.char()
  }

  fn start_token(&mut self) {
    let state = self.state_mut();
    state.token_start = state.offset;
  }

  fn bump_ok(&mut self, token: Self::Token) -> Result<Self::Token, Self::Error> {
    self.bump();
    Ok(token)
  }

  fn teleport(&mut self, offset: usize) {
    self.state_mut().teleport(offset)
  }

  fn char(&self) -> Option<char> {
    self.state().char
  }

  fn bump_while(&mut self, f: impl Fn(char) -> bool) -> usize {
    let mut count = 0;
    while self.char().is_some_and(&f) {
      count += 1;
      self.bump();
    }
    count
  }

  fn skip_whitespace(&mut self) {
    self.bump_while(|c| c.is_ascii_whitespace());
  }

  fn skip_block_comment_content(&mut self) {
    let mut depth = 1;
    while depth != 0 {
      match self.char() {
        Some('/') => {
          if self.bump() == Some('*') {
            self.bump();
            depth += 1;
          }
        }
        Some('*') => {
          if self.bump() == Some('/') {
            self.bump();
            depth -= 1;
          }
        }
        _ => {
          self.bump();
        }
      }
    }
  }
}

pub trait Token: Copy + Eq + Debug {
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
