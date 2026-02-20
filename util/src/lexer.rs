use std::{
  fmt::{self, Debug, Display},
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

  fn eat(&mut self, char: char) -> bool {
    if self.char() == Some(char) {
      self.bump();
      true
    } else {
      false
    }
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

  fn bump_while(&mut self, mut f: impl FnMut(char) -> bool) -> usize {
    let mut count = 0;
    while self.char().is_some_and(&mut f) {
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
}

#[derive(Clone)]
pub struct TokenSet<T: Token> {
  bits: u128,
  groups: usize,
  values: Vec<T>,
}

impl<T: Token> Default for TokenSet<T> {
  fn default() -> Self {
    Self { bits: 0, values: vec![], groups: 0 }
  }
}

impl<T: Token> TokenSet<T> {
  pub fn reset(&mut self) {
    self.bits = 0;
    self.groups = 0;
    self.values.clear();
  }

  pub fn add(&mut self, token: T) {
    if self.groups == 0 {
      let bit = 1 << token.into_u8();
      if self.bits & bit == 0 {
        self.bits |= bit;
        self.values.push(token);
      }
    }
  }

  pub fn start_group(&mut self, token: T) {
    self.add(token);
    self.groups += 1;
  }

  pub fn end_group(&mut self) {
    self.groups -= 1;
  }
}

impl<T: Token> Debug for TokenSet<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_set().entries(&self.values).finish()
  }
}

impl<T: Token + Display> Display for TokenSet<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.values.iter().copied();
    match self.values.len() {
      0 => f.write_str("[unknown]"),
      1 => write!(f, "{}", iter.next().unwrap()),
      2 => write!(f, "{} or {}", iter.next().unwrap(), iter.next().unwrap()),
      _ => {
        for (i, value) in iter.enumerate() {
          if i != 0 {
            f.write_str(", ")?;
          }
          if i == self.values.len() - 1 {
            f.write_str("or ")?;
          }
          write!(f, "{value}")?;
        }
        Ok(())
      }
    }
  }
}
