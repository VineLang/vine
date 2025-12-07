use std::fmt::Debug;

use vine_util::lexer::{Lex, LexerState, Token as TokenTrait};

use crate::parser::ParseError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Token {
  OpenParen,
  CloseParen,
  OpenBrace,
  CloseBrace,
  OpenBracket,
  CloseBracket,

  At,
  Dollar,
  Eq,
  Hole,
  Question,
  Hash,

  N32,
  F32,
  Global,
  Ident,

  Eof,
}

pub struct Lexer<'src> {
  state: LexerState<'src>,
}

impl<'src> Lexer<'src> {
  pub fn new(src: &'src str) -> Self {
    Lexer { state: LexerState::new(src) }
  }
}

impl<'src> Lex<'src> for Lexer<'src> {
  type Token = Token;
  type Error = ParseError<'src>;

  fn state(&self) -> &LexerState<'src> {
    &self.state
  }

  fn state_mut(&mut self) -> &mut LexerState<'src> {
    &mut self.state
  }

  fn lex(&mut self) -> Result<Self::Token, Self::Error> {
    self.skip_whitespace();
    self.start_token();
    match self.char() {
      Some('(') => self.bump_ok(Token::OpenParen),
      Some(')') => self.bump_ok(Token::CloseParen),
      Some('{') => self.bump_ok(Token::OpenBrace),
      Some('}') => self.bump_ok(Token::CloseBrace),
      Some('[') => self.bump_ok(Token::OpenBracket),
      Some(']') => self.bump_ok(Token::CloseBracket),

      Some('@') => self.bump_ok(Token::At),
      Some('$') => self.bump_ok(Token::Dollar),
      Some('=') => self.bump_ok(Token::Eq),
      Some('_') => self.bump_ok(Token::Hole),
      Some('?') => self.bump_ok(Token::Question),
      Some('#') => self.bump_ok(Token::Hash),

      Some('0'..='9') => {
        self.bump();
        self.bump_while(|c| c.is_ascii_alphanumeric() || c == '_');
        Ok(Token::N32)
      }

      Some('+' | '-') => {
        self.bump();
        self.bump_while(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '.' | '+' | '-'));
        Ok(Token::F32)
      }

      Some(':') => {
        self.bump();
        self.bump_while(|c| c == ':' || unicode_id_start::is_id_continue(c));
        Ok(Token::Global)
      }

      Some(c) if unicode_id_start::is_id_start(c) => {
        self.bump();
        self.bump_while(|c| c == ':' || unicode_id_start::is_id_continue(c));
        Ok(Token::Ident)
      }

      Some('/') => match self.bump() {
        Some('/') => {
          self.bump();
          self.bump_while(|c| c != '\n');
          self.lex()
        }
        Some('*') => {
          self.bump();
          self.skip_block_comment_content();
          self.lex()
        }
        _ => Err(ParseError::LexError)?,
      },

      None => Ok(Token::Eof),

      Some(_) => Err(ParseError::LexError)?,
    }
  }
}

impl TokenTrait for Token {
  fn into_u8(self) -> u8 {
    self as u8
  }
}
