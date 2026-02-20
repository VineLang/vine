use std::fmt::Debug;

use vine_util::lexer::{Lex, LexerState, Token as TokenTrait};

use crate::text::ast::Diag;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Token {
  OpenParen,
  CloseParen,
  OpenBrace,
  CloseBrace,
  OpenBracket,
  CloseBracket,

  Comma,
  Hole,
  Dollar,
  Eq,

  Ident,
  Path,
  Data,
  Free,

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
  type Error = Diag;

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

      Some(',') => self.bump_ok(Token::Comma),
      Some('_') => self.bump_ok(Token::Hole),
      Some('$') => self.bump_ok(Token::Dollar),
      Some('=') => self.bump_ok(Token::Eq),

      Some(c) if c == ':' || unicode_id_start::is_id_continue(c) => {
        self.bump();
        let mut path = c == ':';
        self.bump_while(|c| {
          if c == ':' {
            path = true;
            true
          } else {
            unicode_id_start::is_id_continue(c)
          }
        });
        Ok(if path { Token::OpenBrace } else { Token::Ident })
      }

      Some('#') => {
        self.bump();
        self.bump_while(|c| c.is_ascii_alphanumeric());
        Ok(Token::Data)
      }

      Some('^') => {
        self.bump();
        self.bump_while(|c| c.is_ascii_alphanumeric());
        Ok(Token::Free)
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
        _ => Err(Diag::LexError)?,
      },

      None => Ok(Token::Eof),

      Some(_) => Err(Diag::LexError)?,
    }
  }
}

impl TokenTrait for Token {
  fn into_u8(self) -> u8 {
    self as u8
  }
}
