use std::{fmt::Debug, mem::transmute};

use logos::Logos;

use vine_util::lexer::{lex_block_comment, Token as TokenTrait};

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"[ \t\n\f]+")]
#[repr(u8)]
pub enum Token {
  #[token("(")]
  OpenParen,
  #[token(")")]
  CloseParen,
  #[token("{")]
  OpenBrace,
  #[token("}")]
  CloseBrace,

  #[token("@")]
  At,
  #[token("$")]
  Dollar,
  #[token("=")]
  Eq,
  #[token("_")]
  Hole,
  #[token("?")]
  Question,

  #[regex(r"\d[\d\w]*")]
  N32,
  #[regex(r"[+-][\d\w\.\+\-]+")]
  F32,
  #[regex(r"(::\p{ID_Continue}+)+")]
  Global,
  #[regex(r"\p{ID_Start}\p{ID_Continue}*")]
  Ident,

  #[regex("//.*", logos::skip)]
  #[token("/*", lex_block_comment)]
  Skip,
}

impl TokenTrait for Token {
  fn into_u8(self) -> u8 {
    self as u8
  }

  unsafe fn from_u8(value: u8) -> Self {
    unsafe { transmute::<u8, Token>(value) }
  }
}
