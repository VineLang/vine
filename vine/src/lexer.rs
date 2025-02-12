use std::{fmt::Debug, mem::transmute};

use logos::Logos;
use vine_util::lexer::{lex_block_comment, Token as TokenTrait};

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[logos(skip r"[ \t\n\f]+")]
#[repr(u8)]
pub enum Token {
  #[token(".")]
  Dot,
  #[token("..")]
  DotDot,
  #[token("..=")]
  DotDotEq,
  #[token(",")]
  Comma,
  #[token(";")]
  Semi,
  #[token(":")]
  Colon,
  #[token("::")]
  ColonColon,
  #[token("&")]
  And,
  #[token("&&")]
  AndAnd,
  #[token("|")]
  Or,
  #[token("||")]
  OrOr,
  #[token("^")]
  Caret,
  #[token("@")]
  At,
  #[token("#")]
  Hash,
  #[token("+")]
  Plus,
  #[token("++")]
  PlusPlus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("%")]
  Percent,
  #[token("!")]
  Bang,
  #[token("?")]
  Question,
  #[token("$")]
  Dollar,
  #[token("~")]
  Tilde,
  #[token("=")]
  Eq,
  #[token("==")]
  EqEq,
  #[token("!=")]
  Ne,
  #[token("<")]
  Lt,
  #[token(">")]
  Gt,
  #[token("<=")]
  Le,
  #[token(">=")]
  Ge,
  #[token("=>")]
  ThickArrow,
  #[token("->")]
  ThinArrow,
  #[token("<<")]
  Shl,
  #[token(">>")]
  Shr,
  #[token("{")]
  OpenBrace,
  #[token("}")]
  CloseBrace,
  #[token("(")]
  OpenParen,
  #[token(")")]
  CloseParen,
  #[token("[")]
  OpenBracket,
  #[token("]")]
  CloseBracket,
  #[token("_")]
  Hole,

  #[token("pub")]
  Pub,
  #[token("mod")]
  Mod,
  #[token("use")]
  Use,
  #[token("as")]
  As,
  #[token("fn")]
  Fn,
  #[token("pattern")]
  Pattern,
  #[token("struct")]
  Struct,
  #[token("enum")]
  Enum,
  #[token("type")]
  Type,
  #[token("inline_ivy")]
  InlineIvy,
  #[token("trait")]
  Trait,
  #[token("impl")]
  Impl,
  #[token("match")]
  Match,
  #[token("move")]
  Move,
  #[token("let")]
  Let,
  #[token("dyn")]
  Dyn,
  #[token("const")]
  Const,
  #[token("defer")]
  Defer,
  #[token("in")]
  In,
  #[token("is")]
  Is,
  #[token("return")]
  Return,
  #[token("yield")]
  Yield,
  #[token("do")]
  Do,
  #[token("loop")]
  Loop,
  #[token("while")]
  While,
  #[token("for")]
  For,
  #[token("break")]
  Break,
  #[token("continue")]
  Continue,
  #[token("if")]
  If,
  #[token("else")]
  Else,
  #[token("true")]
  True,
  #[token("false")]
  False,

  #[regex(r"\p{ID_Start}\p{ID_Continue}*|_\p{ID_Continue}+")]
  Ident,
  #[regex(r"\d[\d\w]*(\.\d+[\d\w]*)?")]
  Num,
  #[token(r#"""#)]
  DoubleQuote,
  #[token(r#"'"#)]
  SingleQuote,

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

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum StrToken {
  #[token(r#"""#)]
  DoubleQuote,
  #[token("'")]
  SingleQuote,
  #[regex(r#"\\["'\\{]"#)]
  Literal,
  #[token(r#"\n"#)]
  Newline,
  #[token(r#"\t"#)]
  Tab,
  #[token(r#"\r"#)]
  CarriageReturn,
  #[token(r#"\0"#)]
  Null,
  #[regex(r#"\\x[0-7][0-9a-fA-F]"#)]
  Ascii,
  #[regex(r#"\\u\{[0-9a-fA-F]{1,6}\}"#)]
  Unicode,
  #[token("{")]
  OpenBrace,
  #[regex(r#"[^"'\\{]"#)]
  Char,
}

impl TokenTrait for StrToken {
  fn into_u8(self) -> u8 {
    self as u8
  }

  unsafe fn from_u8(value: u8) -> Self {
    transmute(value)
  }
}
