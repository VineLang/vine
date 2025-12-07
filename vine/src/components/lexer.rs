use std::{fmt, mem::transmute};

use vine_util::lexer::{Lex, LexerState, Token as TokenTrait};

use crate::{
  components::loader::FileId,
  structures::{ast::Span, diag::Diag},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Token {
  Dot,
  DotDot,
  DotDotDot,
  DotDotEq,
  Comma,
  Semi,
  Colon,
  ColonColon,
  Amp,
  AmpAmp,
  Pipe,
  PipePipe,
  Caret,
  At,
  Hash,
  HashBracket,
  Plus,
  PlusPlus,
  Minus,
  Star,
  StarStar,
  Slash,
  Percent,
  Bang,
  Question,
  Dollar,
  Tilde,
  Eq,
  EqEq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
  RightArrow,
  LeftArrow,
  Shl,
  Shr,
  OpenBrace,
  CloseBrace,
  OpenParen,
  CloseParen,
  OpenBracket,
  CloseBracket,
  Hole,

  Pub,
  Mod,
  Use,
  As,
  Fn,
  Pattern,
  Struct,
  Enum,
  Type,
  InlineIvy,
  Trait,
  Impl,
  Match,
  Let,
  Dyn,
  Const,
  Defer,
  In,
  Is,
  Return,
  Yield,
  Do,
  Loop,
  While,
  For,
  Break,
  Continue,
  If,
  Assert,
  When,
  Else,
  True,
  False,
  And,
  Or,
  Try,

  Ident,
  Num,
  DoubleQuote,
  SingleQuote,
  DocComment,

  Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'src> {
  pub(crate) file: FileId,
  state: LexerState<'src>,
}

impl<'src> Lexer<'src> {
  pub fn new(file: FileId, src: &'src str) -> Self {
    Lexer { file, state: LexerState::new(src) }
  }

  pub fn span(&self) -> Span {
    let range = self.range();
    Span { file: self.file, start: range.start, end: range.end }
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
      Some('.') => match self.bump() {
        Some('.') => match self.bump() {
          Some('.') => self.bump_ok(Token::DotDotDot),
          Some('=') => self.bump_ok(Token::DotDotEq),
          _ => Ok(Token::DotDot),
        },
        _ => Ok(Token::Dot),
      },
      Some('#') => match self.bump() {
        Some('[') => self.bump_ok(Token::HashBracket),
        _ => Ok(Token::Hash),
      },
      Some('-') => match self.bump() {
        Some('>') => self.bump_ok(Token::RightArrow),
        _ => Ok(Token::Minus),
      },
      Some('<') => match self.bump() {
        Some('-') => self.bump_ok(Token::LeftArrow),
        Some('<') => self.bump_ok(Token::Shl),
        Some('=') => self.bump_ok(Token::Le),
        _ => Ok(Token::Lt),
      },
      Some('>') => match self.bump() {
        Some('>') => self.bump_ok(Token::Shr),
        Some('=') => self.bump_ok(Token::Ge),
        _ => Ok(Token::Gt),
      },
      Some('!') => match self.bump() {
        Some('=') => self.bump_ok(Token::Ne),
        _ => Ok(Token::Bang),
      },

      Some('=') => match self.bump() {
        Some('=') => self.bump_ok(Token::EqEq),
        _ => Ok(Token::Eq),
      },
      Some(':') => match self.bump() {
        Some(':') => self.bump_ok(Token::ColonColon),
        _ => Ok(Token::Colon),
      },
      Some('&') => match self.bump() {
        Some('&') => self.bump_ok(Token::AmpAmp),
        _ => Ok(Token::Amp),
      },
      Some('|') => match self.bump() {
        Some('|') => self.bump_ok(Token::PipePipe),
        _ => Ok(Token::Pipe),
      },
      Some('+') => match self.bump() {
        Some('+') => self.bump_ok(Token::PlusPlus),
        _ => Ok(Token::Plus),
      },
      Some('*') => match self.bump() {
        Some('*') => self.bump_ok(Token::StarStar),
        _ => Ok(Token::Star),
      },

      Some(',') => self.bump_ok(Token::Comma),
      Some(';') => self.bump_ok(Token::Semi),
      Some('^') => self.bump_ok(Token::Caret),
      Some('@') => self.bump_ok(Token::At),
      Some('%') => self.bump_ok(Token::Percent),
      Some('?') => self.bump_ok(Token::Question),
      Some('$') => self.bump_ok(Token::Dollar),
      Some('~') => self.bump_ok(Token::Tilde),
      Some('{') => self.bump_ok(Token::OpenBrace),
      Some('}') => self.bump_ok(Token::CloseBrace),
      Some('(') => self.bump_ok(Token::OpenParen),
      Some(')') => self.bump_ok(Token::CloseParen),
      Some('[') => self.bump_ok(Token::OpenBracket),
      Some(']') => self.bump_ok(Token::CloseBracket),

      Some('"') => self.bump_ok(Token::DoubleQuote),
      Some('\'') => self.bump_ok(Token::SingleQuote),

      Some('/') => match self.bump() {
        Some('/') => match self.bump() {
          Some('/') => {
            self.bump();
            self.bump_while(|c| c != '\n');
            self.bump();
            Ok(Token::DocComment)
          }
          _ => {
            self.bump_while(|c| c != '\n');
            self.bump();
            self.lex()
          }
        },
        Some('*') => {
          self.bump();
          self.skip_block_comment_content();
          self.lex()
        }
        _ => Ok(Token::Slash),
      },

      Some('0'..='9') => {
        self.bump();
        self.bump_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let before_dot = self.offset();
        if self.char() == Some('.') {
          self.bump();
          if self.bump_while(|c| c.is_ascii_digit()) != 0 {
            if self.char() == Some('e') || self.char() == Some('E') {
              self.bump();
              if self.char() == Some('+') || self.char() == Some('-') {
                self.bump();
              }
              while self.char().is_some_and(|c| c.is_ascii_digit()) {
                self.bump();
              }
            }
          } else {
            self.teleport(before_dot);
          }
        }
        Ok(Token::Num)
      }

      Some(c) if unicode_id_start::is_id_start(c) || c == '_' => {
        self.bump();
        self.bump_while(unicode_id_start::is_id_continue);
        let str = &self.src()[self.range().start..self.offset()];
        match str {
          "_" => Ok(Token::Hole),
          "pub" => Ok(Token::Pub),
          "mod" => Ok(Token::Mod),
          "use" => Ok(Token::Use),
          "as" => Ok(Token::As),
          "fn" => Ok(Token::Fn),
          "pattern" => Ok(Token::Pattern),
          "struct" => Ok(Token::Struct),
          "enum" => Ok(Token::Enum),
          "type" => Ok(Token::Type),
          "trait" => Ok(Token::Trait),
          "impl" => Ok(Token::Impl),
          "match" => Ok(Token::Match),
          "let" => Ok(Token::Let),
          "dyn" => Ok(Token::Dyn),
          "const" => Ok(Token::Const),
          "defer" => Ok(Token::Defer),
          "in" => Ok(Token::In),
          "is" => Ok(Token::Is),
          "return" => Ok(Token::Return),
          "yield" => Ok(Token::Yield),
          "do" => Ok(Token::Do),
          "loop" => Ok(Token::Loop),
          "while" => Ok(Token::While),
          "for" => Ok(Token::For),
          "break" => Ok(Token::Break),
          "continue" => Ok(Token::Continue),
          "if" => Ok(Token::If),
          "assert" => Ok(Token::Assert),
          "when" => Ok(Token::When),
          "else" => Ok(Token::Else),
          "true" => Ok(Token::True),
          "false" => Ok(Token::False),
          "and" => Ok(Token::And),
          "or" => Ok(Token::Or),
          "try" => Ok(Token::Try),
          "inline_ivy" if self.char() == Some('!') => self.bump_ok(Token::InlineIvy),
          _ => Ok(Token::Ident),
        }
      }

      None => Ok(Token::Eof),

      Some(_) => Err(Diag::LexError { span: self.span() }),
    }
  }
}

impl TokenTrait for Token {
  fn into_u8(self) -> u8 {
    self as u8
  }

  unsafe fn from_u8(value: u8) -> Self {
    unsafe { transmute::<u8, Token>(value) }
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(match self {
      Token::Dot => "`.`",
      Token::DotDot => "`..`",
      Token::DotDotDot => "`...`",
      Token::DotDotEq => "`..=`",
      Token::Comma => "`,`",
      Token::Semi => "`;`",
      Token::Colon => "`:`",
      Token::ColonColon => "`::`",
      Token::Amp => "`&`",
      Token::AmpAmp => "`&&`",
      Token::Pipe => "`|`",
      Token::PipePipe => "`||`",
      Token::Caret => "`^`",
      Token::At => "`@`",
      Token::Hash => "`#`",
      Token::HashBracket => "`#[`",
      Token::Plus => "`+`",
      Token::PlusPlus => "`++`",
      Token::Minus => "`-`",
      Token::Star => "`*`",
      Token::StarStar => "`**`",
      Token::Slash => "`/`",
      Token::Percent => "`%`",
      Token::Bang => "`!`",
      Token::Question => "`?`",
      Token::Dollar => "`$`",
      Token::Tilde => "`~`",
      Token::Eq => "`=`",
      Token::EqEq => "`==`",
      Token::Ne => "`!=`",
      Token::Lt => "`<`",
      Token::Gt => "`>`",
      Token::Le => "`<=`",
      Token::Ge => "`>=`",
      Token::RightArrow => "`->`",
      Token::LeftArrow => "`<-`",
      Token::Shl => "`<<`",
      Token::Shr => "`>>`",
      Token::OpenBrace => "`{`",
      Token::CloseBrace => "`}`",
      Token::OpenParen => "`(`",
      Token::CloseParen => "`)`",
      Token::OpenBracket => "`[`",
      Token::CloseBracket => "`]`",
      Token::Hole => "`_`",

      Token::Pub => "`pub`",
      Token::Mod => "`mod`",
      Token::Use => "`use`",
      Token::As => "`as`",
      Token::Fn => "`fn`",
      Token::Pattern => "`pattern`",
      Token::Struct => "`struct`",
      Token::Enum => "`enum`",
      Token::Type => "`type`",
      Token::InlineIvy => "`inline_ivy!`",
      Token::Trait => "`trait`",
      Token::Impl => "`impl`",
      Token::Match => "`match`",
      Token::Let => "`let`",
      Token::Dyn => "`dyn`",
      Token::Const => "`const`",
      Token::Defer => "`defer`",
      Token::In => "`in`",
      Token::Is => "`is`",
      Token::Return => "`return`",
      Token::Yield => "`yield`",
      Token::Do => "`do`",
      Token::Loop => "`loop`",
      Token::While => "`while`",
      Token::For => "`for`",
      Token::Break => "`break`",
      Token::Continue => "`continue`",
      Token::If => "`if`",
      Token::Assert => "`assert`",
      Token::When => "`when`",
      Token::Else => "`else`",
      Token::True => "`true`",
      Token::False => "`false`",
      Token::And => "`and`",
      Token::Or => "`or`",
      Token::Try => "`try`",

      Token::Ident => "an identifier",
      Token::Num => "a numeric literal",
      Token::DoubleQuote => "a string literal",
      Token::SingleQuote => "a character literal",
      Token::DocComment => "a doc comment",

      Token::Eof => "eof",
    })
  }
}
