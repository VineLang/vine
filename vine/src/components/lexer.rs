use std::fmt;

use macro_rules_attribute::apply;
use unicode_id_start::{is_id_continue, is_id_start};
use vine_util::{
  for_enum,
  lexer::{Lex, LexerState, Token as TokenTrait},
};

pub struct Diag;

#[apply(for_enum!)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Token {
  Underscore,

  // Keywords
  Pub,
  Mod,
  Use,
  As,
  Fn,
  Type,
  Trait,
  Impl,
  Match,
  Let,
  Const,
  In,
  Is,
  Return,
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
  All,
  Any,
  Try,
  Unsafe,
  Safe,

  // Punctuation
  Dot,
  DotDotDot,
  Comma,
  Semi,
  OpenBrace,
  CloseBrace,
  OpenParen,
  CloseParen,
  OpenBracket,
  CloseBracket,
  Colon,
  ColonColon,
  Hash,
  Dollar,
  Backslash,
  QuestQuest,
  QuestQuestQuest,

  // Operators
  DotDot,
  DotDotEq,
  Plus,
  PlusEq,
  PlusPlus,
  PlusPlusEq,
  Minus,
  MinusEq,
  Star,
  StarEq,
  StarStar,
  StarStarEq,
  Caret,
  CaretEq,
  Amp,
  AmpEq,
  AmpAmp,
  Pipe,
  PipeEq,
  At,
  Question,
  Slash,
  SlashEq,
  Percent,
  PercentEq,
  Bang,
  Shl,
  ShlEq,
  Shr,
  ShrEq,
  Tilde,
  TildeEq,
  Eq,
  EqEq,
  BangEq,
  Lt,
  Gt,
  LtEq,
  GtEq,

  BareIdent,
  TupleKey,
  Numeric,
  OpenString,
  OpenChar,

  Eof,

  // Fake token types for better error reporting
  GroupIdent,
  GroupKeyword,
  GroupExpr,
  GroupExprPostfix,
  GroupPat,
  GroupTy,
  GroupStmt,
  GroupItem,
}

#[derive(Debug, Clone)]
enum TriviaComponent<'src> {
  Line,
  Comment { suppress: bool, block: bool, content: &'src str },
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
  trivia: Vec<(usize, TriviaComponent<'src>)>,
  state: LexerState<'src>,
}

impl<'src> Lexer<'src> {
  pub fn new(src: &'src str) -> Self {
    Lexer { trivia: Vec::new(), state: LexerState::new(src) }
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
    self.start_token();
    match self.char() {
      Some('\n') => {
        self.trivia.push((self.offset(), TriviaComponent::Line));
        self.bump();
        self.lex()
      }

      Some(' ' | '\t' | '\x0c' | '\r') => {
        self.bump();
        self.lex()
      }

      Some('.') => match self.bump() {
        Some('.') => match self.bump() {
          Some('.') => self.bump_ok(Token::DotDotDot),
          _ => self.eq(Token::DotDot, Token::DotDotEq),
        },
        Some('0'..='9') => {
          self.bump();
          self.bump_while(|x| x.is_ascii_digit());
          Ok(Token::TupleKey)
        }
        _ => Ok(Token::Dot),
      },
      Some('<') => match self.bump() {
        Some('<') => self.bump_eq(Token::Shl, Token::ShlEq),
        _ => self.eq(Token::Lt, Token::LtEq),
      },
      Some('>') => match self.bump() {
        Some('>') => self.bump_eq(Token::Shr, Token::ShrEq),
        _ => self.eq(Token::Gt, Token::GtEq),
      },
      Some(':') => match self.bump() {
        Some(':') => self.bump_ok(Token::ColonColon),
        _ => Ok(Token::Colon),
      },
      Some('&') => match self.bump() {
        Some('&') => self.bump_ok(Token::AmpAmp),
        _ => self.eq(Token::Amp, Token::AmpEq),
      },
      Some('+') => match self.bump() {
        Some('+') => self.bump_eq(Token::PlusPlus, Token::PlusPlusEq),
        _ => self.eq(Token::Plus, Token::PlusEq),
      },
      Some('*') => match self.bump() {
        Some('*') => self.bump_eq(Token::StarStar, Token::StarStarEq),
        _ => self.eq(Token::Star, Token::StarEq),
      },
      Some('?') => match self.bump() {
        Some('?') => match self.bump() {
          Some('?') => {
            self.bump_while(|x| x != '\n');
            Ok(Token::QuestQuestQuest)
          }
          _ => Ok(Token::QuestQuest),
        },
        _ => Ok(Token::Question),
      },

      Some('-') => self.bump_eq(Token::Minus, Token::MinusEq),
      Some('!') => self.bump_eq(Token::Bang, Token::BangEq),
      Some('=') => self.bump_eq(Token::Eq, Token::EqEq),
      Some('|') => self.bump_eq(Token::Pipe, Token::PipeEq),
      Some('%') => self.bump_eq(Token::Percent, Token::PercentEq),
      Some('~') => self.bump_eq(Token::Tilde, Token::TildeEq),
      Some('^') => self.bump_eq(Token::Caret, Token::CaretEq),

      Some(',') => self.bump_ok(Token::Comma),
      Some(';') => self.bump_ok(Token::Semi),
      Some('@') => self.bump_ok(Token::At),
      Some('$') => self.bump_ok(Token::Dollar),
      Some('{') => self.bump_ok(Token::OpenBrace),
      Some('}') => self.bump_ok(Token::CloseBrace),
      Some('(') => self.bump_ok(Token::OpenParen),
      Some(')') => self.bump_ok(Token::CloseParen),
      Some('[') => self.bump_ok(Token::OpenBracket),
      Some(']') => self.bump_ok(Token::CloseBracket),
      Some('#') => self.bump_ok(Token::Hash),

      Some('"' | '`') => self.bump_ok(Token::OpenString),
      Some('\'') => self.bump_ok(Token::OpenChar),

      Some('/') => match self.bump() {
        Some('/') => {
          self.skip_comment(false);
          self.lex()
        }
        _ => self.eq(Token::Slash, Token::SlashEq),
      },
      Some('\\') => match self.bump() {
        Some('\\') => {
          self.skip_comment(true);
          self.lex()
        }
        _ => Ok(Token::Backslash),
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
        Ok(Token::Numeric)
      }

      Some(c) if unicode_id_start::is_id_start(c) || c == '_' => {
        self.bump();
        self.bump_while(unicode_id_start::is_id_continue);
        let str = &self.src()[self.range().start..self.offset()];
        Ok(keyword(str).unwrap_or(Token::BareIdent))
      }

      None => Ok(Token::Eof),

      Some(_) => todo!(),
      // Some(_) => Err(Diag::LexError { span: self.span() }),
    }
  }
}

impl<'src> Lexer<'src> {
  fn bump_eq(&mut self, token: Token, token_eq: Token) -> Result<Token, Diag> {
    self.bump();
    self.eq(token, token_eq)
  }

  fn eq(&mut self, token: Token, token_eq: Token) -> Result<Token, Diag> {
    match self.char() {
      Some('=') => self.bump_ok(token_eq),
      _ => Ok(token),
    }
  }

  fn skip_comment(&mut self, suppress: bool) {
    let char = if suppress { '\\' } else { '/' };
    let block = self.char() == Some(char);
    let mut content;
    if block {
      self.bump();
      let mut close = 3;
      while self.char() == Some(char) {
        close += 1;
      }
      self.start_token();
      'main: loop {
        if self.char() == Some(char) {
          self.bump();
          content = self.slice();
          let mut seen = 1;
          while self.char() == Some(char) {
            self.bump();
            seen += 1;
            if seen == close {
              break 'main;
            }
          }
        } else if self.char().is_none() {
          content = self.slice();
          break;
        } else {
          self.bump();
        }
      }
    } else {
      self.start_token();
      while self.char().is_some_and(|x| x != '\n') {
        self.bump();
      }
      content = self.slice();
      self.bump();
    }
    self.trivia.push((self.range().start, TriviaComponent::Comment { suppress, block, content }));
  }
}

fn keyword(str: &str) -> Option<Token> {
  match str {
    "_" => Some(Token::Underscore),
    "pub" => Some(Token::Pub),
    "mod" => Some(Token::Mod),
    "use" => Some(Token::Use),
    "as" => Some(Token::As),
    "fn" => Some(Token::Fn),
    "type" => Some(Token::Type),
    "trait" => Some(Token::Trait),
    "impl" => Some(Token::Impl),
    "match" => Some(Token::Match),
    "let" => Some(Token::Let),
    "const" => Some(Token::Const),
    "in" => Some(Token::In),
    "is" => Some(Token::Is),
    "return" => Some(Token::Return),
    "do" => Some(Token::Do),
    "loop" => Some(Token::Loop),
    "while" => Some(Token::While),
    "for" => Some(Token::For),
    "break" => Some(Token::Break),
    "continue" => Some(Token::Continue),
    "if" => Some(Token::If),
    "assert" => Some(Token::Assert),
    "when" => Some(Token::When),
    "else" => Some(Token::Else),
    "true" => Some(Token::True),
    "false" => Some(Token::False),
    "and" => Some(Token::And),
    "or" => Some(Token::Or),
    "all" => Some(Token::All),
    "any" => Some(Token::Any),
    "try" => Some(Token::Try),
    "unsafe" => Some(Token::Unsafe),
    "safe" => Some(Token::Safe),
    _ => None,
  }
}

pub fn is_word(str: &str) -> bool {
  let mut chars = str.chars();
  chars.next().is_some_and(|x| is_id_start(x) || x == '_') && chars.all(is_id_continue)
}

pub fn is_keyword(str: &str) -> bool {
  keyword(str).is_some()
}

impl TokenTrait for Token {
  fn into_u8(self) -> u8 {
    self as u8
  }
}

pub enum TokenInfo {
  Underscore,
  Keyword(&'static str),
  Punctuation(&'static str),
  Operator(&'static str),
  Other(&'static str),
}

impl Token {
  pub fn info(&self) -> TokenInfo {
    match self {
      Token::Underscore => TokenInfo::Underscore,

      Token::Pub => TokenInfo::Keyword("pub"),
      Token::Mod => TokenInfo::Keyword("mod"),
      Token::Use => TokenInfo::Keyword("use"),
      Token::As => TokenInfo::Keyword("as"),
      Token::Fn => TokenInfo::Keyword("fn"),
      Token::Type => TokenInfo::Keyword("type"),
      Token::Trait => TokenInfo::Keyword("trait"),
      Token::Impl => TokenInfo::Keyword("impl"),
      Token::Match => TokenInfo::Keyword("match"),
      Token::Let => TokenInfo::Keyword("let"),
      Token::Const => TokenInfo::Keyword("const"),
      Token::In => TokenInfo::Keyword("in"),
      Token::Is => TokenInfo::Keyword("is"),
      Token::Return => TokenInfo::Keyword("return"),
      Token::Do => TokenInfo::Keyword("do"),
      Token::Loop => TokenInfo::Keyword("loop"),
      Token::While => TokenInfo::Keyword("while"),
      Token::For => TokenInfo::Keyword("for"),
      Token::Break => TokenInfo::Keyword("break"),
      Token::Continue => TokenInfo::Keyword("continue"),
      Token::If => TokenInfo::Keyword("if"),
      Token::Assert => TokenInfo::Keyword("assert"),
      Token::When => TokenInfo::Keyword("when"),
      Token::Else => TokenInfo::Keyword("else"),
      Token::True => TokenInfo::Keyword("true"),
      Token::False => TokenInfo::Keyword("false"),
      Token::And => TokenInfo::Keyword("and"),
      Token::Or => TokenInfo::Keyword("or"),
      Token::All => TokenInfo::Keyword("all"),
      Token::Any => TokenInfo::Keyword("any"),
      Token::Try => TokenInfo::Keyword("try"),
      Token::Unsafe => TokenInfo::Keyword("unsafe"),
      Token::Safe => TokenInfo::Keyword("safe"),

      Token::Dot => TokenInfo::Punctuation("."),
      Token::DotDotDot => TokenInfo::Punctuation("..."),
      Token::Comma => TokenInfo::Punctuation(","),
      Token::Semi => TokenInfo::Punctuation(";"),
      Token::OpenBrace => TokenInfo::Punctuation("{"),
      Token::CloseBrace => TokenInfo::Punctuation("}"),
      Token::OpenParen => TokenInfo::Punctuation("("),
      Token::CloseParen => TokenInfo::Punctuation(")"),
      Token::OpenBracket => TokenInfo::Punctuation("["),
      Token::CloseBracket => TokenInfo::Punctuation("]"),
      Token::Colon => TokenInfo::Punctuation(":"),
      Token::ColonColon => TokenInfo::Punctuation("::"),
      Token::Hash => TokenInfo::Punctuation("#"),
      Token::Dollar => TokenInfo::Punctuation("$"),
      Token::Backslash => TokenInfo::Punctuation("\\"),
      Token::QuestQuest => TokenInfo::Punctuation("??"),
      Token::QuestQuestQuest => TokenInfo::Punctuation("???"),

      Token::DotDot => TokenInfo::Operator(".."),
      Token::DotDotEq => TokenInfo::Operator("..="),
      Token::Plus => TokenInfo::Operator("+"),
      Token::PlusEq => TokenInfo::Operator("+="),
      Token::PlusPlus => TokenInfo::Operator("++"),
      Token::PlusPlusEq => TokenInfo::Operator("++="),
      Token::Minus => TokenInfo::Operator("-"),
      Token::MinusEq => TokenInfo::Operator("-="),
      Token::Star => TokenInfo::Operator("*"),
      Token::StarEq => TokenInfo::Operator("*="),
      Token::StarStar => TokenInfo::Operator("**"),
      Token::StarStarEq => TokenInfo::Operator("**="),
      Token::Caret => TokenInfo::Operator("^"),
      Token::CaretEq => TokenInfo::Operator("^="),
      Token::Amp => TokenInfo::Operator("&"),
      Token::AmpEq => TokenInfo::Operator("&="),
      Token::AmpAmp => TokenInfo::Operator("&&"),
      Token::Pipe => TokenInfo::Operator("|"),
      Token::PipeEq => TokenInfo::Operator("|="),
      Token::At => TokenInfo::Operator("@"),
      Token::Question => TokenInfo::Operator("?"),
      Token::Slash => TokenInfo::Operator("/"),
      Token::SlashEq => TokenInfo::Operator("/="),
      Token::Percent => TokenInfo::Operator("%"),
      Token::PercentEq => TokenInfo::Operator("%="),
      Token::Bang => TokenInfo::Operator("!"),
      Token::Shl => TokenInfo::Operator("<<"),
      Token::ShlEq => TokenInfo::Operator("<<="),
      Token::Shr => TokenInfo::Operator(">>"),
      Token::ShrEq => TokenInfo::Operator(">>="),
      Token::Tilde => TokenInfo::Operator("~"),
      Token::TildeEq => TokenInfo::Operator("~="),
      Token::Eq => TokenInfo::Operator("="),
      Token::EqEq => TokenInfo::Operator("=="),
      Token::BangEq => TokenInfo::Operator("!="),
      Token::Lt => TokenInfo::Operator("<"),
      Token::Gt => TokenInfo::Operator(">"),
      Token::LtEq => TokenInfo::Operator("<="),
      Token::GtEq => TokenInfo::Operator(">="),

      Token::Eof => TokenInfo::Other("eof"),

      Token::BareIdent => TokenInfo::Other("a bare identifier"),
      Token::Numeric => TokenInfo::Other("a numeric literal"),
      Token::TupleKey => TokenInfo::Other("a tuple key"),
      Token::OpenString => TokenInfo::Other("a string literal"),
      Token::OpenChar => TokenInfo::Other("a character literal"),

      Token::GroupIdent => TokenInfo::Other("an identifier"),
      Token::GroupKeyword => TokenInfo::Other("a keyword"),
      Token::GroupExpr => TokenInfo::Other("an expression"),
      Token::GroupExprPostfix => TokenInfo::Other("an operator"),
      Token::GroupPat => TokenInfo::Other("a pattern"),
      Token::GroupTy => TokenInfo::Other("a type"),
      Token::GroupStmt => TokenInfo::Other("a statement"),
      Token::GroupItem => TokenInfo::Other("an item"),
    }
  }

  pub fn is_keyword(self) -> bool {
    matches!(self.info(), TokenInfo::Underscore | TokenInfo::Keyword(_))
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.info() {
      TokenInfo::Underscore => write!(f, "`_`"),
      TokenInfo::Keyword(str) => write!(f, "`{str}`"),
      TokenInfo::Punctuation(str) => write!(f, "`{str}`"),
      TokenInfo::Operator(str) => write!(f, "`{str}`"),
      TokenInfo::Other(str) => write!(f, "{str}"),
    }
  }
}

#[test]
fn token_roundtrip() {
  let all_tokens = for_enum!(Variant in Token {
    [$(Token::$Variant),*]
  });
  for token in all_tokens {
    let str = match token.info() {
      TokenInfo::Underscore => "_",
      TokenInfo::Keyword(str) | TokenInfo::Punctuation(str) | TokenInfo::Operator(str) => str,
      TokenInfo::Other(_) => continue,
    };
    eprintln!("{token:?} {token}");
    let mut lexer = Lexer::new(str);
    assert_eq!(lexer.lex().ok(), Some(token));
    assert_eq!(lexer.lex().ok(), Some(Token::Eof));
  }
}
