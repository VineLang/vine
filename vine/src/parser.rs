use std::mem::transmute;

use vine_util::{
  lexer::TokenSet,
  parser::{Parser, ParserState},
};

use crate::{
  ast::{
    BinaryOp, Block, ComparisonOp, ConstItem, Enum, FnItem, Ident, Item, ItemKind, LetStmt, Mod,
    ModItem, Path, PatternItem, Stmt, StmtKind, Struct, Term, TermKind, UnaryOp, UseTree,
  },
  lexer::Token,
};

pub struct VineParser<'src> {
  state: ParserState<'src, Token>,
}

impl<'src> Parser<'src> for VineParser<'src> {
  type Token = Token;
  type Error = ParseError<'src>;

  fn state(&mut self) -> &mut ParserState<'src, Self::Token> {
    &mut self.state
  }

  fn lex_error(&self) -> Self::Error {
    ParseError::LexError
  }

  fn unexpected_error(&self) -> ParseError<'src> {
    ParseError::UnexpectedToken { expected: self.state.expected, found: self.state.lexer.slice() }
  }
}

#[derive(Debug, Clone)]
pub enum ParseError<'src> {
  LexError,
  UnexpectedToken { expected: TokenSet<Token>, found: &'src str },
  InvalidNum(&'src str),
  InvalidStr(&'src str),
  InvalidChar,
}

type Parse<'src, T = ()> = Result<T, ParseError<'src>>;

impl<'src> VineParser<'src> {
  pub fn parse(src: &'src str) -> Parse<'src, Mod> {
    let mut parser = VineParser { state: ParserState::new(src) };
    parser.bump()?;
    let mut items = Vec::new();
    while parser.state.token.is_some() {
      items.push(parser.parse_item()?);
    }
    Ok(Mod { items })
  }

  fn parse_item(&mut self) -> Parse<'src, Item> {
    self.maybe_parse_item()?.ok_or_else(|| self.unexpected_error())
  }

  fn maybe_parse_item(&mut self) -> Parse<'src, Option<Item>> {
    Ok(Some(Item {
      kind: match () {
        _ if self.check(Token::Fn) => ItemKind::Fn(self.parse_fn_item()?),
        _ if self.check(Token::Const) => ItemKind::Const(self.parse_const_item()?),
        _ if self.check(Token::Struct) => ItemKind::Struct(self.parse_struct_item()?),
        _ if self.check(Token::Enum) => ItemKind::Enum(self.parse_enum_item()?),
        _ if self.check(Token::Pattern) => ItemKind::Pattern(self.parse_pattern_item()?),
        _ if self.check(Token::Mod) => ItemKind::Mod(self.parse_mod_item()?),
        _ if self.check(Token::Use) => ItemKind::Use(self.parse_use_item()?),
        _ => return Ok(None),
      },
    }))
  }

  fn parse_ident(&mut self) -> Parse<'src, Ident> {
    let token = self.expect(Token::Ident)?;
    Ok(Ident(token.to_owned()))
  }

  fn parse_num(&mut self) -> Parse<'src, u32> {
    let token = self.expect(Token::Num)?;
    let bytes = token.as_bytes(); // all valid ascii
    let (radix, bytes) = match bytes {
      [b'0', b'b', b @ ..] => (2, b),
      [b'0', b'o', b @ ..] => (8, b),
      [b'0', b'x', b @ ..] => (16, b),
      b => (10, b),
    };
    let mut num = 0u32;
    let err = || ParseError::InvalidNum(token);
    for &byte in bytes {
      if byte == b'_' {
        continue;
      }
      let digit = (byte as char).to_digit(radix).ok_or_else(err)?;
      num = num.checked_mul(radix).and_then(|x| x.checked_add(digit)).ok_or_else(err)?;
    }
    Ok(num)
  }

  fn parse_string(&mut self) -> Parse<'src, String> {
    self.parse_string_like(Token::String, ParseError::InvalidStr)
  }

  fn parse_char(&mut self) -> Parse<'src, char> {
    let str = self.parse_string_like(Token::Char, ParseError::InvalidStr)?;
    let mut chars = str.chars();
    let char = chars.next().ok_or(ParseError::InvalidChar)?;
    if chars.next().is_some() {
      Err(ParseError::InvalidChar)?
    }
    Ok(char)
  }

  fn parse_fn_item(&mut self) -> Parse<'src, FnItem> {
    self.expect(Token::Fn)?;
    let name = self.parse_ident()?;
    let params = self.parse_term_list()?;
    let body = self.parse_block()?;
    Ok(FnItem { name, params, body })
  }

  fn parse_const_item(&mut self) -> Parse<'src, ConstItem> {
    self.expect(Token::Const)?;
    let name = self.parse_ident()?;
    self.expect(Token::Eq)?;
    let value = self.parse_term()?;
    self.expect(Token::Semi)?;
    Ok(ConstItem { name, value })
  }

  fn parse_struct_item(&mut self) -> Parse<'src, Struct> {
    self.expect(Token::Struct)?;
    let item = self.parse_struct()?;
    self.expect(Token::Semi)?;
    Ok(item)
  }

  fn parse_enum_item(&mut self) -> Parse<'src, Enum> {
    self.expect(Token::Enum)?;
    let name = self.parse_ident()?;
    let variants = self.parse_delimited(BRACE_COMMA, Self::parse_struct)?;
    Ok(Enum { name, variants })
  }

  fn parse_struct(&mut self) -> Parse<'src, Struct> {
    let name = self.parse_ident()?;
    let fields = if self.check(Token::OpenParen) {
      self.parse_delimited(PAREN_COMMA, Self::parse_ident)?
    } else {
      Vec::new()
    };
    Ok(Struct { name, fields })
  }

  fn parse_pattern_item(&mut self) -> Parse<'src, PatternItem> {
    todo!()
  }

  fn parse_mod_item(&mut self) -> Parse<'src, ModItem> {
    self.expect(Token::Mod)?;
    let name = self.parse_ident()?;
    let items = self.parse_delimited(BRACE, Self::parse_item)?;
    Ok(ModItem { name, inner: Mod { items } })
  }

  fn parse_use_item(&mut self) -> Parse<'src, UseTree> {
    self.expect(Token::Use)?;
    let tree = self.parse_use_tree()?;
    self.expect(Token::Semi)?;
    Ok(tree)
  }

  fn parse_use_tree(&mut self) -> Parse<'src, UseTree> {
    let mut segments = Vec::new();
    while self.check(Token::Ident) {
      segments.push(self.parse_ident()?);
      if !self.eat(Token::ColonColon)? {
        return Ok(UseTree { path: Path { segments }, children: None });
      }
    }
    let children = self.parse_delimited(BRACE_COMMA, Self::parse_use_tree)?;
    Ok(UseTree { path: Path { segments }, children: Some(children) })
  }

  fn parse_term_list(&mut self) -> Parse<'src, Vec<Term>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_term)
  }

  fn parse_block(&mut self) -> Parse<'src, Block> {
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    Ok(Block { stmts })
  }

  fn parse_term(&mut self) -> Parse<'src, Term> {
    self.parse_term_bp(BP::Min)
  }

  fn parse_term_bp(&mut self, bp: BP) -> Parse<'src, Term> {
    let mut term = self.parse_term_prefix()?;
    loop {
      term = match self.parse_term_postfix(term, bp)? {
        Ok(term) => term,
        Err(term) => return Ok(term),
      }
    }
  }

  fn parse_term_prefix(&mut self) -> Parse<'src, Term> {
    if self.eat(Token::And)? {
      return Ok(Term::new_ref(self.parse_term_bp(BP::Prefix)?));
    }
    if self.eat(Token::AndAnd)? {
      return Ok(Term::new_ref(Term::new_ref(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Star)? {
      return Ok(Term::new_deref(self.parse_term_bp(BP::Prefix)?));
    }
    if self.eat(Token::Move)? {
      return Ok(Term::new_move(self.parse_term_bp(BP::Prefix)?));
    }
    if self.eat(Token::Minus)? {
      return Ok(Term::new_unary_op(UnaryOp::Neg, self.parse_term_bp(BP::Prefix)?));
    }
    if self.eat(Token::Bang)? {
      return Ok(Term::new_unary_op(UnaryOp::Not, self.parse_term_bp(BP::Prefix)?));
    }
    if self.check(Token::Num) {
      return Ok(Term::new_num(self.parse_num()?));
    }
    if self.check(Token::Char) {
      return Ok(Term::new_num(self.parse_char()? as u32));
    }
    if self.check(Token::String) {
      return Ok(Term { kind: TermKind::String(self.parse_string()?) });
    }
    if self.check(Token::Num) {
      return Ok(Term::new_num(self.parse_num()?));
    }
    if self.check(Token::Ident) {
      return Ok(Term::new_path(self.parse_path()?));
    }
    if self.check(Token::OpenParen) {
      let mut tuple = false;
      let mut terms = self.parse_delimited(PAREN_COMMA, |slf| {
        let term = slf.parse_term()?;
        if slf.check(Token::Comma) {
          tuple = true;
        }
        Ok(term)
      })?;
      if terms.len() == 1 && !tuple {
        return Ok(terms.pop().unwrap());
      }
      return Ok(Term { kind: TermKind::Tuple(terms) });
    }
    if self.check(Token::OpenBracket) {
      let terms = self.parse_delimited(BRACKET_COMMA, Self::parse_term)?;
      return Ok(Term { kind: TermKind::List(terms) });
    }
    if self.check(Token::OpenBrace) {
      return Ok(Term { kind: TermKind::Block(self.parse_block()?) });
    }
    if self.eat(Token::If)? {
      let cond = self.parse_term()?;
      let then = self.parse_block()?;
      let else_ = if self.eat(Token::Else)? {
        if self.check(Token::If) || self.check(Token::OpenBrace) {
          self.parse_term()?
        } else {
          self.unexpected()?
        }
      } else {
        Term { kind: TermKind::Hole }
      };
      return Ok(Term { kind: TermKind::If(Box::new(cond), then, Box::new(else_)) });
    }
    if self.eat(Token::While)? {
      let cond = self.parse_term()?;
      let body = self.parse_block()?;
      return Ok(Term { kind: TermKind::While(Box::new(cond), body) });
    }
    if self.eat(Token::For)? {
      let pat = self.parse_term()?;
      self.expect(Token::In)?;
      let iter = self.parse_term()?;
      let body = self.parse_block()?;
      return Ok(Term { kind: TermKind::For(Box::new(pat), Box::new(iter), body) });
    }
    if self.eat(Token::Fn)? {
      let params = self.parse_term_list()?;
      let body = self.parse_term()?;
      return Ok(Term { kind: TermKind::Fn(params, Box::new(body)) });
    }
    if self.eat(Token::Match)? {
      let scrut = self.parse_term()?;
      let arms = self.parse_delimited(BRACE_COMMA, |slf| {
        let pat = slf.parse_term()?;
        slf.expect(Token::ThickArrow)?;
        let value = slf.parse_term()?;
        Ok((pat, value))
      })?;
      return Ok(Term { kind: TermKind::Match(Box::new(scrut), arms) });
    }
    if self.eat(Token::Hole)? {
      return Ok(Term { kind: TermKind::Hole });
    }
    self.unexpected()
  }

  fn parse_term_postfix(&mut self, lhs: Term, bp: BP) -> Parse<'src, Result<Term, Term>> {
    for &(lbp, token, op) in BINARY_OP_TABLE {
      let rbp = lbp.inc(); // left-associative
      if bp.permits(lbp) && self.eat(token)? {
        return Ok(Ok(Term::new_binary_op(op, lhs, self.parse_term_bp(rbp)?)));
      }
    }

    if bp.permits(BP::Comparison) {
      let mut rhs = Vec::new();
      'main: loop {
        for &(token, op) in COMPARISON_OP_TABLE {
          if self.eat(token)? {
            rhs.push((op, self.parse_term_bp(BP::Comparison.inc())?));
            continue 'main;
          }
        }
        break;
      }
      if !rhs.is_empty() {
        return Ok(Ok(Term { kind: TermKind::ComparisonOp(Box::new(lhs), rhs) }));
      }
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Eq)? {
      let rhs = self.parse_term_bp(BP::Assignment)?;
      return Ok(Ok(Term { kind: TermKind::Assign(Box::new(lhs), Box::new(rhs)) }));
    }

    if self.eat(Token::Dot)? {
      let path = self.parse_path()?;
      if self.check(Token::OpenParen) {
        let args = self.parse_term_list()?;
        return Ok(Ok(Term { kind: TermKind::Method(Box::new(lhs), path, args) }));
      } else {
        return Ok(Ok(Term { kind: TermKind::Field(Box::new(lhs), path) }));
      }
    }

    if self.check(Token::OpenParen) {
      let args = self.parse_term_list()?;
      return Ok(Ok(Term { kind: TermKind::Call(Box::new(lhs), args) }));
    }

    Ok(Err(lhs))
  }

  fn parse_stmt(&mut self) -> Parse<'src, Stmt> {
    Ok(Stmt {
      kind: if self.check(Token::Let) {
        StmtKind::Let(self.parse_let_stmt()?)
      } else if self.eat(Token::Semi)? {
        StmtKind::Empty
      } else {
        let term = self.parse_term()?;
        let semi = self.eat(Token::Semi)?;
        StmtKind::Term(term, semi)
      },
    })
  }

  fn parse_let_stmt(&mut self) -> Parse<'src, LetStmt> {
    self.expect(Token::Let)?;
    let bind = self.parse_term_bp(BP::Assignment.inc())?;
    let init = self.eat(Token::Eq)?.then(|| self.parse_term()).transpose()?;
    Ok(LetStmt { bind, init })
  }

  fn parse_path(&mut self) -> Parse<'src, Path> {
    let segments = self.parse_delimited(PATH, Self::parse_ident)?;
    Ok(Path { segments })
  }
}

#[allow(clippy::absolute_paths)]
type Delimiters = vine_util::parser::Delimiters<Token>;

const PAREN_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenParen),
  close: Some(Token::CloseParen),
  separator: Some(Token::Comma),
};

const BRACE: Delimiters =
  Delimiters { open: Some(Token::OpenBrace), close: Some(Token::CloseBrace), separator: None };

const BRACE_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenBrace),
  close: Some(Token::CloseBrace),
  separator: Some(Token::Comma),
};

const BRACKET_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenBracket),
  close: Some(Token::CloseBracket),
  separator: Some(Token::Comma),
};

const PATH: Delimiters = Delimiters { open: None, close: None, separator: Some(Token::ColonColon) };

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum BP {
  Min,
  ControlFlow,
  Assignment,
  Range,
  LogicalOr,
  LogicalAnd,
  Comparison,
  BitOr,
  BitXor,
  BitAnd,
  BitShift,
  Additive,
  Multiplicative,
  Prefix,
  Question,
  Max,
}

impl BP {
  const fn inc(self) -> Self {
    if self as u8 == BP::Max as u8 {
      self
    } else {
      unsafe { transmute::<u8, BP>(self as u8 + 1) }
    }
  }

  fn permits(self, other: Self) -> bool {
    other >= self
  }
}

#[rustfmt::skip]
const BINARY_OP_TABLE: &[(BP, Token, BinaryOp)] = &[
  (BP::Range,          Token::DotDot,   BinaryOp::Range),
  (BP::Range,          Token::DotDotEq, BinaryOp::RangeTo),
  (BP::BitOr,          Token::Or,       BinaryOp::BitOr),
  (BP::BitXor,         Token::Caret,    BinaryOp::BitXor),
  (BP::BitAnd,         Token::And,      BinaryOp::BitAnd),
  (BP::BitShift,       Token::Shl,      BinaryOp::Shl),
  (BP::BitShift,       Token::Shr,      BinaryOp::Shr),
  (BP::Additive,       Token::Plus,     BinaryOp::Add),
  (BP::Additive,       Token::Minus,    BinaryOp::Sub),
  (BP::Additive,       Token::PlusPlus, BinaryOp::Concat),
  (BP::Multiplicative, Token::Star,     BinaryOp::Mul),
  (BP::Multiplicative, Token::Slash,    BinaryOp::Div),
  (BP::Multiplicative, Token::Percent,  BinaryOp::Mod),
];

#[rustfmt::skip]
const COMPARISON_OP_TABLE: &[(Token, ComparisonOp)] = &[
  (Token::EqEq, ComparisonOp::Eq),
  (Token::Ne,   ComparisonOp::Ne),
  (Token::Lt,   ComparisonOp::Lt),
  (Token::Gt,   ComparisonOp::Gt),
  (Token::Le,   ComparisonOp::Le),
  (Token::Ge,   ComparisonOp::Ge),
];
