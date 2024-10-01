use std::{mem::transmute, path::PathBuf};

use ivy::parser::IvyParser;
use vine_util::{
  interner::StringInterner,
  parser::{Parser, ParserState},
};

use crate::{
  ast::{
    BinaryOp, Block, ComparisonOp, ConstItem, Enum, FnItem, Ident, InlineIvy, Item, ItemKind,
    LetStmt, ModItem, ModKind, Path, PatternItem, Span, Stmt, StmtKind, Struct, Term, TermKind,
    UnaryOp, UseTree,
  },
  diag::Diag,
  lexer::Token,
};

pub struct VineParser<'ctx, 'src> {
  pub(crate) interner: &'ctx StringInterner<'static>,
  pub(crate) state: ParserState<'src, Token>,
  pub(crate) file: usize,
}

impl<'ctx, 'src> Parser<'src> for VineParser<'ctx, 'src> {
  type Token = Token;
  type Error = Diag;

  fn state(&mut self) -> &mut ParserState<'src, Self::Token> {
    &mut self.state
  }

  fn lex_error(&self) -> Self::Error {
    Diag::LexError { span: self.span() }
  }

  fn unexpected_error(&self) -> Diag {
    Diag::UnexpectedToken {
      span: self.span(),
      expected: self.state.expected,
      found: self.state.token,
    }
  }
}

type Parse<'src, T = ()> = Result<T, Diag>;

impl<'ctx, 'src> VineParser<'ctx, 'src> {
  pub fn parse(
    interner: &'ctx StringInterner<'static>,
    src: &'src str,
    file: usize,
  ) -> Parse<'src, Vec<Item>> {
    let mut parser = VineParser { interner, state: ParserState::new(src), file };
    parser.bump()?;
    let mut items = Vec::new();
    while parser.state.token.is_some() {
      items.push(parser.parse_item()?);
    }
    Ok(items)
  }

  fn parse_item(&mut self) -> Parse<'src, Item> {
    self.maybe_parse_item()?.ok_or_else(|| self.unexpected_error())
  }

  fn maybe_parse_item(&mut self) -> Parse<'src, Option<Item>> {
    let span = self.start_span();
    let kind = match () {
      _ if self.check(Token::Fn) => ItemKind::Fn(self.parse_fn_item()?),
      _ if self.check(Token::Const) => ItemKind::Const(self.parse_const_item()?),
      _ if self.check(Token::Struct) => ItemKind::Struct(self.parse_struct_item()?),
      _ if self.check(Token::Enum) => ItemKind::Enum(self.parse_enum_item()?),
      _ if self.check(Token::Pattern) => ItemKind::Pattern(self.parse_pattern_item()?),
      _ if self.check(Token::Mod) => ItemKind::Mod(self.parse_mod_item()?),
      _ if self.check(Token::Use) => ItemKind::Use(self.parse_use_item()?),
      _ if self.check(Token::InlineIvy) => ItemKind::Ivy(self.parse_ivy_item()?),
      _ => return Ok(None),
    };
    let span = self.end_span(span);
    Ok(Some(Item { span, kind }))
  }

  fn parse_ident(&mut self) -> Parse<'src, Ident> {
    let token = self.expect(Token::Ident)?;
    Ok(Ident(self.interner.intern(token)))
  }

  fn parse_num(&mut self) -> Parse<'src, TermKind> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    if token.contains('.') {
      Ok(TermKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?))
    } else {
      Ok(TermKind::U32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
    }
  }

  fn parse_string(&mut self) -> Parse<'src, String> {
    let span = self.span();
    self.parse_string_like(Token::String, |_| Diag::InvalidStr { span })
  }

  fn parse_char(&mut self) -> Parse<'src, char> {
    let span = self.span();
    let str = self.parse_string_like(Token::Char, |_| Diag::InvalidChar { span })?;
    let mut chars = str.chars();
    let char = chars.next().ok_or(Diag::InvalidChar { span })?;
    if chars.next().is_some() {
      Err(Diag::InvalidChar { span })?
    }
    Ok(char)
  }

  fn parse_fn_item(&mut self) -> Parse<'src, FnItem> {
    self.expect(Token::Fn)?;
    let name = self.parse_ident()?;
    let params = self.parse_term_list()?;
    let body = self.parse_block()?;
    Ok(FnItem { name, params, body: Term { span: body.span, kind: TermKind::Block(body) } })
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
    if self.eat(Token::Eq)? {
      let path = self.parse_string()?;
      self.expect(Token::Semi)?;
      Ok(ModItem { name, kind: ModKind::Unloaded(PathBuf::from(path)) })
    } else {
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      Ok(ModItem { name, kind: ModKind::Loaded(items) })
    }
  }

  fn parse_use_item(&mut self) -> Parse<'src, UseTree> {
    self.expect(Token::Use)?;
    let tree = self.parse_use_tree()?;
    self.expect(Token::Semi)?;
    Ok(tree)
  }

  fn parse_use_tree(&mut self) -> Parse<'src, UseTree> {
    let span = self.start_span();
    let mut path = Path { span: Span::NONE, segments: Vec::new(), absolute: false, resolved: None };
    while self.check(Token::Ident) {
      path.segments.push(self.parse_ident()?);
      path.span = self.end_span(span);
      if !self.eat(Token::ColonColon)? {
        return Ok(UseTree { path, children: None });
      }
    }
    let children = self.parse_delimited(BRACE_COMMA, Self::parse_use_tree)?;
    Ok(UseTree { path, children: Some(children) })
  }

  fn parse_ivy_item(&mut self) -> Parse<'src, InlineIvy> {
    let span = self.span();
    self.expect(Token::InlineIvy)?;
    self.expect(Token::Bang)?;
    let name = self.parse_ident()?;
    if !self.check(Token::OpenBrace) {
      self.unexpected()?;
    }
    let vine_lexer = &mut self.state.lexer;
    let mut ivy_parser = IvyParser { state: ParserState::new(vine_lexer.source()) };
    ivy_parser.state.lexer.bump(vine_lexer.span().end);
    ivy_parser.bump().map_err(|_| Diag::InvalidIvy { span })?;
    let net = ivy_parser.parse_net_inner().map_err(|_| Diag::InvalidIvy { span })?;
    vine_lexer.bump(ivy_parser.state.lexer.span().end - vine_lexer.span().end);
    self.bump()?;
    Ok(InlineIvy { name, net })
  }

  fn parse_term_list(&mut self) -> Parse<'src, Vec<Term>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_term)
  }

  fn parse_block(&mut self) -> Parse<'src, Block> {
    let span = self.start_span();
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    let span = self.end_span(span);
    Ok(Block { span, stmts })
  }

  fn parse_term(&mut self) -> Parse<'src, Term> {
    self.parse_term_bp(BP::Min)
  }

  fn parse_term_bp(&mut self, bp: BP) -> Parse<'src, Term> {
    let span = self.start_span();
    let mut term = self.parse_term_prefix()?;
    loop {
      term = match self.parse_term_postfix(term, bp)? {
        Ok(kind) => Term { span: self.end_span(span), kind },
        Err(term) => return Ok(term),
      }
    }
  }

  fn parse_term_prefix(&mut self) -> Parse<'src, Term> {
    let span = self.start_span();
    let kind = self._parse_term_prefix(span)?;
    let span = self.end_span(span);
    Ok(Term { span, kind })
  }
  fn _parse_term_prefix(&mut self, span: usize) -> Parse<'src, TermKind> {
    if self.eat(Token::Return)? {
      return Ok(TermKind::Return(Box::new(self.parse_term_bp(BP::ControlFlow)?)));
    }
    if self.eat(Token::Break)? {
      return Ok(TermKind::Break);
    }
    if self.eat(Token::And)? {
      return Ok(TermKind::Ref(Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_term_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      return Ok(TermKind::Ref(Box::new(Term { span, kind: TermKind::Ref(Box::new(inner)) })));
    }
    if self.eat(Token::Star)? {
      return Ok(TermKind::Deref(Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Move)? {
      return Ok(TermKind::Move(Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Tilde)? {
      return Ok(TermKind::Inverse(Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Minus)? {
      return Ok(TermKind::UnaryOp(UnaryOp::Neg, Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Bang)? {
      return Ok(TermKind::UnaryOp(UnaryOp::Not, Box::new(self.parse_term_bp(BP::Prefix)?)));
    }
    if self.check(Token::Num) {
      return self.parse_num();
    }
    if self.check(Token::Char) {
      return Ok(TermKind::U32(self.parse_char()? as u32));
    }
    if self.check(Token::String) {
      return Ok(TermKind::String(self.parse_string()?));
    }
    if self.check(Token::Ident) || self.check(Token::ColonColon) {
      return Ok(TermKind::Path(self.parse_path()?));
    }
    if self.check(Token::OpenParen) {
      let mut tuple = false;
      let mut terms = self.parse_delimited(PAREN_COMMA, |self_| {
        let term = self_.parse_term()?;
        if self_.check(Token::Comma) {
          tuple = true;
        }
        Ok(term)
      })?;
      if terms.len() == 1 && !tuple {
        return Ok(terms.pop().unwrap().kind);
      }
      return Ok(TermKind::Tuple(terms));
    }
    if self.check(Token::OpenBracket) {
      let terms = self.parse_delimited(BRACKET_COMMA, Self::parse_term)?;
      return Ok(TermKind::List(terms));
    }
    if self.check(Token::OpenBrace) {
      return Ok(TermKind::Block(self.parse_block()?));
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
        Term { span: Span::NONE, kind: TermKind::Hole }
      };
      return Ok(TermKind::If(Box::new(cond), then, Box::new(else_)));
    }
    if self.eat(Token::While)? {
      if self.eat(Token::Let)? {
        let pat = self.parse_term_bp(BP::Assignment.inc())?;
        self.expect(Token::Eq)?;
        let value = self.parse_term()?;
        let body = self.parse_block()?;
        return Ok(TermKind::WhileLet(Box::new(pat), Box::new(value), body));
      } else {
        let cond = self.parse_term()?;
        let body = self.parse_block()?;
        return Ok(TermKind::While(Box::new(cond), body));
      }
    }
    if self.eat(Token::Loop)? {
      let body = self.parse_block()?;
      return Ok(TermKind::Loop(body));
    }
    if self.eat(Token::For)? {
      let pat = self.parse_term()?;
      self.expect(Token::In)?;
      let iter = self.parse_term()?;
      let body = self.parse_block()?;
      return Ok(TermKind::For(Box::new(pat), Box::new(iter), body));
    }
    if self.eat(Token::Fn)? {
      let params = self.parse_term_list()?;
      let body = self.parse_term()?;
      return Ok(TermKind::Fn(params, Box::new(body)));
    }
    if self.eat(Token::Match)? {
      let scrutinee = self.parse_term()?;
      let arms = self.parse_delimited(BRACE_COMMA, |self_| {
        let pat = self_.parse_term()?;
        self_.expect(Token::ThickArrow)?;
        let value = self_.parse_term()?;
        Ok((pat, value))
      })?;
      return Ok(TermKind::Match(Box::new(scrutinee), arms));
    }
    if self.eat(Token::Hole)? {
      return Ok(TermKind::Hole);
    }
    self.unexpected()
  }

  fn parse_term_postfix(&mut self, lhs: Term, bp: BP) -> Parse<'src, Result<TermKind, Term>> {
    for &(lbp, token, op) in BINARY_OP_TABLE {
      let rbp = lbp.inc(); // left-associative
      if bp.permits(lbp) && self.eat(token)? {
        if self.eat(Token::Eq)? {
          return Ok(Ok(TermKind::BinaryOpAssign(
            op,
            Box::new(lhs),
            Box::new(self.parse_term_bp(BP::Assignment)?),
          )));
        } else {
          return Ok(Ok(TermKind::BinaryOp(op, Box::new(lhs), Box::new(self.parse_term_bp(rbp)?))));
        }
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
        return Ok(Ok(TermKind::ComparisonOp(Box::new(lhs), rhs)));
      }
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Eq)? {
      let rhs = self.parse_term_bp(BP::Assignment)?;
      return Ok(Ok(TermKind::Assign(Box::new(lhs), Box::new(rhs))));
    }

    if self.eat(Token::Dot)? {
      let path = self.parse_path()?;
      if self.check(Token::OpenParen) {
        let args = self.parse_term_list()?;
        return Ok(Ok(TermKind::Method(Box::new(lhs), path, args)));
      } else {
        return Ok(Ok(TermKind::Field(Box::new(lhs), path)));
      }
    }

    if self.check(Token::OpenParen) {
      let args = self.parse_term_list()?;
      return Ok(Ok(TermKind::Call(Box::new(lhs), args)));
    }

    Ok(Err(lhs))
  }

  pub(crate) fn parse_stmt(&mut self) -> Parse<'src, Stmt> {
    let span = self.start_span();
    let kind = if self.check(Token::Let) {
      StmtKind::Let(self.parse_let_stmt()?)
    } else if self.eat(Token::Semi)? {
      StmtKind::Empty
    } else if let Some(item) = self.maybe_parse_item()? {
      StmtKind::Item(item)
    } else {
      let term = self.parse_term()?;
      let semi = self.eat(Token::Semi)?;
      StmtKind::Term(term, semi)
    };
    let span = self.end_span(span);
    Ok(Stmt { span, kind })
  }

  fn parse_let_stmt(&mut self) -> Parse<'src, LetStmt> {
    self.expect(Token::Let)?;
    let bind = self.parse_term_bp(BP::Assignment.inc())?;
    let init = self.eat(Token::Eq)?.then(|| self.parse_term()).transpose()?;
    Ok(LetStmt { bind, init })
  }

  fn parse_path(&mut self) -> Parse<'src, Path> {
    let span = self.start_span();
    let absolute = self.eat(Token::ColonColon)?;
    let segments = self.parse_delimited(PATH, Self::parse_ident)?;
    let span = self.end_span(span);
    Ok(Path { span, segments, absolute, resolved: None })
  }

  fn start_span(&self) -> usize {
    self.state.lexer.span().start
  }

  fn end_span(&self, span: usize) -> Span {
    Span { file: self.file, start: span, end: self.state.lexer.span().end }
  }

  fn span(&self) -> Span {
    self.end_span(self.start_span())
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

#[allow(dead_code)]
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
  (BP::Multiplicative, Token::Percent,  BinaryOp::Rem),
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
