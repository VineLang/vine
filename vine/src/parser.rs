use std::{mem::transmute, path::PathBuf};

use ivy::parser::IvyParser;
use vine_util::{
  interner::StringInterner,
  parser::{Parser, ParserState},
};

use crate::{
  ast::{
    BinaryOp, Block, ComparisonOp, ConstItem, Enum, Expr, ExprKind, FnItem, Ident, InlineIvy, Item,
    ItemKind, LetStmt, LogicalOp, ModItem, ModKind, Pat, PatKind, Path, PatternItem, Span, Stmt,
    StmtKind, Struct, UseTree,
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

  fn parse_num(&mut self) -> Parse<'src, ExprKind> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    if token.contains('.') {
      Ok(ExprKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?))
    } else {
      Ok(ExprKind::U32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
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
    let params = self.parse_pat_list()?;
    let body = self.parse_block()?;
    Ok(FnItem { name, params, body: Expr { span: body.span, kind: ExprKind::Block(body) } })
  }

  fn parse_const_item(&mut self) -> Parse<'src, ConstItem> {
    self.expect(Token::Const)?;
    let name = self.parse_ident()?;
    self.expect(Token::Eq)?;
    let value = self.parse_expr()?;
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

  fn parse_expr_list(&mut self) -> Parse<'src, Vec<Expr>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_expr)
  }

  fn parse_pat_list(&mut self) -> Parse<'src, Vec<Pat>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_pat)
  }

  fn parse_block(&mut self) -> Parse<'src, Block> {
    let span = self.start_span();
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    let span = self.end_span(span);
    Ok(Block { span, stmts })
  }

  fn parse_expr(&mut self) -> Parse<'src, Expr> {
    self.parse_expr_bp(BP::Min)
  }

  fn parse_expr_bp(&mut self, bp: BP) -> Parse<'src, Expr> {
    let span = self.start_span();
    let mut expr = self.parse_expr_prefix()?;
    loop {
      expr = match self.parse_expr_postfix(expr, bp)? {
        Ok(kind) => Expr { span: self.end_span(span), kind },
        Err(expr) => return Ok(expr),
      }
    }
  }

  fn parse_expr_prefix(&mut self) -> Parse<'src, Expr> {
    let span = self.start_span();
    let kind = self._parse_expr_prefix(span)?;
    let span = self.end_span(span);
    Ok(Expr { span, kind })
  }

  fn _parse_expr_prefix(&mut self, span: usize) -> Parse<'src, ExprKind> {
    if self.eat(Token::Return)? {
      if self.check(Token::Semi) {
        return Ok(ExprKind::Return(None));
      }

      return Ok(ExprKind::Return(Some(Box::new(self.parse_expr_bp(BP::ControlFlow)?))));
    }
    if self.eat(Token::Break)? {
      return Ok(ExprKind::Break);
    }
    if self.eat(Token::Continue)? {
      return Ok(ExprKind::Continue);
    }
    if self.eat(Token::And)? {
      return Ok(ExprKind::Ref(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_expr_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      return Ok(ExprKind::Ref(Box::new(Expr { span, kind: ExprKind::Ref(Box::new(inner)) })));
    }
    if self.eat(Token::Star)? {
      return Ok(ExprKind::Deref(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Move)? {
      return Ok(ExprKind::Move(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Tilde)? {
      return Ok(ExprKind::Inverse(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Minus)? {
      return Ok(ExprKind::Neg(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Bang)? {
      return Ok(ExprKind::Not(Box::new(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.check(Token::Num) {
      return self.parse_num();
    }
    if self.check(Token::Char) {
      return Ok(ExprKind::U32(self.parse_char()? as u32));
    }
    if self.check(Token::String) {
      return Ok(ExprKind::String(self.parse_string()?));
    }
    if self.check(Token::Ident) || self.check(Token::ColonColon) {
      return Ok(ExprKind::Path(self.parse_path()?));
    }
    if self.check(Token::OpenParen) {
      let mut tuple = false;
      let mut exprs = self.parse_delimited(PAREN_COMMA, |self_| {
        let expr = self_.parse_expr()?;
        if self_.check(Token::Comma) {
          tuple = true;
        }
        Ok(expr)
      })?;
      if exprs.len() == 1 && !tuple {
        return Ok(exprs.pop().unwrap().kind);
      }
      return Ok(ExprKind::Tuple(exprs));
    }
    if self.check(Token::OpenBracket) {
      let exprs = self.parse_delimited(BRACKET_COMMA, Self::parse_expr)?;
      return Ok(ExprKind::List(exprs));
    }
    if self.check(Token::OpenBrace) {
      return Ok(ExprKind::Block(self.parse_block()?));
    }
    if self.eat(Token::If)? {
      let cond = self.parse_expr()?;
      let then = self.parse_block()?;
      let else_ = if self.eat(Token::Else)? {
        if self.check(Token::If) || self.check(Token::OpenBrace) {
          self.parse_expr()?
        } else {
          self.unexpected()?
        }
      } else {
        Expr { span: Span::NONE, kind: ExprKind::Block(Block::default()) }
      };
      return Ok(ExprKind::If(Box::new(cond), then, Box::new(else_)));
    }
    if self.eat(Token::While)? {
      let cond = self.parse_expr()?;
      let body = self.parse_block()?;
      return Ok(ExprKind::While(Box::new(cond), body));
    }
    if self.eat(Token::Loop)? {
      let body = self.parse_block()?;
      return Ok(ExprKind::Loop(body));
    }
    if self.eat(Token::For)? {
      let pat = self.parse_pat()?;
      self.expect(Token::In)?;
      let iter = self.parse_expr()?;
      let body = self.parse_block()?;
      return Ok(ExprKind::For(Box::new(pat), Box::new(iter), body));
    }
    if self.eat(Token::Fn)? {
      let params = self.parse_pat_list()?;
      let body = self.parse_expr()?;
      return Ok(ExprKind::Fn(params, Box::new(body)));
    }
    if self.eat(Token::Match)? {
      let scrutinee = self.parse_expr()?;
      let arms = self.parse_delimited(BRACE_COMMA, |self_| {
        let pat = self_.parse_pat()?;
        self_.expect(Token::ThickArrow)?;
        let value = self_.parse_expr()?;
        Ok((pat, value))
      })?;
      return Ok(ExprKind::Match(Box::new(scrutinee), arms));
    }
    if self.eat(Token::Hole)? {
      return Ok(ExprKind::Hole);
    }
    self.unexpected()
  }

  fn parse_expr_postfix(&mut self, lhs: Expr, bp: BP) -> Parse<'src, Result<ExprKind, Expr>> {
    for &(lbp, token, op) in BINARY_OP_TABLE {
      let rbp = lbp.inc(); // left-associative
      if bp.permits(lbp) && self.eat(token)? {
        if self.eat(Token::Eq)? {
          return Ok(Ok(ExprKind::BinaryOpAssign(
            op,
            Box::new(lhs),
            Box::new(self.parse_expr_bp(BP::Assignment)?),
          )));
        } else {
          return Ok(Ok(ExprKind::BinaryOp(op, Box::new(lhs), Box::new(self.parse_expr_bp(rbp)?))));
        }
      }
    }

    if bp.permits(BP::LogicalAnd) && self.eat(Token::AndAnd)? {
      let rhs = self.parse_expr_bp(BP::LogicalAnd)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::And, Box::new(lhs), Box::new(rhs))));
    }

    if bp.permits(BP::LogicalOr) && self.eat(Token::OrOr)? {
      let rhs = self.parse_expr_bp(BP::LogicalOr)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::Or, Box::new(lhs), Box::new(rhs))));
    }

    if bp.permits(BP::LogicalImplies) && self.eat(Token::ThickArrow)? {
      let rhs = self.parse_expr_bp(BP::LogicalImplies)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::Implies, Box::new(lhs), Box::new(rhs))));
    }

    if bp.permits(BP::Is) && self.eat(Token::Is)? {
      let rhs = self.parse_pat()?;
      return Ok(Ok(ExprKind::Is(Box::new(lhs), Box::new(rhs))));
    }

    if bp.permits(BP::Comparison) {
      let mut rhs = Vec::new();
      'main: loop {
        for &(token, op) in COMPARISON_OP_TABLE {
          if self.eat(token)? {
            rhs.push((op, self.parse_expr_bp(BP::Comparison.inc())?));
            continue 'main;
          }
        }
        break;
      }
      if !rhs.is_empty() {
        return Ok(Ok(ExprKind::ComparisonOp(Box::new(lhs), rhs)));
      }
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Eq)? {
      let rhs = self.parse_expr_bp(BP::Assignment)?;
      return Ok(Ok(ExprKind::Assign(Box::new(lhs), Box::new(rhs))));
    }

    if self.eat(Token::Dot)? {
      let path = self.parse_path()?;
      if self.check(Token::OpenParen) {
        let args = self.parse_expr_list()?;
        return Ok(Ok(ExprKind::Method(Box::new(lhs), path, args)));
      } else {
        return Ok(Ok(ExprKind::Field(Box::new(lhs), path)));
      }
    }

    if self.check(Token::OpenParen) {
      let args = self.parse_expr_list()?;
      return Ok(Ok(ExprKind::Call(Box::new(lhs), args)));
    }

    Ok(Err(lhs))
  }

  fn parse_pat(&mut self) -> Parse<'src, Pat> {
    let span = self.start_span();
    let kind = self._parse_pat(span)?;
    let span = self.end_span(span);
    Ok(Pat { span, kind })
  }

  fn _parse_pat(&mut self, span: usize) -> Parse<'src, PatKind> {
    if self.eat(Token::Hole)? {
      return Ok(PatKind::Hole);
    }
    if self.check(Token::Ident) || self.check(Token::ColonColon) {
      let path = self.parse_path()?;
      let args = self.check(Token::OpenParen).then(|| self.parse_pat_list()).transpose()?;
      return Ok(PatKind::Adt(path, args));
    }
    if self.eat(Token::And)? {
      return Ok(PatKind::Ref(Box::new(self.parse_pat()?)));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_pat()?;
      let span = self.end_span(span + 1);
      return Ok(PatKind::Ref(Box::new(Pat { span, kind: PatKind::Ref(Box::new(inner)) })));
    }
    if self.eat(Token::Star)? {
      return Ok(PatKind::Deref(Box::new(self.parse_pat()?)));
    }
    if self.eat(Token::Move)? {
      return Ok(PatKind::Move(Box::new(self.parse_pat()?)));
    }
    if self.eat(Token::Tilde)? {
      return Ok(PatKind::Inverse(Box::new(self.parse_pat()?)));
    }
    if self.check(Token::OpenParen) {
      let mut tuple = false;
      let mut pats = self.parse_delimited(PAREN_COMMA, |self_| {
        let expr = self_.parse_pat()?;
        if self_.check(Token::Comma) {
          tuple = true;
        }
        Ok(expr)
      })?;
      if pats.len() == 1 && !tuple {
        return Ok(pats.pop().unwrap().kind);
      }
      return Ok(PatKind::Tuple(pats));
    }
    self.unexpected()
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
      let expr = self.parse_expr()?;
      let semi = self.eat(Token::Semi)?;
      StmtKind::Expr(expr, semi)
    };
    let span = self.end_span(span);
    Ok(Stmt { span, kind })
  }

  fn parse_let_stmt(&mut self) -> Parse<'src, LetStmt> {
    self.expect(Token::Let)?;
    let bind = self.parse_pat()?;
    let init = self.eat(Token::Eq)?.then(|| self.parse_expr()).transpose()?;
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
  LogicalImplies,
  LogicalOr,
  LogicalAnd,
  Is,
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
