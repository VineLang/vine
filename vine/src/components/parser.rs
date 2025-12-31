use std::mem::transmute;

use vine_util::parser::{Parse, ParserState};

use crate::{
  components::{
    lexer::{Lexer, Token},
    loader::FileId,
  },
  structures::{
    ast::{Key, Sign},
    diag::Diag,
  },
};

use crate::structures::ast::{
  Attr, AttrKind, BinaryOp, ComparisonOp, Expr, ExprKind, Flex, Ident, Impl, ImplKind, Item,
  LogicalOp, Pat, PatKind, Span, Stmt, StmtKind, Trait, TraitKind, Ty, TyKind, Vis,
};

pub struct Parser<'src> {
  pub(crate) file: FileId,
  pub(crate) state: ParserState<'src, Lexer<'src>>,
}

impl<'src> Parse<'src> for Parser<'src> {
  type Token = Token;
  type Lexer = Lexer<'src>;
  type Error = Diag;

  fn state(&mut self) -> &mut ParserState<'src, Lexer<'src>> {
    &mut self.state
  }

  fn unexpected_error(&self) -> Diag {
    Diag::UnexpectedToken {
      span: self.span(),
      expected: self.state.expected.clone(),
      found: self.state.token,
    }
  }
}

impl<'src> Parser<'src> {
  pub fn new(lexer: Lexer<'src>) -> Result<Self, Diag> {
    Ok(Parser { file: lexer.file, state: ParserState::new(lexer)? })
  }

  pub fn parse(file: FileId, src: &'src str) -> Result<Vec<Item>, Diag> {
    let mut parser = Parser::new(Lexer::new(file, src))?;
    let mut items = Vec::new();
    while !parser.check(Token::Eof) {
      items.push(parser.parse_item()?);
    }
    Ok(items)
  }

  pub(crate) fn parse_item(&mut self) -> Result<Item, Diag> {
    self.maybe_parse_item()?.ok_or_else(|| self.unexpected_error())
  }

  fn maybe_parse_item(&mut self) -> Result<Option<Item>, Diag> {
    self.state.expected.start_group(Token::GroupItem);
    let span = self.start_span();
    let mut doc = Vec::new();
    while self.check(Token::DocComment) {
      doc.push(self.expect(Token::DocComment)?.to_owned());
    }
    let mut attrs = Vec::new();
    while self.check(Token::HashBracket) {
      attrs.push(self.parse_attr()?);
    }
    let vis = self.parse_vis()?;
    let (name_span, kind) = match () {
      _ if self.check(Token::Fn) => self.parse_fn_item()?,
      _ if self.check(Token::Const) => self.parse_const_item()?,
      _ if self.check(Token::Struct) => self.parse_struct_item()?,
      _ if self.check(Token::Enum) => self.parse_enum_item()?,
      _ if self.check(Token::Type) => self.parse_type_item()?,
      _ if self.check(Token::Mod) => self.parse_mod_item()?,
      _ if self.check(Token::Trait) => self.parse_trait_item()?,
      _ if self.check(Token::Impl) => self.parse_impl_item()?,
      _ if self.check(Token::Use) => self.parse_use_item()?,
      _ => {
        if doc.is_empty() && attrs.is_empty() {
          self.state.expected.end_group();
          return Ok(None);
        } else {
          self.unexpected()?
        }
      }
    };
    let span = self.end_span(span);
    Ok(Some(Item { span, vis, name_span, docs: doc, attrs, kind }))
  }

  pub(crate) fn parse_vis(&mut self) -> Result<Vis, Diag> {
    Ok(if self.eat(Token::Pub)? {
      if self.eat(Token::Dot)? {
        let span = self.start_span();
        let ancestor = self.parse_ident()?;
        let span = self.end_span(span);
        Vis::PublicTo(span, ancestor)
      } else {
        Vis::Public
      }
    } else {
      Vis::Private
    })
  }

  fn parse_attr(&mut self) -> Result<Attr, Diag> {
    let span = self.start_span();
    self.expect(Token::HashBracket)?;
    let ident_span = self.start_span();
    let ident = self.expect(Token::Ident)?;
    let ident_span = self.end_span(ident_span);
    let kind = match ident {
      "builtin" => {
        self.expect(Token::Eq)?;
        AttrKind::Builtin(self.parse_builtin()?)
      }
      "manual" => AttrKind::Manual,
      "basic" => AttrKind::Basic,
      "become" => {
        self.expect(Token::OpenParen)?;
        let path = self.parse_path()?;
        self.expect(Token::CloseParen)?;
        AttrKind::Become(path)
      }
      "cfg" => {
        self.expect(Token::OpenParen)?;
        let cfg = self.parse_cfg()?;
        self.expect(Token::CloseParen)?;
        AttrKind::Cfg(cfg)
      }
      "frameless" => AttrKind::Frameless,
      "test" => AttrKind::Test,
      _ => Err(Diag::UnknownAttribute { span: ident_span })?,
    };
    self.expect(Token::CloseBracket)?;
    let span = self.end_span(span);
    Ok(Attr { span, kind })
  }

  pub fn parse_ident(&mut self) -> Result<Ident, Diag> {
    let token = self.expect(Token::Ident)?;
    Ok(Ident(token.into()))
  }

  pub(crate) fn parse_flex(&mut self) -> Result<Flex, Diag> {
    if self.eat(Token::Plus)? {
      Ok(Flex::Fork)
    } else if self.eat(Token::Question)? {
      Ok(Flex::Drop)
    } else if self.eat(Token::Star)? {
      Ok(Flex::Full)
    } else {
      Ok(Flex::None)
    }
  }

  pub(crate) fn parse_exprs(&mut self) -> Result<Vec<Expr>, Diag> {
    self.parse_delimited(PAREN_COMMA, Self::parse_expr)
  }

  pub(crate) fn parse_expr(&mut self) -> Result<Expr, Diag> {
    self.parse_expr_bp(BP::Min)
  }

  pub(crate) fn maybe_parse_expr_bp(&mut self, bp: BP) -> Result<Option<Expr>, Diag> {
    let span = self.start_span();
    let Some(mut expr) = self.maybe_parse_expr_prefix()? else {
      return Ok(None);
    };
    loop {
      expr = match self.parse_expr_postfix(expr, bp)? {
        Ok(kind) => Expr { span: self.end_span(span), kind: Box::new(kind) },
        Err(expr) => return Ok(Some(expr)),
      }
    }
  }

  pub(crate) fn parse_expr_bp(&mut self, bp: BP) -> Result<Expr, Diag> {
    match self.maybe_parse_expr_bp(bp)? {
      Some(expr) => Ok(expr),
      None => self.unexpected(),
    }
  }

  fn maybe_parse_expr_prefix(&mut self) -> Result<Option<Expr>, Diag> {
    let span = self.start_span();
    let Some(kind) = self._maybe_parse_expr_prefix(span)? else {
      return Ok(None);
    };
    let span = self.end_span(span);
    Ok(Some(Expr { span, kind: Box::new(kind) }))
  }

  fn _maybe_parse_expr_prefix(&mut self, span: usize) -> Result<Option<ExprKind>, Diag> {
    self.state.expected.start_group(Token::GroupExpr);
    if self.check(Token::Amp) || self.check(Token::AmpAmp) {
      return Ok(Some(self.parse_expr_ref(span)?));
    }
    if self.check(Token::Star) || self.check(Token::StarStar) {
      return Ok(Some(self.parse_expr_deref(span)?));
    }
    if self.eat(Token::Tilde)? {
      return Ok(Some(ExprKind::Inverse(self.parse_expr_bp(BP::Prefix)?, false)));
    }
    if self.eat(Token::Plus)? {
      return Ok(Some(ExprKind::Sign(Sign::Pos, self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Minus)? {
      return Ok(Some(ExprKind::Sign(Sign::Neg, self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Bang)? {
      return Ok(Some(ExprKind::Not(self.parse_expr_bp(BP::Prefix)?)));
    }
    if self.check(Token::DotDot) || self.check(Token::DotDotEq) {
      return Ok(Some(self._parse_expr_range(None)?));
    }
    if self.check(Token::Num) {
      return Ok(Some(self.parse_expr_numeric()?));
    }
    if self.check(Token::SingleQuote) {
      return Ok(Some(self.parse_expr_char()?));
    }
    if self.check(Token::DoubleQuote) {
      return Ok(Some(self.parse_expr_string()?));
    }
    if self.eat(Token::True)? {
      return Ok(Some(ExprKind::Bool(true)));
    }
    if self.eat(Token::False)? {
      return Ok(Some(ExprKind::Bool(false)));
    }
    if self.check(Token::Ident) || self.check(Token::Hash) {
      return Ok(Some(self.parse_expr_path()?));
    }
    if self.check(Token::OpenParen) {
      return Ok(Some(self.parse_expr_paren()?));
    }
    if self.check(Token::OpenBrace) {
      return Ok(Some(self.parse_expr_object()?));
    }
    if self.check(Token::OpenBracket) {
      return Ok(Some(self.parse_expr_list()?));
    }
    if self.check(Token::Do) {
      return Ok(Some(self.parse_expr_do()?));
    }
    if self.check(Token::If) {
      return Ok(Some(self.parse_expr_if()?));
    }
    if self.check(Token::When) {
      return Ok(Some(self.parse_expr_when()?));
    }
    if self.check(Token::While) {
      return Ok(Some(self.parse_expr_while()?));
    }
    if self.check(Token::Loop) {
      return Ok(Some(self.parse_expr_loop()?));
    }
    if self.check(Token::For) {
      return Ok(Some(self.parse_expr_for()?));
    }
    if self.check(Token::Fn) {
      return Ok(Some(self.parse_expr_fn()?));
    }
    if self.check(Token::Match) {
      return Ok(Some(self.parse_expr_match()?));
    }
    if self.eat(Token::Hole)? {
      return Ok(Some(ExprKind::Hole));
    }
    if self.check(Token::InlineIvy) {
      return Ok(Some(self.parse_inline_ivy()?));
    }
    self.state.expected.end_group();
    Ok(None)
  }

  fn parse_expr_postfix(&mut self, lhs: Expr, bp: BP) -> Result<Result<ExprKind, Expr>, Diag> {
    self.state.expected.start_group(Token::GroupExprPostfix);
    for &(lbp, associativity, token, op) in BINARY_OP_TABLE {
      let rbp = match associativity {
        Associativity::Left => lbp.inc(),
        Associativity::Right => lbp,
      };
      if bp.permits(lbp) && self.eat(token)? {
        if self.eat(Token::Eq)? {
          return Ok(Ok(ExprKind::BinaryOpAssign(op, lhs, self.parse_expr_bp(BP::Assignment)?)));
        } else {
          return Ok(Ok(ExprKind::BinaryOp(op, lhs, self.parse_expr_bp(rbp)?)));
        }
      }
    }

    if bp.permits(BP::LogicalAnd) && self.eat(Token::And)? {
      let rhs = self.parse_expr_bp(BP::LogicalAnd)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::And, lhs, rhs)));
    }

    if bp.permits(BP::LogicalOr) && self.eat(Token::Or)? {
      let rhs = self.parse_expr_bp(BP::LogicalOr)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::Or, lhs, rhs)));
    }

    if bp.permits(BP::LogicalImplies) && self.eat(Token::Impl)? {
      let rhs = self.parse_expr_bp(BP::LogicalImplies)?;
      return Ok(Ok(ExprKind::LogicalOp(LogicalOp::Implies, lhs, rhs)));
    }

    if bp.permits(BP::Is) && self.eat(Token::Is)? {
      let rhs = self.parse_pat()?;
      return Ok(Ok(ExprKind::Is(lhs, rhs)));
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
        return Ok(Ok(ExprKind::ComparisonOp(lhs, rhs)));
      }
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Eq)? {
      let rhs = self.parse_expr_bp(BP::Assignment)?;
      return Ok(Ok(ExprKind::Assign(false, lhs, rhs)));
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Tilde)? {
      self.expect(Token::Eq)?;
      let rhs = self.parse_expr_bp(BP::Assignment)?;
      return Ok(Ok(ExprKind::Assign(true, lhs, rhs)));
    }

    if bp.permits(BP::Annotation) && self.eat(Token::As)? {
      let ty = self.parse_ty()?;
      return Ok(Ok(ExprKind::Cast(lhs, ty, false)));
    }

    if self.eat(Token::Bang)? {
      return Ok(Ok(ExprKind::Unwrap(lhs)));
    }

    if bp.permits(BP::Range) && (self.check(Token::DotDot) || self.check(Token::DotDotEq)) {
      return Ok(Ok(self._parse_expr_range(Some(lhs))?));
    }

    if self.eat(Token::Dot)? {
      if self.eat(Token::Amp)? {
        return Ok(Ok(ExprKind::Ref(lhs, true)));
      }
      if self.eat(Token::Star)? {
        return Ok(Ok(ExprKind::Deref(lhs, true)));
      }
      if self.eat(Token::Tilde)? {
        return Ok(Ok(ExprKind::Inverse(lhs, true)));
      }
      if self.eat(Token::Try)? {
        return Ok(Ok(ExprKind::Try(lhs)));
      }
      if self.eat(Token::As)? {
        self.expect(Token::OpenBracket)?;
        let ty = self.parse_ty()?;
        self.expect(Token::CloseBracket)?;
        return Ok(Ok(ExprKind::Cast(lhs, ty, true)));
      }
      let ident_span = self.span();
      let ident = self.parse_ident()?;
      if self.check(Token::OpenBracket) || self.check(Token::OpenParen) {
        let generics = self.parse_generic_args()?;
        let args = self.parse_exprs()?;
        return Ok(Ok(ExprKind::Method(lhs, ident_span, ident, generics, args)));
      } else {
        return Ok(Ok(ExprKind::ObjectField(lhs, Key { span: ident_span, ident })));
      }
    }

    if self.check(Token::TupleKey) {
      return Ok(Ok(self._parse_expr_tuple_field(lhs)?));
    }

    if self.check(Token::OpenParen) {
      let args = self.parse_exprs()?;
      return Ok(Ok(ExprKind::Call(lhs, args)));
    }

    self.state.expected.end_group();

    Ok(Err(lhs))
  }

  pub(crate) fn parse_pats(&mut self) -> Result<Vec<Pat>, Diag> {
    self.parse_delimited(PAREN_COMMA, Self::parse_pat)
  }

  pub(crate) fn parse_pat(&mut self) -> Result<Pat, Diag> {
    self.parse_pat_bp(BP::Min)
  }

  pub(crate) fn parse_pat_bp(&mut self, bp: BP) -> Result<Pat, Diag> {
    let span = self.start_span();
    let mut pat = self.parse_pat_prefix()?;
    loop {
      pat = match self.parse_pat_postfix(pat, bp)? {
        Ok(kind) => Pat { span: self.end_span(span), kind: Box::new(kind) },
        Err(pat) => return Ok(pat),
      }
    }
  }

  fn parse_pat_prefix(&mut self) -> Result<Pat, Diag> {
    let span = self.start_span();
    let kind = self._parse_pat_prefix(span)?;
    let span = self.end_span(span);
    Ok(Pat { span, kind: Box::new(kind) })
  }

  fn _parse_pat_prefix(&mut self, span: usize) -> Result<PatKind, Diag> {
    self.state.expected.start_group(Token::GroupPat);
    if self.eat(Token::Hole)? {
      return Ok(PatKind::Hole);
    }
    if self.check(Token::Ident) || self.check(Token::Hash) {
      return self.parse_pat_path();
    }
    if self.check(Token::Amp) || self.check(Token::AmpAmp) {
      return self.parse_pat_ref(span);
    }
    if self.check(Token::Star) || self.check(Token::StarStar) {
      return self.parse_pat_deref(span);
    }
    if self.eat(Token::Tilde)? {
      return Ok(PatKind::Inverse(self.parse_pat_bp(BP::Prefix)?));
    }
    if self.check(Token::OpenParen) {
      return self.parse_pat_paren();
    }
    if self.check(Token::OpenBrace) {
      return self.parse_pat_object();
    }
    self.state.expected.end_group();
    self.unexpected()
  }

  fn parse_pat_postfix(&mut self, lhs: Pat, bp: BP) -> Result<Result<PatKind, Pat>, Diag> {
    if bp.permits(BP::Annotation) && self.eat(Token::Colon)? {
      let ty = self.parse_ty()?;
      return Ok(Ok(PatKind::Annotation(lhs, ty)));
    }
    Ok(Err(lhs))
  }

  pub(crate) fn parse_ty(&mut self) -> Result<Ty, Diag> {
    let span = self.start_span();
    let kind = self._parse_ty(span)?;
    let span = self.end_span(span);
    Ok(Ty { span, kind: Box::new(kind) })
  }

  fn _parse_ty(&mut self, span: usize) -> Result<TyKind, Diag> {
    self.state.expected.start_group(Token::GroupTy);
    if self.eat(Token::Hole)? {
      return Ok(TyKind::Hole);
    }
    if self.eat(Token::Bang)? {
      return Ok(TyKind::Never);
    }
    if self.eat(Token::Fn)? {
      return Ok(TyKind::Fn(self.parse_path()?));
    }
    if self.eat(Token::Dot)? {
      return Ok(TyKind::Key(self.parse_ident()?));
    }
    if self.check(Token::OpenParen) {
      return self.parse_ty_paren();
    }
    if self.check(Token::OpenBrace) {
      return self.parse_ty_object();
    }
    if self.check(Token::Amp) || self.check(Token::AmpAmp) {
      return self.parse_ty_ref(span);
    }
    if self.eat(Token::Tilde)? {
      return Ok(TyKind::Inverse(self.parse_ty()?));
    }
    if self.check(Token::Ident) || self.check(Token::Hash) {
      return Ok(TyKind::Path(self.parse_path()?));
    }
    if self.check(Token::If) {
      return self.parse_ty_if();
    }
    self.state.expected.end_group();
    self.unexpected()
  }

  pub(crate) fn parse_arrow_ty(&mut self) -> Result<Option<Ty>, Diag> {
    self.eat_then(Token::RightArrow, Self::parse_ty)
  }

  pub(crate) fn parse_impl(&mut self) -> Result<Impl, Diag> {
    let span = self.start_span();
    let kind = self._parse_impl()?;
    let span = self.end_span(span);
    Ok(Impl { span, kind: Box::new(kind) })
  }

  fn _parse_impl(&mut self) -> Result<ImplKind, Diag> {
    if self.eat(Token::Hole)? {
      return Ok(ImplKind::Hole);
    }
    if self.check(Token::Ident) || self.check(Token::Hash) {
      return Ok(ImplKind::Path(self.parse_path()?));
    }
    if self.eat(Token::Fn)? {
      return Ok(ImplKind::Fn(self.parse_path()?));
    }
    self.unexpected()
  }

  pub(crate) fn parse_trait(&mut self) -> Result<Trait, Diag> {
    let span = self.start_span();
    let kind = self._parse_trait()?;
    let span = self.end_span(span);
    Ok(Trait { span, kind: Box::new(kind) })
  }

  fn _parse_trait(&mut self) -> Result<TraitKind, Diag> {
    if self.check(Token::Ident) || self.check(Token::Hash) {
      return Ok(TraitKind::Path(self.parse_path()?));
    }
    if self.eat(Token::Fn)? {
      let receiver = self.parse_ty()?;
      let params = self.parse_delimited(PAREN_COMMA, Self::parse_ty)?;
      let ret = self.parse_arrow_ty()?;
      return Ok(TraitKind::Fn(receiver, params, ret));
    }
    self.unexpected()
  }

  pub(crate) fn parse_stmt(&mut self) -> Result<Stmt, Diag> {
    let span = self.start_span();
    let kind = self._parse_stmt()?;
    let span = self.end_span(span);
    Ok(Stmt { span, kind })
  }

  fn _parse_stmt(&mut self) -> Result<StmtKind, Diag> {
    if let Some(item) = self.maybe_parse_item()? {
      return Ok(StmtKind::Item(item));
    }
    self.state.expected.start_group(Token::GroupStmt);
    if self.check(Token::Assert) {
      return self.parse_stmt_assert();
    }
    if self.check(Token::Let) {
      return self.parse_stmt_let();
    }
    if self.eat(Token::Semi)? {
      return Ok(StmtKind::Empty);
    }
    if self.check(Token::If)
      || self.check(Token::When)
      || self.check(Token::Match)
      || self.check(Token::Loop)
      || self.check(Token::While)
      || self.check(Token::For)
      || self.check(Token::Do)
    {
      let Some(expr) = self.maybe_parse_expr_prefix()? else {
        return self.unexpected();
      };
      let semi = self.eat(Token::Semi)?;
      return Ok(StmtKind::Expr(expr, semi));
    }
    if self.eat(Token::Return)? {
      return self.parse_stmt_return();
    }
    if self.eat(Token::Break)? {
      return self.parse_stmt_break();
    }
    if self.eat(Token::Continue)? {
      return self.parse_stmt_continue();
    }
    self.state.expected.end_group();
    let expr = self.parse_expr()?;
    let semi = self.eat(Token::Semi)?;
    Ok(StmtKind::Expr(expr, semi))
  }

  pub(crate) fn start_span(&self) -> usize {
    self.state.lexer.span().start
  }

  pub(crate) fn end_span(&self, span: usize) -> Span {
    Span { file: self.file, start: span, end: self.state.last_token_end }
  }

  pub(crate) fn span(&self) -> Span {
    let span = self.state.lexer.span();
    Span { file: self.file, start: span.start, end: span.end }
  }
}

#[allow(clippy::absolute_paths)]
pub(crate) type Delimiters = vine_util::parser::Delimiters<Token>;

pub(crate) const PAREN_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenParen),
  close: Some(Token::CloseParen),
  separator: Some(Token::Comma),
};

pub(crate) const BRACE: Delimiters =
  Delimiters { open: Some(Token::OpenBrace), close: Some(Token::CloseBrace), separator: None };

pub(crate) const BRACE_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenBrace),
  close: Some(Token::CloseBrace),
  separator: Some(Token::Comma),
};

pub(crate) const BRACKET_COMMA: Delimiters = Delimiters {
  open: Some(Token::OpenBracket),
  close: Some(Token::CloseBracket),
  separator: Some(Token::Comma),
};

pub(crate) const PATH: Delimiters =
  Delimiters { open: None, close: None, separator: Some(Token::ColonColon) };

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Associativity {
  Left,
  Right,
}

/// Binding power.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub(crate) enum BP {
  Min,
  Assignment,
  LogicalImplies,
  LogicalOr,
  LogicalAnd,
  Is,
  Comparison,
  Range,
  BitOr,
  BitXor,
  BitAnd,
  BitShift,
  Additive,
  Multiplicative,
  Exponential,
  Annotation,
  Prefix,
  Max,
}

impl BP {
  const fn inc(self) -> Self {
    if self as u8 == BP::Max as u8 { self } else { unsafe { transmute::<u8, BP>(self as u8 + 1) } }
  }

  pub(crate) fn permits(self, other: Self) -> bool {
    other >= self
  }
}

#[rustfmt::skip]
const BINARY_OP_TABLE: &[(BP, Associativity, Token, BinaryOp)] = &[
  (BP::BitOr,          Associativity::Left, Token::Pipe,       BinaryOp::BitOr),
  (BP::BitXor,         Associativity::Left, Token::Caret,    BinaryOp::BitXor),
  (BP::BitAnd,         Associativity::Left, Token::Amp,      BinaryOp::BitAnd),
  (BP::BitShift,       Associativity::Left, Token::Shl,      BinaryOp::Shl),
  (BP::BitShift,       Associativity::Left, Token::Shr,      BinaryOp::Shr),
  (BP::Additive,       Associativity::Left, Token::Plus,     BinaryOp::Add),
  (BP::Additive,       Associativity::Left, Token::Minus,    BinaryOp::Sub),
  (BP::Additive,       Associativity::Left, Token::PlusPlus, BinaryOp::Concat),
  (BP::Multiplicative, Associativity::Left, Token::Star,     BinaryOp::Mul),
  (BP::Multiplicative, Associativity::Left, Token::Slash,    BinaryOp::Div),
  (BP::Multiplicative, Associativity::Left, Token::Percent,  BinaryOp::Rem),
  (BP::Exponential,    Associativity::Right, Token::StarStar, BinaryOp::Pow),
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
