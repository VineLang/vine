mod parse_string;

use std::{mem::transmute, path::PathBuf};

use ivy::parser::IvyParser;
use vine_util::parser::{Parser, ParserState};

use crate::{core::Core, diag::Diag, lexer::Token};

use crate::ast::{
  Attr, AttrKind, BinaryOp, Block, Builtin, ComparisonOp, ConstItem, DynFnStmt, EnumItem, Expr,
  ExprKind, FnItem, GenericArgs, GenericParams, Generics, Ident, Impl, ImplItem, ImplKind,
  InlineIvy, Item, ItemKind, Key, Label, LetStmt, LogicalOp, ModItem, ModKind, Pat, PatKind, Path,
  Span, Stmt, StmtKind, StructItem, Trait, TraitItem, TraitKind, Ty, TyKind, TypeItem, UseItem,
  UseTree, Variant, Vis,
};

pub struct VineParser<'core, 'src> {
  pub(crate) core: &'core Core<'core>,
  pub(crate) state: ParserState<'src, Token>,
  pub(crate) file: usize,
}

impl<'core, 'src> Parser<'src> for VineParser<'core, 'src> {
  type Token = Token;
  type Error = Diag<'core>;

  fn state(&mut self) -> &mut ParserState<'src, Self::Token> {
    &mut self.state
  }

  fn lex_error(&self) -> Self::Error {
    Diag::LexError { span: self.span() }
  }

  fn unexpected_error(&self) -> Diag<'core> {
    Diag::UnexpectedToken {
      span: self.span(),
      expected: self.state.expected,
      found: self.state.token,
    }
  }
}

type Parse<'core, T = ()> = Result<T, Diag<'core>>;

impl<'core, 'src> VineParser<'core, 'src> {
  pub fn parse(
    core: &'core Core<'core>,
    src: &'src str,
    file: usize,
  ) -> Parse<'core, Vec<Item<'core>>> {
    let mut parser = VineParser { core, state: ParserState::new(src), file };
    parser.bump()?;
    let mut items = Vec::new();
    while parser.state.token.is_some() {
      items.push(parser.parse_item()?);
    }
    Ok(items)
  }

  fn parse_item(&mut self) -> Parse<'core, Item<'core>> {
    self.maybe_parse_item()?.ok_or_else(|| self.unexpected_error())
  }

  fn maybe_parse_item(&mut self) -> Parse<'core, Option<Item<'core>>> {
    let span = self.start_span();
    let mut attrs = Vec::new();
    while self.check(Token::Hash) {
      attrs.push(self.parse_attr()?);
    }
    let vis = self.parse_vis()?;
    let kind = match () {
      _ if self.check(Token::Fn) => ItemKind::Fn(self.parse_fn_item()?),
      _ if self.check(Token::Const) => ItemKind::Const(self.parse_const_item()?),
      _ if self.check(Token::Struct) => ItemKind::Struct(self.parse_struct_item()?),
      _ if self.check(Token::Enum) => ItemKind::Enum(self.parse_enum_item()?),
      _ if self.check(Token::Type) => ItemKind::Type(self.parse_type_item()?),
      _ if self.check(Token::Mod) => ItemKind::Mod(self.parse_mod_item()?),
      _ if self.check(Token::Trait) => ItemKind::Trait(self.parse_trait_item()?),
      _ if self.check(Token::Impl) => ItemKind::Impl(self.parse_impl_item()?),
      _ if self.check(Token::Use) => ItemKind::Use(self.parse_use_item()?),
      _ if self.check(Token::InlineIvy) => ItemKind::Ivy(self.parse_ivy_item()?),
      _ if span == self.start_span() => return Ok(None),
      _ => self.unexpected()?,
    };
    let span = self.end_span(span);
    Ok(Some(Item { vis, span, attrs, kind }))
  }

  fn parse_vis(&mut self) -> Parse<'core, Vis<'core>> {
    Ok(if self.eat(Token::Pub)? {
      if self.eat(Token::OpenParen)? {
        let span = self.start_span();
        let ancestor = self.parse_ident()?;
        let span = self.end_span(span);
        self.expect(Token::CloseParen)?;
        Vis::PublicTo(span, ancestor)
      } else {
        Vis::Public
      }
    } else {
      Vis::Private
    })
  }

  fn parse_attr(&mut self) -> Parse<'core, Attr> {
    let span = self.start_span();
    self.expect(Token::Hash)?;
    self.expect(Token::OpenBracket)?;
    let ident_span = self.start_span();
    let ident = self.expect(Token::Ident)?;
    let ident_span = self.end_span(ident_span);
    let kind = match ident {
      "builtin" => {
        self.expect(Token::Eq)?;
        let str_span = self.start_span();
        let str = self.parse_string()?;
        let str_span = self.end_span(str_span);
        let builtin = match &*str {
          "Bool" => Builtin::Bool,
          "N32" => Builtin::N32,
          "F32" => Builtin::F32,
          "Char" => Builtin::Char,
          "IO" => Builtin::IO,
          "List" => Builtin::List,
          "String" => Builtin::String,
          "concat" => Builtin::Concat,
          "prelude" => Builtin::Prelude,
          "ToString" => Builtin::ToStringTrait,
          "to_string" => Builtin::ToStringFn,
          _ => Err(Diag::BadBuiltin { span: str_span })?,
        };
        AttrKind::Builtin(builtin)
      }
      _ => Err(Diag::UnknownAttribute { span: ident_span })?,
    };
    self.expect(Token::CloseBracket)?;
    let span = self.end_span(span);
    Ok(Attr { span, kind })
  }

  fn parse_ident(&mut self) -> Parse<'core, Ident<'core>> {
    let token = self.expect(Token::Ident)?;
    Ok(self.core.ident(token))
  }

  fn parse_key(&mut self) -> Parse<'core, Key<'core>> {
    let span = self.start_span();
    let ident = self.parse_ident()?;
    let span = self.end_span(span);
    Ok(Key { span, ident })
  }

  fn parse_num(&mut self) -> Parse<'core, ExprKind<'core>> {
    let span = self.span();
    let token = self.expect(Token::Num)?;
    if token.contains('.') {
      Ok(ExprKind::F32(self.parse_f32_like(token, |_| Diag::InvalidNum { span })?))
    } else {
      Ok(ExprKind::N32(self.parse_u32_like(token, |_| Diag::InvalidNum { span })?))
    }
  }

  fn parse_fn_item(&mut self) -> Parse<'core, FnItem<'core>> {
    self.expect(Token::Fn)?;
    let method = self.eat(Token::Dot)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let params = self.parse_delimited(PAREN_COMMA, Self::parse_pat)?;
    let ret = self.eat(Token::ThinArrow)?.then(|| self.parse_type()).transpose()?;
    let body = (!self.eat(Token::Semi)?).then(|| self.parse_block()).transpose()?;
    Ok(FnItem { method, name, generics, params, ret, body })
  }

  fn parse_const_item(&mut self) -> Parse<'core, ConstItem<'core>> {
    self.expect(Token::Const)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let ty = self.parse_type()?;
    let value = self.eat(Token::Eq)?.then(|| self.parse_expr()).transpose()?;
    self.expect(Token::Semi)?;
    Ok(ConstItem { name, generics, ty, value })
  }

  fn parse_struct_item(&mut self) -> Parse<'core, StructItem<'core>> {
    self.expect(Token::Struct)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let (fields, object) = if self.check(Token::OpenParen) {
      (self.parse_delimited(PAREN_COMMA, Self::parse_type)?, false)
    } else if self.check(Token::OpenBrace) {
      (vec![self.parse_type()?], true)
    } else {
      (Vec::new(), false)
    };
    self.eat(Token::Semi)?;
    Ok(StructItem { name, generics, fields, object })
  }

  fn parse_enum_item(&mut self) -> Parse<'core, EnumItem<'core>> {
    self.expect(Token::Enum)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let variants = self.parse_delimited(BRACE_COMMA, Self::parse_variant)?;
    Ok(EnumItem { name, generics, variants })
  }

  fn parse_type_item(&mut self) -> Parse<'core, TypeItem<'core>> {
    self.expect(Token::Type)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Eq)?;
    let ty = self.parse_type()?;
    self.expect(Token::Semi)?;
    Ok(TypeItem { name, generics, ty })
  }

  fn parse_variant(&mut self) -> Parse<'core, Variant<'core>> {
    let name = self.parse_ident()?;
    let fields = if self.check(Token::OpenParen) {
      self.parse_delimited(PAREN_COMMA, Self::parse_type)?
    } else if self.check(Token::OpenBrace) {
      vec![self.parse_type()?]
    } else {
      Vec::new()
    };
    Ok(Variant { name, fields })
  }

  fn parse_generic_params(&mut self) -> Parse<'core, GenericParams<'core>> {
    self.parse_generics(Self::parse_ident, |self_| {
      if self_.check(Token::Ident) {
        let path = self_.parse_path()?;
        if path.as_ident().is_some() && self_.eat(Token::Colon)? {
          let trait_ = self_.parse_trait()?;
          Ok((path.as_ident(), trait_))
        } else {
          Ok((None, Trait { span: path.span, kind: TraitKind::Path(path) }))
        }
      } else {
        Ok((None, self_.parse_trait()?))
      }
    })
  }

  fn parse_generic_args(&mut self) -> Parse<'core, GenericArgs<'core>> {
    self.parse_generics(Self::parse_type, Self::parse_impl)
  }

  fn parse_generics<T, I>(
    &mut self,
    mut parse_t: impl FnMut(&mut Self) -> Parse<'core, T>,
    mut parse_i: impl FnMut(&mut Self) -> Parse<'core, I>,
  ) -> Parse<'core, Generics<T, I>> {
    let span = self.start_span();
    let mut generics = Generics::default();
    if self.eat(Token::OpenBracket)? {
      loop {
        if self.eat(Token::Semi)? || self.check(Token::CloseBracket) {
          break;
        }
        generics.types.push(parse_t(self)?);
        if self.eat(Token::Comma)? {
          continue;
        }
        if self.eat(Token::Semi)? {
          break;
        }
        if !self.check(Token::CloseBracket) {
          self.unexpected()?;
        }
      }
      loop {
        if self.eat(Token::CloseBracket)? {
          break;
        }
        generics.impls.push(parse_i(self)?);
        if self.eat(Token::Comma)? {
          continue;
        }
        self.expect(Token::CloseBracket)?;
        break;
      }
    }
    let span = self.end_span(span);
    generics.span = span;
    Ok(generics)
  }

  fn parse_mod_item(&mut self) -> Parse<'core, ModItem<'core>> {
    self.expect(Token::Mod)?;
    let name = self.parse_ident()?;
    if self.eat(Token::Eq)? {
      let span = self.start_span();
      let path = self.parse_string()?;
      let span = self.end_span(span);
      self.expect(Token::Semi)?;
      Ok(ModItem { name, kind: ModKind::Unloaded(span, PathBuf::from(path)) })
    } else {
      let span = self.start_span();
      let items = self.parse_delimited(BRACE, Self::parse_item)?;
      let span = self.end_span(span);
      Ok(ModItem { name, kind: ModKind::Loaded(span, items) })
    }
  }

  fn parse_trait_item(&mut self) -> Parse<'core, TraitItem<'core>> {
    self.expect(Token::Trait)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    let items = self.parse_delimited(BRACE, Self::parse_item)?;
    Ok(TraitItem { name, generics, items })
  }

  fn parse_impl_item(&mut self) -> Parse<'core, ImplItem<'core>> {
    self.expect(Token::Impl)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let trait_ = self.parse_trait()?;
    let items = self.parse_delimited(BRACE, Self::parse_item)?;
    Ok(ImplItem { name, generics, trait_, items })
  }

  fn parse_use_item(&mut self) -> Parse<'core, UseItem<'core>> {
    self.expect(Token::Use)?;
    let absolute = self.eat(Token::ColonColon)?;
    let mut tree = UseTree::default();
    loop {
      self.parse_use_tree(None, &mut tree)?;
      if !self.eat(Token::Comma)? {
        break;
      }
    }
    tree.prune();
    self.eat(Token::Semi)?;
    Ok(UseItem { absolute, tree })
  }

  fn parse_use_tree(
    &mut self,
    cur_name: Option<Ident<'core>>,
    tree: &mut UseTree<'core>,
  ) -> Parse<'core> {
    if self.check(Token::Ident) {
      let span = self.span();
      let ident = self.parse_ident()?;
      if cur_name == Some(ident) {
        if self.eat(Token::As)? {
          let alias = self.parse_ident()?;
          tree.aliases.push(alias);
        } else {
          tree.aliases.push(ident);
        }
      } else {
        let child = tree.children.entry(ident).or_insert(UseTree { span, ..Default::default() });
        if self.eat(Token::ColonColon)? {
          let is_group = self.check(Token::OpenBrace);
          self.parse_use_tree(is_group.then_some(ident), child)?;
        } else if self.eat(Token::As)? {
          let alias = self.parse_ident()?;
          child.aliases.push(alias);
        } else {
          child.aliases.push(ident);
        }
      }
    } else {
      self.parse_delimited(BRACE_COMMA, |self_| self_.parse_use_tree(cur_name, tree))?;
    }
    Ok(())
  }

  fn parse_ivy_item(&mut self) -> Parse<'core, InlineIvy<'core>> {
    let span = self.span();
    self.expect(Token::InlineIvy)?;
    self.expect(Token::Bang)?;
    let method = self.eat(Token::Dot)?;
    let name = self.parse_ident()?;
    let generics = self.parse_generic_params()?;
    self.expect(Token::Colon)?;
    let ty = self.parse_type()?;
    if !self.check(Token::OpenBrace) {
      self.unexpected()?;
    }
    let net = self.switch(
      |state| IvyParser { state },
      IvyParser::parse_net_inner,
      |_| Diag::InvalidIvy { span },
    )?;
    Ok(InlineIvy { method, name, generics, ty, net })
  }

  fn parse_expr_list(&mut self) -> Parse<'core, Vec<Expr<'core>>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_expr)
  }

  fn parse_pat_list(&mut self) -> Parse<'core, Vec<Pat<'core>>> {
    self.parse_delimited(PAREN_COMMA, Self::parse_pat)
  }

  fn parse_block(&mut self) -> Parse<'core, Block<'core>> {
    let span = self.start_span();
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    let span = self.end_span(span);
    Ok(Block { span, stmts })
  }

  fn parse_expr(&mut self) -> Parse<'core, Expr<'core>> {
    self.parse_expr_bp(BP::Min)
  }

  fn parse_expr_bp(&mut self, bp: BP) -> Parse<'core, Expr<'core>> {
    let span = self.start_span();
    let mut expr = self.parse_expr_prefix()?;
    loop {
      expr = match self.parse_expr_postfix(expr, bp)? {
        Ok(kind) => Expr { span: self.end_span(span), kind },
        Err(expr) => return Ok(expr),
      }
    }
  }

  fn parse_expr_prefix(&mut self) -> Parse<'core, Expr<'core>> {
    let span = self.start_span();
    let kind = self._parse_expr_prefix(span)?;
    let span = self.end_span(span);
    Ok(Expr { span, kind })
  }

  fn _parse_expr_prefix(&mut self, span: usize) -> Parse<'core, ExprKind<'core>> {
    if self.eat(Token::Return)? {
      if self.check(Token::Semi) {
        return Ok(ExprKind::Return(None));
      }

      return Ok(ExprKind::Return(Some(Box::new(self.parse_expr_bp(BP::ControlFlow)?))));
    }
    if self.eat(Token::Break)? {
      let label = self.parse_label()?;

      if self.check(Token::Semi) {
        return Ok(ExprKind::Break(label, None));
      }

      return Ok(ExprKind::Break(label, Some(Box::new(self.parse_expr_bp(BP::ControlFlow)?))));
    }
    if self.eat(Token::Continue)? {
      return Ok(ExprKind::Continue(self.parse_label()?));
    }
    if self.eat(Token::And)? {
      return Ok(ExprKind::Ref(Box::new(self.parse_expr_bp(BP::Prefix)?), false));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_expr_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      return Ok(ExprKind::Ref(
        Box::new(Expr { span, kind: ExprKind::Ref(Box::new(inner), false) }),
        false,
      ));
    }
    if self.eat(Token::Star)? {
      return Ok(ExprKind::Deref(Box::new(self.parse_expr_bp(BP::Prefix)?), false));
    }
    if self.eat(Token::Move)? {
      return Ok(ExprKind::Move(Box::new(self.parse_expr_bp(BP::Prefix)?), false));
    }
    if self.eat(Token::Tilde)? {
      return Ok(ExprKind::Inverse(Box::new(self.parse_expr_bp(BP::Prefix)?), false));
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
    if self.check(Token::SingleQuote) {
      return self.parse_char_expr();
    }
    if self.check(Token::DoubleQuote) {
      return self.parse_string_expr();
    }
    if self.eat(Token::True)? {
      return Ok(ExprKind::Bool(true));
    }
    if self.eat(Token::False)? {
      return Ok(ExprKind::Bool(false));
    }
    if self.check(Token::Ident) || self.check(Token::ColonColon) {
      return Ok(ExprKind::Path(self.parse_path()?));
    }
    if self.eat(Token::OpenParen)? {
      if self.eat(Token::CloseParen)? {
        return Ok(ExprKind::Tuple(vec![]));
      }
      let expr = self.parse_expr()?;
      if self.eat(Token::Semi)? {
        let value = expr;
        let space = self.parse_expr()?;
        self.expect(Token::CloseParen)?;
        return Ok(ExprKind::Place(Box::new(value), Box::new(space)));
      }
      if self.eat(Token::CloseParen)? {
        return Ok(ExprKind::Paren(Box::new(expr)));
      }
      self.expect(Token::Comma)?;
      let mut exprs = vec![expr];
      loop {
        if self.check(Token::CloseParen) {
          break;
        }
        exprs.push(self.parse_expr()?);
        if !self.eat(Token::Comma)? {
          break;
        }
      }
      self.expect(Token::CloseParen)?;
      return Ok(ExprKind::Tuple(exprs));
    }
    if self.check(Token::OpenBrace) {
      return Ok(ExprKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
        let key = self_.parse_key()?;
        let value = if self_.eat(Token::Colon)? {
          self_.parse_expr()?
        } else {
          Expr { span: key.span, kind: ExprKind::Path(key.into()) }
        };
        Ok((key, value))
      })?));
    }
    if self.check(Token::OpenBracket) {
      let exprs = self.parse_delimited(BRACKET_COMMA, Self::parse_expr)?;
      return Ok(ExprKind::List(exprs));
    }
    if self.eat(Token::Do)? {
      return Ok(ExprKind::Do(self.parse_label()?, self.parse_block()?));
    }
    if self.eat(Token::If)? {
      let mut arms = Vec::new();
      loop {
        let cond = self.parse_expr()?;
        let then = self.parse_block()?;
        arms.push((cond, then));
        if self.eat(Token::Else)? {
          if self.eat(Token::If)? {
            continue;
          } else {
            let leg = self.parse_block()?;
            return Ok(ExprKind::If(arms, Some(leg)));
          }
        } else {
          return Ok(ExprKind::If(arms, None));
        }
      }
    }
    if self.eat(Token::While)? {
      let label = self.parse_label()?;
      let cond = self.parse_expr()?;
      let body = self.parse_block()?;
      return Ok(ExprKind::While(label, Box::new(cond), body));
    }
    if self.eat(Token::Loop)? {
      let label = self.parse_label()?;
      let body = self.parse_block()?;
      return Ok(ExprKind::Loop(label, body));
    }
    if self.eat(Token::Fn)? {
      let params = self.parse_delimited(PAREN_COMMA, Self::parse_pat)?;
      let body = self.parse_block()?;
      return Ok(ExprKind::Fn(params, None, body));
    }
    if self.eat(Token::Match)? {
      let scrutinee = self.parse_expr()?;
      let arms = self.parse_delimited(BRACE, |self_| {
        let pat = self_.parse_pat()?;
        let value = self_.parse_block()?;
        Ok((pat, value))
      })?;
      return Ok(ExprKind::Match(Box::new(scrutinee), arms));
    }
    if self.eat(Token::Hole)? {
      return Ok(ExprKind::Hole);
    }
    self.unexpected()
  }

  fn parse_expr_postfix(
    &mut self,
    lhs: Expr<'core>,
    bp: BP,
  ) -> Parse<'core, Result<ExprKind<'core>, Expr<'core>>> {
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
      return Ok(Ok(ExprKind::Assign(false, Box::new(lhs), Box::new(rhs))));
    }

    if bp.permits(BP::Assignment) && self.eat(Token::Tilde)? {
      self.expect(Token::Eq)?;
      let rhs = self.parse_expr_bp(BP::Assignment)?;
      return Ok(Ok(ExprKind::Assign(true, Box::new(lhs), Box::new(rhs))));
    }

    if self.eat(Token::Dot)? {
      if self.eat(Token::And)? {
        return Ok(Ok(ExprKind::Ref(Box::new(lhs), true)));
      }
      if self.eat(Token::Star)? {
        return Ok(Ok(ExprKind::Deref(Box::new(lhs), true)));
      }
      if self.eat(Token::Move)? {
        return Ok(Ok(ExprKind::Move(Box::new(lhs), true)));
      }
      if self.eat(Token::Tilde)? {
        return Ok(Ok(ExprKind::Inverse(Box::new(lhs), true)));
      }
      if self.check(Token::Num) {
        let token_span = self.start_span();
        let num = self.expect(Token::Num)?;
        let token_span = self.end_span(token_span);
        if let Some((i, j)) = num.split_once(".") {
          let i_span =
            Span { file: self.file, start: token_span.start, end: token_span.start + i.len() };
          let j_span =
            Span { file: self.file, start: token_span.end - j.len(), end: token_span.end };
          let i = self.parse_u32_like(i, |_| Diag::InvalidNum { span: i_span })? as usize;
          let j = self.parse_u32_like(j, |_| Diag::InvalidNum { span: j_span })? as usize;
          return Ok(Ok(ExprKind::TupleField(
            Box::new(Expr {
              span: Span { file: self.file, start: lhs.span.start, end: i_span.end },
              kind: ExprKind::TupleField(Box::new(lhs), i, None),
            }),
            j,
            None,
          )));
        } else {
          let i = self.parse_u32_like(num, |_| Diag::InvalidNum { span: token_span })? as usize;
          return Ok(Ok(ExprKind::TupleField(Box::new(lhs), i, None)));
        }
      }
      let key = self.parse_key()?;
      if self.check(Token::OpenBracket) || self.check(Token::OpenParen) {
        let generics = self.parse_generic_args()?;
        let args = self.parse_expr_list()?;
        return Ok(Ok(ExprKind::Method(Box::new(lhs), key.ident, generics, args)));
      } else {
        return Ok(Ok(ExprKind::ObjectField(Box::new(lhs), key)));
      }
    }

    if self.check(Token::OpenParen) {
      let args = self.parse_expr_list()?;
      return Ok(Ok(ExprKind::Call(Box::new(lhs), args)));
    }

    Ok(Err(lhs))
  }

  fn parse_label(&mut self) -> Parse<'core, Label<'core>> {
    Ok(Label::Ident(self.eat(Token::Dot)?.then(|| self.parse_ident()).transpose()?))
  }

  fn parse_pat(&mut self) -> Parse<'core, Pat<'core>> {
    self.parse_pat_bp(BP::Min)
  }

  fn parse_pat_bp(&mut self, bp: BP) -> Parse<'core, Pat<'core>> {
    let span = self.start_span();
    let mut pat = self.parse_pat_prefix()?;
    loop {
      pat = match self.parse_pat_postfix(pat, bp)? {
        Ok(kind) => Pat { span: self.end_span(span), kind },
        Err(pat) => return Ok(pat),
      }
    }
  }

  fn parse_pat_prefix(&mut self) -> Parse<'core, Pat<'core>> {
    let span = self.start_span();
    let kind = self._parse_pat_prefix(span)?;
    let span = self.end_span(span);
    Ok(Pat { span, kind })
  }

  fn _parse_pat_prefix(&mut self, span: usize) -> Parse<'core, PatKind<'core>> {
    if self.eat(Token::Hole)? {
      return Ok(PatKind::Hole);
    }
    if self.check(Token::Ident) || self.check(Token::ColonColon) {
      let path = self.parse_path()?;
      let args = self.check(Token::OpenParen).then(|| self.parse_pat_list()).transpose()?;
      return Ok(PatKind::PathCall(path, args));
    }
    if self.eat(Token::And)? {
      return Ok(PatKind::Ref(Box::new(self.parse_pat_bp(BP::Prefix)?)));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_pat_bp(BP::Prefix)?;
      let span = self.end_span(span + 1);
      return Ok(PatKind::Ref(Box::new(Pat { span, kind: PatKind::Ref(Box::new(inner)) })));
    }
    if self.eat(Token::Star)? {
      return Ok(PatKind::Deref(Box::new(self.parse_pat_bp(BP::Prefix)?)));
    }
    if self.eat(Token::Tilde)? {
      return Ok(PatKind::Inverse(Box::new(self.parse_pat_bp(BP::Prefix)?)));
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
        return Ok(PatKind::Paren(Box::new(pats.pop().unwrap())));
      }
      return Ok(PatKind::Tuple(pats));
    }
    if self.check(Token::OpenBrace) {
      return Ok(PatKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
        let span = self_.start_span();
        let key = self_.parse_key()?;
        let (pat, ty) = if self_.eat(Token::Colon)? {
          if self_.eat(Token::Colon)? {
            (None, Some(self_.parse_type()?))
          } else {
            (Some(self_.parse_pat()?), None)
          }
        } else if self_.eat(Token::ColonColon)? {
          (None, Some(self_.parse_type()?))
        } else {
          (None, None)
        };
        let span = self_.end_span(span);
        let mut pat =
          pat.unwrap_or_else(|| Pat { span: key.span, kind: PatKind::PathCall(key.into(), None) });
        if let Some(ty) = ty {
          pat = Pat { span, kind: PatKind::Annotation(Box::new(pat), Box::new(ty)) };
        }
        Ok((key, pat))
      })?));
    }
    self.unexpected()
  }

  fn parse_pat_postfix(
    &mut self,
    lhs: Pat<'core>,
    bp: BP,
  ) -> Parse<'core, Result<PatKind<'core>, Pat<'core>>> {
    if bp.permits(BP::Annotation) && self.eat(Token::Colon)? {
      let ty = self.parse_type()?;
      return Ok(Ok(PatKind::Annotation(Box::new(lhs), Box::new(ty))));
    }
    Ok(Err(lhs))
  }

  fn parse_type(&mut self) -> Parse<'core, Ty<'core>> {
    let span = self.start_span();
    let kind = self._parse_type(span)?;
    let span = self.end_span(span);
    Ok(Ty { span, kind })
  }

  fn _parse_type(&mut self, span: usize) -> Parse<'core, TyKind<'core>> {
    if self.eat(Token::Hole)? {
      return Ok(TyKind::Hole);
    }
    if self.eat(Token::Fn)? {
      let args = self.parse_delimited(PAREN_COMMA, Self::parse_type)?;
      let ret = self.eat(Token::ThinArrow)?.then(|| self.parse_type()).transpose()?.map(Box::new);
      return Ok(TyKind::Fn(args, ret));
    }
    if self.check(Token::OpenParen) {
      let mut tuple = false;
      let mut types = self.parse_delimited(PAREN_COMMA, |self_| {
        let expr = self_.parse_type()?;
        if self_.check(Token::Comma) {
          tuple = true;
        }
        Ok(expr)
      })?;
      if types.len() == 1 && !tuple {
        return Ok(TyKind::Paren(Box::new(types.pop().unwrap())));
      }
      return Ok(TyKind::Tuple(types));
    }
    if self.check(Token::OpenBrace) {
      return Ok(TyKind::Object(self.parse_delimited(BRACE_COMMA, |self_| {
        let key = self_.parse_key()?;
        self_.expect(Token::Colon)?;
        let value = self_.parse_type()?;
        Ok((key, value))
      })?));
    }
    if self.eat(Token::And)? {
      return Ok(TyKind::Ref(Box::new(self.parse_type()?)));
    }
    if self.eat(Token::AndAnd)? {
      let inner = self.parse_type()?;
      let span = self.end_span(span + 1);
      return Ok(TyKind::Ref(Box::new(Ty { span, kind: TyKind::Ref(Box::new(inner)) })));
    }
    if self.eat(Token::Tilde)? {
      return Ok(TyKind::Inverse(Box::new(self.parse_type()?)));
    }
    if self.check(Token::ColonColon) || self.check(Token::Ident) {
      let path = self.parse_path()?;
      return Ok(TyKind::Path(path));
    }
    self.unexpected()
  }

  fn parse_impl(&mut self) -> Parse<'core, Impl<'core>> {
    let span = self.start_span();
    let kind =
      if self.eat(Token::Hole)? { ImplKind::Hole } else { ImplKind::Path(self.parse_path()?) };
    let span = self.end_span(span);
    Ok(Impl { span, kind })
  }

  fn parse_trait(&mut self) -> Parse<'core, Trait<'core>> {
    let span = self.start_span();
    let kind = TraitKind::Path(self.parse_path()?);
    let span = self.end_span(span);
    Ok(Trait { span, kind })
  }

  pub(crate) fn parse_stmt(&mut self) -> Parse<'core, Stmt<'core>> {
    let span = self.start_span();
    let kind = if self.check(Token::Let) {
      StmtKind::Let(self.parse_let_stmt()?)
    } else if self.check(Token::Dyn) {
      StmtKind::DynFn(self.parse_dyn_fn_stmt()?)
    } else if self.eat(Token::Semi)? {
      StmtKind::Empty
    } else if let Some(item) = self.maybe_parse_item()? {
      StmtKind::Item(item)
    } else if self.check(Token::If)
      || self.check(Token::Match)
      || self.check(Token::Loop)
      || self.check(Token::While)
      || self.check(Token::For)
    {
      let expr = self.parse_expr_prefix()?;
      let semi = self.eat(Token::Semi)?;
      StmtKind::Expr(expr, semi)
    } else {
      let expr = self.parse_expr()?;
      let semi = self.eat(Token::Semi)?;
      StmtKind::Expr(expr, semi)
    };
    let span = self.end_span(span);
    Ok(Stmt { span, kind })
  }

  fn parse_let_stmt(&mut self) -> Parse<'core, LetStmt<'core>> {
    self.expect(Token::Let)?;
    let bind = self.parse_pat()?;
    let init = self.eat(Token::Eq)?.then(|| self.parse_expr()).transpose()?;
    let else_block = self.eat(Token::Else)?.then(|| self.parse_block()).transpose()?;
    self.eat(Token::Semi)?;
    Ok(LetStmt { bind, init, else_block })
  }

  fn parse_dyn_fn_stmt(&mut self) -> Parse<'core, DynFnStmt<'core>> {
    self.expect(Token::Dyn)?;
    self.expect(Token::Fn)?;
    let name = self.parse_ident()?;
    let params = self.parse_delimited(PAREN_COMMA, Self::parse_pat)?;
    let ret = self.eat(Token::ThinArrow)?.then(|| self.parse_type()).transpose()?;
    let body = self.parse_block()?;
    Ok(DynFnStmt { name, id: None, params, ret, body })
  }

  fn parse_path(&mut self) -> Parse<'core, Path<'core>> {
    let span = self.start_span();
    let absolute = self.eat(Token::ColonColon)?;
    let segments = self.parse_delimited(PATH, Self::parse_ident)?;
    let generics = self.check(Token::OpenBracket).then(|| self.parse_generic_args()).transpose()?;
    let span = self.end_span(span);
    Ok(Path { span, absolute, segments, generics })
  }

  fn start_span(&self) -> usize {
    self.state.lexer.span().start
  }

  fn end_span(&self, span: usize) -> Span {
    Span { file: self.file, start: span, end: self.state.last_token_end }
  }

  fn span(&self) -> Span {
    let span = self.state.lexer.span();
    Span { file: self.file, start: span.start, end: span.end }
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
  Annotation,
  Prefix,
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
