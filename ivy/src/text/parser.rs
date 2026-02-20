use std::convert::Infallible;

use vine_util::{
  nat::Nat,
  parser::{Delimiters, Parse, ParserState},
};

use crate::{
  name::{Name, NameId, Table},
  text::{
    ast::{Diag, Expr, Net, Nets, Stmt},
    lexer::{Lexer, Token},
  },
};

pub struct Parser<'src, 't, F = fn(&mut ParserState<'src, Lexer<'src>>) -> Result<Infallible, Diag>>
{
  table: &'t mut Table,
  pub state: ParserState<'src, Lexer<'src>>,
  parse_interpolation: F,
}

impl<'src, F> Parse<'src> for Parser<'src, '_, F> {
  type Token = Token;
  type Lexer = Lexer<'src>;
  type Error = Diag;

  fn state(&mut self) -> &mut ParserState<'src, Lexer<'src>> {
    &mut self.state
  }

  fn unexpected_error(&self) -> Diag {
    Diag::UnexpectedToken { expected: self.state.expected.clone(), found: self.state.token }
  }
}

impl<'t, 'src> Parser<'src, 't> {
  pub fn new(table: &'t mut Table, lexer: Lexer<'src>) -> Result<Self, Diag> {
    Self::with_interpolations(table, lexer, |_| Err(Diag::InvalidInterpolation))
  }

  pub fn parse(table: &'t mut Table, src: &'src str) -> Result<Nets, Diag> {
    Parser::new(table, Lexer::new(src))?.parse_nets()
  }
}

impl<'t, 'src, I, F: FnMut(&mut ParserState<'src, Lexer<'src>>) -> Result<I, Diag>>
  Parser<'src, 't, F>
{
  pub fn with_interpolations(
    table: &'t mut Table,
    lexer: Lexer<'src>,
    parse_interpolation: F,
  ) -> Result<Self, Diag> {
    Ok(Parser { table, state: ParserState::new(lexer)?, parse_interpolation })
  }

  pub fn parse_nets(&mut self) -> Result<Nets<I>, Diag> {
    let mut nets = Vec::new();
    while !self.check(Token::Eof) {
      nets.push((self.parse_name_id()?, self.parse_net()?));
    }
    Ok(Nets { nets })
  }

  pub fn parse_net(&mut self) -> Result<Net<I>, Diag> {
    let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
    Ok(Net { stmts })
  }

  pub fn parse_stmt(&mut self) -> Result<Stmt<I>, Diag> {
    let lhs = self.parse_expr()?;
    self.expect(Token::Eq)?;
    let rhs = self.parse_expr()?;
    self.eat(Token::Semi)?;
    Ok((lhs, rhs))
  }

  pub fn parse_expr(&mut self) -> Result<Expr<I>, Diag> {
    let mut expr = self.parse_expr_prefix()?;
    while self.check(Token::OpenBrace) {
      let stmts = self.parse_delimited(BRACE, Self::parse_stmt)?;
      expr = Expr::Subnet(Box::new(expr), stmts);
    }
    Ok(expr)
  }

  fn parse_expr_prefix(&mut self) -> Result<Expr<I>, Diag> {
    if self.check(Token::Path) {
      let name = self.parse_name()?;
      let children = if self.check(Token::OpenParen) {
        self.parse_delimited(PAREN, Self::parse_expr)?
      } else {
        Vec::new()
      };
      return Ok(Expr::Node(name, children));
    }

    if self.check(Token::Ident) {
      return Ok(Expr::Wire(self.expect(Token::Ident)?.into()));
    }

    if self.check(Token::Free) {
      let token = self.expect(Token::Free)?;
      let n = if token.len() != 1 {
        Some(self.parse_u32_like(&token[1..], |str| Diag::InvalidNumber(str.into()))? as usize)
      } else {
        None
      };
      return Ok(Expr::Free(n));
    }

    if self.eat(Token::Dollar)? {
      if !self.check(Token::OpenBrace) {
        self.unexpected()?;
      }
      return Ok(Expr::Interpolation((self.parse_interpolation)(&mut self.state)?));
    }

    self.unexpected()
  }

  pub fn parse_name_id(&mut self) -> Result<NameId, Diag> {
    let name = self.parse_name()?;
    Ok(self.table.add_name(name))
  }

  pub fn parse_name(&mut self) -> Result<Name, Diag> {
    let path = self.expect(Token::Path)?;
    let path = self.table.add_path(path);

    let payload = if self.check(Token::Payload) {
      let token = self.expect(Token::Payload)?;
      self.parse_nat_like(&token[1..], |str| Diag::InvalidNumber(str.into()))?
    } else {
      Nat::ZERO
    };

    let children = if self.check(Token::OpenBracket) {
      self.parse_delimited(BRACKET_COMMA, Self::parse_name_id)?
    } else {
      Vec::new()
    };

    Ok(Name { path, children, payload })
  }
}
const PAREN: Delimiters<Token> =
  Delimiters { open: Some(Token::OpenParen), separator: None, close: Some(Token::CloseParen) };

const BRACE: Delimiters<Token> =
  Delimiters { open: Some(Token::OpenBrace), separator: None, close: Some(Token::CloseBrace) };

const BRACKET_COMMA: Delimiters<Token> = Delimiters {
  open: Some(Token::OpenBracket),
  separator: Some(Token::Comma),
  close: Some(Token::CloseBracket),
};
