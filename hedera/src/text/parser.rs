use vine_util::parser::{Delimiters, Parse, ParserState};

use crate::{
  name::{Name, NameId, Table},
  text::{
    ast::{Diag, Expr, Net, Nets, Stmt},
    lexer::{Lexer, Token},
  },
};

pub struct Parser<'src, 't> {
  table: &'t mut Table,
  pub state: ParserState<'src, Lexer<'src>>,
}

impl<'src> Parse<'src> for Parser<'src, '_> {
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
    Ok(Parser { table, state: ParserState::new(lexer)? })
  }

  pub fn parse(table: &'t mut Table, src: &'src str) -> Result<Nets, Diag> {
    Parser::new(table, Lexer::new(src))?.parse_nets()
  }

  fn parse_nets(&mut self) -> Result<Nets, Diag> {
    let mut nets = Vec::new();
    while !self.check(Token::Eof) {
      nets.push((self.parse_name_id()?, self.parse_net()?));
    }
    Ok(Nets { nets })
  }

  fn parse_net_inner(&mut self) -> Result<Net, Diag> {
    let mut stmts = Vec::new();
    while !self.check(Token::CloseBrace) {
      stmts.push(self.parse_stmt()?);
    }
    Ok(Net { stmts })
  }

  fn parse_net(&mut self) -> Result<Net, Diag> {
    self.expect(Token::OpenBrace)?;
    let net = self.parse_net_inner()?;
    self.expect(Token::CloseBrace)?;
    Ok(net)
  }

  fn parse_stmt(&mut self) -> Result<Stmt, Diag> {
    let lhs = self.parse_expr()?;
    if self.eat(Token::Eq)? {
      let rhs = self.parse_expr()?;
      Ok(Stmt::Link(lhs, rhs))
    } else {
      Ok(Stmt::Expr(lhs))
    }
  }

  fn parse_expr(&mut self) -> Result<Expr, Diag> {
    if self.check(Token::Path) {
      let name = self.parse_name()?;
      let children = if self.check(Token::OpenParen) {
        self.parse_delimited(
          Delimiters {
            open: Some(Token::OpenParen),
            separator: Some(Token::Comma),
            close: Some(Token::CloseBrace),
          },
          |self_| (!self_.eat(Token::Hole)?).then(|| self_.parse_expr()).transpose(),
        )?
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

    self.unexpected()
  }

  fn parse_name_id(&mut self) -> Result<NameId, Diag> {
    let name = self.parse_name()?;
    Ok(self.table.add_name(name))
  }

  fn parse_name(&mut self) -> Result<Name, Diag> {
    let path = self.expect(Token::Path)?;
    let path = self.table.add_path(path);

    let children = if self.check(Token::OpenBracket) {
      self.parse_delimited(
        Delimiters {
          open: Some(Token::OpenBracket),
          separator: Some(Token::Comma),
          close: Some(Token::CloseBracket),
        },
        Self::parse_name_id,
      )?
    } else {
      Vec::new()
    };

    let data = if self.check(Token::Data) {
      let token = self.expect(Token::Data)?;
      Some(self.parse_nat_like(&token[1..], |str| Diag::InvalidNumber(str.into()))?)
    } else {
      None
    };

    Ok(Name { path, children, data })
  }
}
