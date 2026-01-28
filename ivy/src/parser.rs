use vine_util::{
  lexer::{Lex, TokenSet},
  parser::{Parse, ParserState},
};

use crate::{
  ast::{Net, Nets, Tree},
  lexer::{Lexer, Token},
};

pub struct Parser<'src> {
  pub state: ParserState<'src, Lexer<'src>>,
}

#[derive(Debug, Clone)]
pub enum ParseError<'src> {
  LexError,
  UnexpectedToken { expected: TokenSet<Token>, found: &'src str },
  InvalidNum(&'src str),
  InvalidLabel(&'src str),
  InvalidExtFn(&'src str),
  InvalidBranch(&'src str),
}

impl<'src> Parse<'src> for Parser<'src> {
  type Token = Token;
  type Lexer = Lexer<'src>;
  type Error = ParseError<'src>;

  fn state(&mut self) -> &mut ParserState<'src, Lexer<'src>> {
    &mut self.state
  }

  fn unexpected_error(&self) -> ParseError<'src> {
    ParseError::UnexpectedToken {
      expected: self.state.expected.clone(),
      found: self.state.lexer.slice(),
    }
  }
}

impl<'src> Parser<'src> {
  pub fn new(lexer: Lexer<'src>) -> Result<Self, ParseError<'src>> {
    Ok(Parser { state: ParserState::new(lexer)? })
  }

  pub fn parse(src: &'src str) -> Result<Nets, ParseError<'src>> {
    let mut parser = Parser::new(Lexer::new(src))?;
    let mut nets = Nets::default();
    while !parser.check(Token::Eof) {
      let name = parser.expect(Token::Global)?.to_owned();
      let net = parser.parse_net()?;
      nets.insert(name, net);
    }
    Ok(nets)
  }

  fn parse_n32(&mut self) -> Result<u32, ParseError<'src>> {
    let token = self.expect(Token::N32)?;
    self.parse_u32_like(token, ParseError::InvalidNum)
  }

  fn parse_f32(&mut self) -> Result<f32, ParseError<'src>> {
    let token = self.expect(Token::F32)?;
    self.parse_f32_like(token, ParseError::InvalidNum)
  }

  fn parse_f64(&mut self) -> Result<f64, ParseError<'src>> {
    let token = self.expect(Token::F64)?;
    self.parse_f64_like(token.strip_suffix("f64").unwrap(), ParseError::InvalidNum)
  }

  fn parse_net(&mut self) -> Result<Net, ParseError<'src>> {
    self.expect(Token::OpenBrace)?;
    let net = self.parse_net_inner()?;
    self.expect(Token::CloseBrace)?;
    Ok(net)
  }

  #[doc(hidden)] // used by Vine to parse `inline_ivy!`
  pub fn parse_net_inner(&mut self) -> Result<Net, ParseError<'src>> {
    let root = self.parse_tree()?;
    let mut pairs = Vec::new();
    while !self.check(Token::CloseBrace) {
      pairs.push(self.parse_pair()?);
    }
    Ok(Net { root, pairs })
  }

  pub(super) fn parse_pair(&mut self) -> Result<(Tree, Tree), ParseError<'src>> {
    let a = self.parse_tree()?;
    self.expect(Token::Eq)?;
    let b = self.parse_tree()?;
    Ok((a, b))
  }

  fn parse_tree(&mut self) -> Result<Tree, ParseError<'src>> {
    if self.check(Token::N32) {
      return Ok(Tree::N32(self.parse_n32()?));
    }

    if self.check(Token::F32) {
      return Ok(Tree::F32(self.parse_f32()?));
    }

    if self.check(Token::F64) {
      return Ok(Tree::F64(self.parse_f64()?));
    }

    if self.check(Token::Global) {
      return Ok(Tree::Global(self.expect(Token::Global)?.to_owned()));
    }

    if self.check(Token::Ident) {
      let ident = self.expect(Token::Ident)?.to_owned();
      if self.eat(Token::OpenParen)? {
        let label = ident;
        let a = self.parse_tree()?;
        let b = self.parse_tree()?;
        self.expect(Token::CloseParen)?;
        return Ok(Tree::Comb(label, Box::new(a), Box::new(b)));
      } else {
        return Ok(Tree::Var(ident));
      }
    }

    if self.eat(Token::At)? {
      let name = self.expect(Token::Ident)?;
      let ext_fn = name.to_string();
      let swapped = self.eat(Token::Dollar)?;
      self.expect(Token::OpenParen)?;
      let a = self.parse_tree()?;
      let b = self.parse_tree()?;
      self.expect(Token::CloseParen)?;
      return Ok(Tree::ExtFn(ext_fn, swapped, Box::new(a), Box::new(b)));
    }

    if self.eat(Token::Question)? {
      if self.eat(Token::Caret)? {
        self.expect(Token::OpenParen)?;
        let a = self.parse_tree()?;
        let b = self.parse_tree()?;
        self.expect(Token::CloseParen)?;
        return Ok(Tree::BranchSplit(Box::new(a), Box::new(b)));
      }

      self.expect(Token::OpenParen)?;
      let mut branches = Vec::new();
      while !self.check(Token::CloseParen) {
        branches.push(self.parse_tree()?);
      }
      self.expect(Token::CloseParen)?;

      let Some(ctx) = branches.pop() else { Err(ParseError::InvalidBranch("Empty branch node"))? };
      if branches.is_empty() {
        Err(ParseError::InvalidBranch("Branch nodes must have at one branch"))?
      }
      let branches = Box::new(branch_tree(&branches, 0, 0));

      return Ok(Tree::BranchStart(branches, Box::new(ctx)));
    }

    if self.eat(Token::Hole)? {
      return Ok(Tree::Erase);
    }

    if self.eat(Token::Hash)? {
      self.expect(Token::OpenBracket)?;
      let inner = self.parse_tree()?;
      self.expect(Token::CloseBracket)?;
      return Ok(Tree::BlackBox(Box::new(inner)));
    }

    self.unexpected()
  }
}

fn branch_tree(branches: &[Tree], path: usize, depth: usize) -> Tree {
  if (path | 1 << depth) >= branches.len() {
    return branches[path].clone();
  }

  let lhs = branch_tree(branches, path, depth + 1);
  let rhs = branch_tree(branches, path | 1 << depth, depth + 1);

  Tree::BranchSplit(Box::new(lhs), Box::new(rhs))
}
