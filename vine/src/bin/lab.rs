use vine::structures::origins::Origins;
use vine_util::new_idx;

mod ast {
  use core::fmt;

  #[derive(Default, Debug)]
  pub struct Defs {
    pub types: Vec<TypeDef>,
    pub nets: Vec<NetDef>,
    pub agents: Vec<AgentDef>,
  }

  #[derive(Debug, Clone, Copy)]
  pub enum Variance {
    Pos,
    Neg,
    Inv,
  }

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub enum Polarity {
    Pos,
    Neg,
  }

  #[derive(Debug, Clone, Copy)]
  pub enum Relation {
    Lt,
    Le,
    Gt,
    Ge,
  }

  #[derive(Debug)]
  pub struct TypeDef(pub String);
  #[derive(Debug)]
  pub struct NetDef {
    pub name: String,
    pub origins: OriginParams,
    pub ports: Vec<Port>,
    pub body: Option<Vec<Component>>,
  }

  #[derive(Debug)]
  pub struct AgentDef {
    pub name: String,
    pub origins: OriginParams,
    pub ports: Vec<Port>,
  }

  #[derive(Debug)]
  pub struct OriginParams(pub Vec<(Option<(Origin, Relation)>, Origin)>);

  #[derive(Debug)]
  pub struct Component {
    pub name: String,
    pub ports: Vec<String>,
  }

  #[derive(Debug, Clone)]
  pub struct Origin(pub Vec<String>);

  #[derive(Debug)]
  pub struct Type {
    pub polarity: Polarity,
    pub name: String,
    pub source: Origin,
    pub dest: Origin,
  }

  #[derive(Debug)]
  pub struct Port {
    pub name: String,
    pub ty: Type,
  }

  impl fmt::Display for Origin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let mut start = true;
      for x in &self.0 {
        if !start {
          f.write_str(" ")?;
        }
        start = false;
        write!(f, ":{x}")?;
      }
      Ok(())
    }
  }
}

mod parser {

  use logos::Logos;
  use vine_util::{
    lexer::lex_block_comment,
    parser::{Delimiters, Parser, ParserState},
  };

  use super::ast::*;

  #[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
  #[logos(skip r"[ \t\r\n\f]+")]
  #[repr(u8)]
  enum Token {
    #[token("type")]
    Type,
    #[token("agent")]
    Agent,
    #[token("net")]
    Net,

    #[token(",")]
    Comma,
    #[token(";")]
    Semi,

    #[token(":")]
    Colon,

    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,

    #[token("+")]
    Plus,
    #[cfg_attr(not(rust_analyzer), token("-"))]
    Minus,
    #[token("!")]
    Bang,

    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,

    #[regex(r"\p{ID_Start}\p{ID_Continue}*|_\p{ID_Continue}+")]
    Ident,

    #[regex("//.*", logos::skip)]
    #[token("/*", lex_block_comment)]
    Skip,
  }

  impl vine_util::lexer::Token for Token {
    fn into_u8(self) -> u8 {
      self as u8
    }

    unsafe fn from_u8(value: u8) -> Self {
      unsafe { std::mem::transmute(value) }
    }
  }

  struct LabParser<'a>(ParserState<'a, Token>);

  impl<'a> Parser<'a> for LabParser<'a> {
    type Token = Token;
    type Error = String;

    fn state(&mut self) -> &mut ParserState<'a, Self::Token> {
      &mut self.0
    }

    fn lex_error(&self) -> Self::Error {
      "lexer error".into()
    }

    fn unexpected_error(&self) -> Self::Error {
      format!("expected {:?} found {:?}", self.0.expected, self.0.token)
    }
  }

  impl<'a> LabParser<'a> {
    fn parse_ident(&mut self) -> Result<String, String> {
      Ok(self.expect(Token::Ident)?.into())
    }

    fn parse_origin(&mut self) -> Result<Origin, String> {
      let mut origins = Vec::new();
      while self.eat(Token::Colon)? {
        origins.push(self.parse_ident()?);
      }
      if origins.is_empty() {
        Err("empty origin")?
      }
      Ok(Origin(origins))
    }

    fn parse_variance(&mut self) -> Result<Option<Variance>, String> {
      Ok(if self.check(Token::Plus) {
        Some(Variance::Pos)
      } else if self.check(Token::Minus) {
        Some(Variance::Neg)
      } else if self.check(Token::Bang) {
        Some(Variance::Inv)
      } else {
        None
      })
    }

    fn parse_origin_params(&mut self) -> Result<OriginParams, String> {
      let mut params = Vec::new();
      if self.eat(Token::OpenBracket)? && !self.eat(Token::CloseBracket)? {
        let mut prev = self.parse_origin()?;
        params.push((None, prev.clone()));
        while !self.eat(Token::CloseBracket)? {
          let relation = if self.eat(Token::Comma)? {
            None
          } else if self.eat(Token::Lt)? {
            Some(Relation::Lt)
          } else if self.eat(Token::Le)? {
            Some(Relation::Le)
          } else if self.eat(Token::Ge)? {
            Some(Relation::Ge)
          } else if self.eat(Token::Gt)? {
            Some(Relation::Gt)
          } else {
            self.unexpected()?
          };
          let origin = self.parse_origin()?;
          params.push((relation.map(|r| (prev, r)), origin.clone()));
          prev = origin;
        }
      }
      Ok(OriginParams(params))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
      let polarity = if self.eat(Token::Plus)? {
        Polarity::Pos
      } else if self.eat(Token::Minus)? {
        Polarity::Neg
      } else {
        self.unexpected()?
      };
      let name = self.parse_ident()?;
      let mut sources = Vec::new();
      let mut destinations = Vec::new();
      loop {
        if self.eat(Token::Lt)? {
          sources.push(self.parse_ident()?)
        } else if self.eat(Token::Gt)? {
          destinations.push(self.parse_ident()?)
        } else {
          break;
        }
      }
      Ok(Type { polarity, name, source: Origin(sources), dest: Origin(destinations) })
    }

    fn parse_port(&mut self) -> Result<Port, String> {
      let name = self.parse_ident()?;
      self.expect(Token::Colon)?;
      let ty = self.parse_type()?;
      Ok(Port { name, ty })
    }

    fn parse_agent(&mut self) -> Result<Component, String> {
      let name = self.parse_ident()?;
      let ports = self.parse_delimited(PAREN_COMMA, Self::parse_ident)?;
      Ok(Component { name, ports })
    }

    fn parse_type_def(&mut self) -> Result<TypeDef, String> {
      self.expect(Token::Type)?;
      let name = self.parse_ident()?;
      Ok(TypeDef(name))
    }

    fn parse_agent_def(&mut self) -> Result<AgentDef, String> {
      self.expect(Token::Agent)?;
      let name = self.parse_ident()?;
      let params = self.parse_origin_params()?;
      let ports = self.parse_delimited(PAREN_COMMA, Self::parse_port)?;
      Ok(AgentDef { name, origins: params, ports })
    }

    fn parse_net_def(&mut self) -> Result<NetDef, String> {
      self.expect(Token::Net)?;
      let name = self.parse_ident()?;
      let params = self.parse_origin_params()?;
      let ports = self.parse_delimited(PAREN_COMMA, Self::parse_port)?;
      let net = self
        .check_then(Token::OpenBrace, |self_| self_.parse_delimited(BRACE, Self::parse_agent))?;
      Ok(NetDef { name, origins: params, ports, body: net })
    }

    fn parse_defs(&mut self) -> Result<Defs, String> {
      let mut defs = Defs::default();
      loop {
        if self.check(Token::Type) {
          defs.types.push(self.parse_type_def()?);
        } else if self.check(Token::Agent) {
          defs.agents.push(self.parse_agent_def()?);
        } else if self.check(Token::Net) {
          defs.nets.push(self.parse_net_def()?);
        } else if self.0.token.is_none() {
          break;
        } else {
          self.unexpected()?;
        }
      }
      Ok(defs)
    }
  }

  const PAREN_COMMA: Delimiters<Token> = Delimiters {
    open: Some(Token::OpenParen),
    close: Some(Token::CloseParen),
    separator: Some(Token::Comma),
  };

  const BRACE: Delimiters<Token> =
    Delimiters { open: Some(Token::OpenBrace), close: Some(Token::CloseBrace), separator: None };

  pub fn parse(source: &str) -> Defs {
    let mut parser = LabParser(ParserState::new(source));
    parser.bump().unwrap();
    parser.parse_defs().unwrap()
  }
}

mod model {
  use std::{
    collections::{BTreeMap, HashMap},
    fmt::Write as _,
  };

  use ast::Polarity;
  use vine::structures::origins::{relation::Relation, Origin, Origins};
  use vine_util::{idx::IdxVec, new_idx};

  use crate::ast;

  #[derive(Default, Debug)]
  pub struct OriginParams {
    pub names: IdxVec<Origin, ast::Origin>,
    pub lookup: HashMap<String, Origin>,
    pub origins: Origins,
  }

  impl OriginParams {
    pub fn build(params: &ast::OriginParams) -> Self {
      let mut self_ = Self::default();
      for (rel, b) in &params.0 {
        let b = self_.lookup(b, true);
        if let Some((a, rel)) = rel {
          let rel = match rel {
            ast::Relation::Lt => Relation::LT,
            ast::Relation::Le => Relation::LE,
            ast::Relation::Gt => Relation::GT,
            ast::Relation::Ge => Relation::GE,
          };
          let a = self_.lookup(a, true);
          self_.origins.relate(a, b, rel);
        }
      }
      self_
    }

    pub fn lookup(&mut self, origin: &ast::Origin, create: bool) -> Origin {
      assert!(!origin.0.is_empty());
      let mut iter = origin.0.iter().cloned();
      let name = iter.next().unwrap();
      let mut cur = self.lookup_one(name.clone(), create);
      let mut name = vec![name];
      for next in iter {
        name.push(next.clone());
        let next = self.lookup_one(next, create);
        cur = self.origins.join(cur, next);
        self.names.push_to(cur, ast::Origin(name.clone()));
      }
      cur
    }

    fn lookup_one(&mut self, name: String, create: bool) -> Origin {
      *self.lookup.entry(name.clone()).or_insert_with(|| {
        assert!(create);
        let origin = self.origins.new_origin();
        self.names.push_to(origin, ast::Origin(vec![name]));
        origin
      })
    }

    pub fn import(&mut self, other: &OriginParams, scope: &str) -> impl Fn(Origin) -> Origin {
      for name in other.names.values() {
        _ = self.names.push(ast::Origin(name.0.iter().map(|x| format!("{scope}.{x}")).collect()));
      }
      self.origins.import(&other.origins)
    }

    pub fn show(&self) -> String {
      let mut lines = String::new();
      for (a, facts) in &self.origins.origins {
        for (&b, &rel) in &facts.relations {
          writeln!(lines, "  {} {:?} {}", self.names[a], rel, self.names[b]).unwrap();
        }
      }
      lines
    }
  }

  #[derive(Debug, Default)]
  pub struct Types {
    pub names: IdxVec<TypeId, String>,
    pub lookup: HashMap<String, TypeId>,
  }

  impl Types {
    pub fn def(&mut self, def: &ast::TypeDef) -> TypeId {
      let ty = self.names.push(def.0.clone());
      let old = self.lookup.insert(def.0.clone(), ty);
      assert!(old.is_none());
      ty
    }

    pub fn get(&self, ty: &str) -> TypeId {
      self.lookup[ty]
    }
  }

  new_idx!(pub TypeId);

  #[derive(Debug, Clone, Copy)]
  pub struct Type {
    pub polarity: Polarity,
    pub id: TypeId,
    pub source: Origin,
    pub dest: Origin,
  }

  impl Polarity {
    pub fn inverse(self) -> Self {
      match self {
        Polarity::Pos => Polarity::Neg,
        Polarity::Neg => Polarity::Pos,
      }
    }
  }

  impl Type {
    pub fn inverse(self) -> Type {
      Type { polarity: self.polarity.inverse(), id: self.id, source: self.dest, dest: self.source }
    }

    pub fn build(types: &Types, origins: &mut OriginParams, ty: &ast::Type) -> Self {
      Type {
        polarity: ty.polarity,
        id: types.get(&ty.name),
        source: origins.lookup(&ty.source, false),
        dest: origins.lookup(&ty.dest, false),
      }
    }
  }

  #[derive(Debug)]
  pub struct Component {
    pub name: String,
    pub origins: OriginParams,
    pub ports: Vec<Type>,
  }

  impl Component {
    pub fn build(
      types: &Types,
      name: &str,
      origins: &ast::OriginParams,
      ports: &[ast::Port],
    ) -> Self {
      let mut origins = OriginParams::build(origins);
      let ports = ports.iter().map(|port| Type::build(types, &mut origins, &port.ty)).collect();
      Component { name: name.to_owned(), origins, ports }
    }
  }

  #[derive(Debug, Default)]
  pub struct Components {
    pub lookup: HashMap<String, ComponentId>,
    pub components: IdxVec<ComponentId, Component>,
  }

  new_idx!(pub ComponentId);

  impl Components {
    pub fn def(&mut self, component: Component) -> ComponentId {
      let name = component.name.clone();
      let id = self.components.push(component);
      let old = self.lookup.insert(name, id);
      assert!(old.is_none());
      id
    }

    pub fn get(&self, name: &str) -> ComponentId {
      *self.lookup.get(name).unwrap_or_else(|| panic!("no component {name}"))
    }
  }

  #[derive(Debug)]
  pub struct Net {
    pub name: String,
    pub interface: (ComponentId, Vec<String>),
    pub components: Vec<(ComponentId, Vec<String>)>,
  }

  impl Net {
    pub fn build(components: &Components, net: &ast::NetDef) -> Self {
      let interface =
        (components.get(&net.name), net.ports.iter().map(|x| x.name.clone()).collect());
      let components = net
        .body
        .as_ref()
        .unwrap()
        .iter()
        .map(|component| (components.get(&component.name), component.ports.clone()))
        .collect();
      Net { name: net.name.clone(), interface, components }
    }
  }

  #[derive(Debug)]
  pub struct Model {
    pub types: Types,
    pub components: Components,
    pub nets: Vec<Net>,
  }

  impl Model {
    pub fn build(defs: &ast::Defs) -> Self {
      let mut types = Types::default();
      for def in &defs.types {
        types.def(def);
      }
      let mut components = Components::default();
      for agent in &defs.agents {
        components.def(Component::build(&types, &agent.name, &agent.origins, &agent.ports));
      }
      for net in &defs.nets {
        components.def(Component::build(&types, &net.name, &net.origins, &net.ports));
      }
      let mut nets = Vec::new();
      for net in &defs.nets {
        if net.body.is_some() {
          nets.push(Net::build(&components, net));
        }
      }
      Model { types, components, nets }
    }

    pub fn check_net(&self, net: &Net) {
      let mut origins = OriginParams::default();
      let mut vars = BTreeMap::<_, Vec<_>>::new();
      let _ = [(&net.interface, true)]
        .into_iter()
        .chain(net.components.iter().map(|x| (x, false)))
        .enumerate()
        .map(|(i, ((component, ports), invert))| {
          let component = &self.components.components[*component];
          let map = origins.import(&component.origins, &format!("{i}_{}", component.name));
          assert_eq!(ports.len(), component.ports.len());
          for (var, &ty) in ports.iter().zip(component.ports.iter()) {
            let ty = if invert { ty.inverse() } else { ty };
            vars.entry(var).or_default().push(Type {
              polarity: ty.polarity,
              id: ty.id,
              source: map(ty.source),
              dest: map(ty.dest),
            });
          }
        })
        .collect::<Vec<_>>();
      for (_, mut tys) in vars {
        assert_eq!(tys.len(), 2);
        let a = tys.pop().unwrap();
        let b = tys.pop().unwrap();
        assert_eq!(a.polarity.inverse(), b.polarity);
        assert_eq!(a.id, b.id);
        origins.origins.relate(a.source, b.dest, Relation::LE);
        origins.origins.relate(b.source, a.dest, Relation::LE);
      }
      let system = origins.show();
      origins.origins.solve();
      println!("{}: {}", net.name, !origins.origins.inconsistent);
      // println!("{system}");
    }
  }
}

fn main() {
  let input = "
type Nat

agent nat[:a, :b](n: +Nat <a >b)

agent add[:a < :b < :c](a: -Nat >a <c, b: -Nat >a <c, out: +Nat <b >c)
agent dup[:a < :b, :c](a: -Nat >a <c, o1: +Nat <b >c, o2: +Nat <b >c)

net double[:a < :b, :c, :d](a: -Nat >a <c, out: +Nat <b >d) {
  dup(a, x, y)
  add(x, y, out)
}

type RefNat

agent ref_nat[:a < :b, :c, :d](r: +RefNat >a <c, i: -Nat <b >c, o: +Nat <b >d)
agent deref_nat[:a < :b, :c](r: -RefNat >a <c, i: +Nat <b >c, o: -Nat <b >c)

net vicious[:a, :b](o: +Nat <a >b) {
  dup(x0, x1, o)
  nat(one)
  add(x1, one, x0)
}

net inc[:a, :b < :c, :d](r: -RefNat <b >a, o: +Nat <a <c >d) {
  deref_nat(r, x0, x2)
  nat(one)
  add(x0, one, x1)
  dup(x1, x2, o)
}

net inc_add[:a, :b](o: +Nat <a >b) {
  nat(x0)
  ref_nat(xr, x0, x1)
  inc(xr, y)
  add(x1, y, o)
}

net inc_add_vicious[:a, :b](o: +Nat <a >b) {
  ref_nat(xr, x0, x1)
  inc(xr, y)
  add(x1, y, z)
  dup(z, o, x0)
}

net swap[:a, :b](x: -RefNat <a >b, y: -RefNat <a >b) {
  deref_nat(x, p, q)
  deref_nat(y, q, p)
}

agent erase[:a, :b](x: -Nat <a >b)

net swap_swap() {
  nat(x0)
  ref_nat(x01, x0, x1)
  ref_nat(x12, x1, x2)
  erase(x2)
  nat(y0)
  ref_nat(y01, y0, y1)
  ref_nat(y12, y1, y2)
  erase(y2)
  swap(x01, y01)
  swap(x12, y12)
}

net swap_swap_vicious() {
  nat(x0)
  ref_nat(x01, x0, x1)
  ref_nat(x12, x1, x2)
  erase(x2)
  nat(y0)
  ref_nat(y01, y0, y1)
  ref_nat(y12, y1, y2)
  erase(y2)
  swap(x01, y12)
  swap(x12, y01)
}

  ";
  let ast = parser::parse(input);
  // dbg!(ast);
  let mut model = model::Model::build(&ast);

  for component in model.components.components.values_mut() {
    component.origins.origins.solve();
    assert!(!component.origins.origins.inconsistent);
  }

  for net in &model.nets {
    model.check_net(net);
  }
  // dbg!(model);
}
