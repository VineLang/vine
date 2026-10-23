use vine_util::idx::Counter;

use super::*;

use std::{
  any::{Any, TypeId},
  collections::HashMap,
  marker::PhantomData,
};

use crate::{
  component::{
    lexer::{Lexer, Token},
    parser::{Assoc, Parser, Prec},
  },
  feature::block::{
    closure::ExprClosure, do_::ExprDo, for_::ExprFor, label, loop_::ExprLoop, when::ExprWhen,
  },
};

#[derive_(Debug, Clone, Default)]
pub struct Grammar {
  types: HashMap<TypeId, RuleId>,
  pratt: HashMap<(TypeId, Option<Prec>), RuleId>,
  rules: IdxVec<RuleId, Rule>,
}

impl Grammar {
  pub fn named(&mut self, label: impl Into<String>, node: impl ToNode) -> Rule {
    Rule { label: label.into(), hidden: false, node: node.to_node(self) }
  }

  pub fn hidden(&mut self, label: impl Into<String>, node: impl ToNode) -> Rule {
    Rule { label: label.into(), hidden: true, node: node.to_node(self) }
  }
}

new_idx!(pub RuleId);
new_idx!(pub PrattId);

#[derive_(Debug, Clone)]
pub struct Rule {
  label: String,
  hidden: bool,
  node: Node,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
  Rule(RuleId),
  Token(Token),
  Seq(Vec<Node>),
  Choice(Vec<Node>),
  Repeat(Box<Node>, /* mandatory */ bool),
  Optional(Box<Node>),
  Field(String, Box<Node>),
}

pub trait ToNode: Sized + Clone {
  fn to_node(self, grammar: &mut Grammar) -> Node;

  fn named(self, label: impl Into<String>) -> impl ToNode {
    let label = label.into();
    from_fn(|grammar| {
      let node = self.to_node(grammar);
      Node::Rule(grammar.rules.push(Rule { label, hidden: false, node }))
    })
  }

  fn field(self, label: impl Into<String>) -> impl ToNode {
    let label = label.into();
    from_fn(|grammar| Node::Field(label, Box::new(self.to_node(grammar))))
  }

  fn optional(self) -> impl ToNode {
    from_fn(|grammar| Node::Optional(Box::new(self.to_node(grammar))))
  }

  fn repeat(self, mandatory: bool) -> impl ToNode {
    from_fn(move |grammar| Node::Repeat(Box::new(self.to_node(grammar)), mandatory))
  }
}

fn from_fn(f: impl Clone + FnOnce(&mut Grammar) -> Node) -> impl ToNode {
  #[derive(Clone)]
  struct With<F>(F);
  impl<F: Clone + FnOnce(&mut Grammar) -> Node> ToNode for With<F> {
    fn to_node(self, grammar: &mut Grammar) -> Node {
      self.0(grammar)
    }
  }
  With(f)
}

pub trait ToOptionNode: Sized + Clone {
  fn to_option_node(self, grammar: &mut Grammar) -> Option<Node>;
}

impl<T: ToNode> ToOptionNode for T {
  fn to_option_node(self, grammar: &mut Grammar) -> Option<Node> {
    Some(self.to_node(grammar))
  }
}

impl<T: ToNode> ToOptionNode for Option<T> {
  fn to_option_node(self, grammar: &mut Grammar) -> Option<Node> {
    Some(self?.to_node(grammar))
  }
}

pub trait ToNodes: Sized + Clone {
  fn to_nodes(self, grammar: &mut Grammar) -> impl Iterator<Item = Node>;
}

macro_rules! impl_tuple {
  ($($A:ident)* | $B:ident $($C:ident)*) => {
    impl_tuple!($($A)* |);
    impl_tuple!($($A)* $B | $($C)*);
  };

  ($($T:ident)* |) => {
    #[allow(nonstandard_style, unused)]
    impl<$($T: ToOptionNode,)*> ToNodes for ($($T,)*) {
      fn to_nodes(self, grammar: &mut Grammar) -> impl Iterator<Item = Node> {
        let ($($T,)*) = self;
        [$($T.to_option_node(grammar),)*].into_iter().flatten()
      }
    }
  }
}

impl_tuple!(A | B C D E F G H I J L M N O P Q R S T U V W X Y Z);

impl ToNode for Node {
  fn to_node(self, _: &mut Grammar) -> Node {
    self
  }
}

impl ToNode for Token {
  fn to_node(self, _: &mut Grammar) -> Node {
    Node::Token(self)
  }
}

impl<T: ToNodes> ToNode for T {
  fn to_node(self, grammar: &mut Grammar) -> Node {
    Node::Seq(self.to_nodes(grammar).collect())
  }
}

pub fn empty() -> Node {
  Node::Seq(Vec::new())
}

pub fn choice<T: ToNodes>(nodes: T) -> impl ToNode {
  from_fn(|grammar| Node::Choice(nodes.to_nodes(grammar).collect()))
}

pub fn delimited<T: GrammarRule>(open: Token, sep: Token, close: Token) -> impl ToNode {
  (open, (rule::<T>(), (sep, rule::<T>()).repeat(false), sep.optional()).optional(), close)
}

pub trait GrammarRule: Any {
  fn _rule(grammar: &mut Grammar) -> Rule;

  fn rule(grammar: &mut Grammar) -> RuleId {
    let next = grammar.rules.next_index();
    let id = *grammar.types.entry(TypeId::of::<Self>()).or_insert(next);
    if id == next {
      grammar.rules.push_to(id, Rule { label: String::new(), hidden: true, node: Node::Rule(id) });
      let rule = Self::_rule(grammar);
      grammar.rules[id] = rule;
    }
    id
  }
}

pub trait GrammarPratt: Any + GrammarPrattKind {
  const NAME: &str;
  const ALLOW_DOT: bool = false;

  fn pratt_rule(grammar: &mut Grammar, prec: Prec) -> RuleId {
    let key = (TypeId::of::<Self>(), Some(prec));
    if let Some(&id) = grammar.pratt.get(&key) {
      return id;
    }
    let next = prec.next().map(|next| Self::pratt_rule(grammar, next));
    if let Some(&id) = grammar.pratt.get(&key) {
      return id;
    }
    let id = grammar.rules.push(Rule {
      label: String::new(),
      hidden: true,
      node: Node::Choice(Vec::new()),
    });
    grammar.pratt.insert(key, id);
    let mut rules = Vec::new();
    Self::rules(grammar, prec, &mut rules);
    if rules.is_empty()
      && let Some(next) = next
    {
      assert_eq!(id.0, grammar.rules.len() - 1);
      grammar.rules.pop();
      grammar.pratt.insert(key, next);
      return next;
    }
    let node = Node::Choice(
      next
        .into_iter()
        .chain(rules.into_iter().map(|mut rule| {
          rule.label = format!("{}{}", Self::NAME, rule.label);
          grammar.rules.push(rule)
        }))
        .map(Node::Rule)
        .collect::<Vec<_>>(),
    );
    grammar.rules[id] = Rule { label: format!("{}_{:?}", Self::NAME, prec), hidden: true, node };
    id
  }
}

#[allow(unused_variables)]
pub trait GrammarPrattKind {
  fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>);
}

impl<T: GrammarPratt> GrammarRule for T {
  fn _rule(_: &mut Grammar) -> Rule {
    unimplemented!()
  }

  fn rule(grammar: &mut Grammar) -> RuleId {
    Self::pratt_rule(grammar, Prec::Min)
  }
}

pub fn rule<T: GrammarRule>() -> impl ToNode {
  from_fn(|grammar| Node::Rule(T::rule(grammar)))
}

pub fn rule_variant<T: GrammarRule, U>(_: fn(T) -> U) -> impl ToNode {
  rule::<T>()
}

pub fn left<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, prec.assoc() == Assoc::Left)
}

pub fn right<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, prec.assoc() == Assoc::Right)
}

pub fn below<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, false)
}

fn _pratt<T: GrammarPratt>(prec: Prec, inclusive: bool) -> impl ToNode {
  let prec = if inclusive { prec } else { prec.next().unwrap() };
  from_fn(move |grammar| Node::Rule(T::pratt_rule(grammar, prec)))
}

fn pratt_rules_variant<T: GrammarPrattKind, U>(
  _: fn(T) -> U,
  grammar: &mut Grammar,
  prec: Prec,
  rules: &mut Vec<Rule>,
) {
  T::rules(grammar, prec, rules);
}

macro_rules! GrammarPrattKind {
  (($_:tt) $($Term:ident $TermKind:ident $(+ $Extra:ty)?),*) => {
    $(impl GrammarPrattKind for $Term {
      fn rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {
        for_enum!(Kind in $TermKind {
          $_(pratt_rules_variant($TermKind::$Kind, grammar, prec, rules);)*
        });
        if prec == Prec::Max {
          rules.push(grammar.named("Paren", (Token::OpenParen, rule::<$Term>(), Token::CloseParen)));
        }
      }
    })*
  };
}

GrammarPrattKind!(($) Expr ExprKind, Pat PatKind, Ty TyKind, Impl ImplKind, Trait TraitKind);

impl GrammarPratt for Expr {
  const NAME: &str = "Expr";
  const ALLOW_DOT: bool = true;
}

impl GrammarPratt for Pat {
  const NAME: &str = "Pat";
}

impl GrammarPratt for Ty {
  const NAME: &str = "Ty";
  const ALLOW_DOT: bool = true;
}

impl GrammarPratt for Impl {
  const NAME: &str = "Impl";
}

impl GrammarPratt for Trait {
  const NAME: &str = "Trait";
}

impl GrammarRule for Ident {
  fn _rule(grammar: &mut Grammar) -> Rule {
    grammar.named("Ident", Token::BareIdent) // todo
  }
}

impl Grammar {
  pub fn to_treesitter(&self) -> String {
    let mut str = String::new();
    str += &format!(
      "module.exports.keywords = [\"{}\"]\n",
      for_enum!(Token in Token { [$(Token::$Token,)*] })
        .into_iter()
        .filter_map(|x| x.as_keyword())
        .collect::<Vec<_>>()
        .join("\", \"")
    );
    str += "module.exports.rules = {\n";
    for rule in self.rules.values() {
      str += &format!(
        "  {}: $ => {},\n",
        rule_name(rule.hidden, &rule.label),
        self.to_treesitter_node(&rule.node)
      );
    }
    str += "}\n";
    str
  }

  fn to_treesitter_node(&self, node: &Node) -> String {
    match node {
      Node::Rule(id) => {
        let rule = &self.rules[*id];
        format!("$.{}", rule_name(rule.hidden, &rule.label))
      }
      Node::Token(token) => match token.value() {
        Some(value) => format!("{:?}", value),
        None => format!("$.{}", rule_name(true, &format!("{token:?}"))),
      },
      Node::Seq(nodes) => format!(
        "seq({})",
        nodes.iter().map(|n| self.to_treesitter_node(n)).collect::<Vec<_>>().join(", ")
      ),
      Node::Choice(nodes) => format!(
        "choice({})",
        nodes.iter().map(|n| self.to_treesitter_node(n)).collect::<Vec<_>>().join(", ")
      ),
      Node::Repeat(node, mandatory) => {
        format!("repeat{}({})", if *mandatory { "1" } else { "" }, self.to_treesitter_node(node))
      }
      Node::Optional(node) => format!("optional({})", self.to_treesitter_node(node)),
      Node::Field(name, node) => format!("field({:?}, {})", name, self.to_treesitter_node(node)),
    }
  }
}

fn rule_name(hidden: bool, label: &str) -> String {
  let mut str = String::new();
  if hidden {
    str.push('_');
  }

  for (i, char) in label.chars().enumerate() {
    if i != 0 && char != char.to_ascii_lowercase() {
      str.push('_');
    }
    str.push(char.to_ascii_lowercase());
  }
  str
}

impl Grammar {
  fn flatten_choices(&mut self) {
    for id in self.rules.keys() {
      self._flatten_choices(id);
    }
  }

  fn _flatten_choices(&mut self, id: RuleId) -> Vec<RuleId> {
    let rule = &self.rules[id];
    let Node::Choice(choices) = &rule.node else {
      return vec![id];
    };
    let Ok(rules) = choices
      .iter()
      .map(|node| match node {
        Node::Rule(id) => Ok(*id),
        _ => Err(()),
      })
      .collect::<Result<Vec<_>, ()>>()
    else {
      return vec![id];
    };
    let rules = rules.into_iter().flat_map(|x| self._flatten_choices(x)).collect::<Vec<_>>();
    self.rules[id].node = Node::Choice(rules.iter().copied().map(Node::Rule).collect());
    rules
  }
}

/// A PCG random number generator (XSH RR 64/32 (LCG) variant).
struct Pcg32(u64, u64);

impl Pcg32 {
  /// A canonical Pcg32 instance.
  pub const CANONICAL: Pcg32 = Self::new(0xcafef00d_d15ea5e5, 0x14057b7e_f767814f);

  /// Create a new `Pcg32` from the internal state and increment values.
  pub const fn new(mut state: u64, mut increment: u64) -> Pcg32 {
    increment |= 1;
    state += increment;
    state *= Self::MULTIPLIER;
    state += increment;
    Pcg32(state, increment)
  }

  const MULTIPLIER: u64 = 0x5851f42d_4c957f2d;

  /// Generate a pseudo-random `u32`, updating the state of the RNG.
  pub fn gen_u32(&mut self) -> u32 {
    let Pcg32(state, increment) = self;
    let (lo, hi) = (*state as u32, (*state >> 32) as u32);
    *state *= Self::MULTIPLIER;
    *state += *increment;
    u32::rotate_right((hi >> 13) ^ (hi << 5) ^ (lo >> 27), hi >> 27)
  }
}

struct Sampler<'a> {
  grammar: &'a Grammar,
  rng: Pcg32,
}

impl<'a> Sampler<'a> {
  pub fn new(grammar: &'a Grammar) -> Self {
    Sampler { grammar, rng: Pcg32::CANONICAL }
  }

  fn choose(&mut self, options: usize, depth: usize) -> usize {
    if depth == 0 { 0 } else { (self.rng.gen_u32() as usize) % options }
  }

  fn sample(&mut self, id: RuleId, depth: usize) -> Tree {
    let rule = &self.grammar.rules[id];
    let mut children = Vec::new();
    self.sample_node(&rule.node, depth, &mut children);
    Tree::Node(id, children)
  }

  fn sample_node(&mut self, node: &Node, mut depth: usize, output: &mut Vec<Tree>) {
    match node {
      Node::Rule(id) => {
        let rule = &self.grammar.rules[*id];
        if rule.hidden {
          self.sample_node(&rule.node, depth, output);
        } else {
          output.push(self.sample(*id, depth.saturating_sub(1)));
        }
      }
      Node::Token(token) => {
        output.push(Tree::Token(*token));
      }
      Node::Seq(nodes) => {
        for node in nodes {
          self.sample_node(node, depth, output);
        }
      }
      Node::Choice(nodes) => {
        let i = self.choose(nodes.len(), depth);
        self.sample_node(&nodes[i], depth, output);
      }
      Node::Repeat(node, mandatory) => {
        if *mandatory || self.choose(2, depth) > 0 {
          self.sample_node(node, depth, output);
        }
        while self.choose(3, depth) > 0 {
          self.sample_node(node, depth, output);
          depth -= 1;
        }
      }
      Node::Optional(node) => {
        if self.choose(3, depth) > 0 {
          self.sample_node(node, depth, output);
        }
      }
      Node::Field(_, node) => {
        self.sample_node(node, depth, output);
      }
    }
  }
}

#[derive(Debug)]
enum Tree {
  Node(RuleId, Vec<Tree>),
  Token(Token),
}

impl Tree {
  fn to_string(&self) -> String {
    let mut str = String::new();
    self._to_string(&mut str);
    str
  }

  fn _to_string(&self, str: &mut String) {
    match self {
      Tree::Node(_, trees) => {
        for tree in trees {
          tree._to_string(str);
        }
      }
      Tree::Token(token) => {
        let token = match token {
          Token::BareIdent => "x",
          _ => token.value().unwrap(),
        };
        if !str.is_empty() {
          str.push(' ');
        }
        str.push_str(token);
      }
    }
  }
}

#[test]
fn sampler() {
  let mut grammar = Grammar::default();
  let rule = Expr::rule(&mut grammar);
  grammar.flatten_choices();
  let mut sampler = Sampler::new(&grammar);
  let mut best_len = 0;
  for _ in 0..100 {
    let src = sampler.sample(rule, 12).to_string();
    println!("{}\n", src);
    _ = Parser::new(Lexer::new(&src)).unwrap().parse::<Expr>();
  }
}
