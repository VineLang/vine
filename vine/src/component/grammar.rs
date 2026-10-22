use vine_util::idx::Counter;

use super::*;

use std::{
  any::{Any, TypeId},
  collections::HashMap,
  marker::PhantomData,
};

use crate::{
  component::{
    lexer::Token,
    parser::{Assoc, Prec},
  },
  feature::control::{
    closure::ExprClosure, do_::ExprDo, for_::ExprFor, loop_::ExprLoop, when::ExprWhen,
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

  #[allow(unused_variables)]
  fn extra_rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {}

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
    Self::extra_rules(grammar, prec, &mut rules);
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

pub fn left<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, prec.assoc() == Assoc::Left)
}

pub fn right<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, prec.assoc() == Assoc::Right)
}

pub fn both<T: GrammarPratt>(prec: Prec) -> impl ToNode {
  _pratt::<T>(prec, false)
}

fn _pratt<T: GrammarPratt>(prec: Prec, inclusive: bool) -> impl ToNode {
  let prec = if inclusive { prec } else { prec.next().unwrap() };
  from_fn(move |grammar| Node::Rule(T::pratt_rule(grammar, prec)))
}

fn pratt_variant<T: GrammarPrattKind, U>(
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
        if prec == Prec::Max {
          rules.push(grammar.named("Paren", (Token::OpenParen, rule::<$Term>(), Token::CloseParen)));
        }
        for_enum!(Kind in $TermKind {
          $_(pratt_variant($TermKind::$Kind, grammar, prec, rules);)*
        });
      }
    })*
  };
}

GrammarPrattKind!(($) Expr ExprKind, Pat PatKind, Ty TyKind, Impl ImplKind, Trait TraitKind);

impl GrammarPratt for Expr {
  const NAME: &str = "Expr";
  const ALLOW_DOT: bool = true;
  fn extra_rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {}
}

pub struct ExprFull;

impl GrammarRule for ExprFull {
  fn _rule(grammar: &mut Grammar) -> Rule {
    grammar.hidden(
      "ExprFull",
      choice(
        rule::<ExprDo>(),
        rule::<ExprWhen>(),
        rule::<ExprFor>(),
        rule::<ExprClosure>(),
        rule::<ExprLoop>(),
        rule::<ExprDo>(),
      ),
    );
    todo!()
  }
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

#[test]
fn grammar() {
  let mut grammar = Grammar::default();
  _ = Expr::rule(&mut grammar);
  _ = Pat::rule(&mut grammar);
  _ = Ty::rule(&mut grammar);
  _ = Impl::rule(&mut grammar);
  _ = Trait::rule(&mut grammar);
  dbg!(&grammar);
  println!("{}", grammar.to_treesitter());
}

#[cfg(test)]
impl Grammar {
  fn rule_name(&self, rule_id: RuleId) -> String {
    let mut str = String::new();
    let rule = &self.rules[rule_id];
    if rule.hidden {
      str.push('_');
    }
    for (i, char) in rule.label.chars().enumerate() {
      if i != 0 && char != char.to_ascii_lowercase() {
        str.push('_');
      }
      str.push(char.to_ascii_lowercase());
    }
    str
  }

  fn to_treesitter(&self) -> String {
    let mut str = String::new();
    for (id, rule) in &self.rules {
      str += &format!("{}: $ => {},\n", self.rule_name(id), self.to_treesitter_node(&rule.node));
    }
    str
  }

  fn to_treesitter_node(&self, node: &Node) -> String {
    match node {
      Node::Rule(id) => {
        format!("$.{}", self.rule_name(*id))
      }
      Node::Token(token) => match token.value() {
        Some(value) => format!("{:?}", value),
        None => format!("$.{:?}", token),
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
