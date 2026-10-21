use vine_util::idx::Counter;

use super::*;

use std::{
  any::{Any, TypeId},
  collections::HashMap,
  marker::PhantomData,
};

use crate::component::{
  lexer::Token,
  parser::{Assoc, Prec},
};

#[derive_(Debug, Clone, Default)]
pub struct Grammar {
  types: HashMap<TypeId, RuleId>,
  pratt: HashMap<(TypeId, Prec), RuleId>,
  rules: IdxVec<RuleId, Option<Rule>>,
}

impl Grammar {
  pub fn named(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: false, node: node.into_node(self) }
  }

  pub fn hidden(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: true, node: node.into_node(self) }
  }

  pub fn define_rule<T: GrammarRule>(&mut self) -> RuleId {
    let next = self.rules.next_index();
    let id = *self.types.entry(TypeId::of::<T>()).or_insert(next);
    if id == next {
      self.rules.push_to(id, Rule { label: String::new(), hidden: true, node: Node::Rule(id) });
      let rule = T::_rule(self);
      self.rules[id] = Some(rule);
    }
    id
  }

  pub fn define_pratt<T: GrammarPratt>(&mut self, prec: Prec) -> RuleId {}
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

pub trait GrammarRule: Any {
  fn _rule(grammar: &mut Grammar) -> Rule;

  fn rule(grammar: &mut Grammar) -> RuleId {
    let next = grammar.rules.next_index();
    let id = *grammar.types.entry(TypeId::of::<Self>()).or_insert(next);
    if id == next {
      grammar.rules.push_to(id, None);
      let rule = Self::_rule(grammar);
      grammar.rules[id] = Some(rule);
    }
    id
  }
}

impl<T: GrammarPratt> GrammarRule for T {
  fn _rule(grammar: &mut Grammar) -> Rule {
    unimplemented!()
  }

  fn rule(grammar: &mut Grammar) -> RuleId {
    Self::pratt_rule(grammar, Prec::Min)
  }
}

pub trait GrammarPratt: Any + GrammarPrattKind {
  const NAME: &str;
  const ALLOW_DOT: bool = false;

  fn pratt_rule(grammar: &mut Grammar, prec: Prec) -> RuleId {
    let key = (TypeId::of::<Self>(), prec);
    if let Some(id) = grammar.pratt.get(key) {
      return id;
    }
    let next = prec.next().map(|next| grammar.define_pratt::<Self>(next));
    let id = grammar.rules.push(Rule {
      label: String::new(),
      hidden: true,
      node: Node::Choice(Vec::new()),
    });
    grammar.pratt.insert(key, id);
    let mut rules = Vec::new();
    Self::_rules(grammar, prec, &mut rules);
    if rules.is_empty() {
      assert_eq!(id.0, grammar.rules.len() - 1);
      grammar.rules.pop();
      grammar.pratt.insert(key, next);
      return next.unwrap();
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
    grammar.rules[id] = Rule {
      label: if prec == Prec::Min {
        Self::NAME.into()
      } else {
        format!("{}_{:?}", Self::NAME, prec)
      },
      hidden: prec != Prec::Min,
      node,
    };
    id
  }
}

pub trait GrammarPrattKind {
  fn _rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>);

  fn rules(self, grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {
    Self::_rules(grammar, prec, rules)
  }
}

impl<T: GrammarPrattKind, U> GrammarPrattKind for fn(T) -> U {
  fn _rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {
    T::_rules(grammar, prec, rules)
  }
}

pub trait IntoNode: Sized + Clone {
  fn into_node(self, grammar: &mut Grammar) -> Node;

  fn named(self, label: impl Into<String>) -> impl IntoNode {
    #[derive(Clone)]
    struct Named<T>(String, T);
    impl<T: IntoNode> IntoNode for Named<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        let node = self.1.into_node(grammar);
        Node::Rule(grammar.rules.push(Rule { label: self.0, hidden: false, node }))
      }
    }
    Named(label.into(), self)
  }

  fn field(self, label: impl Into<String>) -> impl IntoNode {
    #[derive(Clone)]
    struct Field<T>(String, T);
    impl<T: IntoNode> IntoNode for Field<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Field(self.0, Box::new(self.1.into_node(grammar)))
      }
    }
    Field(label.into(), self)
  }

  fn optional(self) -> impl IntoNode {
    #[derive(Clone)]
    struct Optional<T>(T);
    impl<T: IntoNode> IntoNode for Optional<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Optional(Box::new(self.0.into_node(grammar)))
      }
    }
    Optional(self)
  }

  fn repeat(self, mandatory: bool) -> impl IntoNode {
    #[derive(Clone)]
    struct Repeat<T>(T, bool);
    impl<T: IntoNode> IntoNode for Repeat<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Repeat(Box::new(self.0.into_node(grammar)), self.1)
      }
    }
    Repeat(self, mandatory)
  }

  fn repeat1(self) -> impl IntoNode {
    #[derive(Clone)]
    struct Repeat1<T>(T);
    impl<T: IntoNode> IntoNode for Repeat1<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Repeat1(Box::new(self.0.into_node(grammar)))
      }
    }
    Repeat1(self)
  }
}

impl IntoNode for Node {
  fn into_node(self, _: &mut Grammar) -> Node {
    self
  }
}

impl IntoNode for Token {
  fn into_node(self, _: &mut Grammar) -> Node {
    Node::Token(self)
  }
}

pub trait IntoNodes: Sized + Clone {
  fn into_nodes(self, grammar: &mut Grammar) -> impl IntoIterator<Item = Node>;
}

macro_rules! impl_tuple {
  ($($A:ident)* | $B:ident $($C:ident)*) => {
    impl_tuple!($($A)* |);
    impl_tuple!($($A)* $B | $($C)*);
  };

  ($($T:ident)* |) => {
    #[allow(nonstandard_style, unused)]
    impl<$($T: IntoNode,)*> IntoNodes for ($($T,)*) {
      fn into_nodes(self, grammar: &mut Grammar) -> impl IntoIterator<Item = Node> {
        let ($($T,)*) = self;
        [$($T.into_node(grammar),)*]
      }
    }
  }
}

impl_tuple!(A | B C D E F G H I J L M N O P Q R S T U V W X Y Z);

impl<T: IntoNodes> IntoNode for T {
  fn into_node(self, grammar: &mut Grammar) -> Node {
    Node::Seq(self.into_nodes(grammar).into_iter().collect())
  }
}

pub fn empty() -> Node {
  Node::Seq(Vec::new())
}

pub fn choice<T: IntoNodes>(nodes: T) -> impl IntoNode {
  #[derive(Clone)]
  struct Choice<T>(T);
  impl<T: IntoNodes> IntoNode for Choice<T> {
    fn into_node(self, grammar: &mut Grammar) -> Node {
      Node::Choice(self.0.into_nodes(grammar).into_iter().collect())
    }
  }
  Choice(nodes)
}

pub fn rule<T: GrammarRule>() -> impl IntoNode {
  _rule::<T>(None)
}

pub fn left<T: GrammarRule>(prec: Prec) -> impl IntoNode {
  _rule::<T>(Some((prec, Assoc::Left)))
}

pub fn right<T: GrammarRule>(prec: Prec) -> impl IntoNode {
  _rule::<T>(Some((prec, Assoc::Right)))
}

pub fn both<T: GrammarRule>(prec: Prec) -> impl IntoNode {
  _rule::<T>(Some((prec, Assoc::None)))
}

fn _rule<T: GrammarRule>(prec: Option<(Prec, Assoc)>) -> impl IntoNode {
  struct DefineRule<T>(PhantomData<T>, Option<(Prec, Assoc)>);
  impl<T> Clone for DefineRule<T> {
    fn clone(&self) -> Self {
      Self(self.0, self.1)
    }
  }
  impl<T: GrammarRule + 'static> IntoNode for DefineRule<T> {
    fn into_node(self, grammar: &mut Grammar) -> Node {
      Node::Rule(grammar.define_rule::<T>())
    }
  }
  DefineRule::<T>(PhantomData, prec)
}

impl GrammarRule for Ident {
  fn _rule(grammar: &mut Grammar) -> Rule {
    grammar.named("Ident", Token::BareIdent) // todo
  }
}

#[test]
fn math() {
  let mut grammar = Grammar::default();
  grammar.define_rule::<Expr>();
  grammar.define_rule::<Pat>();
  grammar.define_rule::<Ty>();
  grammar.define_rule::<Impl>();
  grammar.define_rule::<Trait>();
  // dbg!(&grammar);
  grammar.eliminate_pratt();
  // dbg!(&grammar);
  println!("{}", grammar.to_treesitter());
}

impl Grammar {
  // pub fn eliminate_pratt(&mut self) {
  //   let mut replacements = HashMap::new();

  //   for id in self.rules.keys() {
  //     let mut rule = self.rules[id].take().unwrap();
  //     if let RuleKind::Pratt(options) = rule.kind {
  //       let mut levels = BTreeMap::<Prec, Vec<RuleId>>::new();
  //       for (prec, rule) in options {
  //         levels.entry(prec).or_default().push(rule);
  //       }
  //       let mut last_id = None;
  //       let levels = levels
  //         .into_iter()
  //         .rev()
  //         .map(|(prec, rules)| {
  //           let id = self.rules.push(Some(Rule {
  //             label: format!("{}_{:?}", rule.label, prec),
  //             hidden: true,
  //             kind: RuleKind::Node(Node::Choice(
  //               last_id.into_iter().chain(rules).map(|id| Node::Rule(id,
  // None)).collect(),             )),
  //           }));
  //           last_id = Some(id);
  //           (prec, id)
  //         })
  //         .collect::<Vec<_>>();
  //       replacements.insert(id, levels);
  //       rule.kind = RuleKind::Node(Node::Rule(last_id.unwrap(), None));
  //     }
  //     self.rules[id] = Some(rule);
  //   }

  //   for rule in self.rules.values_mut() {
  //     if let RuleKind::Node(node) = &mut rule.as_mut().unwrap().kind {
  //       replace(node, &replacements);
  //     }
  //   }

  //   fn replace(node: &mut Node, replacements: &HashMap<RuleId, Vec<(Prec,
  // RuleId)>>) {     match node {
  //       Node::Rule(id, prec) => {
  //         if let Some((prec, assoc)) = prec.take() {
  //           *id = replacements[id]
  //             .iter()
  //             .rev()
  //             .find(|&&(other, _)| {
  //               prec < other || prec == other && assoc != Assoc::None &&
  // prec.assoc() == assoc             })
  //             .unwrap()
  //             .1;
  //         }
  //       }
  //       Node::Token(_) => {}
  //       Node::Seq(nodes) | Node::Choice(nodes) => {
  //         for node in nodes {
  //           replace(node, replacements)
  //         }
  //       }
  //       Node::Repeat(node) | Node::Repeat1(node) | Node::Optional(node) |
  // Node::Field(_, node) => {         replace(node, replacements)
  //       }
  //     }
  //   }
  // }

  fn to_treesitter(&self) -> String {
    let mut str = String::new();
    for rule in self.rules.values() {
      let rule = rule.as_ref().unwrap();
      str += &format!(
        "{}{}: $ => {},\n",
        if rule.hidden { "_" } else { "" },
        rule.label,
        self.to_treesitter_node(rule.node)
      );
    }
    str
  }

  fn to_treesitter_node(&self, node: &Node) -> String {
    match node {
      Node::Rule(rule_id) => {
        let rule = self.rules[*rule_id].as_ref().unwrap();
        format!("$.{}{}", if rule.hidden { "_" } else { "" }, rule.label)
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
        format!("repeat{}({})", if mandatory { "1" } else { "" }, self.to_treesitter_node(node))
      }
      Node::Optional(node) => format!("optional({})", self.to_treesitter_node(node)),
      Node::Field(name, node) => format!("field({:?}, {})", name, self.to_treesitter_node(node)),
    }
  }
}

macro_rules! GrammarPrattKind {
  (($_:tt) $($Term:ident $TermKind:ident),*) => {
    $(impl GrammarPrattKind for $Term {
      fn _rules(grammar: &mut Grammar, prec: Prec, rules: &mut Vec<Rule>) {
        for_enum!(Kind in $TermKind {
          $_($TermKind::$Kind.rules(grammar, prec, rules);)*
        });
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

#[derive(Default, Debug)]
struct Choices {
  vec: Vec<usize>,
  i: Counter<usize>,
}

impl Choices {
  pub fn choose(&mut self, max: usize) -> usize {
    if max == 1 {
      return 0;
    }
    if self.i.count() == self.vec.len() {
      self.vec.push(max - 1);
    }
    max - 1 - self.vec[self.i.next()]
  }

  pub fn advance(&mut self) -> bool {
    self.i.0 = 0;
    while let Some(i) = self.vec.pop() {
      if i == 0 {
        continue;
      }
      self.vec.push(i - 1);
      return true;
    }
    false
  }
}

// #[test]
// fn sampler() {
//   let mut grammar = Grammar::default();
//   let rule = grammar.define_rule::<Expr>();
//   grammar.eliminate_pratt();
//   println!("{}", grammar.to_treesitter());
//   let mut choices = Choices::default();
//   let mut tokens = Vec::new();
//   let mut str = String::new();
//   loop {
//     // dbg!(&choices);
//     if let Some(()) = grammar.sample_rule(rule, 3, &mut choices, &mut tokens)
// {       for token in &tokens {
//         match token.value() {
//           Some(value) => str += value,
//           None => match token {
//             Token::BareIdent => str += "x",
//             Token::TupleField => str += ".0",
//             t => unreachable!("{t:?}"),
//           },
//         }
//         str += " ";
//       }
//       println!("{}", str);
//     }
//     str.clear();
//     tokens.clear();
//     if !choices.advance() {
//       break;
//     }
//   }
// }

// impl Grammar {
//   fn sample_rule(
//     &self,
//     rule: RuleId,
//     depth: usize,
//     choices: &mut Choices,
//     tokens: &mut Vec<Token>,
//   ) -> Option<()> {
//     let rule = self.rules[rule].as_ref().unwrap();
//     let RuleKind::Node(node) = &rule.kind else { unreachable!() };
//     self.sample_node(node, depth.checked_sub(!rule.hidden as usize)?,
// choices, tokens)   }

//   fn sample_node(
//     &self,
//     node: &Node,
//     depth: usize,
//     choices: &mut Choices,
//     tokens: &mut Vec<Token>,
//   ) -> Option<()> {
//     match node {
//       Node::Rule(rule, _) => self.sample_rule(*rule, depth, choices,
// tokens)?,       Node::Token(token) => tokens.push(*token),
//       Node::Seq(nodes) => {
//         for node in nodes {
//           self.sample_node(node, depth, choices, tokens)?;
//         }
//       }
//       Node::Choice(nodes) => {
//         self.sample_node(&nodes[choices.choose(nodes.len())], depth, choices,
// tokens)?;       }
//       Node::Repeat(node) => {
//         for _ in 0..choices.choose(1) {
//           self.sample_node(node, depth, choices, tokens)?;
//         }
//       }
//       Node::Repeat1(node) => {
//         for _ in 0..=choices.choose(1) {
//           self.sample_node(node, depth, choices, tokens)?;
//         }
//       }
//       Node::Optional(node) => {
//         if choices.choose(2) == 1 {
//           self.sample_node(node, depth, choices, tokens)?;
//         }
//       }
//       Node::Field(_, node) => self.sample_node(node, depth, choices,
// tokens)?,     }
//     Some(())
//   }
// }
