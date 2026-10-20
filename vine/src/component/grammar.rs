use super::*;

use std::{
  any::{Any, TypeId},
  collections::{BTreeMap, HashMap},
  marker::PhantomData,
};

use crate::component::{
  lexer::Token,
  parser::{Assoc, Prec},
};

#[derive_(Debug, Clone, Default)]
pub struct Grammar {
  types: HashMap<TypeId, RuleId>,
  rules: IdxVec<RuleId, Option<Rule>>,
}

impl Grammar {
  pub fn pratt(
    &mut self,
    label: impl Into<String>,
    options: impl IntoIterator<Item = (Prec, Rule)>,
  ) -> Rule {
    Rule {
      label: label.into(),
      hidden: true,
      kind: RuleKind::Pratt(
        options.into_iter().map(|(prec, rule)| (prec, self.rules.push(Some(rule)))).collect(),
      ),
    }
  }

  pub fn named(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: false, kind: RuleKind::Node(node.into_node(self)) }
  }

  pub fn hidden(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: true, kind: RuleKind::Node(node.into_node(self)) }
  }

  pub fn rule<T: GrammarRule>(&mut self) -> RuleId {
    let next = self.rules.next_index();
    let id = *self.types.entry(TypeId::of::<T>()).or_insert(next);
    if id == next {
      self.rules.push_to(id, None);
      let rule = T::rule(self);
      self.rules[id] = Some(rule);
    }
    id
  }
}

new_idx!(pub RuleId);
new_idx!(pub PrattId);

#[derive_(Debug, Clone)]
pub struct Rule {
  label: String,
  hidden: bool,
  kind: RuleKind,
}

#[derive_(Debug, Clone)]
pub enum RuleKind {
  Node(Node),
  Pratt(Vec<(Prec, RuleId)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
  Rule(RuleId, Option<(Prec, Assoc)>),
  Token(Token),
  Seq(Vec<Node>),
  Choice(Vec<Node>),
  Repeat(Box<Node>),
  Optional(Box<Node>),
  Field(String, Box<Node>),
}

pub trait GrammarRule: Any {
  fn rule(grammar: &mut Grammar) -> Rule;
}

pub trait GrammarPratt: Any {
  fn rules(grammar: &mut Grammar) -> Vec<(Prec, Rule)>;
}

pub trait IntoNode: Sized {
  fn into_node(self, grammar: &mut Grammar) -> Node;

  fn named(self, label: impl Into<String>) -> impl IntoNode {
    struct Named<T>(String, T);
    impl<T: IntoNode> IntoNode for Named<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        let node = self.1.into_node(grammar);
        Node::Rule(
          grammar.rules.push(Some(Rule {
            label: self.0,
            hidden: false,
            kind: RuleKind::Node(node),
          })),
          None,
        )
      }
    }
    Named(label.into(), self)
  }

  fn field(self, label: impl Into<String>) -> impl IntoNode {
    struct Field<T>(String, T);
    impl<T: IntoNode> IntoNode for Field<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Field(self.0, Box::new(self.1.into_node(grammar)))
      }
    }
    Field(label.into(), self)
  }

  fn optional(self) -> impl IntoNode {
    struct Optional<T>(T);
    impl<T: IntoNode> IntoNode for Optional<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Optional(Box::new(self.0.into_node(grammar)))
      }
    }
    Optional(self)
  }

  fn repeat(self) -> impl IntoNode {
    struct Repeat<T>(T);
    impl<T: IntoNode> IntoNode for Repeat<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        Node::Repeat(Box::new(self.0.into_node(grammar)))
      }
    }
    Repeat(self)
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

pub trait IntoNodes: Sized {
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
  impl<T: GrammarRule + 'static> IntoNode for DefineRule<T> {
    fn into_node(self, grammar: &mut Grammar) -> Node {
      Node::Rule(grammar.rule::<T>(), self.1)
    }
  }
  DefineRule::<T>(PhantomData, prec)
}

impl GrammarRule for Ident {
  fn rule(grammar: &mut Grammar) -> Rule {
    todo!()
  }
}

struct Math;
impl GrammarRule for Math {
  fn rule(grammar: &mut Grammar) -> Rule {
    let options = [
      (Prec::Max, grammar.named("underscore", Token::Underscore)),
      (Prec::Max, grammar.named("paren", (Token::OpenParen, rule::<Math>(), Token::CloseParen))),
      (
        Prec::Additive,
        grammar
          .named("add", (left::<Math>(Prec::Additive), Token::Plus, right::<Math>(Prec::Additive))),
      ),
      (
        Prec::Multiplicative,
        grammar.named(
          "mul",
          (left::<Math>(Prec::Multiplicative), Token::Star, right::<Math>(Prec::Multiplicative)),
        ),
      ),
    ];
    grammar.pratt("math", options)
  }
}

#[test]
fn math() {
  let mut grammar = Grammar::default();
  grammar.rule::<Math>();
  dbg!(&grammar);
  grammar.eliminate_pratt();
  dbg!(&grammar);
  println!("{}", grammar.to_treesitter());
}

impl Grammar {
  pub fn eliminate_pratt(&mut self) {
    let mut replacements = HashMap::new();

    for id in self.rules.keys() {
      let mut rule = self.rules[id].take().unwrap();
      if let RuleKind::Pratt(options) = rule.kind {
        let mut levels = BTreeMap::<Prec, Vec<RuleId>>::new();
        for (prec, rule) in options {
          levels.entry(prec).or_default().push(rule);
        }
        let mut last_id = None;
        let levels = levels
          .into_iter()
          .rev()
          .map(|(prec, rules)| {
            let id = self.rules.push(Some(Rule {
              label: format!("{}_{:?}", rule.label, prec),
              hidden: true,
              kind: RuleKind::Node(Node::Choice(
                last_id.into_iter().chain(rules).map(|id| Node::Rule(id, None)).collect(),
              )),
            }));
            last_id = Some(id);
            (prec, id)
          })
          .collect::<Vec<_>>();
        replacements.insert(id, levels);
        rule.kind = RuleKind::Node(Node::Rule(last_id.unwrap(), None));
      }
      self.rules[id] = Some(rule);
    }

    for rule in self.rules.values_mut() {
      if let RuleKind::Node(node) = &mut rule.as_mut().unwrap().kind {
        replace(node, &replacements);
      }
    }

    fn replace(node: &mut Node, replacements: &HashMap<RuleId, Vec<(Prec, RuleId)>>) {
      match node {
        Node::Rule(id, prec) => {
          if let Some((prec, assoc)) = prec.take() {
            *id = replacements[id]
              .iter()
              .rev()
              .find(|&&(other, _)| {
                prec < other || prec == other && assoc != Assoc::None && prec.assoc() == assoc
              })
              .unwrap()
              .1;
          }
        }
        Node::Token(_) => {}
        Node::Seq(nodes) | Node::Choice(nodes) => {
          for node in nodes {
            replace(node, replacements)
          }
        }
        Node::Repeat(node) | Node::Optional(node) | Node::Field(_, node) => {
          replace(node, replacements)
        }
      }
    }
  }

  fn to_treesitter(&self) -> String {
    let mut str = String::new();
    for rule in self.rules.values() {
      let rule = rule.as_ref().unwrap();
      let RuleKind::Node(node) = &rule.kind else { unreachable!() };
      str += &format!(
        "{}{}: $ => {},\n",
        if rule.hidden { "_" } else { "" },
        rule.label,
        self.to_treesitter_node(node)
      );
    }
    str
  }

  fn to_treesitter_node(&self, node: &Node) -> String {
    match node {
      Node::Rule(rule_id, _) => {
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
      Node::Repeat(node) => format!("repeat({})", self.to_treesitter_node(node)),
      Node::Optional(node) => format!("optional({})", self.to_treesitter_node(node)),
      Node::Field(name, node) => format!("field({:?}, {})", name, self.to_treesitter_node(node)),
    }
  }
}

fn pratt_variant<T: GrammarPratt, U>(
  grammar: &mut Grammar,
  variant: fn(T) -> U,
) -> Vec<(Prec, Rule)> {
  T::rules(grammar)
}

impl GrammarRule for Expr {
  fn rule(grammar: &mut Grammar) -> Rule {
    let variants = for_enum!(Kind in ExprKind {
      [$(pratt_variant(grammar, ExprKind::$Kind)),*]
    });
    grammar.pratt("Expr", variants.into_iter().flatten())
  }
}

impl GrammarRule for Ty {
  fn rule(grammar: &mut Grammar) -> Rule {
    let variants = for_enum!(Kind in TyKind {
      [$(pratt_variant(grammar, TyKind::$Kind)),*]
    });
    grammar.pratt("Ty", variants.into_iter().flatten())
  }
}

impl GrammarRule for Pat {
  fn rule(grammar: &mut Grammar) -> Rule {
    let variants = for_enum!(Kind in PatKind {
      [$(pratt_variant(grammar, PatKind::$Kind)),*]
    });
    grammar.pratt("Pat", variants.into_iter().flatten())
  }
}

impl GrammarRule for Impl {
  fn rule(grammar: &mut Grammar) -> Rule {
    let variants = for_enum!(Kind in ImplKind {
      [$(pratt_variant(grammar, ImplKind::$Kind)),*]
    });
    grammar.pratt("Impl", variants.into_iter().flatten())
  }
}

impl GrammarRule for Trait {
  fn rule(grammar: &mut Grammar) -> Rule {
    let variants = for_enum!(Kind in TraitKind {
      [$(pratt_variant(grammar, TraitKind::$Kind)),*]
    });
    grammar.pratt("Trait", variants.into_iter().flatten())
  }
}
