use super::*;

use std::{
  any::{Any, TypeId},
  collections::HashMap,
  marker::PhantomData,
};

use crate::component::lexer::Token;

pub struct Grammar {
  types: HashMap<TypeId, RuleId>,
  rules: IdxVec<RuleId, Option<Rule>>,
}

impl Grammar {
  pub fn named(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: false, node: node.into_node(self) }
  }

  pub fn hidden(&mut self, label: impl Into<String>, node: impl IntoNode) -> Rule {
    Rule { label: label.into(), hidden: true, node: node.into_node(self) }
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

pub struct Rule<N = Node> {
  label: String,
  hidden: bool,
  node: N,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
  Rule(RuleId),
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

pub trait IntoNode: Sized {
  fn into_node(self, grammar: &mut Grammar) -> Node;

  fn named(self, label: impl Into<String>) -> impl IntoNode {
    struct Named<T>(String, T);
    impl<T: IntoNode> IntoNode for Named<T> {
      fn into_node(self, grammar: &mut Grammar) -> Node {
        let node = self.1.into_node(grammar);
        Node::Rule(grammar.rules.push(Some(Rule { label: self.0, hidden: false, node })))
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
  struct DefineRule<T>(PhantomData<T>);
  impl<T: GrammarRule + 'static> IntoNode for DefineRule<T> {
    fn into_node(self, grammar: &mut Grammar) -> Node {
      Node::Rule(grammar.rule::<T>())
    }
  }
  DefineRule::<T>(PhantomData)
}

impl GrammarRule for Ident {
  fn rule(grammar: &mut Grammar) -> Rule {
    todo!()
  }
}
