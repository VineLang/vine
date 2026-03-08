use std::{collections::HashMap, rc::Rc};

use vine_util::{idx::Counter, new_idx};

use crate::{
  name::{NameId, PathId, Table},
  net::{FlatNet, FlatNode},
};

pub mod common;

new_idx!(RuleId);

#[derive(Default)]
pub struct Translator<'a> {
  rule_id: Counter<RuleId>,
  #[allow(clippy::type_complexity)]
  rules: HashMap<PathId, Vec<(RuleId, Rc<dyn Fn(FlatNode, &mut Table, &mut FlatNet) + 'a>)>>,
}

impl<'a> Translator<'a> {
  pub fn new(rules: impl Rules<'a>) -> Self {
    let mut translator = Translator::default();
    translator.register(rules);
    translator
  }

  pub fn register(&mut self, rules: impl Rules<'a>) {
    rules.register_all(self);
  }

  pub fn register_one(
    &mut self,
    paths: impl IntoIterator<Item = PathId>,
    rule: impl Fn(FlatNode, &mut Table, &mut FlatNet) + 'a,
  ) {
    let id = self.rule_id.next();
    let rule = Rc::new(rule);
    for path in paths {
      self.rules.entry(path).or_default().push((id, rule.clone()));
    }
  }

  pub fn translate(&self, table: &mut Table, net: &mut FlatNet) {
    let mut final_nodes = Vec::with_capacity(net.nodes.len());
    for _ in 0..net.nodes.len() {
      self.apply(table, net, &mut final_nodes, RuleId(0));
    }
    assert!(net.nodes.is_empty());
    net.nodes = final_nodes;
    net.nodes.reverse();
  }

  pub fn translate_all(&self, table: &mut Table, nets: &mut HashMap<NameId, FlatNet>) {
    nets.values_mut().for_each(|net| self.translate(table, net));
  }

  fn apply(
    &self,
    table: &mut Table,
    net: &mut FlatNet,
    final_nodes: &mut Vec<FlatNode>,
    base: RuleId,
  ) {
    let node = net.nodes.pop().unwrap();
    if let Some(rules) = self.rules.get(&node.name.path)
      && let Some((id, rule)) = rules.iter().find(|&&(id, _)| id >= base)
    {
      let base = net.nodes.len();
      rule(node, table, net);
      for _ in 0..(net.nodes.len() - base) {
        self.apply(table, net, final_nodes, RuleId(id.0 + 1));
      }
    } else {
      final_nodes.push(node);
    }
  }
}

pub struct Rule<I: IntoIterator<Item = PathId>, F: Fn(FlatNode, &mut Table, &mut FlatNet)>(
  pub I,
  pub F,
);

impl<'a, I: IntoIterator<Item = PathId>, F: Fn(FlatNode, &mut Table, &mut FlatNet) + 'a> Rules<'a>
  for Rule<I, F>
{
  fn register_all(self, translator: &mut Translator<'a>) {
    let Rule(paths, f) = self;
    translator.register_one(paths, f);
  }
}

pub trait Rules<'a>: Sized {
  fn register_all(self, translator: &mut Translator<'a>);
}

macro_rules! impl_tuple {
  ($($T:ident)*) => {
    #[allow(warnings)]
    impl<'a, $($T: Rules<'a>,)*> Rules<'a> for ($($T,)*) {
      fn register_all(self, translator: &mut Translator<'a>) {
        let ($($T,)*) = self;
        $(translator.register($T);)*
      }
    }
  };
}

impl_tuple!();
impl_tuple!(A);
impl_tuple!(A B);
impl_tuple!(A B C);
impl_tuple!(A B C D);
impl_tuple!(A B C D E);
impl_tuple!(A B C D E F);
impl_tuple!(A B C D E F G);
impl_tuple!(A B C D E F G H);
impl_tuple!(A B C D E F G H I);
impl_tuple!(A B C D E F G H I J);
impl_tuple!(A B C D E F G H I J K);
impl_tuple!(A B C D E F G H I J K L);
impl_tuple!(A B C D E F G H I J K L M);
impl_tuple!(A B C D E F G H I J K L M N);
impl_tuple!(A B C D E F G H I J K L M N O);
