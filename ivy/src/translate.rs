use std::{collections::HashMap, mem::take, rc::Rc};

use vine_util::{idx::Counter, new_idx, register::Register};

use crate::{
  name::{NameId, PathId, Table},
  net::{FlatNet, FlatNode, Wire},
};

pub mod common;

new_idx!(RuleId);

#[allow(clippy::type_complexity)]
pub struct Translator<'a> {
  rule_id: Counter<RuleId>,
  rules: HashMap<PathId, Vec<(RuleId, Rc<dyn Fn(FlatNode, &mut Table, &mut FlatNet) + 'a>)>>,
  free: Vec<(RuleId, Box<dyn Fn(Vec<Wire>, &mut Table, &mut FlatNet) + 'a>)>,
}

impl Default for Translator<'_> {
  fn default() -> Self {
    Self { rule_id: Counter(RuleId(1)), rules: HashMap::new(), free: Vec::new() }
  }
}

impl<'a> Translator<'a> {
  pub fn new(rules: impl Register<Translator<'a>>) -> Self {
    let mut translator = Translator::default();
    translator.register(rules);
    translator
  }

  pub fn register(&mut self, rules: impl Register<Translator<'a>>) {
    rules.register(self);
  }

  fn register_rule(
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

  fn register_free(&mut self, rule: impl Fn(Vec<Wire>, &mut Table, &mut FlatNet) + 'a) {
    let id = self.rule_id.next();
    let rule = Box::new(rule);
    self.free.push((id, rule));
  }

  pub fn translate(&self, table: &mut Table, net: &mut FlatNet) {
    let mut final_nodes = Vec::with_capacity(net.nodes.len());
    for _ in 0..net.nodes.len() {
      self.apply(table, net, &mut final_nodes, RuleId(0));
    }
    for (id, rule) in &self.free {
      rule(take(&mut net.free), table, net);
      for _ in 0..net.nodes.len() {
        self.apply(table, net, &mut final_nodes, *id)
      }
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
    base_rule: RuleId,
  ) {
    let node = net.nodes.pop().unwrap();
    if let Some(rules) = self.rules.get(&node.name.path)
      && let Some((id, rule)) = rules.iter().find(|&&(id, _)| id > base_rule)
    {
      let base_node = net.nodes.len();
      rule(node, table, net);
      for _ in 0..(net.nodes.len() - base_node) {
        self.apply(table, net, final_nodes, *id);
      }
    } else {
      final_nodes.push(node);
    }
  }
}

pub struct Translate<I: IntoIterator<Item = PathId>, F: Fn(FlatNode, &mut Table, &mut FlatNet)>(
  pub I,
  pub F,
);

impl<'a, I: IntoIterator<Item = PathId>, F: Fn(FlatNode, &mut Table, &mut FlatNet) + 'a>
  Register<Translator<'a>> for Translate<I, F>
{
  fn register(self, translator: &mut Translator<'a>) {
    let Translate(paths, f) = self;
    translator.register_rule(paths, f);
  }
}

pub struct TranslateFree<F: Fn(Vec<Wire>, &mut Table, &mut FlatNet)>(pub F);

impl<'a, F: Fn(Vec<Wire>, &mut Table, &mut FlatNet) + 'a> Register<Translator<'a>>
  for TranslateFree<F>
{
  fn register(self, registry: &mut Translator<'a>) {
    registry.register_free(self.0);
  }
}
