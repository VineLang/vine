use std::{collections::BTreeMap, mem::take};

use ivy::ast::{Net, Tree};

use super::{
  stage_name, Compiler, Interface, InterfaceId, Local, Stage, StageId, Step, VarGen, WireDir,
};

impl Compiler {
  pub(super) fn finish_nets(&mut self, init: StageId) {
    self.nets.reserve(self.stages.len());
    for (id, stage) in self.stages.drain(..).enumerate() {
      let name = if id == init { self.name.clone() } else { stage_name(&self.name, id) };
      let net = FinishNets::new(&self.interfaces).finish_stage(stage);
      self.nets.insert(name, net);
    }
    self.interfaces.clear();
  }
}

#[derive(Debug, Default)]
struct FinishNets<'a> {
  interfaces: &'a [Interface],
  locals: BTreeMap<Local, LocalState>,
  pairs: Vec<(Tree, Tree)>,
  var: VarGen,
}

#[derive(Debug, Default)]
struct LocalState {
  value: Option<Tree>,
  uses: Vec<Tree>,
  end: Option<Tree>,
}

impl<'a> FinishNets<'a> {
  fn new(interfaces: &'a [Interface]) -> Self {
    FinishNets {
      interfaces,
      locals: Default::default(),
      pairs: Default::default(),
      var: Default::default(),
    }
  }

  fn finish_stage(mut self, stage: Stage) -> Net {
    self.pairs = stage.pairs;
    self.var = stage.var;
    let root = self.interface(stage.outer, true);
    for step in stage.steps {
      match step {
        Step::Get(l, t) => self.get_local(l, t),
        Step::Set(l, t) => self.set_local(l, t),
        Step::Move(l, t) => self.move_local(l, t),
        Step::Call(i, t) => {
          let i = self.interface(i, false);
          self.pairs.push((i, t));
        }
      }
    }
    for (_, mut l) in self.locals.into_iter() {
      if l.end.is_some() || l.value.is_some() {
        l.uses.extend(l.end);
        self.pairs.push((l.value.unwrap_or(Tree::Erase), Tree::n_ary("dup", l.uses.into_iter())));
      }
    }
    Net { root, pairs: take(&mut self.pairs) }
  }

  fn interface(&mut self, interface: InterfaceId, enter: bool) -> Tree {
    Tree::n_ary(
      "x",
      self.interfaces[interface].wires.iter().map(|&(local, inw)| {
        let v = self.var.gen();
        match (enter, inw) {
          (true, WireDir::Input) => self.locals.entry(local).or_default().value = Some(v.0),
          (true, WireDir::Output) => self.locals.entry(local).or_default().end = Some(v.0),
          (false, WireDir::Input) => self.get_local(local, v.0),
          (false, WireDir::Output) => self.set_local(local, v.0),
        }
        v.1
      }),
    )
  }

  fn get_local(&mut self, local: Local, tree: Tree) {
    let local = self.locals.entry(local).or_default();
    if local.value.is_some() {
      local.uses.push(tree);
    } else if !matches!(tree, Tree::Erase) {
      self.pairs.push((Tree::Erase, tree));
    }
  }

  fn set_local(&mut self, local: Local, tree: Tree) {
    let local = self.locals.entry(local).or_default();
    let prev = local.value.replace(tree);
    if let Some(value) = prev {
      self.pairs.push((value, Tree::n_ary("dup", local.uses.drain(..))));
    }
    debug_assert!(local.uses.is_empty());
  }

  fn move_local(&mut self, local: Local, tree: Tree) {
    let local = self.locals.entry(local).or_default();
    if let Some(value) = local.value.take() {
      self.pairs.push((value, Tree::n_ary("dup", local.uses.drain(..).chain([tree]))));
    } else if !matches!(tree, Tree::Erase) {
      self.pairs.push((Tree::Erase, tree));
    }
    debug_assert!(local.uses.is_empty());
  }
}
