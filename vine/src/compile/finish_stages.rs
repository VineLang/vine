use std::{
  collections::BTreeMap,
  mem::{replace, take},
};

use super::{
  stage_name, Agent, Compiler, Interface, InterfaceId, Local, NetBuilder, Port, Stage, StageId,
  Step, WireDir,
};

impl Compiler<'_> {
  pub(super) fn finish_stages(&mut self, init: StageId) {
    self.nets.reserve(self.stages.len());
    for (id, stage) in self.stages.drain(..).enumerate() {
      let name = if id == init { self.name.clone() } else { stage_name(&self.name, id) };
      let mut finish = FinishStage {
        interfaces: &self.interfaces,
        net: &mut self.net,
        locals: Default::default(),
      };
      let root = finish.finish_stage(stage);
      let net = finish.net.finish(root);
      self.nets.insert(name, net);
    }
    self.interfaces.clear();
  }
}

#[derive(Debug)]
struct FinishStage<'a> {
  interfaces: &'a [Interface],
  net: &'a mut NetBuilder,
  locals: BTreeMap<Local, LocalState>,
}

#[derive(Debug, Default)]
struct LocalState {
  value: Port,
  uses: Vec<Port>,
  end: Option<Port>,
}

impl<'a> FinishStage<'a> {
  fn finish_stage(&mut self, stage: Stage) -> Port {
    assert!(self.net.agents.is_empty());
    self.net.agents = stage.agents;

    let mut root = self.interface(stage.outer, true);

    if let Some((new_root, old_root)) = stage.header {
      self.net.link(old_root, root);
      root = new_root;
    }

    for step in stage.steps {
      match step {
        Step::Get(l, t) => self.get_local(l, t),
        Step::Set(l, t) => self.set_local(l, t),
        Step::Move(l, t) => self.move_local(l, t),
        Step::Call(i, t) => {
          let i = self.interface(i, false);
          self.net.link(i, t);
        }
      }
    }

    for (_, mut l) in take(&mut self.locals).into_iter() {
      if l.end.is_some() || !l.value.can_copy() {
        l.uses.extend(l.end);
        self.dup_tree(l.value, l.uses);
      }
    }

    root
  }

  fn interface(&mut self, interface: InterfaceId, enter: bool) -> Port {
    let root = self.net.new_wire();
    let mut cursor = root.0;
    let mut cur = None;
    for &(local, dir) in self.interfaces[interface].wires.iter() {
      let next = {
        let v = self.net.new_wire();
        match (enter, dir) {
          (true, WireDir::Input) => self.locals.entry(local).or_default().value = v.0,
          (true, WireDir::Output) => self.locals.entry(local).or_default().end = Some(v.0),
          (false, WireDir::Input) => self.get_local(local, v.0),
          (false, WireDir::Output) => self.set_local(local, v.0),
        }
        v.1
      };
      if let Some(cur) = cur.replace(next) {
        let r = self.net.new_wire();
        self.net.agents.push(Agent::Comb("x".into(), cursor, cur, r.0));
        cursor = r.1;
      }
    }
    self.net.link(cur.unwrap_or(Port::Erase), cursor);
    root.1
  }

  fn get_local(&mut self, local: Local, port: Port) {
    let port = self.net.follow(port);
    let local = self.locals.entry(local).or_default();
    if !port.is_erase() {
      if local.value.can_copy() {
        self.net.link(port, local.value.clone());
      } else {
        local.uses.push(port);
      }
    }
  }

  fn set_local(&mut self, local: Local, port: Port) {
    let local = self.locals.entry(local).or_default();
    let prev = replace(&mut local.value, port);
    let uses = take(&mut local.uses);
    self.dup_tree(prev, uses);
  }

  fn move_local(&mut self, local: Local, port: Port) {
    self.get_local(local, port);
    self.set_local(local, Port::Erase);
  }

  fn dup_tree(&mut self, mut value: Port, uses: Vec<Port>) {
    let last = uses
      .into_iter()
      .reduce(|cur, next| {
        let r = self.net.new_wire();
        let prev = replace(&mut value, r.1);
        self.net.agents.push(Agent::Comb("dup".into(), prev, cur, r.0));
        next
      })
      .unwrap_or(Port::Erase);
    self.net.link(last, value);
  }
}
