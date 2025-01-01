use std::{
  collections::{BTreeSet, HashMap},
  mem::take,
};

use usage::Usage;
use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::Local,
  vir::{Interface, InterfaceId, Invocation, Stage, StageId, Step, VIR},
};

pub mod usage;

pub fn analyze(vir: &mut VIR) {
  Analyzer {
    globals: &vir.globals,
    stages: &vir.stages,
    interfaces: &mut vir.interfaces,
    usage: IdxVec::from([Usage::None]),
    effects: IdxVec::from([Vec::new()]),
    relations: IdxVec::new(),
    segments: Vec::new(),
    dirty: BTreeSet::new(),
    locals: Vec::new(),
    disconnects: Vec::new(),
  }
  .analyze();
}

#[derive(Debug)]
struct Analyzer<'a> {
  globals: &'a Vec<(Local, Usage)>,
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a mut IdxVec<InterfaceId, Interface>,

  usage: IdxVec<UsageVar, Usage>,
  effects: IdxVec<UsageVar, Vec<RelationId>>,
  relations: IdxVec<RelationId, (UsageVar, UsageVar, UsageVar, UsageVar)>,
  segments: Vec<(UsageVar, HashMap<Local, Usage>)>,
  dirty: BTreeSet<UsageVar>,
  locals: Vec<(Local, StageId)>,
  disconnects: Vec<(StageId, UsageVar, UsageVar)>,
}

new_idx!(pub UsageVar; n => ["u{n}"]);
new_idx!(pub RelationId; n => ["R{n}"]);

impl UsageVar {
  pub const NONE: Self = UsageVar(0);
}

impl Analyzer<'_> {
  fn analyze(&mut self) {
    self.sweep(InterfaceId(0));

    self.locals.extend(self.globals.iter().map(|&(g, _)| (g, StageId(usize::MAX))));
    let root_segment =
      (self.get_transfer(InterfaceId(0)).1, self.globals.iter().copied().collect());
    self.segments.push(root_segment);

    self.build();

    for (local, stage) in take(&mut self.locals) {
      self.process_local(local, stage);
    }

    for &(global, usage) in self.globals {
      self.interfaces[InterfaceId(0)].wires.insert(global, (Usage::Zero, Usage::Mut.effect(usage)));
    }
  }

  fn sweep(&mut self, interface_id: InterfaceId) {
    let interface = &mut self.interfaces[interface_id];
    if interface.incoming != 0 {
      interface.incoming += 1;
      return;
    }
    interface.incoming = 1;
    for i in 0..interface.kind.stages().len() {
      let stage = &self.stages[self.interfaces[interface_id].kind.stages()[i]];
      for step in &stage.steps {
        if let Step::Transfer(transfer) = step {
          self.sweep(transfer.interface);
        }
      }
    }
  }

  fn build(&mut self) {
    let mut segments = Vec::new();
    let mut transfers = Vec::new();
    let mut forwards = Vec::new();
    let mut backwards = Vec::new();

    for stage in self.stages.values() {
      if self.interfaces[stage.interface].incoming == 0 {
        continue;
      }

      segments.clear();
      transfers.clear();
      forwards.clear();
      backwards.clear();
      self.locals.extend(stage.declarations.iter().map(|&l| (l, stage.id)));

      let incoming = self.get_transfer(stage.interface);
      if stage.declarations.is_empty() {
        transfers.push((incoming.1, incoming.0));
      } else {
        let connected = self.new_var();
        let disconnected = self.new_var();
        let inner = (self.new_var(), self.new_var());
        self.new_relation(incoming.1, connected, UsageVar::NONE, inner.0);
        self.new_relation(inner.1, connected, UsageVar::NONE, incoming.0);
        self.new_relation(disconnected, UsageVar::NONE, UsageVar::NONE, inner.0);
        self.new_relation(disconnected, UsageVar::NONE, UsageVar::NONE, incoming.0);
        self.disconnects.push((stage.id, connected, disconnected));
        transfers.push(inner);
      }

      let mut current_segment = HashMap::new();

      for step in &stage.steps {
        if let Step::Invoke(local, invocation) = step {
          let usage = current_segment.entry(*local).or_insert(Usage::None);
          *usage = usage.join(invocation.usage());
        } else if let Step::Transfer(transfer) = step {
          if !current_segment.is_empty() {
            let var = self.new_var();
            self.segments.push((var, take(&mut current_segment)));
            segments.push(var);
          } else {
            segments.push(UsageVar::NONE);
          }
          let transfer = self.get_transfer(transfer.interface);
          transfers.push(transfer);
        }
      }

      if !current_segment.is_empty() {
        let var = self.new_var();
        self.segments.push((var, current_segment));
        segments.push(var);
      } else {
        segments.push(UsageVar::NONE);
      }

      debug_assert_eq!(transfers.len(), segments.len());
      let n = segments.len();

      let mut forward = UsageVar::NONE;
      forwards.reserve(n);
      forwards.push(forward);
      for i in 0..(n - 1) {
        let var = self.new_var();
        self.new_relation(forward, transfers[i].0, segments[i], var);
        forward = var;
        forwards.push(forward);
      }

      let mut backward = segments[n - 1];
      backwards.reserve(n);
      backwards.push(backward);
      for i in (0..(n - 1)).rev() {
        let var = self.new_var();
        self.new_relation(segments[i], transfers[i + 1].0, backward, var);
        backward = var;
        backwards.push(backward);
      }

      for ((&forward, &transfer), &backward) in
        forwards.iter().zip(transfers.iter()).zip(backwards.iter().rev())
      {
        self.new_relation(backward, UsageVar::NONE, forward, transfer.1);
      }
    }
  }

  fn process_local(&mut self, local: Local, declared: StageId) {
    self.usage.fill(Usage::Zero);
    self.usage[UsageVar::NONE] = Usage::None;
    self.dirty.insert(UsageVar::NONE);
    for (var, usage) in &mut self.segments {
      let usage = usage.remove(&local).unwrap_or(Usage::None);
      self.usage[*var] = usage;
      self.dirty.insert(*var);
    }
    for &(stage, connected, disconnected) in &self.disconnects {
      let var = if stage == declared { disconnected } else { connected };
      self.usage[var] = Usage::None;
      self.dirty.insert(var);
    }
    while let Some(var) = self.dirty.pop_first() {
      for &relation in &self.effects[var] {
        let (a, b, c, out) = self.relations[relation];
        let abc = self.usage[a].join(self.usage[b]).join(self.usage[c]);
        let old = self.usage[out];
        let new = old.union(abc);
        if new != old {
          self.usage[out] = new;
          self.dirty.insert(out);
        }
      }
    }
    for interface in self.interfaces.values_mut() {
      if interface.incoming != 0 {
        let interior = self.usage[interface.interior.unwrap()];
        let exterior = self.usage[interface.exterior.unwrap()];
        if interior == Usage::Zero || exterior == Usage::Zero {
          panic!("infinite loop")
        }
        if interior != Usage::None && exterior != Usage::None {
          assert!(matches!(exterior, Usage::Mut | Usage::Erase | Usage::Set | Usage::Take));
          interface.wires.insert(local, (exterior.effect(interior), interior.effect(exterior)));
        }
      }
    }
  }

  fn new_relation(&mut self, a: UsageVar, b: UsageVar, c: UsageVar, o: UsageVar) {
    let relation = self.relations.push((a, b, c, o));
    self.effects[a].push(relation);
    self.effects[b].push(relation);
    self.effects[c].push(relation);
  }

  fn new_var(&mut self) -> UsageVar {
    self.effects.push(Vec::new());
    self.usage.push(Usage::Zero)
  }

  fn get_transfer(&mut self, interface: InterfaceId) -> (UsageVar, UsageVar) {
    let mut interfaces = take(self.interfaces);
    let interface = &mut interfaces[interface];
    let transfer = (
      *interface.interior.get_or_insert_with(|| self.new_var()),
      *interface.exterior.get_or_insert_with(|| self.new_var()),
    );
    *self.interfaces = interfaces;
    transfer
  }
}

impl Invocation {
  fn usage(&self) -> Usage {
    match self {
      Invocation::Erase => Usage::Erase,
      Invocation::Get(_) => Usage::Get,
      Invocation::Hedge(_) => Usage::Hedge,
      Invocation::Take(_) => Usage::Take,
      Invocation::Set(_) => Usage::Set,
      Invocation::Mut(..) => Usage::Mut,
    }
  }
}
