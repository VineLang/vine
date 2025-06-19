use std::{
  collections::{BTreeSet, HashMap},
  mem::take,
};

use vine_util::{
  idx::{IdxVec, IntMap},
  new_idx,
};

use crate::{
  components::analyzer::effect::Effect,
  structures::{
    ast::Span,
    core::Core,
    diag::Diag,
    tir::Local,
    types::Inverted,
    vir::{Interface, InterfaceId, Invocation, Stage, StageId, Step, Vir, VirLocal},
  },
};

pub mod effect;

pub fn analyze<'core>(core: &'core Core<'core>, span: Span, vir: &mut Vir) {
  Analyzer {
    core,
    infinite_loop: false,
    span,
    locals: &vir.locals,
    globals: &vir.globals,
    stages: &vir.stages,
    interfaces: &mut vir.interfaces,
    effects: IdxVec::from([Effect::P]),
    dependent: IdxVec::from([Vec::new()]),
    relations: IdxVec::new(),
    segments: Vec::new(),
    dirty: BTreeSet::new(),
    local_declarations: IntMap::default(),
    disconnects: Vec::new(),
  }
  .analyze();
}

#[derive(Debug)]
struct Analyzer<'core, 'a> {
  core: &'core Core<'core>,
  infinite_loop: bool,
  span: Span,

  locals: &'a IdxVec<Local, VirLocal>,
  globals: &'a Vec<(Local, Effect)>,
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a mut IdxVec<InterfaceId, Interface>,

  effects: IdxVec<EffectVar, Effect>,
  dependent: IdxVec<EffectVar, Vec<RelationId>>,
  relations: IdxVec<RelationId, (EffectVar, EffectVar, EffectVar, EffectVar)>,
  segments: Vec<(EffectVar, HashMap<Local, Effect>)>,
  dirty: BTreeSet<EffectVar>,
  local_declarations: IntMap<Local, Vec<StageId>>,
  disconnects: Vec<(StageId, EffectVar, EffectVar)>,
}

new_idx!(pub EffectVar; n => ["e{n}"]);
new_idx!(pub RelationId; n => ["R{n}"]);

impl EffectVar {
  pub const P: Self = EffectVar(0);
}

impl<'core> Analyzer<'core, '_> {
  fn analyze(&mut self) {
    self.sweep(InterfaceId(0));

    self.local_declarations.extend(self.globals.iter().map(|&(g, _)| (g, Vec::new())));
    let root_segment =
      (self.get_transfer(InterfaceId(0)).1, self.globals.iter().copied().collect());
    self.segments.push(root_segment);

    self.build();

    for (local, stages) in take(&mut self.local_declarations) {
      self.process_local(local, stages);
    }
  }

  fn sweep(&mut self, interface_id: InterfaceId) {
    let interface = &mut self.interfaces[interface_id];
    if interface.incoming != 0 {
      interface.incoming += 1;
      return;
    }
    interface.incoming = 1;
    for stage_id in interface.kind.stages() {
      let stage = &self.stages[stage_id];
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

      for &local in &stage.declarations {
        self.local_declarations.entry(local).or_default().push(stage.id);
      }

      let incoming = self.get_transfer(stage.interface);
      if stage.declarations.is_empty() {
        transfers.push((incoming.1, incoming.0));
      } else {
        let connected = self.new_var();
        let disconnected = self.new_var();
        let inner = (self.new_var(), self.new_var());
        self.new_relation(incoming.1, connected, EffectVar::P, inner.0);
        self.new_relation(inner.1, connected, EffectVar::P, incoming.0);
        self.new_relation(disconnected, EffectVar::P, EffectVar::P, inner.0);
        self.new_relation(disconnected, EffectVar::P, EffectVar::P, incoming.0);
        self.disconnects.push((stage.id, connected, disconnected));
        transfers.push(inner);
      }

      let mut current_segment = HashMap::new();

      for step in &stage.steps {
        if let Step::Invoke(local, invocation) = step {
          let effect = current_segment.entry(*local).or_insert(Effect::P);
          let local = &self.locals[*local];
          let other = invocation.effect(local.inv, local.fork.is_some());
          *effect = if local.inv.0 { other.join(*effect) } else { effect.join(other) };
        } else if let Step::Transfer(transfer) = step {
          if !current_segment.is_empty() {
            let var = self.new_var();
            self.segments.push((var, take(&mut current_segment)));
            segments.push(var);
          } else {
            segments.push(EffectVar::P);
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
        segments.push(EffectVar::P);
      }

      debug_assert_eq!(transfers.len(), segments.len());
      let n = segments.len();

      let mut forward = EffectVar::P;
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
        self.new_relation(backward, EffectVar::P, forward, transfer.1);
      }
    }
  }

  fn process_local(&mut self, local: Local, declared: Vec<StageId>) {
    let inv = self.locals[local].inv;
    self.effects.fill(Effect::X);
    self.effects[EffectVar::P] = Effect::P;
    self.dirty.insert(EffectVar::P);
    for (var, effects) in &mut self.segments {
      let effect = effects.remove(&local).unwrap_or(Effect::P);
      self.effects[*var] = effect;
      self.dirty.insert(*var);
    }
    for &(stage, connected, disconnected) in &self.disconnects {
      let var = if declared.contains(&stage) { disconnected } else { connected };
      self.effects[var] = Effect::P;
      self.dirty.insert(var);
    }
    while let Some(var) = self.dirty.pop_first() {
      for &relation in &self.dependent[var] {
        let (a, b, c, out) = self.relations[relation];
        let (a, b, c) = (self.effects[a], self.effects[b], self.effects[c]);
        let abc = if inv.0 { c.join(b).join(a) } else { a.join(b).join(c) };
        let old = self.effects[out];
        let new = old.union(abc);
        if new != old {
          self.effects[out] = new;
          self.dirty.insert(out);
        }
      }
    }
    for interface in self.interfaces.values_mut() {
      if interface.incoming != 0 {
        let interior = self.effects[interface.interior.unwrap()];
        let exterior = self.effects[interface.exterior.unwrap()];
        if interior == Effect::X || exterior == Effect::X {
          if !self.infinite_loop {
            self.core.report(Diag::InfiniteLoop { span: self.span });
            self.infinite_loop = true;
          }
          continue;
        }
        if interior != Effect::P && exterior != Effect::P {
          assert!(!exterior.pass());
          let bar = interior.bar();
          let output = interior.write() && exterior.read();
          let input = exterior.write() && (interior.read() || output && interior.pass());
          let wire = if inv.0 { (output, bar, input) } else { (input, bar, output) };
          interface.wires.insert(local, wire);
        }
      }
    }
  }

  fn new_relation(&mut self, a: EffectVar, b: EffectVar, c: EffectVar, o: EffectVar) {
    let relation = self.relations.push((a, b, c, o));
    self.dependent[a].push(relation);
    self.dependent[b].push(relation);
    self.dependent[c].push(relation);
  }

  fn new_var(&mut self) -> EffectVar {
    self.dependent.push(Vec::new());
    self.effects.push(Effect::X)
  }

  fn get_transfer(&mut self, interface: InterfaceId) -> (EffectVar, EffectVar) {
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
  fn effect(&self, inv: Inverted, fork: bool) -> Effect {
    match (self, inv.0) {
      (Invocation::Bar, _) => Effect::B,
      (Invocation::Read(_), false) | (Invocation::Write(_), true) if fork => Effect::RP,
      (Invocation::Read(_) | Invocation::ReadBar(_), false)
      | (Invocation::Write(_) | Invocation::BarWrite(_), true) => Effect::RB,
      (Invocation::Write(_) | Invocation::BarWrite(_), false)
      | (Invocation::Read(_) | Invocation::ReadBar(_), true) => Effect::BW,
      (Invocation::ReadWrite(..), _) => Effect::RBW,
    }
  }
}
