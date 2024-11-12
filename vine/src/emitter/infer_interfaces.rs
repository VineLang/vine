use std::{collections::BTreeMap, mem::take};

use vine_util::{
  bicycle::{Bicycle, BicycleState},
  idx::{IdxVec, RangeExt},
};

use super::{Emitter, Interface, InterfaceId, Local, Port, Stage, StageId, Step, Usage, WireDir};

impl<'core> Emitter<'core, '_> {
  pub(super) fn infer_interfaces(&mut self) {
    let all = self.interfaces.range();
    FlowOut { stages: &self.stages, interfaces: &mut self.interfaces }.visit_all(all.iter());
    FlowIn { stages: &self.stages, interfaces: &mut self.interfaces }.visit_all(all.iter());
    for i in all.iter() {
      self.interfaces[i].calc_wires();
    }
  }
}

struct FlowOut<'a> {
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a mut IdxVec<InterfaceId, Interface>,
}

impl<'a> Bicycle for FlowOut<'a> {
  type Node = InterfaceId;

  fn state(&mut self, node: Self::Node) -> &BicycleState {
    &self.interfaces[node].state
  }

  fn visit(&mut self, node: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    let mut staging = BTreeMap::<Local, Usage>::new();
    let mut references = BTreeMap::<Local, usize>::new();

    let stages = take(&mut self.interfaces[node].stages);
    for &stage in &stages {
      let stage = &self.stages[stage];
      for step in &stage.steps {
        if let Step::Call(i, _) = *step {
          recurse(self, i);
        }
        step.usage(self.interfaces, |l, u| staging.entry(l).or_insert(Usage::NONE).append(u));
      }

      let interface = &mut self.interfaces[node];
      for (&l, &u) in &staging {
        if !interface.declarations.contains(&l) {
          interface.outward.entry(l).or_insert(u).union_with(u);
          *references.entry(l).or_default() += 1;
        }
      }
      staging.clear();
    }

    let out = &mut self.interfaces[node].outward;
    for (l, r) in references {
      if r != stages.len() {
        out.get_mut(&l).unwrap().union_with(Usage::NONE);
      }
    }

    self.interfaces[node].stages = stages;
  }
}

struct FlowIn<'a> {
  stages: &'a IdxVec<StageId, Stage>,
  interfaces: &'a mut IdxVec<InterfaceId, Interface>,
}

impl<'a> Bicycle for FlowIn<'a> {
  type Node = InterfaceId;

  fn state(&mut self, node: Self::Node) -> &BicycleState {
    &self.interfaces[node].state
  }

  fn visit(&mut self, node: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    let mut staging = BTreeMap::<Local, Usage>::new();

    let parents = take(&mut self.interfaces[node].parents);
    for &d in &parents {
      recurse(self, d)
    }
    self.interfaces[node].parents = parents;

    let stages = take(&mut self.interfaces[node].stages);
    for &stage in &stages {
      staging.clear();
      staging.extend(&self.interfaces[node].inward);
      let stage = &self.stages[stage];
      for step in stage.steps.iter() {
        if let Step::Call(i, _) = *step {
          let inner = &mut self.interfaces[i];
          for (&l, &u) in &staging {
            let u = u.forward();
            if !u.is_empty() {
              inner.inward.entry(l).or_insert(u).union_with(u);
            }
          }
        }
        step.usage(self.interfaces, |l, u| staging.entry(l).or_insert(Usage::NONE).append(u));
      }
      staging.clear();
      staging.extend(&self.interfaces[node].inward);
      for step in stage.steps.iter().rev() {
        if let Step::Call(i, _) = *step {
          let inner = &mut self.interfaces[i];
          for (&l, &u) in &staging {
            let u = u.backward();
            if !u.is_empty() {
              inner.inward.entry(l).or_insert(u).union_with(u);
            }
          }
        }
        step.usage(self.interfaces, |l, u| staging.entry(l).or_insert(Usage::NONE).prepend(u));
      }
    }
    self.interfaces[node].stages = stages;
  }
}

impl Interface {
  fn calc_wires(&mut self) {
    for (local, outward) in &self.outward {
      let inward = self.inward.get(local).copied().unwrap_or(Usage::NONE);
      let output =
        inward.may_get() & outward.may_set() & (outward.may_not_set() | outward.may_set_some());
      let input = inward.may_set()
        & inward.may_set_some()
        & (outward.may_get() | outward.may_not_set() & output);
      if input {
        self.wires.insert((*local, WireDir::Input));
      }
      if output {
        self.wires.insert((*local, WireDir::Output));
      }
    }
  }
}

impl Step {
  fn usage(&self, interfaces: &IdxVec<InterfaceId, Interface>, mut add: impl FnMut(Local, Usage)) {
    match *self {
      Step::Get(l, _) => add(l, Usage::GET),
      Step::Set(l, Port::Erase) => add(l, Usage::ERASE),
      Step::Set(l, _) => add(l, Usage::SET),
      Step::Move(l, _) => add(l, Usage::MOVE),
      Step::Call(i, _) => {
        for (&l, &u) in &interfaces[i].outward {
          add(l, u);
        }
      }
    }
  }
}
