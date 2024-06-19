use std::{collections::BTreeMap, mem::take};

use vine_util::bicycle::{Bicycle, BicycleState};

use super::{Compiler, Interface, InterfaceId, Local, Stage, Step, Usage, WireDir};

impl Compiler {
  pub(super) fn infer_interfaces(&mut self) {
    let all = 0..self.interfaces.len();
    FlowOut { stages: &self.stages, interfaces: &mut self.interfaces }.visit_all(all.clone());
    FlowIn { stages: &self.stages, interfaces: &mut self.interfaces }.visit_all(all.clone());
    for i in all {
      self.interfaces[i].calc_wires();
    }
  }
}

struct FlowOut<'a> {
  stages: &'a Vec<Stage>,
  interfaces: &'a mut Vec<Interface>,
}

impl<'a> Bicycle for FlowOut<'a> {
  type Node = InterfaceId;

  fn state(&mut self, node: Self::Node) -> &BicycleState {
    &self.interfaces[node].state
  }

  fn visit(&mut self, node: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    let mut staging = BTreeMap::<Local, Usage>::new();

    let stages = take(&mut self.interfaces[node].stages);
    for &stage in &stages {
      let stage = &self.stages[stage];
      for step in &stage.steps {
        match *step {
          Step::Get(l, _) => staging.entry(l).or_default().append(Usage::Get),
          Step::Set(l, _) => staging.entry(l).or_default().append(Usage::Set),
          Step::Call(i, _) => {
            recurse(self, i);
            for (&l, &u) in &self.interfaces[i].outward {
              staging.entry(l).or_default().append(u);
            }
          }
        }
      }

      let out = &mut self.interfaces[node].outward;
      for (&l, &u) in &staging {
        out.entry(l).or_default().union(u);
      }
      staging.clear();
    }
    self.interfaces[node].stages = stages;
  }
}

struct FlowIn<'a> {
  stages: &'a Vec<Stage>,
  interfaces: &'a mut Vec<Interface>,
}

impl<'a> Bicycle for FlowIn<'a> {
  type Node = InterfaceId;

  fn state(&mut self, node: Self::Node) -> &BicycleState {
    &self.interfaces[node].state
  }

  fn visit(&mut self, node: Self::Node, mut recurse: impl FnMut(&mut Self, Self::Node)) {
    let mut staging = BTreeMap::new();

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
        match *step {
          Step::Get(l, _) => staging.entry(l).or_default().append(Usage::Get),
          Step::Set(l, _) => staging.entry(l).or_default().append(Usage::Set),
          Step::Call(i, _) => {
            let inw = &mut self.interfaces[i].inward;
            for (&l, &u) in &staging {
              let u = u.forward();
              if u != Usage::None {
                inw.entry(l).or_default().union(u);
              }
            }
          }
        }
      }
      staging.clear();
      staging.extend(&self.interfaces[node].inward);
      for step in stage.steps.iter().rev() {
        match *step {
          Step::Get(l, _) => staging.entry(l).or_default().prepend(Usage::Get),
          Step::Set(l, _) => staging.entry(l).or_default().prepend(Usage::Set),
          Step::Call(i, _) => {
            let inw = &mut self.interfaces[i].inward;
            for (&l, &u) in &staging {
              let u = u.backward();
              if u != Usage::None {
                inw.entry(l).or_default().union(u);
              }
            }
          }
        }
      }
    }
    self.interfaces[node].stages = stages;
  }
}

impl Interface {
  fn calc_wires(&mut self) {
    for (local, outward) in &self.outward {
      let inward = self.inward.get(local).copied().unwrap_or_default();
      if inward.forward() == Usage::Set && outward.backward() == Usage::Get {
        self.wires.insert((*local, WireDir::Input));
      }
      if outward.forward() == Usage::Set && inward.backward() == Usage::Get {
        self.wires.insert((*local, WireDir::Output));
      }
    }
  }
}
