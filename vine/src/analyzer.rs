use std::{collections::HashMap, mem::take};

use usage::Usage;
use vine_util::idx::IdxVec;

use crate::{
  ast::Local,
  vir::{Interface, InterfaceId, Invocation, Stage, StageId, Step, VIR},
};

pub mod usage;

pub fn analyze(vir: &mut VIR) {
  Analyzer { stages: &vir.stages, interfaces: &mut vir.interfaces }.analyze();
}

struct Analyzer<'a> {
  pub stages: &'a IdxVec<StageId, Stage>,
  pub interfaces: &'a mut IdxVec<InterfaceId, Interface>,
}

impl Analyzer<'_> {
  fn analyze(&mut self) {
    // println!("sweep");
    self.sweep(InterfaceId(0));
    // println!("update_interior");
    for i in self.interfaces.keys() {
      if self.interfaces[i].incoming != 0 {
        self.update_interior(i, 0, 1);
      }
    }
    // println!("update_exterior");
    for i in self.interfaces.keys() {
      if self.interfaces[i].incoming != 0 {
        self.update_exterior(i, 0, 1);
      }
    }
    for i in self.interfaces.keys() {
      let interface = &mut self.interfaces[i];
      if interface.incoming != 0 {
        for (local, interior) in interface.interior.iter() {
          if let Some(&exterior) = interface.exterior.0.get(&local) {
            interface.wires.insert(local, (exterior.effect(interior), interior.effect(exterior)));
          }
        }
      }
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
          self.interfaces[transfer.interface].parents.push(interface_id);
          self.sweep(transfer.interface);
        }
      }
    }
  }

  fn update_interior(
    &mut self,
    interface_id: InterfaceId,
    invalidated: usize,
    depth: usize,
  ) -> usize {
    // println!("{:?} {} {}", interface_id, invalidated, depth);
    let interface = &mut self.interfaces[interface_id];
    if interface.interior_canonicity > invalidated {
      return interface.interior_canonicity;
    }
    interface.interior_canonicity = depth;

    let mut working = take(&mut interface.interior);
    let mut staging = Usages::default();
    let mut canonicity = usize::MAX;
    for i in 0..interface.kind.stages().len() {
      let stage = &self.stages[self.interfaces[interface_id].kind.stages()[i]];
      for step in &stage.steps {
        if let Step::Invoke(local, invocation) = step {
          staging.join_back(*local, invocation.usage());
        }
        if let Step::Transfer(transfer) = step {
          canonicity =
            canonicity.min(self.update_interior(transfer.interface, invalidated, depth + 1));
          staging.join_back_all(&self.interfaces[transfer.interface].interior);
        }
      }
      for &local in &stage.declarations {
        staging.0.remove(&local);
      }
      working.union_all(&staging);
      staging.0.clear();
    }

    let interface = &mut self.interfaces[interface_id];
    interface.interior = working;

    if canonicity == depth {
      interface.interior_canonicity = usize::MAX;
      canonicity = usize::MAX;
      for i in 0..interface.kind.stages().len() {
        let stage = &self.stages[self.interfaces[interface_id].kind.stages()[i]];
        for step in &stage.steps {
          if let Step::Transfer(transfer) = step {
            let _canonicity = self.update_interior(transfer.interface, depth, depth + 1);
            assert_eq!(_canonicity, usize::MAX);
          }
        }
      }
    }

    let interface = &mut self.interfaces[interface_id];
    interface.interior_canonicity = canonicity;
    canonicity
  }

  fn update_exterior(
    &mut self,
    interface_id: InterfaceId,
    invalidated: usize,
    depth: usize,
  ) -> usize {
    let interface = &mut self.interfaces[interface_id];
    if interface.exterior_canonicity > invalidated {
      return interface.exterior_canonicity;
    }

    interface.exterior_canonicity = depth;
    let mut canonicity = usize::MAX;
    for i in 0..interface.parents.len() {
      let parent = self.interfaces[interface_id].parents[i];
      canonicity = canonicity.min(self.update_exterior(parent, invalidated, depth + 1));
    }

    let interface = &mut self.interfaces[interface_id];
    for i in 0..interface.kind.stages().len() {
      let stage = &self.stages[self.interfaces[interface_id].kind.stages()[i]];
      let mut reverse = Vec::new();
      let mut staging = Usages::default();
      for step in stage.steps.iter().rev() {
        if let Step::Invoke(local, invocation) = step {
          staging.join_front(*local, invocation.usage());
        }
        if let Step::Transfer(transfer) = step {
          reverse.push(staging.clone());
          staging.join_front_all(&self.interfaces[transfer.interface].interior);
        }
      }
      let interface = &mut self.interfaces[interface_id];
      staging.clone_from(&interface.exterior);
      for &local in &stage.declarations {
        staging.0.remove(&local);
      }
      // dbg!(&staging);
      for step in &stage.steps {
        // dbg!(&step);
        if let Step::Invoke(local, invocation) = step {
          staging.join_back(*local, invocation.usage());
        }
        if let Step::Transfer(transfer) = step {
          let target = &mut self.interfaces[transfer.interface];
          let mut reverse = reverse.pop().unwrap();
          for (local, forward_usage) in staging.iter() {
            let reverse_usage = reverse.0.remove(&local).unwrap_or(Usage::None);
            // println!(
            //   "exterior {:?} -> {:?}: {:?} {:?} ({:?} ; {:?})",
            //   interface_id,
            //   transfer.interface,
            //   local,
            //   reverse_usage.join(forward_usage),
            //   reverse_usage,
            //   forward_usage,
            // );
            target.exterior.union(local, reverse_usage.join(forward_usage));
          }
          target.exterior.union_all(&reverse);
          staging.join_back_all(&target.interior);
        }
      }
    }

    if canonicity == depth {
      let interface = &mut self.interfaces[interface_id];
      interface.exterior_canonicity = usize::MAX;
      canonicity = usize::MAX;
      for i in 0..interface.parents.len() {
        let parent = self.interfaces[interface_id].parents[i];
        let _canonicity = self.update_exterior(parent, depth, depth + 1);
        assert_eq!(_canonicity, usize::MAX);
      }
    }

    let interface = &mut self.interfaces[interface_id];
    interface.exterior_canonicity = canonicity;
    canonicity
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

#[derive(Debug, Default, Clone)]
pub struct Usages(HashMap<Local, Usage>);

impl Usages {
  pub fn iter(&self) -> impl Iterator<Item = (Local, Usage)> + use<'_> {
    self.0.iter().map(|(&l, &u)| (l, u))
  }

  pub fn join_back_all(&mut self, usages: &Usages) {
    for (local, usage) in usages.iter() {
      self.join_back(local, usage);
    }
  }

  pub fn join_front_all(&mut self, usages: &Usages) {
    for (local, usage) in usages.iter() {
      self.join_front(local, usage);
    }
  }

  pub fn union_all(&mut self, usages: &Usages) {
    for (local, usage) in usages.iter() {
      self.union(local, usage);
    }
  }

  pub fn join_back(&mut self, local: Local, usage: Usage) {
    let entry = self.0.entry(local).or_insert(Usage::None);
    *entry = entry.join(usage);
  }

  pub fn join_front(&mut self, local: Local, usage: Usage) {
    let entry = self.0.entry(local).or_insert(Usage::None);
    *entry = usage.join(*entry);
  }

  pub fn union(&mut self, local: Local, usage: Usage) {
    let entry = self.0.entry(local).or_insert(usage);
    *entry = entry.union(usage);
  }
}
