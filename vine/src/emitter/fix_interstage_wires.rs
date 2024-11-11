use std::collections::{hash_map::Entry, HashMap, VecDeque};

use vine_util::idx::RangeExt;

use crate::emitter::StageId;

use super::{Emitter, Port, Step};

impl Emitter<'_> {
  pub(super) fn fix_interstage_wires(&mut self) {
    let mut wires = HashMap::<usize, StageId>::new();
    for i in self.stages.range().iter() {
      let (a, b) = self.stages.vec.split_at_mut(i.0);
      let x = &mut b[0];
      let mut new_steps = VecDeque::new();
      for p in x
        .agents
        .iter_mut()
        .flat_map(|x| x.ports_mut())
        .chain(x.steps.iter_mut().map(|x| x.port_mut()))
        .chain(x.header.iter_mut().flat_map(|(a, b)| [a, b]))
      {
        *p = self.net.follow(p.clone());
        if let Some(w) = p.wire() {
          match wires.entry(w) {
            Entry::Occupied(e) => {
              let h = e.remove();
              if h != i {
                let local = self.locals.next();
                a[h.0].steps.push_front(Step::Set(local, Port::Wire(w)));
                new_steps.push_back(Step::Move(local, Port::Wire(w)));
              }
            }
            Entry::Vacant(e) => {
              e.insert(i);
            }
          }
        }
      }
      x.steps.append(&mut new_steps);
    }
    debug_assert!(wires.is_empty());
  }
}
