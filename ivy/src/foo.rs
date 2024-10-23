#![allow(unused)]

use std::{collections::HashMap, mem::take};

use ivm::ext::ExtFn;
use slab::Slab;

type Wire = usize;
type Time = usize;

#[derive(Debug, Clone)]
enum Port {
  Erase,
  U32(u32),
  F32(f32),
  Global(String),
  Comb(String, [Wire; 2]),
  ExtFn(ExtFn, [Wire; 2]),
  Branch([Wire; 3]),
  Wire(Wire),
}

impl Port {
  fn aux(&self) -> &[Wire] {
    match self {
      Port::Erase | Port::U32(_) | Port::F32(_) | Port::Global(_) => &[],
      Port::Comb(_, a) | Port::ExtFn(_, a) => a,
      Port::Branch(a) => a,
      Port::Wire(_) => unreachable!(),
    }
  }
}

struct Net {
  wires: Slab<Option<(Port, Time)>>,
  inert: Vec<(Port, Port, Time)>,
  stack: Vec<(Port, Port)>,
  clock: Time,
}

macro_rules! sym {
  ($a: pat, $b: pat) => {
    ($a, $b) | ($b, $a)
  };
  ($a: pat) => {
    ($a, $a)
  };
}

impl Net {
  fn link(&mut self, a: Port, b: Port) {
    match (a, b) {
      sym!(Port::Wire(a), p) => {
        self.link_wire(a, p);
      }
      (a, b) => {
        self.stack.push((a, b));
      }
    }
  }

  fn link_wire(&mut self, a: Wire, b: Port) {
    if let Port::Wire(b) = b {
      self.link_wire_wire(a, b);
    } else {
      let slot = &mut self.wires[a];
      if let Some((a, _)) = slot.take() {
        self.link(a, b);
      } else {
        *slot = Some((b, self.clock));
        self.clock += 1;
      }
    }
  }

  fn link_wire_wire(&mut self, a: Wire, b: Wire) {
    let slots = self.wires.get2_mut(a, b).unwrap();
    if let Some((a, _)) = slots.0.take() {
      self.link_wire(b, a);
    } else if let Some((b, _)) = slots.1.take() {
      self.link_wire(a, b);
    } else {
      *slots.0 = Some((Port::Wire(b), self.clock));
      *slots.1 = Some((Port::Wire(a), self.clock));
      self.clock += 1;
    }
  }

  fn new_wire(&mut self) -> Wire {
    self.wires.insert(None)
  }

  fn finish(&mut self) {
    self.inert.extend(
      take(&mut self.wires)
        .into_iter()
        .filter_map(|(k, v)| Some((k, v?)))
        .filter(|(a, (b, _))| match b {
          Port::Wire(b) => a < b,
          _ => true,
        })
        .map(|(a, (b, t))| (Port::Wire(a), b, t)),
    );

    self.inert.sort_unstable_by_key(|x| x.2);
  }

  fn interact(&mut self, a: Port, b: Port) {
    match (a, b) {
      sym!(Port::Wire(a), b) => self.link_wire(a, b),
      sym!(Port::Erase, x) => {
        for &p in x.aux() {
          self.link_wire(p, Port::Erase);
        }
      }
      (a, b) => {
        self.inert.push((a, b, self.clock));
        self.clock += 1;
      } /* sym!((Wire, _), _) | sym!((Erase | ExtVal, _)) =>
         * unreachable!(),
         * sym!((Global, c), (Comb, d)) if !unsafe { c.as_global()
         * }.labels.has(d.label()) => {
         *   self.copy(c, d)
         * }
         * sym!((Global, c), (_, p)) => self.expand(c, p),
         * ((Comb, a), (Comb, b)) | ((ExtFn, a), (ExtFn, b)) |
         * ((Branch, a), (Branch, b))
         *   if a.label() == b.label() =>
         * {
         *   self.annihilate(a, b)
         * }
         * sym!((Erase, n), (_, b)) | sym!((ExtVal, n), (Comb, b)) =>
         * self.copy(n, b),
         * ((Comb | ExtFn | Branch, a), (Comb | ExtFn | Branch, b))
         * => self.commute(a, b),
         * sym!((ExtFn, f), (ExtVal, v)) => self.call(f, v),
         * sym!((Branch, b), (ExtVal, v)) => self.branch(b, v), */
    }
  }
}
