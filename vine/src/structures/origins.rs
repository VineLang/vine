pub mod relation;

use std::collections::hash_map::Entry;

use vine_util::{
  idx::{IdxVec, IntMap},
  new_idx,
};

use relation::Relation;

new_idx!(pub Origin; n => ["o{n}"]);

#[derive(Default, Debug, Clone)]
pub struct Origins {
  pub origins: IdxVec<Origin, OriginFacts>,
  pub delta: Vec<Delta>,
  pub inconsistent: bool,
}

#[derive(Default, Debug, Clone)]
pub struct OriginFacts {
  pub relations: IntMap<Origin, Relation>,
  pub joins: IntMap<Origin, Origin>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Delta {
  Relation(Origin, Origin),
  Join(Origin, Origin, Origin),
}

impl Origins {
  pub fn new_origin(&mut self) -> Origin {
    self.origins.push(OriginFacts::default())
  }

  pub fn relate(&mut self, a: Origin, b: Origin, rel: Relation) {
    if a != b {
      if self._relate(a, b, rel) {
        self._relate(b, a, rel.rev());
        self.delta(if a < b { Delta::Relation(a, b) } else { Delta::Relation(b, a) })
      }
    } else if !rel.allows_equal() {
      self.inconsistent = true;
    }
  }

  fn _relate(&mut self, a: Origin, b: Origin, rel: Relation) -> bool {
    match self.origins[a].relations.entry(b) {
      Entry::Occupied(mut e) => {
        let current = *e.get();
        let updated = current & rel;
        *e.get_mut() = updated;
        updated != current
      }
      Entry::Vacant(e) => {
        e.insert(rel);
        true
      }
    }
  }

  pub fn delta(&mut self, delta: Delta) {
    self.delta.push(delta);
  }

  pub fn join(&mut self, a: Origin, b: Origin) -> Origin {
    if a == b {
      return a;
    }
    let next = self.origins.next_index();
    match self.origins[a].joins.entry(b) {
      Entry::Occupied(e) => *e.get(),
      Entry::Vacant(e) => {
        let j = next;
        e.insert(j);
        self.new_origin();
        self.origins[b].joins.insert(a, j);
        self.relate(a, j, Relation::LE);
        self.relate(b, j, Relation::LE);
        self.delta(Delta::Join(j, a, b));
        j
      }
    }
  }

  pub fn solve(&mut self) {
    let mut buffer = Vec::new();
    while let Some(delta) = self.delta.pop() {
      self.process(delta, &mut buffer);
    }
  }

  fn process(&mut self, delta: Delta, buffer: &mut Vec<Origin>) {
    match delta {
      Delta::Relation(a, b) => {
        self._process_relation(a, b, buffer);
        self._process_relation(b, a, buffer);
      }
      Delta::Join(j, a, b) => {
        buffer.extend(self.origins[a].relations.keys().copied());
        for w in buffer.drain(..) {
          if self.origins[a].relations[&w].le()
            && self.origins[b].relations.get(&w).is_some_and(|bw| bw.le())
          {
            self.relate(j, w, Relation::LE);
          }
        }
      }
    }
  }

  fn _process_relation(&mut self, a: Origin, b: Origin, buffer: &mut Vec<Origin>) {
    let ab = self.origins[a].relations[&b];
    buffer.extend(self.origins[b].relations.keys().copied());
    for c in buffer.drain(..) {
      let bc = self.origins[b].relations[&c];
      if let Some(ac) = ab + bc {
        self.relate(a, c, ac);
      }
    }
    if ab.le() {
      buffer.extend(self.origins[a].joins.keys().copied());
      for x in buffer.drain(..) {
        if self.origins[x].relations.get(&b).is_some_and(|xb| xb.le()) {
          let j = self.origins[a].joins[&x];
          self.relate(j, b, Relation::LE);
        }
      }
    }
  }

  pub fn relation(&self, a: Origin, b: Origin) -> Option<Relation> {
    self.origins[a].relations.get(&b).copied()
  }

  pub fn import(&mut self, other: &Origins) -> impl Fn(Origin) -> Origin {
    assert!(!other.inconsistent);
    let base = self.origins.len();
    self.origins.vec.extend(other.origins.values().map(|facts| OriginFacts {
      relations: facts.relations.iter().map(|(&a, &rel)| (Origin(a.0 + base), rel)).collect(),
      joins: facts.joins.iter().map(|(&a, &b)| (Origin(a.0 + base), Origin(b.0 + base))).collect(),
    }));
    move |a| Origin(a.0 + base)
  }
}

#[test]
fn basic() {
  let mut origins = Origins::default();
  let a = origins.new_origin();
  let b = origins.new_origin();
  let ab = origins.join(a, b);
  let c = origins.new_origin();
  origins.relate(c, ab, Relation::LT);
  origins.relate(a, c, Relation::LE);
  origins.relate(b, c, Relation::LE);
  origins.solve();
  dbg!(&origins);
  assert!(origins.inconsistent);
}
