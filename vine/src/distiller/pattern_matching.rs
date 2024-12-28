use std::collections::{hash_map::Entry, HashMap};

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  ast::{Local, Pat, PatKind},
  resolver::DefId,
  vir::{Layer, Stage},
};

use super::Distiller;

#[derive(Debug, Clone, Copy)]
enum Form {
  Value,
  Space,
  Place,
}

impl Form {
  fn inverse(self) -> Self {
    match self {
      Form::Value => Form::Space,
      Form::Space => Form::Value,
      Form::Place => Form::Place,
    }
  }
}

new_idx!(VarId);
new_idx!(ArmId);

struct Row<'p, 'core> {
  cells: HashMap<VarId, &'p Pat<'core>>,
  bindings: Vec<(VarId, &'p Pat<'core>)>,
  arm: ArmId,
}

struct Matcher<'core, 'd, 'r> {
  vars: IdxVec<VarId, (Local, Form)>,
  outstanding: Vec<VarId>,
  distiller: &'d mut Distiller<'core, 'r>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MatchKind {
  Ref,
  Inverse,
  Tuple,
  Adt(DefId),
}

impl<'core, 'd, 'r> Matcher<'core, 'd, 'r> {
  fn foo(&mut self, mut rows: Vec<Row>, layer: &mut Layer, stage: &mut Stage) {
    let var = self.outstanding.pop().unwrap();

    let mut kind = None;
    for row in &mut rows {
      if let Entry::Occupied(mut e) = row.cells.entry(var) {
        let row_kind = self.match_kind(e.get_mut());
        if let Some(row_kind) = row_kind {
          assert!(kind.is_none_or(|kind| kind == row_kind));
          kind = Some(row_kind);
        } else {
          row.bindings.push((var, e.remove()));
        }
      }
    }

    let Some(kind) = kind else { todo!() };

    let (local, form) = self.vars[var];

    match kind {
      MatchKind::Ref => {}
      MatchKind::Inverse => todo!(),
      MatchKind::Tuple => todo!(),
      MatchKind::Adt(def_id) => todo!(),
    }
  }

  fn add_to_row<'p>(&self, row: &mut Row<'p, 'core>, var: VarId, mut pat: &'p Pat<'core>) {
    if self.match_kind(&mut pat).is_some() {
      row.cells.insert(var, pat);
    } else {
      row.bindings.push((var, pat));
    }
  }

  fn match_kind(&self, pat: &mut &Pat) -> Option<MatchKind> {
    loop {
      return match &pat.kind {
        PatKind::Error(_) => unreachable!(),
        PatKind::Hole | PatKind::Local(_) => None,
        PatKind::Paren(p) => {
          *pat = p;
          continue;
        }
        PatKind::Adt(variant, _) => Some(MatchKind::Adt(
          self.distiller.resolver.defs[variant.path.resolved.unwrap()]
            .variant_def
            .as_ref()
            .unwrap()
            .adt,
        )),
        PatKind::Ref(p) => self.match_kind(&mut &**p).map(|_| MatchKind::Ref),
        PatKind::Deref(p) => {
          if let PatKind::Ref(p) = &p.kind {
            *pat = p;
            continue;
          }
          None
        }
        PatKind::Inverse(p) => {
          if let PatKind::Inverse(p) = &p.kind {
            *pat = p;
            continue;
          }
          self.match_kind(&mut &**p).map(|_| MatchKind::Inverse)
        }
        PatKind::Tuple(_) => Some(MatchKind::Tuple),
      };
    }
  }
}
