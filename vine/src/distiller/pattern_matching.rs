use std::{
  collections::{btree_map::Entry, BTreeMap},
  ops::Range,
};

use vine_util::{
  idx::{IdxVec, RangeExt},
  new_idx,
};

use crate::{
  chart::EnumId,
  tir::{Local, TirPat, TirPatKind},
  vir::{Interface, InterfaceId, InterfaceKind, Layer, Port, Stage, Step, Transfer},
};

use super::Distiller;

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn distill_pattern_match(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    value: Port,
    rows: Vec<Row<'_>>,
  ) {
    let local = self.new_local(stage);
    stage.set_local_to(local, value);
    let mut matcher = Matcher { vars: IdxVec::from([(local, Form::Value)]), distiller: self };
    matcher.distill_rows(rows, layer, stage);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub struct Row<'p> {
  cells: BTreeMap<VarId, &'p TirPat>,
  bindings: Vec<(VarId, &'p TirPat)>,
  arm: InterfaceId,
}

impl<'p> Row<'p> {
  pub fn new(pat: Option<&'p TirPat>, arm: InterfaceId) -> Self {
    Row { cells: pat.into_iter().map(|pat| (VarId(0), pat)).collect(), bindings: Vec::new(), arm }
  }
}

#[derive(Debug)]
struct Matcher<'core, 'd, 'r> {
  vars: IdxVec<VarId, (Local, Form)>,
  distiller: &'d mut Distiller<'core, 'r>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MatchKind {
  Ref,
  Inverse,
  Composite(usize),
  Enum(EnumId),
}

impl<'core, 'd, 'r> Matcher<'core, 'd, 'r> {
  fn distill_rows<'p>(&mut self, mut rows: Vec<Row<'p>>, layer: &mut Layer, stage: &mut Stage) {
    while !rows.is_empty() {
      let Some((&var, _)) = rows[0].cells.last_key_value() else {
        return self.distill_row(rows.swap_remove(0), stage);
      };

      let mut kind = None;
      for row in &mut rows {
        if let Entry::Occupied(mut e) = row.cells.entry(var) {
          let row_kind = Self::match_kind(e.get_mut());
          if let Some(row_kind) = row_kind {
            assert!(kind.is_none_or(|kind| kind == row_kind));
            kind = Some(row_kind);
          } else {
            let pat = e.remove();
            if !matches!(pat.kind, TirPatKind::Hole) {
              row.bindings.push((var, pat));
            }
          }
        }
      }

      let Some(kind) = kind else {
        continue;
      };

      let (local, form) = self.vars[var];

      match kind {
        MatchKind::Ref => {
          let (new_var, new_local) = self.new_var(stage, Form::Place);
          match form {
            Form::Value => {
              let reference = stage.take_local(local);
              let val_0 = stage.new_wire();
              let val_1 = stage.new_wire();
              stage.steps.push(Step::Ref(reference, val_0.0, val_1.1));
              stage.mut_local_to(new_local, val_1.0, val_0.1);
            }
            Form::Place => {
              let (ref_in, ref_out) = stage.mut_local(local);
              let val_0 = stage.new_wire();
              let val_1 = stage.new_wire();
              let val_2 = stage.new_wire();
              stage.steps.push(Step::Ref(ref_in, val_0.0, val_2.1));
              stage.steps.push(Step::Ref(ref_out, val_1.1, val_2.0));
              stage.mut_local_to(new_local, val_1.0, val_0.1);
            }
            Form::Space => unreachable!(),
          }

          self.eliminate_col(&mut rows, var, |p| match &p.kind {
            TirPatKind::Ref(inner) => [(new_var, &**inner)],
            _ => unreachable!(),
          });
        }

        MatchKind::Inverse => {
          let (new_var, new_local) = self.new_var(stage, form.inverse());
          match form {
            Form::Value => {
              let port = stage.take_local(local);
              stage.set_local_to(new_local, port);
            }
            Form::Space => {
              let port = stage.set_local(local);
              stage.take_local_to(new_local, port);
            }
            Form::Place => {
              let (a, b) = stage.mut_local(local);
              stage.mut_local_to(new_local, a, b);
            }
          }

          self.eliminate_col(&mut rows, var, |p| match &p.kind {
            TirPatKind::Inverse(inner) => [(new_var, &**inner)],
            _ => unreachable!(),
          });
        }

        MatchKind::Composite(len) => {
          let (new_vars, new_locals) = self.new_var_range(stage, form, len);
          match form {
            Form::Value => {
              let value = stage.take_local(local);
              let spaces = new_locals.iter().map(|l| stage.set_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Composite(value, spaces));
            }
            Form::Space => {
              let space = stage.set_local(local);
              let values = new_locals.iter().map(|l| stage.take_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Composite(space, values));
            }
            Form::Place => {
              let (value, space) = stage.mut_local(local);
              let (values, spaces) =
                new_locals.iter().map(|l| stage.mut_local(l)).collect::<(Vec<_>, Vec<_>)>();
              stage.steps.push(Step::Composite(value, spaces));
              stage.steps.push(Step::Composite(space, values));
            }
          }

          self.eliminate_col(&mut rows, var, |p| match &p.kind {
            TirPatKind::Composite(els) => new_vars.iter().zip(els),
            _ => unreachable!(),
          });
        }

        MatchKind::Enum(enum_id) => {
          let enum_ = &self.distiller.chart.enums[enum_id];
          let interface = self.distiller.interfaces.push(None);
          let stages = enum_
            .variants
            .iter()
            .map(|(variant_id, variant)| {
              let mut stage = self.distiller.new_stage(layer, interface);
              let (new_var, new_local) =
                opt_tuple(variant.data.is_some().then(|| self.new_var(&mut stage, form)));

              let mut rows = rows
                .iter()
                .filter(|r| {
                  r.cells.get(&var).is_none_or(
                    |p| matches!(&p.kind, TirPatKind::Enum(_, i, ..) if *i == variant_id),
                  )
                })
                .cloned()
                .collect::<Vec<_>>();

              if form == Form::Place
                || rows.iter().any(|x| x.bindings.iter().any(|&(v, _)| v == var))
              {
                let (result, header) = opt_tuple(new_local.map(|l| stage.mut_local(l)));
                stage.header.extend(header);
                let adt = stage.new_wire();
                stage.steps.push(Step::Enum(adt.0, enum_id, variant_id, result));
                stage.set_local_to(local, adt.1);
              } else {
                let local = new_local.map(|l| stage.set_local(l));
                stage.header.extend(local);
              }

              self.eliminate_col(&mut rows, var, |p| match &p.kind {
                TirPatKind::Enum(.., data) => new_var.into_iter().zip(data.as_deref()),
                _ => unreachable!(),
              });

              self.distill_rows(rows, layer, &mut stage);

              let stage_id = stage.id;
              self.distiller.finish_stage(stage);
              stage_id
            })
            .collect::<Vec<_>>()
            .into();
          self.distiller.interfaces[interface] =
            Some(Interface::new(interface, layer.id, InterfaceKind::Match(enum_id, stages)));
          let value = stage.take_local(local);
          stage.transfer = Some(Transfer { interface, data: Some(value) });
          return;
        }
      }
    }
  }

  fn eliminate_col<'p, F: FnMut(&'p TirPat) -> I, I: IntoIterator<Item = (VarId, &'p TirPat)>>(
    &mut self,
    rows: &mut Vec<Row<'p>>,
    var: VarId,
    mut f: F,
  ) {
    for row in rows {
      if let Some(pat) = row.cells.remove(&var) {
        for (var, pat) in f(pat) {
          self.add_to_row(row, var, pat);
        }
      }
    }
  }

  fn new_var(&mut self, stage: &mut Stage, form: Form) -> (VarId, Local) {
    let local = self.distiller.new_local(stage);
    let var = self.vars.push((local, form));
    (var, local)
  }

  fn new_var_range(
    &mut self,
    stage: &mut Stage,
    form: Form,
    len: usize,
  ) -> (Range<VarId>, Range<Local>) {
    let start_var = self.vars.next_index();
    let start_local = self.distiller.locals.peek_next();
    for _ in 0..len {
      _ = self.new_var(stage, form);
    }
    let end_var = self.vars.next_index();
    let end_local = self.distiller.locals.peek_next();
    (start_var..end_var, start_local..end_local)
  }

  fn add_to_row<'p>(&self, row: &mut Row<'p>, var: VarId, mut pat: &'p TirPat) {
    if Self::match_kind(&mut pat).is_some() {
      row.cells.insert(var, pat);
    } else {
      row.bindings.push((var, pat));
    }
  }

  fn match_kind(pat: &mut &TirPat) -> Option<MatchKind> {
    loop {
      return match &pat.kind {
        TirPatKind::Error(_) => unreachable!(),
        TirPatKind::Hole | TirPatKind::Local(_) => None,
        TirPatKind::Struct(_, p) => {
          *pat = p;
          continue;
        }
        TirPatKind::Enum(enum_id, _, _) => Some(MatchKind::Enum(*enum_id)),
        TirPatKind::Ref(p) => Self::match_kind(&mut &**p).map(|_| MatchKind::Ref),
        TirPatKind::Deref(p) => {
          if let TirPatKind::Ref(p) = &p.kind {
            *pat = p;
            continue;
          }
          None
        }
        TirPatKind::Inverse(p) => {
          if let TirPatKind::Inverse(p) = &p.kind {
            *pat = p;
            continue;
          }
          if let TirPatKind::Hole = &p.kind {
            *pat = p;
            continue;
          }
          Self::match_kind(&mut &**p).map(|_| MatchKind::Inverse)
        }
        TirPatKind::Composite(e) => Some(MatchKind::Composite(e.len())),
      };
    }
  }

  fn distill_row<'p>(&mut self, row: Row<'p>, stage: &mut Stage) {
    for (var, pat) in row.bindings {
      let (local, form) = self.vars[var];
      match form {
        Form::Value => {
          let port = self.distiller.distill_pat_value(stage, pat);
          stage.take_local_to(local, port);
        }
        Form::Space => {
          let port = self.distiller.distill_pat_space(stage, pat);
          stage.set_local_to(local, port);
        }
        Form::Place => {
          let (a, b) = self.distiller.distill_pat_place(stage, pat);
          stage.mut_local_to(local, a, b);
        }
      }
    }
    stage.transfer = Some(Transfer::unconditional(row.arm));
  }
}

fn opt_tuple<A, B>(opt: Option<(A, B)>) -> (Option<A>, Option<B>) {
  match opt {
    Some((a, b)) => (Some(a), Some(b)),
    None => (None, None),
  }
}
