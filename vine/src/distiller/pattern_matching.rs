use std::{
  collections::{btree_map::Entry, BTreeMap},
  ops::Range,
};

use vine_util::{
  idx::{IdxVec, RangeExt},
  new_idx,
};

use crate::{
  ast::{Local, Pat, PatKind},
  chart::{AdtId, VariantId},
  vir::{Interface, InterfaceId, InterfaceKind, Layer, Port, Stage, Step, Transfer},
};

use super::Distiller;

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn distill_pattern_match(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    value: Port,
    rows: Vec<Row<'core, '_>>,
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
pub struct Row<'core, 'p> {
  cells: BTreeMap<VarId, &'p Pat<'core>>,
  bindings: Vec<(VarId, &'p Pat<'core>)>,
  arm: InterfaceId,
}

impl<'core, 'p> Row<'core, 'p> {
  pub fn new(pat: &'p Pat<'core>, arm: InterfaceId) -> Self {
    Row { cells: BTreeMap::from([(VarId(0), pat)]), bindings: Vec::new(), arm }
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
  Tuple(usize),
  Object(usize),
  Adt(AdtId),
}

impl<'core, 'd, 'r> Matcher<'core, 'd, 'r> {
  fn distill_rows<'p>(
    &mut self,
    mut rows: Vec<Row<'core, 'p>>,
    layer: &mut Layer,
    stage: &mut Stage,
  ) {
    while !rows.is_empty() {
      let Some((&var, _)) = rows[0].cells.last_key_value() else {
        return self.distill_row(rows.swap_remove(0), stage);
      };

      let mut kind = None;
      for row in &mut rows {
        if let Entry::Occupied(mut e) = row.cells.entry(var) {
          let row_kind = self.match_kind(e.get_mut());
          if let Some(row_kind) = row_kind {
            assert!(kind.is_none_or(|kind| kind == row_kind));
            kind = Some(row_kind);
          } else {
            let pat = e.remove();
            if !matches!(pat.kind, PatKind::Hole) {
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
            PatKind::Ref(inner) => [(new_var, &**inner)],
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
            PatKind::Inverse(inner) => [(new_var, &**inner)],
            _ => unreachable!(),
          });
        }

        MatchKind::Tuple(len) => {
          let (new_vars, new_locals) = self.new_var_range(stage, form, len);
          match form {
            Form::Value => {
              let value = stage.take_local(local);
              let spaces = new_locals.iter().map(|l| stage.set_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Tuple(value, spaces));
            }
            Form::Space => {
              let space = stage.set_local(local);
              let values = new_locals.iter().map(|l| stage.take_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Tuple(space, values));
            }
            Form::Place => {
              let (value, space) = stage.mut_local(local);
              let (values, spaces) =
                new_locals.iter().map(|l| stage.mut_local(l)).collect::<(Vec<_>, Vec<_>)>();
              stage.steps.push(Step::Tuple(value, spaces));
              stage.steps.push(Step::Tuple(space, values));
            }
          }

          self.eliminate_col(&mut rows, var, |p| match &p.kind {
            PatKind::Tuple(tuple) | PatKind::Adt(.., tuple) => new_vars.iter().zip(tuple),
            _ => unreachable!(),
          });
        }

        MatchKind::Object(len) => {
          let (new_vars, new_locals) = self.new_var_range(stage, form, len);
          match form {
            Form::Value => {
              let value = stage.take_local(local);
              let spaces = new_locals.iter().map(|l| stage.set_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Tuple(value, spaces));
            }
            Form::Space => {
              let space = stage.set_local(local);
              let values = new_locals.iter().map(|l| stage.take_local(l)).collect::<Vec<_>>();
              stage.steps.push(Step::Tuple(space, values));
            }
            Form::Place => {
              let (value, space) = stage.mut_local(local);
              let (values, spaces) =
                new_locals.iter().map(|l| stage.mut_local(l)).collect::<(Vec<_>, Vec<_>)>();
              stage.steps.push(Step::Tuple(value, spaces));
              stage.steps.push(Step::Tuple(space, values));
            }
          }

          self.eliminate_col(&mut rows, var, |p| match &p.kind {
            PatKind::Object(fields) => new_vars.iter().zip(
              fields.iter().map(|(k, v)| (k.ident, v)).collect::<BTreeMap<_, _>>().into_values(),
            ),
            _ => unreachable!(),
          });
        }

        MatchKind::Adt(adt_id) => {
          let adt = &self.distiller.chart.adts[adt_id];
          let interface = self.distiller.interfaces.push(None);
          let stages = adt
            .variants
            .iter()
            .map(|(variant_id, variant)| {
              let mut stage = self.distiller.new_stage(layer, interface);
              let (new_vars, new_locals) =
                self.new_var_range(&mut stage, form, variant.fields.len());

              let mut rows = rows
                .iter()
                .filter(|r| {
                  r.cells
                    .get(&var)
                    .is_none_or(|p| matches!(&p.kind, PatKind::Adt(_, i, ..) if *i == variant_id))
                })
                .cloned()
                .collect::<Vec<_>>();

              if form == Form::Place
                || rows.iter().any(|x| x.bindings.iter().any(|&(v, _)| v == var))
              {
                let (result, header) = new_locals.iter().map(|l| stage.mut_local(l)).collect();
                stage.header = header;
                let adt = stage.new_wire();
                stage.steps.push(Step::Adt(adt_id, variant_id, adt.0, result));
                stage.set_local_to(local, adt.1);
              } else {
                stage.header = new_locals.iter().map(|l| stage.set_local(l)).collect();
              }

              self.eliminate_col(&mut rows, var, |p| match &p.kind {
                PatKind::Adt(.., fields) => new_vars.iter().zip(fields),
                _ => unreachable!(),
              });

              self.distill_rows(rows, layer, &mut stage);

              let stage_id = stage.id;
              self.distiller.finish_stage(stage);
              stage_id
            })
            .collect::<Vec<_>>();
          self.distiller.interfaces[interface] =
            Some(Interface::new(interface, layer.id, InterfaceKind::Match(adt_id, stages)));
          let value = stage.take_local(local);
          stage.transfer = Some(Transfer { interface, data: Some(value) });
          return;
        }
      }
    }
  }

  fn eliminate_col<
    'p,
    F: FnMut(&'p Pat<'core>) -> I,
    I: IntoIterator<Item = (VarId, &'p Pat<'core>)>,
  >(
    &mut self,
    rows: &mut Vec<Row<'core, 'p>>,
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

  fn add_to_row<'p>(&self, row: &mut Row<'core, 'p>, var: VarId, mut pat: &'p Pat<'core>) {
    if self.match_kind(&mut pat).is_some() {
      row.cells.insert(var, pat);
    } else {
      row.bindings.push((var, pat));
    }
  }

  fn match_kind(&self, pat: &mut &Pat) -> Option<MatchKind> {
    loop {
      return match &pat.kind {
        PatKind::Error(_) | PatKind::PathCall(..) => unreachable!(),
        PatKind::Hole | PatKind::Local(_) => None,
        PatKind::Paren(p) | PatKind::Annotation(p, _) => {
          *pat = p;
          continue;
        }
        PatKind::Adt(adt_id, _, _, _) => {
          let adt_def = &self.distiller.chart.adts[*adt_id];
          if adt_def.variants.len() == 1 {
            Some(MatchKind::Tuple(adt_def.variants[VariantId(0)].fields.len()))
          } else {
            Some(MatchKind::Adt(*adt_id))
          }
        }
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
          if let PatKind::Hole = &p.kind {
            *pat = p;
            continue;
          }
          self.match_kind(&mut &**p).map(|_| MatchKind::Inverse)
        }
        PatKind::Tuple(t) => Some(MatchKind::Tuple(t.len())),
        PatKind::Object(e) => Some(MatchKind::Object(e.len())),
      };
    }
  }

  fn distill_row<'p>(&mut self, row: Row<'core, 'p>, stage: &mut Stage) {
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
