use std::collections::BTreeMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::{
  components::distiller::Distiller,
  structures::{
    ast::Span,
    chart::{EnumId, StructId, VariantId},
    diag::Diag,
    tir::{Local, TirPat, TirPatKind},
    types::{Inverted, Type, TypeKind},
    vir::{InterfaceId, Layer, Port, Stage, Step, Transfer},
  },
};

impl<'r> Distiller<'r> {
  pub fn distill_pattern_match(
    &mut self,
    span: Span,
    layer: &mut Layer,
    stage: &mut Stage,
    value: Port,
    rows: Vec<Row<'_>>,
  ) {
    let ty = value.ty;
    let local = self.new_local(stage, span, ty);
    stage.local_barrier_write_to(local, value);
    let vars =
      IdxVec::from([MatchVar { ty, kind: MatchVarKind::Local(local, MatchVarForm::Value) }]);
    let mut matcher = Matcher { span, exhaustive: true, distiller: self };
    matcher.distill_rows(layer, stage, vars, rows);
    if !matcher.exhaustive {
      self.core.report(Diag::NonExhaustiveMatch { span });
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MatchVarForm {
  Value,
  Place,
}

new_idx!(pub(crate) VarId);

#[derive(Debug, Clone)]
pub struct Row<'p> {
  cells: BTreeMap<VarId, &'p TirPat>,
  bindings: Vec<(VarId, &'p TirPat)>,
  arm: InterfaceId,
}

#[derive(Debug, Clone)]
pub(crate) struct MatchVar {
  ty: Type,
  kind: MatchVarKind,
}

#[derive(Debug, Clone)]
pub(crate) enum MatchVarKind {
  Local(Local, MatchVarForm),
  Composite(Vec<VarId>),
  Enum(EnumId, VariantId, Option<VarId>),
  Struct(StructId, VarId),
  Ref(Local),
}

impl MatchVarKind {
  fn form(&self) -> MatchVarForm {
    match self {
      MatchVarKind::Local(_, form) => *form,
      _ => MatchVarForm::Value,
    }
  }
}

impl<'p> Row<'p> {
  pub fn new(pat: Option<&'p TirPat>, arm: InterfaceId) -> Self {
    let mut row = Row { cells: BTreeMap::new(), bindings: Vec::new(), arm };
    if let Some(pat) = pat {
      row.add(VarId(0), pat);
    }
    row
  }

  fn add(&mut self, var: VarId, pat: &'p TirPat) {
    if matches!(*pat.kind, TirPatKind![incomplete]) {
      self.cells.insert(var, pat);
    } else if !matches!(*pat.kind, TirPatKind::Hole) {
      self.bindings.push((var, pat));
    }
  }
}

#[derive(Debug)]
pub(crate) struct Matcher<'d, 'r> {
  pub(crate) span: Span,
  exhaustive: bool,
  pub(crate) distiller: &'d mut Distiller<'r>,
}

impl<'d, 'r> Matcher<'d, 'r> {
  pub(crate) fn distill_rows<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    vars: IdxVec<VarId, MatchVar>,
    mut rows: Vec<Row<'p>>,
  ) {
    if rows.is_empty() {
      self.exhaustive = false;
      return;
    }

    let Some((&var_id, _)) = rows[0].cells.last_key_value() else {
      return self.distill_row(stage, &vars, rows.swap_remove(0));
    };

    let var = &vars[var_id];
    let ty = var.ty;
    let form = var.kind.form();

    match self.distiller.types.kind(ty) {
      Some((inv, TypeKind::Tuple(els))) => {
        self.match_composite(layer, stage, vars, rows, var_id, form, inv, els.clone());
      }
      Some((inv, TypeKind::Object(entries))) => {
        let els = entries.values().copied().collect();
        self.match_composite(layer, stage, vars, rows, var_id, form, inv, els);
      }
      Some((Inverted(false), TypeKind::Ref(inner))) => {
        self.match_ref(layer, stage, vars, rows, var_id, *inner);
      }
      Some((Inverted(false), TypeKind::Struct(struct_id, type_params))) => {
        let struct_id = *struct_id;
        let sig = &self.distiller.sigs.structs[struct_id];
        let sig = self.distiller.types.import(sig, Some(&type_params.clone()));
        self.match_struct(layer, stage, vars, rows, var_id, form, struct_id, sig);
      }
      Some((Inverted(false), TypeKind::Enum(enum_id, type_params))) => {
        let enum_id = *enum_id;
        let sig = &self.distiller.sigs.enums[enum_id];
        let sig = self.distiller.types.import(sig, Some(&type_params.clone()));
        self.match_enum(layer, stage, vars, rows, var_id, form, enum_id, sig);
      }
      Some((_, TypeKind::Error(_))) => {}
      _ => unreachable!(),
    }
  }

  pub(crate) fn eliminate_col<
    'p,
    F: FnMut(&'p TirPat) -> Option<I>,
    I: IntoIterator<Item = (VarId, &'p TirPat)>,
  >(
    &mut self,
    rows: &mut Vec<Row<'p>>,
    var: VarId,
    mut f: F,
  ) {
    rows.retain_mut(|row| {
      if let Some(pat) = row.cells.remove(&var) {
        if let Some(new_cells) = f(pat) {
          for (var, pat) in new_cells {
            row.add(var, pat);
          }
          true
        } else {
          false
        }
      } else {
        true
      }
    });
  }

  pub(crate) fn new_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, MatchVar>,
    form: MatchVarForm,
    ty: Type,
  ) -> (VarId, Local) {
    let local = self.distiller.new_local(stage, self.span, ty);
    let var = vars.push(MatchVar { ty, kind: MatchVarKind::Local(local, form) });
    (var, local)
  }

  fn distill_row<'p>(&mut self, stage: &mut Stage, vars: &IdxVec<VarId, MatchVar>, row: Row<'p>) {
    for (var_id, pat) in row.bindings {
      let var = &vars[var_id];
      let value = self.take_var(stage, vars, var);
      match self.set_var(stage, var) {
        None => {
          let v = self.distiller.distill_pat_value(stage, pat);
          stage.steps.push(Step::Link(value, v));
        }
        Some(space) => {
          let (v, s) = self.distiller.distill_pat_place(stage, pat);
          stage.steps.push(Step::Link(value, v));
          stage.steps.push(Step::Link(space, s));
        }
      }
    }
    stage.transfer = Some(Transfer::unconditional(row.arm));
  }

  pub(crate) fn borrow_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, MatchVar>,
    id: VarId,
    new_var: MatchVarKind,
  ) -> Port {
    let value = self.take_var(stage, vars, &vars[id]);
    self.restore_var(stage, vars, id, new_var);
    value
  }

  pub(crate) fn restore_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, MatchVar>,
    id: VarId,
    new_var: MatchVarKind,
  ) {
    let var = &mut vars[id];
    let ty = var.ty;
    match self.set_var(stage, var) {
      None => var.kind = new_var,
      Some(space) => {
        let new_value = self.take_var(stage, vars, &MatchVar { ty, kind: new_var });
        stage.steps.push(Step::Link(space, new_value));
      }
    }
  }

  pub(crate) fn take_var(
    &mut self,
    stage: &mut Stage,
    vars: &IdxVec<VarId, MatchVar>,
    var: &MatchVar,
  ) -> Port {
    let wire = stage.new_wire(self.span, var.ty);
    match &var.kind {
      MatchVarKind::Local(local, _) => stage.local_read_barrier_to(*local, wire.neg),
      MatchVarKind::Composite(els) => {
        let els = els.iter().map(|&v| self.take_var(stage, vars, &vars[v])).collect();
        stage.steps.push(Step::Composite(wire.neg, els));
      }
      MatchVarKind::Ref(local) => {
        let (value, space) =
          stage.local_read_write(*local, self.span, self.distiller.locals[*local].ty);
        stage.steps.push(Step::Ref(wire.neg, value, space));
      }
      MatchVarKind::Struct(struct_id, content) => {
        let content = self.take_var(stage, vars, &vars[*content]);
        stage.steps.push(Step::Struct(*struct_id, wire.neg, content));
      }
      MatchVarKind::Enum(enum_id, variant_id, content) => {
        let content = content.map(|v| self.take_var(stage, vars, &vars[v]));
        stage.steps.push(Step::Enum(*enum_id, *variant_id, wire.neg, content));
      }
    }
    wire.pos
  }

  fn set_var(&mut self, stage: &mut Stage, var: &MatchVar) -> Option<Port> {
    if let MatchVarKind::Local(local, MatchVarForm::Place) = var.kind {
      Some(stage.local_barrier_write(local, self.span, var.ty))
    } else {
      None
    }
  }
}
