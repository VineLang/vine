use std::collections::BTreeMap;

use vine_util::{idx::IdxVec, new_idx};

use crate::structures::{
  ast::Span,
  chart::{EnumId, StructId, VariantId},
  diag::Diag,
  signatures::{EnumSig, StructSig},
  tir::{Local, TirPat, TirPatKind},
  types::{Inverted, Type, TypeKind},
  vir::{Header, Interface, InterfaceId, InterfaceKind, Layer, Port, Stage, Step, Transfer},
};

use super::Distiller;

impl<'core, 'r> Distiller<'core, 'r> {
  pub fn distill_pattern_match(
    &mut self,
    span: Span,
    layer: &mut Layer,
    stage: &mut Stage,
    value: Port,
    rows: Vec<Row<'_>>,
  ) {
    let ty = value.ty;
    let local = self.new_local(stage, ty);
    stage.set_local_to(local, value);
    let vars = IdxVec::from(vec![Var { ty, kind: VarKind::Local(local, Form::Value) }]);
    let mut matcher = Matcher { exhaustive: true, distiller: self };
    matcher.distill_rows(layer, stage, vars, rows);
    if !matcher.exhaustive {
      self.core.report(Diag::NonExhaustiveMatch { span });
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
  Value,
  Place,
}

new_idx!(VarId);

#[derive(Debug, Clone)]
pub struct Row<'p> {
  cells: BTreeMap<VarId, &'p TirPat>,
  bindings: Vec<(VarId, &'p TirPat)>,
  arm: InterfaceId,
}

#[derive(Debug, Clone)]
struct Var {
  ty: Type,
  kind: VarKind,
}

#[derive(Debug, Clone)]
enum VarKind {
  Local(Local, Form),
  Composite(Vec<VarId>),
  Enum(EnumId, VariantId, Option<VarId>),
  Struct(StructId, VarId),
  Ref(Local),
}

impl VarKind {
  fn form(&self) -> Form {
    match self {
      VarKind::Local(_, form) => *form,
      _ => Form::Value,
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
struct Matcher<'core, 'd, 'r> {
  exhaustive: bool,
  distiller: &'d mut Distiller<'core, 'r>,
}

impl<'core, 'd, 'r> Matcher<'core, 'd, 'r> {
  fn distill_rows<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    vars: IdxVec<VarId, Var>,
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

  fn match_composite<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, Var>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    form: Form,
    inv: Inverted,
    element_tys: Vec<Type>,
  ) {
    let element_tys = element_tys.iter().map(|&t| t.invert_if(inv));
    let (element_vars, element_locals): (Vec<_>, Vec<_>) =
      element_tys.clone().map(|t| self.new_var(stage, &mut vars, form, t)).collect();
    let value = self.borrow_var(stage, &mut vars, var_id, VarKind::Composite(element_vars.clone()));
    let element_ports =
      Vec::from_iter(element_tys.zip(element_locals).map(|(ty, local)| stage.set_local(local, ty)));
    stage.steps.push(Step::Composite(value, element_ports));
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Composite(pats) => Some(element_vars.iter().copied().zip(pats)),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }

  fn match_ref<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, Var>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    inner_ty: Type,
  ) {
    let (inner_var, inner_local) = self.new_var(stage, &mut vars, Form::Place, inner_ty);
    let value = self.borrow_var(stage, &mut vars, var_id, VarKind::Ref(inner_local));
    let (a, b) = stage.mut_local(inner_local, inner_ty);
    stage.steps.push(Step::Ref(value, b, a));
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Ref(inner) => Some([(inner_var, inner)]),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }

  fn match_struct<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    mut vars: IdxVec<VarId, Var>,
    mut rows: Vec<Row<'p>>,
    var_id: VarId,
    form: Form,
    struct_id: StructId,
    sig: StructSig,
  ) {
    let content_ty = sig.data;
    let (content_var, content_local) = self.new_var(stage, &mut vars, form, content_ty);
    let value = self.borrow_var(stage, &mut vars, var_id, VarKind::Struct(struct_id, content_var));
    let content = stage.new_wire(content_ty);
    stage.steps.push(Step::Struct(struct_id, value, content.neg));
    stage.set_local_to(content_local, content.pos);
    self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
      TirPatKind::Struct(_, inner) => Some([(content_var, inner)]),
      _ => unreachable!(),
    });
    self.distill_rows(layer, stage, vars, rows);
  }

  fn match_enum<'p>(
    &mut self,
    layer: &mut Layer,
    stage: &mut Stage,
    vars: IdxVec<VarId, Var>,
    rows: Vec<Row<'p>>,
    var_id: VarId,
    form: Form,
    enum_id: EnumId,
    sig: EnumSig,
  ) {
    let interface = self.distiller.interfaces.push(None);

    let stages = Vec::from_iter(sig.variant_data.iter().map(|(variant_id, &content_ty)| {
      let mut vars = vars.clone();
      let mut rows = rows.clone();

      let mut stage = self.distiller.new_stage(layer, interface);

      let content = content_ty.map(|ty| {
        let (var, local) = self.new_var(&mut stage, &mut vars, form, ty);
        (ty, var, local)
      });

      let new_var = VarKind::Enum(enum_id, variant_id, content.map(|(_, var, _)| var));
      self.restore_var(&mut stage, &mut vars, var_id, new_var);
      stage.header = Header::Match(content.map(|(ty, _, local)| stage.set_local(local, ty)));

      self.eliminate_col(&mut rows, var_id, |pat| match &*pat.kind {
        TirPatKind::Enum(_, pat_variant_id, content_pat) => (pat_variant_id == &variant_id)
          .then(|| content.map(|(_, var, _)| var).into_iter().zip(content_pat)),
        _ => unreachable!(),
      });

      self.distill_rows(layer, &mut stage, vars, rows);

      self.distiller.finish_stage(stage)
    }));

    self.distiller.interfaces[interface] =
      Some(Interface::new(interface, layer.id, InterfaceKind::Match(enum_id, stages)));

    let value = self.take_var(stage, &vars, &vars[var_id]);
    stage.transfer = Some(Transfer { interface, data: Some(value) });
  }

  fn eliminate_col<
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

  fn new_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, Var>,
    form: Form,
    ty: Type,
  ) -> (VarId, Local) {
    let local = self.distiller.new_local(stage, ty);
    let var = vars.push(Var { ty, kind: VarKind::Local(local, form) });
    (var, local)
  }

  fn distill_row<'p>(&mut self, stage: &mut Stage, vars: &IdxVec<VarId, Var>, row: Row<'p>) {
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

  fn borrow_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, Var>,
    id: VarId,
    new_var: VarKind,
  ) -> Port {
    let value = self.take_var(stage, vars, &vars[id]);
    self.restore_var(stage, vars, id, new_var);
    value
  }

  fn restore_var(
    &mut self,
    stage: &mut Stage,
    vars: &mut IdxVec<VarId, Var>,
    id: VarId,
    new_var: VarKind,
  ) {
    let var = &mut vars[id];
    let ty = var.ty;
    match self.set_var(stage, var) {
      None => var.kind = new_var,
      Some(space) => {
        let new_value = self.take_var(stage, vars, &Var { ty, kind: new_var });
        stage.steps.push(Step::Link(space, new_value));
      }
    }
  }

  fn take_var(&mut self, stage: &mut Stage, vars: &IdxVec<VarId, Var>, var: &Var) -> Port {
    let wire = stage.new_wire(var.ty);
    match &var.kind {
      VarKind::Local(local, _) => stage.take_local_to(*local, wire.neg),
      VarKind::Composite(els) => {
        let els = els.iter().map(|&v| self.take_var(stage, vars, &vars[v])).collect();
        stage.steps.push(Step::Composite(wire.neg, els));
      }
      VarKind::Ref(local) => {
        let (value, space) = stage.mut_local(*local, self.distiller.locals[*local]);
        stage.steps.push(Step::Ref(wire.neg, value, space));
      }
      VarKind::Struct(struct_id, content) => {
        let content = self.take_var(stage, vars, &vars[*content]);
        stage.steps.push(Step::Struct(*struct_id, wire.neg, content));
      }
      VarKind::Enum(enum_id, variant_id, content) => {
        let content = content.map(|v| self.take_var(stage, vars, &vars[v]));
        stage.steps.push(Step::Enum(*enum_id, *variant_id, wire.neg, content));
      }
    }
    wire.pos
  }

  fn set_var(&mut self, stage: &mut Stage, var: &Var) -> Option<Port> {
    if let VarKind::Local(local, Form::Place) = var.kind {
      Some(stage.set_local(local, var.ty))
    } else {
      None
    }
  }
}
