use std::collections::{btree_map::Entry, BTreeMap};

use vine_util::{idx::IdxVec, unwrap_idx_vec};

use crate::structures::{
  ast::Span,
  chart::{Chart, DefId, GenericsId},
  core::Core,
  resolutions::{Fragment, Rels},
  signatures::Signatures,
  tir::Local,
  types::{Type, Types},
  vir::{
    Header, Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, PortKind, Stage, StageId,
    Step, Transfer, Vir, VirLocal, WireId,
  },
};

pub fn normalize<'core>(
  core: &'core Core<'core>,
  chart: &Chart<'core>,
  sigs: &Signatures<'core>,
  fragment: &Fragment<'core>,
  source: &Vir<'core>,
) -> Vir<'core> {
  let mut normalizer = Normalizer {
    core,
    chart,
    sigs,
    source,
    def: fragment.def,
    generics: fragment.generics,
    types: source.types.clone(),
    rels: source.rels.clone(),
    locals: source.locals.clone(),
    interfaces: source.interfaces.clone(),
    stages: IdxVec::from(vec![None; source.stages.len()]),
    layer_divergence: IdxVec::from(vec![None; source.layers.len()]),
    final_transfers: IdxVec::from(vec![None; source.layers.len()]),
  };

  for layer in source.layers.values() {
    if layer.parent.is_none() {
      normalizer.normalize_layer(layer);
    }
  }

  Vir {
    types: normalizer.types,
    locals: normalizer.locals,
    rels: normalizer.rels,
    layers: IdxVec::new(),
    interfaces: normalizer.interfaces,
    stages: unwrap_idx_vec(normalizer.stages),
    closures: source.closures.clone(),
  }
}

#[derive(Debug)]
struct Normalizer<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  sigs: &'a Signatures<'core>,

  source: &'a Vir<'core>,
  def: DefId,
  generics: GenericsId,

  types: Types<'core>,
  rels: Rels,
  locals: IdxVec<Local, VirLocal>,
  interfaces: IdxVec<InterfaceId, Interface>,
  stages: IdxVec<StageId, Option<Stage>>,

  layer_divergence: IdxVec<LayerId, Option<LayerId>>,
  final_transfers: IdxVec<LayerId, Option<Transfer>>,
}

impl<'core> Normalizer<'core, '_> {
  fn normalize_layer(&mut self, layer: &Layer) {
    for &stage in &layer.stages {
      let source = &self.source.stages[stage];
      let mut open_wires = BTreeMap::new();
      let mut stage = Stage {
        id: source.id,
        span: source.span,
        interface: source.interface,
        layer: LayerId::NONE,
        header: source.header.clone(),
        declarations: source.declarations.clone(),
        steps: Vec::new(),
        transfer: None,
        wires: source.wires,
      };
      for port in stage.header.ports() {
        update_open_wires(&mut open_wires, port);
      }
      for step in &source.steps {
        for port in step.ports() {
          update_open_wires(&mut open_wires, port);
        }
        if self.step_divergence(step) <= layer.id {
          let new_stage = self.stages.push(None);
          let interface = self.interfaces.push(Interface::new(
            self.interfaces.next_index(),
            LayerId::NONE,
            InterfaceKind::Unconditional(new_stage),
          ));
          let mut new_stage: Stage = Stage {
            id: new_stage,
            interface,
            span: source.span,
            layer: LayerId::NONE,
            header: Header::None,
            declarations: Vec::new(),
            steps: Vec::new(),
            transfer: None,
            wires: source.wires,
          };
          for (&wire, &(span, ty)) in &open_wires {
            let Self { core, chart, sigs, def, generics, ref mut types, ref mut rels, .. } = *self;
            let vir_local = VirLocal::new(core, chart, sigs, def, generics, types, rels, span, ty);
            let local = self.locals.push(vir_local);
            stage.declarations.push(local);
            stage.local_barrier_write_to(local, Port { ty, kind: PortKind::Wire(span, wire) });
            new_stage.local_read_barrier_to(
              local,
              Port { ty: ty.inverse(), kind: PortKind::Wire(span, wire) },
            );
          }
          match step {
            Step::Transfer(transfer) => {
              let interface = &self.source.interfaces[transfer.interface];
              let sub_layer = &self.source.layers[interface.layer];
              self.final_transfers[sub_layer.id] =
                Some(Transfer::unconditional(new_stage.interface));
              self.normalize_layer(sub_layer);
              stage.steps.push(Step::Transfer(transfer.clone()));
            }
            Step::Diverge(super_layer, transfer) => {
              let transfer = transfer.clone().or(self.final_transfers[*super_layer].clone());
              if let Some(transfer) = transfer {
                stage.steps.push(Step::Transfer(transfer));
              }
            }
            _ => unreachable!(),
          }
          let id = stage.id;
          self.stages[id] = Some(stage);
          stage = new_stage;
        } else {
          if let Step::Transfer(transfer) = step {
            let interface = &self.source.interfaces[transfer.interface];
            let sub_layer = &self.source.layers[interface.layer];
            if sub_layer.parent.is_some() {
              self.normalize_layer(sub_layer);
            }
          }
          stage.steps.push(step.clone());
        }
      }
      let transfer = source.transfer.clone().or(self.final_transfers[layer.id].clone());
      if let Some(transfer) = transfer {
        stage.steps.push(Step::Transfer(transfer));
      }
      let id = stage.id;
      self.stages[id] = Some(stage);
    }
  }

  fn layer_divergence(&mut self, layer: &Layer) -> LayerId {
    if layer.parent.is_none() {
      return LayerId::NONE;
    }
    if let Some(divergence) = self.layer_divergence[layer.id] {
      return divergence;
    }
    let mut divergence = layer.id;
    for &stage in &layer.stages {
      let stage = &self.source.stages[stage];
      for step in &stage.steps {
        divergence = divergence.min(self.step_divergence(step));
      }
    }
    self.layer_divergence[layer.id] = Some(divergence);
    divergence
  }

  fn step_divergence(&mut self, step: &Step) -> LayerId {
    match step {
      Step::Diverge(divergence, _) => *divergence,
      Step::Transfer(transfer) => {
        let interface = &self.source.interfaces[transfer.interface];
        let sub_layer = &self.source.layers[interface.layer];
        if sub_layer.parent.is_some() {
          self.layer_divergence(sub_layer)
        } else {
          LayerId::NONE
        }
      }
      _ => LayerId::NONE,
    }
  }
}

fn update_open_wires(open_wires: &mut BTreeMap<WireId, (Span, Type)>, port: &Port) {
  if let Port { ty, kind: PortKind::Wire(span, wire) } = port {
    match open_wires.entry(*wire) {
      Entry::Vacant(e) => {
        e.insert((*span, ty.inverse()));
      }
      Entry::Occupied(e) => {
        e.remove();
      }
    }
  }
}
