use std::collections::{btree_map::Entry, BTreeMap};

use vine_util::{
  idx::{Counter, IdxVec},
  unwrap_idx_vec,
};

use crate::{
  ast::Local,
  vir::{
    Interface, InterfaceId, InterfaceKind, Layer, LayerId, Port, Stage, StageId, Step, Transfer,
    WireId, VIR,
  },
};

pub fn normalize(source: &VIR) -> VIR {
  let mut normalizer = Normalizer {
    source,
    locals: source.locals,
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

  VIR {
    locals: normalizer.locals,
    layers: IdxVec::new(),
    interfaces: normalizer.interfaces,
    stages: unwrap_idx_vec(normalizer.stages),
    globals: source.globals.clone(),
  }
}

#[derive(Debug)]
struct Normalizer<'a> {
  source: &'a VIR,

  locals: Counter<Local>,
  interfaces: IdxVec<InterfaceId, Interface>,
  stages: IdxVec<StageId, Option<Stage>>,

  layer_divergence: IdxVec<LayerId, Option<LayerId>>,
  final_transfers: IdxVec<LayerId, Option<Transfer>>,
}

impl<'a> Normalizer<'a> {
  fn normalize_layer(&mut self, layer: &Layer) {
    for &stage in &layer.stages {
      let source = &self.source.stages[stage];
      let mut wire_counts = BTreeMap::<WireId, ()>::new();
      let mut stage = Stage {
        id: source.id,
        interface: source.interface,
        layer: LayerId::NONE,
        header: source.header.clone(),
        declarations: source.declarations.clone(),
        steps: Vec::new(),
        transfer: None,
        wires: source.wires,
      };
      for port in &stage.header {
        if let Port::Wire(wire) = port {
          toggle(&mut wire_counts, *wire);
        }
      }
      for step in &source.steps {
        for port in step.ports() {
          if let Port::Wire(wire) = port {
            toggle(&mut wire_counts, *wire);
          }
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
            layer: LayerId::NONE,
            header: Vec::new(),
            declarations: Vec::new(),
            steps: Vec::new(),
            transfer: None,
            wires: source.wires,
          };
          for &wire in wire_counts.keys() {
            let local = self.locals.next();
            stage.declarations.push(local);
            stage.set_local_to(local, Port::Wire(wire));
            new_stage.take_local_to(local, Port::Wire(wire));
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
            self.normalize_layer(sub_layer);
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

fn toggle<T: Ord>(wire_counts: &mut BTreeMap<T, ()>, key: T) {
  match wire_counts.entry(key) {
    Entry::Vacant(e) => *e.insert(()),
    Entry::Occupied(e) => e.remove(),
  }
}
