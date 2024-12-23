use std::collections::{btree_map::Entry, BTreeMap};

use vine_util::idx::IdxVec;

use crate::{
  ast::Local,
  vir::{Layer, LayerId, Port, Stage, StageId, Step, Transfer, WireId, VIR},
};

pub struct Normalizer<'a> {
  vir: &'a VIR,
  out_stages: IdxVec<StageId, Option<Stage>>,
  divergence: IdxVec<LayerId, Option<LayerId>>,
  ends: IdxVec<LayerId, Option<Transfer>>,
}

const CONVERGENT: LayerId = LayerId(usize::MAX);

impl<'a> Normalizer<'a> {
  pub fn new(vir: &'a VIR) -> Self {
    todo!()
  }

  fn new_local(&mut self) -> Local {
    todo!()
  }

  pub fn normalize_layer(&mut self, layer: &Layer) {
    for &stage in &layer.stages {
      let stage = &self.vir.stages[stage];
      let mut wire_counts = BTreeMap::<WireId, ()>::new();
      let mut out_stage = Stage {
        id: stage.id,
        interface: stage.interface,
        layer: stage.layer,
        declarations: stage.declarations.clone(),
        steps: Vec::new(),
        transfer: None,
        wire: stage.wire,
      };
      for step in &stage.steps {
        for port in step.ports() {
          if let Port::Wire(wire) = port {
            match wire_counts.entry(*wire) {
              Entry::Vacant(e) => *e.insert(()),
              Entry::Occupied(e) => e.remove(),
            }
          }
        }
        if self.step_divergence(step) <= layer.id {
          let id = self.out_stages.push(None);
          let mut new_stage: Stage = Stage {
            id,
            interface: todo(),
            layer: todo(),
            declarations: Vec::new(),
            steps: Vec::new(),
            transfer: None,
            wire: stage.wire,
          };
          for &wire in wire_counts.keys() {
            let local = self.new_local();
            out_stage.declarations.push(local);
            out_stage.set_local_to(local, Port::Wire(wire));
            new_stage.take_local_to(local, Port::Wire(wire));
          }
          match step {
            Step::Transfer(transfer) => {
              let interface = &self.vir.interfaces[transfer.interface];
              let sub_layer = &self.vir.layers[interface.layer];
              self.ends[sub_layer.id] = Some(Transfer::unconditional(new_stage.interface));
              self.normalize_layer(sub_layer);
              out_stage.steps.push(Step::Transfer(transfer.clone()));
            }
            Step::Diverge(super_layer, transfer) => {
              let transfer = transfer.clone().or(self.ends[*super_layer].clone());
              if let Some(transfer) = transfer {
                out_stage.steps.push(Step::Transfer(transfer));
              }
            }
            _ => unreachable!(),
          }
          let id = out_stage.id;
          self.out_stages[id] = Some(out_stage);
          out_stage = new_stage;
        } else {
          if let Step::Transfer(transfer) = step {
            let interface = &self.vir.interfaces[transfer.interface];
            let sub_layer = &self.vir.layers[interface.layer];
            self.normalize_layer(sub_layer);
          }
          out_stage.steps.push(step.clone());
        }
      }
      let transfer = stage.transfer.clone().or(self.ends[layer.id].clone());
      if let Some(transfer) = transfer {
        out_stage.steps.push(Step::Transfer(transfer));
      }
      let id = out_stage.id;
      self.out_stages[id] = Some(out_stage);
    }
  }

  fn layer_divergence(&mut self, layer: &Layer) -> LayerId {
    if let Some(divergence) = self.divergence[layer.id] {
      return divergence;
    }
    let mut divergence = layer.id;
    for &stage in &layer.stages {
      let stage = &self.vir.stages[stage];
      for step in &stage.steps {
        divergence = divergence.min(self.step_divergence(step));
      }
    }
    self.divergence[layer.id] = Some(divergence);
    divergence
  }

  fn step_divergence(&mut self, step: &Step) -> LayerId {
    match step {
      Step::Diverge(divergence, _) => *divergence,
      Step::Transfer(transfer) => {
        let interface = &self.vir.interfaces[transfer.interface];
        let sub_layer = &self.vir.layers[interface.layer];
        if sub_layer.parent.is_some() {
          self.layer_divergence(sub_layer)
        } else {
          CONVERGENT
        }
      }
      _ => CONVERGENT,
    }
  }
}

fn todo<T>() -> T {
  todo!()
}
