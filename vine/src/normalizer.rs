use vine_util::idx::IdxVec;

use crate::vir::{Layer, LayerId, Port, Step, VIR};

pub struct Normalizer<'a> {
  vir: &'a VIR,
  divergence: IdxVec<LayerId, LayerId>,
}

impl Normalizer<'_> {
  fn calc_divergence(&mut self, layer: &Layer) -> LayerId {
    let mut divergence = layer.id;
    for &stage in &layer.stages {
      let stage = &self.vir.stages[stage];
      for step in &stage.steps {
        match step {
          Step::Diverge(to, _) => {
            divergence = divergence.min(*to);
          }
          Step::Transfer(transfer) => {
            let interface = &self.vir.interfaces[transfer.interface];
            let sub_layer = &self.vir.layers[interface.layer];
            if sub_layer.parent.is_some() {
              assert_eq!(sub_layer.parent, Some(layer.id));
              divergence = divergence.min(self.calc_divergence(sub_layer));
            }
          }
          _ => {}
        }
      }
    }
    self.divergence[layer.id] = divergence;
    divergence
  }

  fn foo(&mut self, layer: &Layer) {
    let step = Step::String(Port::Erase, String::new());
    match step {
      Step::Local(local, local_use) => todo!(),
      Step::Transfer(transfer) => todo!(),
      Step::Diverge(layer_id, transfer) => todo!(),
      Step::Link(port, port1) => todo!(),
      Step::Fn(port, vec, port1) => todo!(),
      Step::Tuple(port, vec) => todo!(),
      Step::Adt(def_id, port, vec) => todo!(),
      Step::Ref(port, port1, port2) => todo!(),
      Step::ExtFn(ext_fn, port, port1, port2) => todo!(),
      Step::Dup(port, port1, port2) => todo!(),
      Step::List(port, vec) => todo!(),
      Step::String(port, _) => todo!(),
    }
  }
}
