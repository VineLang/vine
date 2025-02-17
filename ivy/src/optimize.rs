use crate::{ast::Nets, host::Host};

mod eta_reduce;
mod inline_globals;
mod inline_vars;
mod prune;

use inline_globals::inline_globals;
use inline_vars::InlineVars;
use ivm::{heap::Heap, port::Port, IVM};
use prune::prune;

#[derive(Default)]
pub struct Optimizer {
  inline_vars: InlineVars,
}

impl Optimizer {
  pub fn optimize(&mut self, nets: &mut Nets) {
    prune(nets);
    loop {
      for (_, net) in nets.iter_mut() {
        loop {
          self.inline_vars.apply(net);
          let reduced = net.eta_reduce();
          if !reduced {
            break;
          }
        }
      }
      let inlined = inline_globals(nets);
      if !inlined {
        break;
      }
    }
    prune(nets);

    let heap = &Heap::new();
    let mut host = &mut Host::default();
    host.insert_nets(nets);

    for (name, net) in nets.iter_mut() {
      if name.contains("::main") {
        continue;
      }
      let ivm = &mut IVM::new(heap);
      let r = ivm.new_wire();
      ivm.execute(&host.get(name).unwrap().instructions, Port::new_wire(r.0));
      ivm.normalize();
      net.root = host.read(ivm, &Port::new_wire(r.1));
      net.pairs.clear();
    }
  }
}
