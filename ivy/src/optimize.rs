use crate::ast::Nets;

mod eta_reduce;
mod inline_globals;
mod inline_vars;
mod pre_reduce;
mod prune;

use inline_globals::inline_globals;
use inline_vars::InlineVars;
use pre_reduce::pre_reduce;
use prune::prune;

#[derive(Default)]
pub struct Optimizer {
  inline_vars: InlineVars,
}

impl Optimizer {
  pub fn optimize(&mut self, nets: &mut Nets, keep: &[String]) {
    self._optimize(nets, keep);
    pre_reduce(nets);
    self._optimize(nets, keep);
  }

  fn _optimize(&mut self, nets: &mut Nets, keep: &[String]) {
    prune(nets, keep);
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
    prune(nets, keep);
  }
}
