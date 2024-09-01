use crate::ast::Nets;

mod eta_reduce;
mod inline_globals;
mod inline_vars;
mod pre_reduce;
mod prune;

use inline_globals::inline_globals;
use inline_vars::InlineVars;
use prune::prune;

#[derive(Default)]
pub struct Optimizer {
  inline_vars: InlineVars,
}

impl Optimizer {
  pub fn optimize(&mut self, nets: &mut Nets) {
    prune(nets);
    for net in nets.values_mut() {
      self.inline_vars.apply(net, false);
      net.eta_reduce();
      self.inline_vars.apply(net, false);
    }
    inline_globals(nets);
    prune(nets);
    nets.pre_reduce();
    prune(nets);
    for net in nets.values_mut() {
      self.inline_vars.apply(net, true);
    }
  }
}
