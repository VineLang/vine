use crate::ast::Nets;

mod inline_vars;
mod prune;

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
      self.inline_vars.apply(net);
    }
  }
}
