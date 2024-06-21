use crate::ast::Nets;

mod inline_vars;

use inline_vars::InlineVars;

#[derive(Default)]
pub struct Optimizer {
  inline_vars: InlineVars,
}

impl Optimizer {
  pub fn optimize(&mut self, nets: &mut Nets) {
    for net in nets.values_mut() {
      self.inline_vars.apply(net);
    }
  }
}
