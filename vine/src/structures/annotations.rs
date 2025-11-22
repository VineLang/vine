use std::collections::{BTreeMap, BTreeSet};

use crate::structures::ast::Span;

#[derive(Default, Debug, Clone)]
pub struct Annotations {
  pub definitions: BTreeMap<Span, BTreeSet<Span>>,
  pub references: BTreeMap<Span, BTreeSet<Span>>,
  pub hovers: BTreeMap<Span, Vec<String>>,
}

impl Annotations {
  pub fn record_reference(&mut self, ref_span: Span, def_span: Span) {
    self.references.entry(def_span).or_default().insert(ref_span);
    self.definitions.entry(ref_span).or_default().insert(def_span);
  }

  pub fn record_hover(&mut self, span: Span, hover: String) {
    self.hovers.entry(span).or_default().push(hover);
  }
}
