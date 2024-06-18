use std::collections::HashMap;

use crate::{
  ast::{Ident, Term, TermKind},
  visit::VisitMut,
};

use super::{NodeId, Resolver};

impl Resolver {
  pub fn resolve_terms(&mut self) {
    let mut visitor = ResolveVisitor {
      resolver: self,
      node: 0,
      scope: HashMap::new(),
      scope_depth: 0,
      next_local: 0,
    };

    for node_id in 0..visitor.resolver.nodes.len() {
      visitor.node = node_id;

      let mut value = visitor.resolver.nodes[node_id].value.take();
      if let Some(value) = &mut value {
        visitor.visit_term(value);
      }
      visitor.resolver.nodes[node_id].value = value;

      visitor.scope.clear();
      visitor.next_local = 0;
    }
  }
}

struct ResolveVisitor<'a> {
  resolver: &'a mut Resolver,
  node: NodeId,
  scope: HashMap<Ident, Vec<ScopeEntry>>,
  scope_depth: usize,
  next_local: usize,
}

struct ScopeEntry {
  depth: usize,
  local: usize,
}

impl VisitMut<'_> for ResolveVisitor<'_> {
  fn enter_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn exit_scope(&mut self) {
    self.scope_depth -= 1;
    for stack in self.scope.values_mut() {
      if stack.last().is_some_and(|x| x.depth > self.scope_depth) {
        stack.pop();
      }
    }
  }

  fn visit_bind(&mut self, term: &mut Term) {
    DeclareVisitor(self).visit_term(term);
  }

  fn visit_term(&mut self, term: &mut Term) {
    match &mut term.kind {
      TermKind::Path(path) => {
        if let Some(ident) = path.as_ident() {
          if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
            term.kind = TermKind::Local(bind.local);
            return;
          }
        }
        if !path.absolute {
          let resolved = self.resolver.resolve_path(self.node, path);
          *path = self.resolver.nodes[resolved].canonical.clone();
          debug_assert!(path.absolute);
        }
      }
      _ => self._visit_term(term),
    }
  }
}

struct DeclareVisitor<'a, 'b>(&'a mut ResolveVisitor<'b>);

impl VisitMut<'_> for DeclareVisitor<'_, '_> {
  fn visit_term(&mut self, term: &mut Term) {
    match &mut term.kind {
      TermKind::Path(path) => {
        let ident = path.as_ident().unwrap();
        let local = self.0.next_local;
        self.0.next_local += 1;
        let stack = self.0.scope.entry(ident).or_default();
        let top = stack.last_mut();
        if top.as_ref().is_some_and(|x| x.depth == self.0.scope_depth) {
          top.unwrap().local = local;
        } else {
          stack.push(ScopeEntry { depth: self.0.scope_depth, local })
        }
        term.kind = TermKind::Local(local);
      }
      _ => self._visit_term(term),
    }
  }
}
