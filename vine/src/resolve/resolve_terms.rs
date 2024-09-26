use std::{
  collections::{BTreeMap, HashMap},
  ops::Range,
};

use crate::{
  ast::{Ident, Path, Term, TermKind},
  visit::{VisitMut, Visitee},
};

use super::{NodeId, NodeValue, Resolver};

impl Resolver {
  pub fn resolve_terms(&mut self) {
    self._resolve_terms(0..self.nodes.len());
  }

  pub(crate) fn _resolve_terms(&mut self, range: Range<NodeId>) {
    let mut visitor = ResolveVisitor {
      resolver: self,
      node: 0,
      scope: HashMap::new(),
      scope_depth: 0,
      next_local: 0,
    };

    for node_id in range {
      visitor.node = node_id;

      let node = &mut visitor.resolver.nodes[node_id];
      let mut value = node.value.take();
      if let Some(NodeValue::Term(value)) = &mut value {
        visitor.visit_term(value);
      }

      let node = &mut visitor.resolver.nodes[node_id];
      node.value = value;
      node.locals = visitor.next_local;

      visitor.scope.clear();
      visitor.next_local = 0;
    }
  }

  pub(crate) fn resolve_custom<'t>(
    &mut self,
    node: NodeId,
    initial: &BTreeMap<usize, Ident>,
    local_count: &mut usize,
    visitee: &'t mut impl Visitee<'t>,
  ) -> impl Iterator<Item = (Ident, usize)> {
    let mut visitor = ResolveVisitor {
      resolver: self,
      node,
      scope: initial.iter().map(|(&l, &i)| (i, vec![ScopeEntry { depth: 0, local: l }])).collect(),
      scope_depth: 0,
      next_local: *local_count,
    };
    visitor.visit(visitee);
    *local_count = visitor.next_local;
    visitor.scope.into_iter().filter_map(|(k, e)| {
      let entry = e.first()?;
      (entry.depth == 0).then_some((k, entry.local))
    })
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
        self.visit_path(path);
      }
      TermKind::Field(_, p) => self.visit_path(p),
      TermKind::Method(_, p, _) => self.visit_path(p),
      _ => {}
    }
    self._visit_term(term);
  }
}

impl<'a> ResolveVisitor<'a> {
  fn visit_path(&mut self, path: &mut Path) {
    let resolved = self.resolver.resolve_path(self.node, path);
    *path = self.resolver.nodes[resolved].canonical.clone();
    debug_assert!(path.absolute && path.resolved.is_some());
  }
}

struct DeclareVisitor<'a, 'b>(&'a mut ResolveVisitor<'b>);

impl VisitMut<'_> for DeclareVisitor<'_, '_> {
  fn visit_term(&mut self, term: &mut Term) {
    match &mut term.kind {
      TermKind::Path(path) => {
        if let Some(resolved) = self.0.resolver.try_resolve_path(self.0.node, path) {
          if self.0.resolver.nodes[resolved].variant.is_some() {
            *path = self.0.resolver.nodes[resolved].canonical.clone();
            debug_assert!(path.absolute && path.resolved.is_some());
            return;
          }
        }

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
