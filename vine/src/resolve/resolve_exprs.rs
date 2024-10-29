use std::{
  collections::{BTreeMap, HashMap},
  mem::take,
  ops::Range,
};

use crate::{
  ast::{Expr, ExprKind, Ident, LogicalOp, Pat, PatKind, Path, Type, TypeKind},
  diag::Diag,
  visit::{VisitMut, Visitee},
};

use super::{NodeId, NodeValueKind, Resolver};

impl Resolver {
  pub fn resolve_exprs(&mut self) {
    self._resolve_exprs(0..self.nodes.len());
  }

  pub(crate) fn _resolve_exprs(&mut self, range: Range<NodeId>) {
    let mut visitor = ResolveVisitor {
      resolver: self,
      node: 0,
      generics: Vec::new(),
      scope: HashMap::new(),
      scope_depth: 0,
      next_local: 0,
    };

    for node_id in range {
      visitor.node = node_id;

      let node = &mut visitor.resolver.nodes[node_id];
      let mut value = node.value.take();
      if let Some(value) = &mut value {
        visitor.generics = take(&mut value.generics);
        if let Some(ty) = &mut value.ty {
          visitor.visit_type(ty);
        }
        if let NodeValueKind::Expr(expr) = &mut value.kind {
          visitor.visit_expr(expr);
        }
        value.generics = take(&mut visitor.generics);
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
      generics: Vec::new(),
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
  generics: Vec<Ident>,
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

  fn visit_pat(&mut self, pat: &mut Pat) {
    if let PatKind::Adt(path, children) = &mut pat.kind {
      let non_local_err = match self.resolver.resolve_path(self.node, &path.path) {
        Ok(resolved) => {
          if self.resolver.nodes[resolved].variant.is_some() {
            path.path = self.resolver.nodes[resolved].canonical.clone();
            debug_assert!(path.path.absolute && path.path.resolved.is_some());
            self._visit_pat(pat);
            return;
          } else {
            Diag::BadPatternPath { span: path.path.span }
          }
        }
        Err(e) => e,
      };

      let (Some(ident), None) = (path.path.as_ident(), children) else {
        pat.kind = PatKind::Error(self.resolver.diags.add(non_local_err));
        return;
      };
      let local = self.next_local;
      self.next_local += 1;
      let stack = self.scope.entry(ident).or_default();
      let top = stack.last_mut();
      if top.as_ref().is_some_and(|x| x.depth == self.scope_depth) {
        top.unwrap().local = local;
      } else {
        stack.push(ScopeEntry { depth: self.scope_depth, local })
      }
      pat.kind = PatKind::Local(local);
    }
    self._visit_pat(pat);
  }

  fn visit_expr(&mut self, expr: &mut Expr) {
    match &mut expr.kind {
      ExprKind![cond] => {
        self.enter_scope();
        self.visit_cond(expr);
        self.exit_scope();
      }
      ExprKind::If(cond, block, els) => {
        self.enter_scope();
        self.visit_cond(cond);
        self.visit_block(block);
        self.exit_scope();
        self.visit_expr(els);
      }
      ExprKind::While(cond, block) => {
        self.enter_scope();
        self.visit_cond(cond);
        self.visit_block(block);
        self.exit_scope();
      }
      _ => {
        let result = match &mut expr.kind {
          ExprKind::Path(path) => {
            if let Some(ident) = path.path.as_ident() {
              if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
                expr.kind = ExprKind::Local(bind.local);
                return;
              }
            }
            self.visit_path(&mut path.path)
          }
          ExprKind::Field(_, p) => self.visit_path(&mut p.path),
          ExprKind::Method(_, p, _) => self.visit_path(&mut p.path),
          _ => Ok(()),
        };
        self._visit_expr(expr);
        if let Err(diag) = result {
          expr.kind = ExprKind::Error(self.resolver.diags.add(diag));
        }
      }
    }
  }

  fn visit_type(&mut self, ty: &mut Type) {
    if let TypeKind::Path(path) = &mut ty.kind {
      if path.args.is_none() {
        if let Some(ident) = path.path.as_ident() {
          if let Some((i, _)) = self.generics.iter().enumerate().find(|(_, &g)| g == ident) {
            ty.kind = TypeKind::Generic(i);
            return;
          }
        }
      }
      if let Err(diag) = self.visit_path(&mut path.path) {
        ty.kind = TypeKind::Error(self.resolver.diags.add(diag));
      }
      self._visit_type(ty);
    }
  }
}

impl<'a> ResolveVisitor<'a> {
  fn visit_path(&mut self, path: &mut Path) -> Result<(), Diag> {
    let resolved = self.resolver.resolve_path(self.node, path)?;
    *path = self.resolver.nodes[resolved].canonical.clone();
    debug_assert!(path.absolute && path.resolved.is_some());
    Ok(())
  }

  fn visit_cond(&mut self, cond: &mut Expr) {
    match &mut cond.kind {
      ExprKind![!cond] => self.visit_expr(cond),
      ExprKind::Not(e) => {
        self.enter_scope();
        self.visit_cond(e);
        self.exit_scope();
      }
      ExprKind::Is(e, p) => {
        self.visit_expr(e);
        self.visit_pat(p);
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        self.visit_cond(a);
        self.visit_cond(b);
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        self.enter_scope();
        self.visit_cond(a);
        self.exit_scope();
        self.enter_scope();
        self.visit_cond(b);
        self.exit_scope();
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        self.enter_scope();
        self.visit_cond(a);
        self.visit_cond(b);
        self.exit_scope();
      }
    }
  }
}
