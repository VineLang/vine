use std::{
  collections::{BTreeMap, HashMap},
  mem::take,
  ops::Range,
};

use vine_util::idx::{Counter, RangeExt};

use crate::{
  ast::{
    DynFnId, Expr, ExprKind, GenericPath, Ident, Local, LogicalOp, Pat, PatKind, Stmt, StmtKind,
    Ty, TyKind,
  },
  diag::Diag,
  visit::{VisitMut, Visitee},
};

use super::{DefId, Resolver, ValueDefKind};

impl<'core> Resolver<'core> {
  pub fn resolve_defs(&mut self) {
    self._resolve_defs(self.defs.range());
  }

  pub(crate) fn _resolve_defs(&mut self, range: Range<DefId>) {
    let mut visitor = ResolveVisitor {
      resolver: self,
      def: DefId::ROOT,
      generics: Vec::new(),
      scope: HashMap::new(),
      scope_depth: 0,
      locals: Counter::default(),
      dyn_fns: Counter::default(),
    };

    for def_id in range.iter() {
      visitor.def = def_id;

      let def = &mut visitor.resolver.defs[def_id];
      let mut value_def = def.value_def.take();
      if let Some(value_def) = &mut value_def {
        visitor.generics = take(&mut value_def.generics);
        if let Some(ty) = &mut value_def.annotation {
          visitor.visit_type(ty);
        }
        if let ValueDefKind::Expr(expr) = &mut value_def.kind {
          visitor.visit_expr(expr);
        }
        value_def.generics = take(&mut visitor.generics);
        value_def.locals = visitor.locals;
      }

      let def = &mut visitor.resolver.defs[def_id];
      def.value_def = value_def;

      let mut type_def = def.type_def.take();
      if let Some(type_def) = &mut type_def {
        if let Some(alias) = &mut type_def.alias {
          visitor.generics = take(&mut type_def.generics);
          visitor.visit_type(alias);
          type_def.generics = take(&mut visitor.generics);
        }
      }

      let def = &mut visitor.resolver.defs[def_id];
      def.type_def = type_def;

      let mut variant_def = def.variant_def.take();
      if let Some(variant_def) = &mut variant_def {
        visitor.generics = take(&mut variant_def.generics);
        visitor.visit(&mut variant_def.fields);
        variant_def.generics = take(&mut visitor.generics);
      }

      let def = &mut visitor.resolver.defs[def_id];
      def.variant_def = variant_def;

      visitor.scope.clear();
      visitor.locals = Counter::default();
    }
  }

  pub(crate) fn resolve_custom<'t>(
    &mut self,
    def: DefId,
    initial: &BTreeMap<Local, Ident<'core>>,
    local_count: &mut Counter<Local>,
    visitee: impl Visitee<'core, 't>,
  ) -> impl Iterator<Item = (Ident<'core>, Local)> {
    let mut visitor = ResolveVisitor {
      resolver: self,
      def,
      generics: Vec::new(),
      scope: initial
        .iter()
        .map(|(&l, &i)| (i, vec![ScopeEntry { depth: 0, binding: Binding::Local(l) }]))
        .collect(),
      scope_depth: 0,
      locals: *local_count,
      dyn_fns: Counter::default(),
    };
    visitor.visit(visitee);
    *local_count = visitor.locals;
    visitor.scope.into_iter().filter_map(|(k, e)| {
      let entry = e.first()?;
      if entry.depth == 0 {
        if let Binding::Local(l) = entry.binding {
          Some((k, l))
        } else {
          None
        }
      } else {
        None
      }
    })
  }
}

#[derive(Debug)]
struct ResolveVisitor<'core, 'a> {
  resolver: &'a mut Resolver<'core>,
  def: DefId,
  generics: Vec<Ident<'core>>,
  scope: HashMap<Ident<'core>, Vec<ScopeEntry>>,
  scope_depth: usize,
  locals: Counter<Local>,
  dyn_fns: Counter<DynFnId>,
}

#[derive(Debug)]
struct ScopeEntry {
  depth: usize,
  binding: Binding,
}

#[derive(Debug, Clone, Copy)]
enum Binding {
  Local(Local),
  DynFn(DynFnId),
}

impl<'core> VisitMut<'core, '_> for ResolveVisitor<'core, '_> {
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

  fn visit_stmt(&mut self, stmt: &mut Stmt<'core>) {
    self._visit_stmt(stmt);
    if let StmtKind::DynFn(d) = &mut stmt.kind {
      let id = self.dyn_fns.next();
      self.bind(d.name, Binding::DynFn(id));
      d.id = Some(id);
    }
  }

  fn visit_pat(&mut self, pat: &mut Pat<'core>) {
    if let PatKind::Adt(path, children) = &mut pat.kind {
      let non_local_err =
        match self.resolver.resolve_path(path.span, self.def, self.def, &path.path) {
          Ok(resolved) => {
            if self.resolver.defs[resolved].variant_def.is_some() {
              path.path = self.resolver.defs[resolved].canonical.clone();
              debug_assert!(path.path.absolute && path.path.resolved.is_some());
              self._visit_pat(pat);
              return;
            } else {
              Diag::BadPatternPath { span: path.span }
            }
          }
          Err(e) => e,
        };

      let (Some(ident), None) = (path.path.as_ident(), children) else {
        pat.kind = PatKind::Error(self.resolver.core.report(non_local_err));
        return;
      };
      let local = self.locals.next();
      let binding = Binding::Local(local);
      self.bind(ident, binding);
      pat.kind = PatKind::Local(local);
    }
    self._visit_pat(pat);
  }

  fn visit_expr(&mut self, expr: &mut Expr<'core>) {
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
                expr.kind = bind.binding.into();
                return;
              }
            }
            self.visit_path(path)
          }
          ExprKind::Field(_, p) => self.visit_path(p),
          ExprKind::Method(_, p, _) if p.path.as_ident().is_none() => self.visit_path(p),
          _ => Ok(()),
        };
        self._visit_expr(expr);
        if let Err(diag) = result {
          expr.kind = ExprKind::Error(self.resolver.core.report(diag));
        }
      }
    }
  }

  fn visit_type(&mut self, ty: &mut Ty<'core>) {
    if let TyKind::Path(path) = &mut ty.kind {
      if path.generics.is_none() {
        if let Some(ident) = path.path.as_ident() {
          if let Some((i, _)) = self.generics.iter().enumerate().find(|(_, &g)| g == ident) {
            ty.kind = TyKind::Generic(i);
            return;
          }
        }
      }
      if let Err(diag) = self.visit_path(path) {
        ty.kind = TyKind::Error(self.resolver.core.report(diag));
      }
    }
    self._visit_type(ty);
  }
}

impl<'core> ResolveVisitor<'core, '_> {
  fn bind(&mut self, ident: Ident<'core>, binding: Binding) {
    let stack = self.scope.entry(ident).or_default();
    let top = stack.last_mut();
    if top.as_ref().is_some_and(|x| x.depth == self.scope_depth) {
      top.unwrap().binding = binding;
    } else {
      stack.push(ScopeEntry { depth: self.scope_depth, binding })
    }
  }

  fn visit_path(&mut self, path: &mut GenericPath<'core>) -> Result<(), Diag<'core>> {
    let resolved = self.resolver.resolve_path(path.span, self.def, self.def, &path.path)?;
    path.path = self.resolver.defs[resolved].canonical.clone();
    debug_assert!(path.path.absolute && path.path.resolved.is_some());
    Ok(())
  }

  fn visit_cond(&mut self, cond: &mut Expr<'core>) {
    match &mut cond.kind {
      ExprKind![!cond] => self.visit_expr(cond),
      ExprKind::Bool(_) => {}
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

impl From<Binding> for ExprKind<'_> {
  fn from(val: Binding) -> Self {
    match val {
      Binding::Local(l) => ExprKind::Local(l),
      Binding::DynFn(d) => ExprKind::DynFn(d),
    }
  }
}
