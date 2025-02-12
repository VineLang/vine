use crate::ast::{
  Block, Expr, ExprKind, GenericArgs, GenericParams, Impl, ImplKind, Item, ItemKind, ModKind, Pat,
  PatKind, Stmt, StmtKind, Trait, TraitKind, Ty, TyKind,
};

pub trait VisitMut<'core, 'a> {
  fn enter_scope(&mut self) {}
  fn exit_scope(&mut self) {}

  fn visit_expr(&mut self, expr: &'a mut Expr<'core>) {
    self._visit_expr(expr);
  }

  fn visit_pat(&mut self, pat: &'a mut Pat<'core>) {
    self._visit_pat(pat);
  }

  fn visit_type(&mut self, ty: &'a mut Ty<'core>) {
    self._visit_type(ty);
  }

  fn visit_impl(&mut self, ty: &'a mut Impl<'core>) {
    self._visit_impl(ty);
  }

  fn visit_trait(&mut self, trait_: &'a mut Trait<'core>) {
    self._visit_trait(trait_);
  }

  fn visit_stmt(&mut self, stmt: &'a mut Stmt<'core>) {
    self._visit_stmt(stmt)
  }

  fn visit_item(&mut self, item: &'a mut Item<'core>) {
    self._visit_item(item);
  }

  fn visit_block(&mut self, block: &'a mut Block<'core>) {
    self._visit_block(block);
  }

  fn visit_generic_params(&mut self, generics: &'a mut GenericParams<'core>) {
    self._visit_generic_params(generics);
  }

  fn visit_generic_args(&mut self, generics: &'a mut GenericArgs<'core>) {
    self._visit_generic_args(generics);
  }

  fn _visit_expr(&mut self, expr: &'a mut Expr<'core>) {
    match &mut expr.kind {
      ExprKind::Hole
      | ExprKind::Local(_)
      | ExprKind::DynFn(_)
      | ExprKind::Bool(_)
      | ExprKind::N32(_)
      | ExprKind::F32(_)
      | ExprKind::Char(_)
      | ExprKind::Continue(_)
      | ExprKind::Error(_)
      | ExprKind::CopyLocal(_)
      | ExprKind::MoveLocal(_)
      | ExprKind::Return(None)
      | ExprKind::Break(_, None)
      | ExprKind::SetLocal(_)
      | ExprKind::HedgeLocal(_)
      | ExprKind::Rel(..) => {}
      ExprKind::Paren(a)
      | ExprKind::Ref(a, _)
      | ExprKind::Deref(a, _)
      | ExprKind::Move(a, _)
      | ExprKind::Neg(a)
      | ExprKind::Not(a)
      | ExprKind::Break(_, Some(a))
      | ExprKind::Return(Some(a))
      | ExprKind::TupleField(a, _, _)
      | ExprKind::ObjectField(a, _)
      | ExprKind::Inverse(a, _)
      | ExprKind::Copy(a)
      | ExprKind::Hedge(a)
      | ExprKind::Set(a) => self.visit_expr(a),
      ExprKind::Assign(_, a, b)
      | ExprKind::Place(a, b)
      | ExprKind::BinaryOp(_, a, b)
      | ExprKind::BinaryOpAssign(_, a, b)
      | ExprKind::LogicalOp(_, a, b) => {
        self.visit_expr(a);
        self.visit_expr(b);
      }

      ExprKind::Path(path) => self.visit(&mut path.generics),
      ExprKind::Def(_, g) => self.visit(g),
      ExprKind::Do(_, b) | ExprKind::Loop(_, b) => self.visit_block(b),
      ExprKind::Match(a, b) => {
        self.visit_expr(a);
        for (p, b) in b {
          self.enter_scope();
          self.visit_pat(p);
          self.visit_block(b);
          self.exit_scope();
        }
      }
      ExprKind::If(arms, leg) => {
        for (cond, block) in arms {
          self.enter_scope();
          self.visit(cond);
          self.visit(block);
          self.exit_scope();
        }
        self.visit(leg);
      }
      ExprKind::While(_, a, b) => {
        self.visit_expr(a);
        self.visit_block(b);
      }
      ExprKind::Fn(a, b, c) => {
        self.enter_scope();
        self.visit(a);
        self.visit(b);
        self.visit_block(c);
        self.exit_scope();
      }
      ExprKind::Tuple(a) | ExprKind::List(a) => {
        self.visit(a);
      }
      ExprKind::Adt(_, _, a, b) => {
        self.visit(a);
        self.visit(b);
      }
      ExprKind::Call(a, b) => {
        self.visit_expr(a);
        for t in b {
          self.visit_expr(t);
        }
      }
      ExprKind::Method(a, _, b, c) => {
        self.visit_expr(a);
        self.visit(b);
        self.visit(c);
      }
      ExprKind::ComparisonOp(a, b) => {
        self.visit_expr(a);
        self.visit(b.iter_mut().map(|(_, b)| b));
      }
      ExprKind::Is(e, p) => {
        self.visit_expr(e);
        self.visit_pat(p);
      }
      ExprKind::Object(o) => {
        self.visit(o.iter_mut().map(|(_, v)| v));
      }
      ExprKind::String(_, i) => {
        self.visit(i.iter_mut().map(|x| &mut x.0));
      }
    }
  }

  fn _visit_pat(&mut self, pat: &'a mut Pat<'core>) {
    match &mut pat.kind {
      PatKind::Hole | PatKind::Local(_) | PatKind::Error(_) => {}
      PatKind::Paren(a) | PatKind::Ref(a) | PatKind::Deref(a) | PatKind::Inverse(a) => {
        self.visit_pat(a)
      }
      PatKind::Tuple(a) => {
        for t in a {
          self.visit_pat(t);
        }
      }
      PatKind::PathCall(p, a) => {
        self.visit(&mut p.generics);
        self.visit(a);
      }
      PatKind::Adt(_, _, a, b) => {
        self.visit(a);
        self.visit(b);
      }
      PatKind::Annotation(p, t) => {
        self.visit_pat(p);
        self.visit_type(t);
      }
      PatKind::Object(o) => {
        for (_, v) in o {
          self.visit_pat(v);
        }
      }
    }
  }

  fn _visit_type(&mut self, ty: &'a mut Ty<'core>) {
    match &mut ty.kind {
      TyKind::Hole | TyKind::Param(_) => {}
      TyKind::Paren(t) | TyKind::Ref(t) | TyKind::Inverse(t) => self.visit_type(t),
      TyKind::Path(p) => self.visit(&mut p.generics),
      TyKind::Def(_, a) => self.visit(a),
      TyKind::Tuple(a) => {
        for t in a {
          self.visit_type(t);
        }
      }
      TyKind::Fn(a, r) => {
        for t in a {
          self.visit_type(t);
        }
        if let Some(r) = r {
          self.visit_type(r);
        }
      }
      TyKind::Object(o) => {
        for (_, v) in o {
          self.visit_type(v);
        }
      }
      TyKind::Error(_) => {}
    }
  }

  fn _visit_impl(&mut self, impl_: &'a mut Impl<'core>) {
    match &mut impl_.kind {
      ImplKind::Hole | ImplKind::Param(_) | ImplKind::Error(_) => {}
      ImplKind::Path(p) => self.visit(&mut p.generics),
      ImplKind::Def(_, a) => self.visit(a),
    }
  }

  fn _visit_trait(&mut self, trait_: &'a mut Trait<'core>) {
    match &mut trait_.kind {
      TraitKind::Error(_) => {}
      TraitKind::Path(p) => self.visit(&mut p.generics),
      TraitKind::Def(_, a) => self.visit(a),
    }
  }

  fn _visit_stmt(&mut self, stmt: &'a mut Stmt<'core>) {
    match &mut stmt.kind {
      StmtKind::Let(l) => {
        if let Some(init) = &mut l.init {
          self.visit_expr(init);
        }
        if let Some(block) = &mut l.else_block {
          self.visit_block(block);
        }
        self.visit_pat(&mut l.bind);
      }
      StmtKind::DynFn(d) => {
        self.enter_scope();
        for p in &mut d.params {
          self.visit_pat(p);
        }
        if let Some(t) = &mut d.ret {
          self.visit_type(t);
        }
        self.visit_block(&mut d.body);
        self.exit_scope();
      }
      StmtKind::Expr(t, _) => self.visit_expr(t),
      StmtKind::Item(i) => self.visit_item(i),
      StmtKind::Empty => {}
    }
  }

  fn _visit_item(&mut self, item: &'a mut Item<'core>) {
    match &mut item.kind {
      ItemKind::Fn(f) => {
        for p in &mut f.params {
          self.visit_pat(p);
        }
        if let Some(ty) = &mut f.ret {
          self.visit_type(ty);
        }
        self.visit(&mut f.body);
      }
      ItemKind::Const(c) => {
        self.visit_type(&mut c.ty);
        self.visit(&mut c.value);
      }
      ItemKind::Mod(m) => match &mut m.kind {
        ModKind::Loaded(_, items) => {
          for item in items {
            self.visit_item(item);
          }
        }
        ModKind::Unloaded(..) | ModKind::Error(_) => {}
      },
      ItemKind::Struct(s) => {
        for ty in &mut s.fields {
          self.visit_type(ty);
        }
      }
      ItemKind::Enum(e) => {
        for v in &mut e.variants {
          for ty in &mut v.fields {
            self.visit_type(ty);
          }
        }
      }
      ItemKind::Type(t) => {
        self.visit_type(&mut t.ty);
      }
      ItemKind::Trait(t) => {
        self.visit(&mut t.items);
      }
      ItemKind::Impl(t) => {
        self.visit(&mut t.items);
      }
      ItemKind::Use(..) | ItemKind::Ivy(_) | ItemKind::Taken => {}
    }
  }

  fn _visit_block(&mut self, block: &'a mut Block<'core>) {
    self.enter_scope();
    for stmt in &mut block.stmts {
      self.visit_stmt(stmt);
    }
    self.exit_scope();
  }

  fn _visit_generic_params(&mut self, generics: &'a mut GenericParams<'core>) {
    let x = generics.impls.iter_mut().map(|x| &mut x.1);
    self.visit(x);
  }

  fn _visit_generic_args(&mut self, generics: &'a mut GenericArgs<'core>) {
    self.visit(&mut generics.types);
    self.visit(&mut generics.impls);
  }

  fn visit(&mut self, visitee: impl Visitee<'core, 'a>) {
    visitee.visit(self);
  }
}

pub trait Visitee<'core, 't>: Sized {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized));
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Expr<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_expr(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Pat<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_pat(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Ty<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_type(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Impl<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_impl(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Trait<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_trait(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Stmt<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_stmt(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Item<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_item(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut Block<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_block(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut GenericArgs<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_generic_args(self)
  }
}

impl<'core: 't, 't> Visitee<'core, 't> for &'t mut GenericParams<'core> {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    visitor.visit_generic_params(self)
  }
}

impl<'core: 't, 't, I: IntoIterator<Item = T>, T: Visitee<'core, 't>> Visitee<'core, 't> for I {
  fn visit(self, visitor: &mut (impl VisitMut<'core, 't> + ?Sized)) {
    for item in self {
      visitor.visit(item)
    }
  }
}
