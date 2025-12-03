use crate::structures::ast::{
  Block, Expr, ExprKind, GenericArgs, GenericParams, Impl, ImplKind, Item, ItemKind, ModKind, Pat,
  PatKind, Stmt, StmtKind, Trait, TraitKind, Ty, TyKind,
};

pub trait VisitMut<'a> {
  fn visit_expr(&mut self, expr: &'a mut Expr) {
    self._visit_expr(expr);
  }

  fn visit_pat(&mut self, pat: &'a mut Pat) {
    self._visit_pat(pat);
  }

  fn visit_type(&mut self, ty: &'a mut Ty) {
    self._visit_type(ty);
  }

  fn visit_impl(&mut self, ty: &'a mut Impl) {
    self._visit_impl(ty);
  }

  fn visit_trait(&mut self, trait_: &'a mut Trait) {
    self._visit_trait(trait_);
  }

  fn visit_stmt(&mut self, stmt: &'a mut Stmt) {
    self._visit_stmt(stmt)
  }

  fn visit_item(&mut self, item: &'a mut Item) {
    self._visit_item(item);
  }

  fn visit_block(&mut self, block: &'a mut Block) {
    self._visit_block(block);
  }

  fn visit_generic_params(&mut self, generics: &'a mut GenericParams) {
    self._visit_generic_params(generics);
  }

  fn visit_generic_args(&mut self, generics: &'a mut GenericArgs) {
    self._visit_generic_args(generics);
  }

  fn _visit_expr(&mut self, expr: &'a mut Expr) {
    match &mut *expr.kind {
      ExprKind::Hole
      | ExprKind::Bool(_)
      | ExprKind::N32(_)
      | ExprKind::F32(_)
      | ExprKind::Char(_)
      | ExprKind::Error(_) => {}
      ExprKind::Paren(a)
      | ExprKind::Ref(a, _)
      | ExprKind::Deref(a, _)
      | ExprKind::Sign(_, a)
      | ExprKind::Not(a)
      | ExprKind::TupleField(a, _)
      | ExprKind::ObjectField(a, _)
      | ExprKind::Inverse(a, _)
      | ExprKind::Unwrap(a)
      | ExprKind::Try(a) => self.visit_expr(a),
      ExprKind::Assign(_, a, b)
      | ExprKind::Place(a, b)
      | ExprKind::BinaryOp(_, a, b)
      | ExprKind::BinaryOpAssign(_, a, b)
      | ExprKind::LogicalOp(_, a, b) => {
        self.visit_expr(a);
        self.visit_expr(b);
      }

      ExprKind::Path(path, args) => {
        self.visit(&mut path.generics);
        self.visit(args);
      }
      ExprKind::Do(_, a, b) | ExprKind::Loop(_, a, b) => {
        self.visit(a);
        self.visit_block(b);
      }
      ExprKind::Match(a, b, c) => {
        self.visit_expr(a);
        self.visit(b);
        for (p, b) in c {
          self.visit_pat(p);
          self.visit_block(b);
        }
      }
      ExprKind::If(cond, ty, then, else_) => {
        self.visit(cond);
        self.visit(ty);
        self.visit(then);
        self.visit(else_);
      }
      ExprKind::When(_, ty, arms, leg) => {
        self.visit(ty);
        for (cond, block) in arms {
          self.visit(cond);
          self.visit(block);
        }
        self.visit(leg);
      }
      ExprKind::While(_, a, ty, b, c) => {
        self.visit_expr(a);
        self.visit(ty);
        self.visit_block(b);
        self.visit(c);
      }
      ExprKind::Fn(_, a, b, c) => {
        self.visit(a);
        self.visit(b);
        self.visit_block(c);
      }
      ExprKind::For(_, a, b, ty, c, d) => {
        self.visit(a);
        self.visit_expr(b);
        self.visit(ty);
        self.visit(c);
        self.visit(d);
      }
      ExprKind::Tuple(a) | ExprKind::List(a) => {
        self.visit(a);
      }
      ExprKind::Call(a, b) => {
        self.visit_expr(a);
        for t in b {
          self.visit_expr(t);
        }
      }
      ExprKind::Method(a, _, _, b, c) => {
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
      ExprKind::InlineIvy(binds, ty, _, _) => {
        for (_, _, expr) in binds {
          self.visit_expr(expr);
        }
        self.visit_type(ty);
      }
      ExprKind::Cast(e, t, _) => {
        self.visit_expr(e);
        self.visit_type(t);
      }
      ExprKind::RangeExclusive(start, end) => {
        self.visit(start.as_mut());
        self.visit(end.as_mut());
      }
      ExprKind::RangeInclusive(start, end) => {
        self.visit(start.as_mut());
        self.visit(end);
      }
      ExprKind::Nat(_, _, t) => {
        self.visit(t);
      }
    }
  }

  fn _visit_pat(&mut self, pat: &'a mut Pat) {
    match &mut *pat.kind {
      PatKind::Hole | PatKind::Error(_) => {}
      PatKind::Paren(a) | PatKind::Ref(a) | PatKind::Deref(a) | PatKind::Inverse(a) => {
        self.visit_pat(a)
      }
      PatKind::Tuple(a) => {
        for t in a {
          self.visit_pat(t);
        }
      }
      PatKind::Path(p, a) => {
        self.visit(&mut p.generics);
        self.visit(a.as_deref_mut());
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

  fn _visit_type(&mut self, ty: &'a mut Ty) {
    match &mut *ty.kind {
      TyKind::Hole => {}
      TyKind::Never => {}
      TyKind::Key(_) => {}
      TyKind::Paren(t) | TyKind::Ref(t) | TyKind::Inverse(t) => self.visit_type(t),
      TyKind::Path(p) | TyKind::Fn(p) => self.visit(&mut p.generics),
      TyKind::Tuple(a) => {
        for t in a {
          self.visit_type(t);
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

  fn _visit_impl(&mut self, impl_: &'a mut Impl) {
    match &mut *impl_.kind {
      ImplKind::Hole | ImplKind::Error(_) => {}
      ImplKind::Path(p) | ImplKind::Fn(p) => self.visit(&mut p.generics),
    }
  }

  fn _visit_trait(&mut self, trait_: &'a mut Trait) {
    match &mut *trait_.kind {
      TraitKind::Error(_) => {}
      TraitKind::Path(p) => self.visit(&mut p.generics),
      TraitKind::Fn(a, b, c) => {
        self.visit(a);
        self.visit(b);
        self.visit(c);
      }
    }
  }

  fn _visit_stmt(&mut self, stmt: &'a mut Stmt) {
    match &mut stmt.kind {
      StmtKind::Assert(a) => {
        self.visit_expr(&mut a.expr);
        self.visit(&mut a.else_);
      }
      StmtKind::Let(l) => {
        if let Some(init) = &mut l.init {
          self.visit_expr(init);
        }
        self.visit_pat(&mut l.bind);
      }
      StmtKind::LetFn(d) => {
        for p in &mut d.params {
          self.visit_pat(p);
        }
        if let Some(t) = &mut d.ret {
          self.visit_type(t);
        }
        self.visit_block(&mut d.body);
      }
      StmtKind::Expr(t, _) | StmtKind::Return(Some(t)) | StmtKind::Break(_, Some(t)) => {
        self.visit_expr(t)
      }
      StmtKind::Item(i) => self.visit_item(i),
      StmtKind::Empty
      | StmtKind::Return(None)
      | StmtKind::Break(_, None)
      | StmtKind::Continue(_) => {}
    }
  }

  fn _visit_item(&mut self, item: &'a mut Item) {
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
        ModKind::Loaded(_, _, items) => {
          for item in items {
            self.visit_item(item);
          }
        }
        ModKind::Unloaded(..) | ModKind::Error(_) => {}
      },
      ItemKind::Struct(s) => {
        self.visit(&mut s.data);
      }
      ItemKind::Enum(e) => {
        for v in &mut e.variants {
          self.visit(&mut v.data);
        }
      }
      ItemKind::Type(t) => {
        self.visit(&mut t.ty);
      }
      ItemKind::Trait(t) => {
        self.visit(&mut t.items);
      }
      ItemKind::Impl(t) => match &mut t.kind {
        super::ImplItemKind::Direct(_, items) => self.visit(items),
        super::ImplItemKind::Indirect(impl_) => self.visit(impl_),
      },
      ItemKind::Use(..) | ItemKind::OuterMod | ItemKind::Taken => {}
    }
  }

  fn _visit_block(&mut self, block: &'a mut Block) {
    for stmt in &mut block.stmts {
      self.visit_stmt(stmt);
    }
  }

  fn _visit_generic_params(&mut self, generics: &'a mut GenericParams) {
    self.visit(generics.impls.iter_mut().map(|x| &mut x.trait_));
  }

  fn _visit_generic_args(&mut self, generics: &'a mut GenericArgs) {
    self.visit(&mut generics.types);
    self.visit(&mut generics.impls);
  }

  fn visit(&mut self, visitee: impl Visitee<'a>) {
    visitee.visit(self);
  }
}

pub trait Visitee<'t>: Sized {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized));
}

impl<'t> Visitee<'t> for &'t mut Expr {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_expr(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Pat {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_pat(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Ty {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_type(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Impl {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_impl(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Trait {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_trait(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Stmt {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_stmt(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Item {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_item(self)
  }
}

impl<'t> Visitee<'t> for &'t mut Block {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_block(self)
  }
}

impl<'t> Visitee<'t> for &'t mut GenericArgs {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_generic_args(self)
  }
}

impl<'t> Visitee<'t> for &'t mut GenericParams {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_generic_params(self)
  }
}

impl<'t, I: IntoIterator<Item = T>, T: Visitee<'t>> Visitee<'t> for I {
  fn visit(self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    for item in self {
      visitor.visit(item)
    }
  }
}
