use crate::{
  ast::{
    Block, Expr, ExprKind, GenericPath, Item, ItemKind, ModKind, Pat, PatKind, Stmt, StmtKind, Ty,
    TyKind,
  },
  resolve::{Def, ValueDefKind},
};

pub trait VisitMut<'a> {
  fn enter_scope(&mut self) {}
  fn exit_scope(&mut self) {}

  fn visit_def(&mut self, def: &'a mut Def) {
    self._visit_def(def)
  }

  fn _visit_def(&mut self, def: &'a mut Def) {
    if let Some(value_def) = &mut def.value_def {
      if let ValueDefKind::Expr(expr) = &mut value_def.kind {
        self.visit_expr(expr);
      }
    }
  }

  fn visit_expr(&mut self, expr: &'a mut Expr) {
    self._visit_expr(expr);
  }

  fn visit_pat(&mut self, pat: &'a mut Pat) {
    self._visit_pat(pat);
  }

  fn visit_type(&mut self, ty: &'a mut Ty) {
    self._visit_type(ty);
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

  fn visit_generic_path(&mut self, path: &'a mut GenericPath) {
    self._visit_generic_path(path)
  }

  fn _visit_expr(&mut self, expr: &'a mut Expr) {
    match &mut expr.kind {
      ExprKind::Hole
      | ExprKind::Local(_)
      | ExprKind::DynFn(_)
      | ExprKind::U32(_)
      | ExprKind::F32(_)
      | ExprKind::String(_)
      | ExprKind::Continue
      | ExprKind::Error(_)
      | ExprKind::CopyLocal(_)
      | ExprKind::MoveLocal(_)
      | ExprKind::Return(None)
      | ExprKind::Break(None)
      | ExprKind::SetLocal(_) => {}
      ExprKind::Paren(a)
      | ExprKind::Ref(a)
      | ExprKind::Deref(a)
      | ExprKind::Move(a)
      | ExprKind::Field(a, _)
      | ExprKind::Neg(a)
      | ExprKind::Not(a)
      | ExprKind::Break(Some(a))
      | ExprKind::Return(Some(a))
      | ExprKind::Inverse(a)
      | ExprKind::Copy(a)
      | ExprKind::Set(a)
      | ExprKind::Temp(a) => self.visit_expr(a),
      ExprKind::Assign(a, b)
      | ExprKind::BinaryOp(_, a, b)
      | ExprKind::BinaryOpAssign(_, a, b)
      | ExprKind::LogicalOp(_, a, b) => {
        self.visit_expr(a);
        self.visit_expr(b);
      }

      ExprKind::Path(p) => self.visit_generic_path(p),
      ExprKind::Block(b) | ExprKind::Loop(b) => self.visit_block(b),
      ExprKind::Match(a, b) => {
        self.visit_expr(a);
        for (t, u) in b {
          self.enter_scope();
          self.visit_pat(t);
          self.visit_expr(u);
          self.exit_scope();
        }
      }
      ExprKind::If(a, b, c) => {
        self.visit_expr(a);
        self.visit_block(b);
        self.visit_expr(c);
      }
      ExprKind::While(a, b) => {
        self.visit_expr(a);
        self.visit_block(b);
      }
      ExprKind::For(a, b, c) => {
        self.enter_scope();
        self.visit_expr(b);
        self.visit_pat(a);
        self.visit_block(c);
        self.exit_scope();
      }
      ExprKind::Fn(a, b, c) => {
        self.enter_scope();
        for (p, t) in a {
          self.visit_pat(p);
          if let Some(t) = t {
            self.visit_type(t);
          }
        }
        if let Some(Some(b)) = b {
          self.visit_type(b);
        }
        self.visit_expr(c);
        self.exit_scope();
      }
      ExprKind::Tuple(a) | ExprKind::List(a) => {
        for t in a {
          self.visit_expr(t);
        }
      }
      ExprKind::Call(a, b) => {
        self.visit_expr(a);
        for t in b {
          self.visit_expr(t);
        }
      }
      ExprKind::Method(a, p, b) => {
        self.visit_expr(a);
        self.visit_generic_path(p);
        for t in b {
          self.visit_expr(t);
        }
      }
      ExprKind::ComparisonOp(a, b) => {
        self.visit_expr(a);
        for (_, t) in b {
          self.visit_expr(t);
        }
      }
      ExprKind::Is(e, p) => {
        self.visit_expr(e);
        self.visit_pat(p);
      }
    }
  }

  fn _visit_pat(&mut self, pat: &'a mut Pat) {
    match &mut pat.kind {
      PatKind::Hole | PatKind::Local(_) | PatKind::Error(_) => {}
      PatKind::Paren(a)
      | PatKind::Ref(a)
      | PatKind::Deref(a)
      | PatKind::Move(a)
      | PatKind::Inverse(a) => self.visit_pat(a),
      PatKind::Tuple(a) => {
        for t in a {
          self.visit_pat(t);
        }
      }
      PatKind::Adt(p, a) => {
        self.visit_generic_path(p);
        if let Some(a) = a {
          for t in a {
            self.visit_pat(t);
          }
        }
      }
    }
  }

  fn _visit_type(&mut self, ty: &'a mut Ty) {
    match &mut ty.kind {
      TyKind::Hole | TyKind::Generic(_) => {}
      TyKind::Paren(t) | TyKind::Ref(t) | TyKind::Inverse(t) => self.visit_type(t),
      TyKind::Path(p) => self.visit_generic_path(p),
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
      TyKind::Error(_) => {}
    }
  }

  fn _visit_stmt(&mut self, stmt: &'a mut Stmt) {
    match &mut stmt.kind {
      StmtKind::Let(l) => {
        if let Some(ty) = &mut l.ty {
          self.visit_type(ty);
        }
        if let Some(init) = &mut l.init {
          self.visit_expr(init);
        }
        self.visit_pat(&mut l.bind);
      }
      StmtKind::DynFn(d) => {
        self.enter_scope();
        for (p, t) in &mut d.params {
          self.visit_pat(p);
          if let Some(t) = t {
            self.visit_type(t);
          }
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

  fn _visit_item(&mut self, item: &'a mut Item) {
    match &mut item.kind {
      ItemKind::Fn(f) => {
        for (param, ty) in &mut f.params {
          self.visit_pat(param);
          if let Some(ty) = ty {
            self.visit_type(ty);
          }
        }
        if let Some(ty) = &mut f.ret {
          self.visit_type(ty);
        }
        self.visit_block(&mut f.body);
      }
      ItemKind::Const(c) => {
        self.visit_type(&mut c.ty);
        self.visit_expr(&mut c.value);
      }
      ItemKind::Mod(m) => match &mut m.kind {
        ModKind::Loaded(items) => {
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
      ItemKind::Pattern(_) => todo!(),
      ItemKind::Use(..) | ItemKind::Ivy(_) | ItemKind::Taken => {}
    }
  }

  fn _visit_block(&mut self, block: &'a mut Block) {
    self.enter_scope();
    for stmt in &mut block.stmts {
      self.visit_stmt(stmt);
    }
    self.exit_scope();
  }

  fn _visit_generic_path(&mut self, path: &'a mut GenericPath) {
    if let Some(args) = &mut path.generics {
      for t in args {
        self.visit_type(t)
      }
    }
  }

  fn visit(&mut self, visitee: &'a mut (impl Visitee<'a> + ?Sized)) {
    visitee.visit(self);
  }
}

pub trait Visitee<'t> {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized));
}

impl<'t> Visitee<'t> for Def {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_def(self)
  }
}

impl<'t> Visitee<'t> for Expr {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_expr(self)
  }
}

impl<'t> Visitee<'t> for Pat {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_pat(self)
  }
}

impl<'t> Visitee<'t> for Ty {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_type(self)
  }
}

impl<'t> Visitee<'t> for Stmt {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_stmt(self)
  }
}

impl<'t> Visitee<'t> for Item {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_item(self)
  }
}

impl<'t> Visitee<'t> for Block {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_block(self)
  }
}

impl<'t> Visitee<'t> for GenericPath {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_generic_path(self)
  }
}

impl<'t, I: ?Sized, T: Visitee<'t> + 't + ?Sized> Visitee<'t> for I
where
  for<'a> &'a mut I: IntoIterator<Item = &'a mut T>,
{
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    for item in self {
      visitor.visit(item)
    }
  }
}
