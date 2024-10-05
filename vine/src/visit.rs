use crate::{
  ast::{Block, Expr, ExprKind, Item, ItemKind, ModKind, Pat, PatKind, Stmt, StmtKind},
  resolve::{Node, NodeValue},
};

pub trait VisitMut<'a> {
  fn enter_scope(&mut self) {}
  fn exit_scope(&mut self) {}

  fn visit_node(&mut self, node: &'a mut Node) {
    self._visit_node(node)
  }

  fn _visit_node(&mut self, node: &'a mut Node) {
    if let Some(NodeValue::Expr(value)) = &mut node.value {
      self.visit_expr(value);
    }
  }

  fn visit_expr(&mut self, expr: &'a mut Expr) {
    self._visit_expr(expr);
  }

  fn visit_pat(&mut self, pat: &'a mut Pat) {
    self._visit_pat(pat);
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

  fn _visit_expr(&mut self, expr: &'a mut Expr) {
    match &mut expr.kind {
      ExprKind::Hole
      | ExprKind::Path(_)
      | ExprKind::Local(_)
      | ExprKind::U32(_)
      | ExprKind::F32(_)
      | ExprKind::String(_)
      | ExprKind::Break
      | ExprKind::Error(_)
      | ExprKind::CopyLocal(_)
      | ExprKind::MoveLocal(_)
      | ExprKind::SetLocal(_) => {}
      ExprKind::Ref(a)
      | ExprKind::Deref(a)
      | ExprKind::Move(a)
      | ExprKind::Field(a, _)
      | ExprKind::UnaryOp(_, a)
      | ExprKind::Return(a)
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
      ExprKind::WhileLet(a, b, c) => {
        self.visit_pat(a);
        self.visit_expr(b);
        self.visit_block(c);
      }
      ExprKind::For(a, b, c) => {
        self.enter_scope();
        self.visit_expr(b);
        self.visit_pat(a);
        self.visit_block(c);
        self.exit_scope();
      }
      ExprKind::Fn(a, b) => {
        self.enter_scope();
        for t in a {
          self.visit_pat(t);
        }
        self.visit_expr(b);
        self.exit_scope();
      }
      ExprKind::Tuple(a) | ExprKind::List(a) => {
        for t in a {
          self.visit_expr(t);
        }
      }
      ExprKind::Call(a, b) | ExprKind::Method(a, _, b) => {
        self.visit_expr(a);
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
    }
  }

  fn _visit_pat(&mut self, pat: &'a mut Pat) {
    match &mut pat.kind {
      PatKind::Hole | PatKind::Local(_) | PatKind::Error(_) | PatKind::Adt(_, None) => {}
      PatKind::Ref(a) | PatKind::Deref(a) | PatKind::Move(a) | PatKind::Inverse(a) => {
        self.visit_pat(a)
      }
      PatKind::Adt(_, Some(a)) | PatKind::Tuple(a) => {
        for t in a {
          self.visit_pat(t);
        }
      }
    }
  }

  fn _visit_stmt(&mut self, stmt: &'a mut Stmt) {
    match &mut stmt.kind {
      StmtKind::Let(l) => {
        if let Some(init) = &mut l.init {
          self.visit_expr(init);
        }
        self.visit_pat(&mut l.bind);
      }
      StmtKind::Expr(t, _) => self.visit_expr(t),
      StmtKind::Item(i) => self.visit_item(i),
      StmtKind::Empty => {}
    }
  }

  fn _visit_item(&mut self, item: &'a mut Item) {
    match &mut item.kind {
      ItemKind::Fn(f) => {
        for param in &mut f.params {
          self.visit_pat(param);
        }
        self.visit_expr(&mut f.body);
      }
      ItemKind::Const(c) => {
        self.visit_expr(&mut c.value);
      }
      ItemKind::Mod(m) => match &mut m.kind {
        ModKind::Loaded(items) => {
          for item in items {
            self.visit_item(item);
          }
        }
        ModKind::Unloaded(_) | ModKind::Error(_) => {}
      },
      ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Use(_) | ItemKind::Ivy(_) => {}
      ItemKind::Pattern(_) => todo!(),
      ItemKind::Taken => {}
    }
  }

  fn _visit_block(&mut self, block: &'a mut Block) {
    self.enter_scope();
    for stmt in &mut block.stmts {
      self.visit_stmt(stmt);
    }
    self.exit_scope();
  }

  fn visit(&mut self, visitee: &'a mut (impl Visitee<'a> + ?Sized)) {
    visitee.visit(self);
  }
}

pub trait Visitee<'t> {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized));
}

impl<'t> Visitee<'t> for Node {
  fn visit(&'t mut self, visitor: &mut (impl VisitMut<'t> + ?Sized)) {
    visitor.visit_node(self)
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
