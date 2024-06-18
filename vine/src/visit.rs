use crate::ast::{Block, Item, ItemKind, Stmt, StmtKind, Term, TermKind};

pub trait VisitMut<'a> {
  fn visit_bind(&mut self, term: &'a mut Term) {
    self._visit_term(term);
  }

  fn visit_term(&mut self, term: &'a mut Term) {
    self._visit_term(term);
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

  fn _visit_term(&mut self, term: &'a mut Term) {
    match &mut term.kind {
      TermKind::Hole
      | TermKind::Path(_)
      | TermKind::Local(_)
      | TermKind::Num(_)
      | TermKind::String(_) => {}
      TermKind::Ref(a)
      | TermKind::Deref(a)
      | TermKind::Move(a)
      | TermKind::Field(a, _)
      | TermKind::UnaryOp(_, a) => self.visit_term(a),
      TermKind::Assign(a, b) | TermKind::BinaryOp(_, a, b) | TermKind::LogicalOp(_, a, b) => {
        self.visit_term(a);
        self.visit_term(b);
      }
      TermKind::Block(b) => self.visit_block(b),
      TermKind::Match(a, b) => {
        self.visit_term(a);
        for (t, u) in b {
          self.visit_bind(t);
          self.visit_term(u);
        }
      }
      TermKind::If(a, b, c) => {
        self.visit_term(a);
        self.visit_block(b);
        self.visit_term(c);
      }
      TermKind::While(a, b) => {
        self.visit_term(a);
        self.visit_block(b);
      }
      TermKind::For(a, b, c) => {
        self.visit_bind(a);
        self.visit_term(b);
        self.visit_block(c);
      }
      TermKind::Fn(a, b) => {
        for t in a {
          self.visit_bind(t);
        }
        self.visit_term(b);
      }
      TermKind::Tuple(a) | TermKind::List(a) => {
        for t in a {
          self.visit_term(t);
        }
      }
      TermKind::Call(a, b) | TermKind::Method(a, _, b) => {
        self.visit_term(a);
        for t in b {
          self.visit_term(t);
        }
      }
      TermKind::ComparisonOp(a, b) => {
        self.visit_term(a);
        for (_, t) in b {
          self.visit_term(t);
        }
      }
    }
  }

  fn _visit_stmt(&mut self, stmt: &'a mut Stmt) {
    match &mut stmt.kind {
      StmtKind::Let(l) => {
        if let Some(init) = &mut l.init {
          self.visit_term(init);
        }
        self.visit_bind(&mut l.bind);
      }
      StmtKind::Term(t, _) => self.visit_term(t),
      StmtKind::Empty => {}
    }
  }

  fn _visit_item(&mut self, item: &'a mut Item) {
    match &mut item.kind {
      ItemKind::Fn(f) => {
        for param in &mut f.params {
          self.visit_term(param);
        }
        self.visit_term(&mut f.body);
      }
      ItemKind::Const(c) => {
        self.visit_term(&mut c.value);
      }
      ItemKind::Mod(m) => {
        for item in &mut m.inner.items {
          self.visit_item(item)
        }
      }
      ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::Use(_) => {}
      ItemKind::Pattern(_) => todo!(),
    }
  }

  fn _visit_block(&mut self, block: &'a mut Block) {
    for stmt in &mut block.stmts {
      self.visit_stmt(stmt);
    }
  }
}
