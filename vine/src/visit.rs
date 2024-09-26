use crate::{
  ast::{Block, Item, ItemKind, ModKind, Stmt, StmtKind, Term, TermKind},
  resolve::{Node, NodeValue},
};

pub trait VisitMut<'a> {
  fn enter_scope(&mut self) {}
  fn exit_scope(&mut self) {}

  fn visit_node(&mut self, node: &'a mut Node) {
    self._visit_node(node)
  }

  fn _visit_node(&mut self, node: &'a mut Node) {
    if let Some(NodeValue::Term(value)) = &mut node.value {
      self.visit_term(value);
    }
  }

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
      | TermKind::U32(_)
      | TermKind::F32(_)
      | TermKind::String(_)
      | TermKind::Break => {}
      TermKind::Ref(a)
      | TermKind::Deref(a)
      | TermKind::Move(a)
      | TermKind::Field(a, _)
      | TermKind::UnaryOp(_, a)
      | TermKind::Return(a) => self.visit_term(a),
      TermKind::Assign(a, b)
      | TermKind::BinaryOp(_, a, b)
      | TermKind::BinaryOpAssign(_, a, b)
      | TermKind::LogicalOp(_, a, b) => {
        self.visit_term(a);
        self.visit_term(b);
      }
      TermKind::Block(b) | TermKind::Loop(b) => self.visit_block(b),
      TermKind::Match(a, b) => {
        self.visit_term(a);
        for (t, u) in b {
          self.enter_scope();
          self.visit_bind(t);
          self.visit_term(u);
          self.exit_scope();
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
      TermKind::WhileLet(a, b, c) => {
        self.visit_bind(a);
        self.visit_term(b);
        self.visit_block(c);
      }
      TermKind::For(a, b, c) => {
        self.enter_scope();
        self.visit_term(b);
        self.visit_bind(a);
        self.visit_block(c);
        self.exit_scope();
      }
      TermKind::Fn(a, b) => {
        self.enter_scope();
        for t in a {
          self.visit_bind(t);
        }
        self.visit_term(b);
        self.exit_scope();
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
      StmtKind::Item(i) => self.visit_item(i),
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
      ItemKind::Mod(m) => match &mut m.kind {
        ModKind::Loaded(items) => {
          for item in items {
            self.visit_item(item);
          }
        }
        ModKind::Unloaded(_) => {}
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
}
