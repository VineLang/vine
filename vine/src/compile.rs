use std::{collections::HashMap, mem::take};

use ivm::ext::{ExtFn, ExtFnKind};
use ivy::ast::{Net, Nets, Tree};

use crate::ast::*;

#[derive(Default)]
pub struct Compiler<'ast> {
  next_var: usize,
  pairs: Vec<(Tree, Tree)>,
  scope: HashMap<&'ast Ident, Vec<(usize, Tree)>>,
  scope_depth: usize,
  pub nets: Nets,
}

impl<'ast> Compiler<'ast> {
  pub fn compile_item(&mut self, item: &'ast Item) {
    assert_eq!(self.scope_depth, 0);
    assert_eq!(self.next_var, 0);
    assert_eq!(self.pairs.len(), 0);
    let (path, root) = match &item.kind {
      ItemKind::Fn(f) => (&f.name, self.compile_fn(&f.params, &f.body)),
      ItemKind::Const(c) => (&c.name, self.compile_expr(&c.value)),
      _ => todo!(),
    };
    let pairs = take(&mut self.pairs);
    let net = Net { root, pairs };
    self.nets.insert(format!("{path}"), net);
    self.next_var = 0;
  }

  fn new_var(&mut self) -> (Tree, Tree) {
    let n = self.next_var;
    self.next_var += 1;
    let str = format!("n{n}");
    (Tree::Var(str.clone()), Tree::Var(str))
  }

  fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn pop_scope(&mut self) {
    self.scope_depth -= 1;
    for s in self.scope.values_mut() {
      if s.last().is_some_and(|x| x.0 > self.scope_depth) {
        self.pairs.push((Tree::Erase, s.pop().unwrap().1));
      }
    }
  }

  fn declare(&mut self, i: &'ast Ident, v: Tree) {
    let stack = self.scope.entry(i).or_default();
    if stack.last().is_some_and(|x| x.0 == self.scope_depth) {
      self.pairs.push((Tree::Erase, stack.pop().unwrap().1));
    }
    stack.push((self.scope_depth, v));
  }

  fn compile_expr(&mut self, t: &'ast Term) -> Tree {
    match &t.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Num(n) => Tree::U32(*n),
      TermKind::Path(p) => Tree::Global(format!("{}", p)),
      TermKind::Var(v) => {
        let (d, t) = self.scope.get_mut(v).unwrap().pop().unwrap();
        let t = self.dup(t);
        self.scope.get_mut(v).unwrap().push((d, t.0));
        t.1
      }
      TermKind::Block(b) => self.compile_block(b),
      TermKind::Fn(params, body) => self.compile_fn(params, body),
      TermKind::Call(f, args) => {
        let mut f = self.compile_expr(f);
        for arg in args {
          let arg = self.compile_expr(arg);
          let out = self.new_var();
          self.pairs.push((f, Tree::Comb("lam".to_owned(), Box::new(arg), Box::new(out.0))));
          f = out.1;
        }
        f
      }
      TermKind::UnaryOp(op, rhs) => {
        let (f, lhs) = match op {
          UnaryOp::Neg => (ExtFnKind::u32_sub, Tree::U32(0)),
          UnaryOp::Not => (ExtFnKind::u32_xor, Tree::U32(u32::MAX)),
        };
        let rhs = self.compile_expr(rhs);
        self.ext_fn(f.into(), lhs, rhs)
      }
      TermKind::BinaryOp(op, lhs, rhs) => {
        let f = match op {
          BinaryOp::BitOr => ExtFnKind::u32_or,
          BinaryOp::BitXor => ExtFnKind::u32_xor,
          BinaryOp::BitAnd => ExtFnKind::u32_and,
          BinaryOp::Shl => ExtFnKind::u32_shl,
          BinaryOp::Shr => ExtFnKind::u32_shr,
          BinaryOp::Add => ExtFnKind::u32_add,
          BinaryOp::Sub => ExtFnKind::u32_sub,
          BinaryOp::Mul => ExtFnKind::u32_mul,
          BinaryOp::Div => ExtFnKind::u32_div,
          BinaryOp::Rem => ExtFnKind::u32_rem,
          _ => todo!(),
        };
        let lhs = self.compile_expr(lhs);
        let rhs = self.compile_expr(rhs);
        self.ext_fn(f.into(), lhs, rhs)
      }
      TermKind::ComparisonOp(init, cmps) => {
        let mut last_result = Tree::U32(1);
        let mut lhs = self.compile_expr(init);
        for (i, (op, rhs)) in cmps.iter().enumerate() {
          let f = match op {
            ComparisonOp::Eq => ExtFn::from(ExtFnKind::u32_eq),
            ComparisonOp::Ne => ExtFn::from(ExtFnKind::u32_ne),
            ComparisonOp::Lt => ExtFn::from(ExtFnKind::u32_lt),
            ComparisonOp::Gt => ExtFn::from(ExtFnKind::u32_lt).swap(),
            ComparisonOp::Le => ExtFn::from(ExtFnKind::u32_le),
            ComparisonOp::Ge => ExtFn::from(ExtFnKind::u32_le).swap(),
          };
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.compile_expr(rhs);
          let rhs = if last { (rhs, Tree::Erase) } else { self.dup(rhs) };
          let result = self.ext_fn(f, lhs, rhs.0);
          lhs = rhs.1;
          last_result = if first {
            result
          } else {
            self.ext_fn(ExtFnKind::u32_and.into(), last_result, result)
          };
        }
        last_result
      }
      TermKind::Tuple(t) => Self::tup(t.iter().map(|x| self.compile_pat(x))),
      TermKind::String(s) => {
        let v = self.new_var();
        Tree::Comb(
          "tup".to_owned(),
          Box::new(Tree::U32(s.chars().count() as u32)),
          Box::new(Tree::Comb(
            "lam".to_owned(),
            Box::new(v.0),
            Box::new(s.chars().rev().fold(v.1, |tail, char| {
              Tree::Comb("tup".to_owned(), Box::new(Tree::U32(char as u32)), Box::new(tail))
            })),
          )),
        )
      }
      _ => todo!(),
    }
  }

  fn compile_fn(&mut self, params: &'ast [Term], body: &'ast Term) -> Tree {
    let mut result = Tree::Erase;
    let mut cur = &mut result;
    self.push_scope();
    for param in params {
      let param = self.compile_pat(param);
      *cur = Tree::Comb("lam".to_owned(), Box::new(param), Box::new(Tree::Erase));
      let Tree::Comb(.., next) = cur else { unreachable!() };
      cur = next;
    }
    *cur = self.compile_expr(body);
    self.pop_scope();
    result
  }

  fn compile_pat(&mut self, t: &'ast Term) -> Tree {
    match &t.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Var(i) => {
        let v = self.new_var();
        self.declare(i, v.0);
        v.1
      }
      TermKind::Tuple(t) => Self::tup(t.iter().map(|x| self.compile_pat(x))),
      _ => todo!(),
    }
  }

  fn tup(ts: impl Iterator<Item = Tree>) -> Tree {
    ts.reduce(|a, b| Tree::Comb("tup".to_owned(), Box::new(a), Box::new(b))).unwrap_or(Tree::Erase)
  }

  fn dup(&mut self, t: Tree) -> (Tree, Tree) {
    let a = self.new_var();
    let b = self.new_var();
    self.pairs.push((t, Tree::Comb("dup".to_owned(), Box::new(a.0), Box::new(b.0))));
    (a.1, b.1)
  }

  fn ext_fn(&mut self, f: ExtFn, lhs: Tree, rhs: Tree) -> Tree {
    let o = self.new_var();
    self.pairs.push((lhs, Tree::ExtFn(f, Box::new(rhs), Box::new(o.0))));
    o.1
  }

  fn compile_block(&mut self, b: &'ast Block) -> Tree {
    self.push_scope();
    let mut last = Tree::Erase;
    for s in &b.stmts {
      self.erase(last);
      last = self.compile_stmt(s);
    }
    self.pop_scope();
    last
  }

  fn erase(&mut self, t: Tree) {
    if !matches!(t, Tree::Erase) {
      self.pairs.push((Tree::Erase, t));
    }
  }

  fn compile_stmt(&mut self, s: &'ast Stmt) -> Tree {
    match &s.kind {
      StmtKind::Let(l) => {
        let i = l.init.as_ref().map(|x| self.compile_expr(x)).unwrap_or_default();
        let b = self.compile_pat(&l.bind);
        self.pairs.push((b, i));
        Tree::Erase
      }
      StmtKind::Term(t, semi) => {
        let t = self.compile_expr(t);
        if *semi {
          self.erase(t);
          Tree::Erase
        } else {
          t
        }
      }
      StmtKind::Empty => Tree::Erase,
    }
  }
}
