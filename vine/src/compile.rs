use std::{
  collections::BTreeMap,
  mem::{replace, take},
};

use ivm::ext::{ExtFn, ExtFnKind};
use ivy::ast::{Net, Nets, Tree};

use crate::ast::*;

#[derive(Default)]
struct VarGen(usize);

impl VarGen {
  fn gen(&mut self) -> (Tree, Tree) {
    let n = self.0;
    self.0 += 1;
    let str = format!("n{n}");
    (Tree::Var(str.clone()), Tree::Var(str))
  }
}

#[derive(Default)]
pub struct Compiler {
  var: VarGen,
  pairs: Vec<(Tree, Tree)>,
  scope: BTreeMap<usize, ScopeEntry>,
  scope_depth: usize,
  pub nets: Nets,
}

struct ScopeEntry {
  depth: usize,
  value: Tree,
  last_use: Option<Tree>,
}

impl ScopeEntry {
  fn finish(&mut self) -> (Tree, Tree) {
    (take(&mut self.value), self.last_use.take().unwrap_or(Tree::Erase))
  }
}

impl Compiler {
  pub fn compile_global(&mut self, path: &Path, term: &Term) {
    assert_eq!(self.scope_depth, 0);
    assert_eq!(self.var.0, 0);
    assert_eq!(self.pairs.len(), 0);
    let root = self.compile_expr(term);
    let pairs = take(&mut self.pairs);
    let net = Net { root, pairs };
    self.nets.insert(path.to_string(), net);
    self.var.0 = 0;
  }

  fn push_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn pop_scope(&mut self) {
    self.scope_depth -= 1;
    self.scope.retain(|_, x| {
      if x.depth <= self.scope_depth {
        true
      } else {
        self.pairs.push(x.finish());
        false
      }
    });
  }

  fn declare(&mut self, i: usize, value: Tree) {
    self.scope.insert(i, ScopeEntry { depth: self.scope_depth, value, last_use: None });
  }

  fn compile_expr(&mut self, term: &Term) -> Tree {
    match &term.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Num(num) => Tree::U32(*num),
      TermKind::Path(path) => Tree::Global(path.to_string()),
      TermKind::Local(local) => {
        let v = self.var.gen();
        let entry = self.scope.get_mut(local).unwrap();
        if let Some(prev_use) = replace(&mut entry.last_use, Some(v.0)) {
          let new_value = self.var.gen();
          let value = replace(&mut entry.value, new_value.0);
          self
            .pairs
            .push((value, Tree::Comb("dup".to_owned(), Box::new(prev_use), Box::new(new_value.1))));
        }
        v.1
      }
      TermKind::Block(block) => self.compile_block(block),
      TermKind::Fn(params, body) => self.compile_fn(params, body),
      TermKind::Call(f, args) => {
        let mut f = self.compile_expr(f);
        for arg in args {
          let arg = self.compile_expr(arg);
          let out = self.var.gen();
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
      TermKind::Tuple(t) => Self::tup(t.iter().map(|x| self.compile_expr(x))),
      TermKind::String(s) => {
        let v = self.var.gen();
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

  fn compile_fn(&mut self, params: &[Term], body: &Term) -> Tree {
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

  fn compile_pat(&mut self, t: &Term) -> Tree {
    match &t.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Local(i) => {
        let v = self.var.gen();
        self.declare(*i, v.0);
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
    let a = self.var.gen();
    let b = self.var.gen();
    self.pairs.push((t, Tree::Comb("dup".to_owned(), Box::new(a.0), Box::new(b.0))));
    (a.1, b.1)
  }

  fn ext_fn(&mut self, f: ExtFn, lhs: Tree, rhs: Tree) -> Tree {
    let o = self.var.gen();
    self.pairs.push((lhs, Tree::ExtFn(f, Box::new(rhs), Box::new(o.0))));
    o.1
  }

  fn compile_block(&mut self, b: &Block) -> Tree {
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

  fn compile_stmt(&mut self, s: &Stmt) -> Tree {
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
