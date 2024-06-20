use std::mem::{swap, take};

use ivm::ext::{ExtFn, ExtFnKind};
use ivy::ast::Tree;

use crate::{ast::*, resolve::Node};

use super::{stage_name, Compiler, Interface, InterfaceId, Local, Stage, StageId, Step, Usage};

impl Compiler {
  pub(super) fn build_stages(&mut self, node: &Node, term: &Term) -> StageId {
    self.local_count = node.locals;
    self.name = node.canonical.to_string();
    let i = self.new_interface();
    self.new_stage(i, |slf, s| {
      slf.cur = Stage::default();
      let root = slf.build_expr(term);
      let root_val = slf.new_local();
      slf.cur.steps.push(Step::Set(root_val, root));
      slf.interfaces[i].inward.insert(root_val, Usage::Get);
      s
    })
  }

  fn build_expr(&mut self, term: &Term) -> Tree {
    match &term.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Num(num) => Tree::U32(*num),
      TermKind::Path(path) => Tree::Global(path.to_string()),
      TermKind::Local(local) => {
        let v = self.cur.var.gen();
        self.cur.steps.push(Step::Get(*local, v.0));
        v.1
      }
      TermKind::Assign(p, e) => {
        let e = self.build_expr(e);
        let p = self.build_pat(p);
        self.cur.pairs.push((p, e));
        Tree::Erase
      }
      TermKind::Block(block) => self.build_block(block),
      TermKind::Fn(params, body) => {
        let mut result = Tree::Erase;
        let mut cur = &mut result;
        for param in params {
          let param = self.build_pat(param);
          *cur = Tree::Comb("fn".to_owned(), Box::new(param), Box::new(Tree::Erase));
          let Tree::Comb(.., next) = cur else { unreachable!() };
          cur = next;
        }
        *cur = self.build_expr(body);
        result
      }
      TermKind::Call(f, args) => {
        let mut f = self.build_expr(f);
        for arg in args {
          let arg = self.build_expr(arg);
          let out = self.cur.var.gen();
          self.cur.pairs.push((f, Tree::Comb("fn".to_owned(), Box::new(arg), Box::new(out.0))));
          f = out.1;
        }
        f
      }
      TermKind::UnaryOp(op, rhs) => {
        let (f, lhs) = match op {
          UnaryOp::Neg => (ExtFnKind::u32_sub, Tree::U32(0)),
          UnaryOp::Not => (ExtFnKind::u32_xor, Tree::U32(u32::MAX)),
        };
        let rhs = self.build_expr(rhs);
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
        let lhs = self.build_expr(lhs);
        let rhs = self.build_expr(rhs);
        self.ext_fn(f.into(), lhs, rhs)
      }
      TermKind::ComparisonOp(init, cmps) => {
        let mut last_result = Tree::U32(1);
        let mut lhs = self.build_expr(init);
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
          let rhs = self.build_expr(rhs);
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
      TermKind::Tuple(t) => Tree::n_ary("tup", t.iter().map(|x| self.build_expr(x))),
      TermKind::String(s) => {
        let v = self.cur.var.gen();
        Tree::Comb(
          "tup".to_owned(),
          Box::new(Tree::U32(s.chars().count() as u32)),
          Box::new(Tree::Comb(
            "fn".to_owned(),
            Box::new(v.0),
            Box::new(s.chars().rev().fold(v.1, |tail, char| {
              Tree::Comb("tup".to_owned(), Box::new(Tree::U32(char as u32)), Box::new(tail))
            })),
          )),
        )
      }
      TermKind::While(cond, body) => {
        let i = self.new_interface();
        let j = self.new_interface();

        let start = self.new_stage(i, |slf, start| {
          let cond = slf.build_expr(cond);

          let body = slf.new_stage(j, |slf, id| {
            let body = slf.build_block(body);
            slf.erase(body);
            let start = slf.stage_tree(start);
            slf.cur.steps.push(Step::Call(i, start));
            id
          });

          let end = slf.new_stage(j, |_, id| id);

          let r = slf.cur.var.gen();
          slf.cur.pairs.push((
            cond,
            Tree::Branch(
              Box::new(slf.stage_tree(end)),
              Box::new(slf.stage_tree(body)),
              Box::new(r.0),
            ),
          ));
          slf.cur.steps.push(Step::Call(j, r.1));

          start
        });

        self.cur.steps.push(Step::Call(i, self.stage_tree(start)));

        Tree::Erase
      }
      TermKind::If(cond, then, els) => {
        let val = self.new_local();
        let i = self.new_interface();

        let cond = self.build_expr(cond);

        let then = self.new_stage(i, |slf, id| {
          let then = slf.build_block(then);
          slf.cur.steps.push(Step::Set(val, then));
          id
        });

        let els = self.new_stage(i, |slf, id| {
          let els = slf.build_expr(els);
          slf.cur.steps.push(Step::Set(val, els));
          id
        });

        let r = self.cur.var.gen();
        self.cur.pairs.push((
          cond,
          Tree::Branch(
            Box::new(self.stage_tree(els)),
            Box::new(self.stage_tree(then)),
            Box::new(r.0),
          ),
        ));
        self.cur.steps.push(Step::Call(i, r.1));

        let v = self.cur.var.gen();
        self.cur.steps.push(Step::Get(val, v.0));
        v.1
      }
      _ => todo!(),
    }
  }

  fn build_pat(&mut self, t: &Term) -> Tree {
    match &t.kind {
      TermKind::Hole => Tree::Erase,
      TermKind::Local(local) => {
        let v = self.cur.var.gen();
        self.cur.steps.push(Step::Set(*local, v.0));
        v.1
      }
      TermKind::Tuple(t) => Tree::n_ary("tup", t.iter().map(|x| self.build_pat(x))),
      _ => todo!(),
    }
  }

  fn build_block(&mut self, b: &Block) -> Tree {
    let mut last = Tree::Erase;
    for s in &b.stmts {
      self.erase(last);
      last = self.build_stmt(s);
    }
    last
  }

  fn build_stmt(&mut self, s: &Stmt) -> Tree {
    match &s.kind {
      StmtKind::Let(l) => {
        let i = l.init.as_ref().map(|x| self.build_expr(x)).unwrap_or_default();
        let b = self.build_pat(&l.bind);
        self.cur.pairs.push((b, i));
        Tree::Erase
      }
      StmtKind::Term(t, semi) => {
        let t = self.build_expr(t);
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

  fn dup(&mut self, t: Tree) -> (Tree, Tree) {
    let a = self.cur.var.gen();
    let b = self.cur.var.gen();
    self.cur.pairs.push((t, Tree::Comb("dup".to_owned(), Box::new(a.0), Box::new(b.0))));
    (a.1, b.1)
  }

  fn ext_fn(&mut self, f: ExtFn, lhs: Tree, rhs: Tree) -> Tree {
    let o = self.cur.var.gen();
    self.cur.pairs.push((lhs, Tree::ExtFn(f, Box::new(rhs), Box::new(o.0))));
    o.1
  }

  fn erase(&mut self, t: Tree) {
    if !matches!(t, Tree::Erase) {
      self.cur.pairs.push((Tree::Erase, t));
    }
  }

  fn stage_tree(&self, id: StageId) -> Tree {
    Tree::Global(stage_name(&self.name, id))
  }

  fn new_interface(&mut self) -> InterfaceId {
    let id = self.interfaces.len();
    self.interfaces.push(Interface::default());
    id
  }

  fn new_stage<T>(&mut self, interface: InterfaceId, f: impl FnOnce(&mut Self, StageId) -> T) -> T {
    let old_id = self.cur_id;
    let id = self.stages.len();
    self.stages.push(take(&mut self.cur));
    self.cur_id = id;
    self.cur.outer = interface;

    let res = f(self, id);

    self.interfaces[interface].stages.push(id);
    for step in &self.cur.steps {
      if let &Step::Call(inner, _) = step {
        self.interfaces[inner].parents.insert(interface);
      }
    }

    swap(&mut self.cur, &mut self.stages[id]);
    self.cur_id = old_id;

    res
  }

  fn new_local(&mut self) -> Local {
    let local = self.local_count;
    self.local_count += 1;
    local
  }
}
