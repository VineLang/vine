use std::mem::{swap, take};

use ivm::ext::{ExtFn, ExtFnKind};

use crate::{ast::*, resolve::Node};

use super::{Compiler, Port, StageId, Step, Usage, WireDir};

mod control_flow;
mod net_utils;
mod pattern_matching;
mod stage_utils;

enum ExprCtx {
  Value,
  Place,
  Space,
  ValueClear,
  SpaceClone,
}

#[must_use]
enum Expr {
  Value(Port),
  Place(Port, Port),
  Space(Port),
}

impl Compiler<'_> {
  pub(super) fn build_stages(&mut self, locals: usize, name: String, term: &Term) -> StageId {
    self.local_count = locals;
    self.name = name;

    let i = self.new_interface();

    let init = self.new_fork(|self_| {
      self_.new_stage(i, |self_, _| {
        let root = self_.lower_expr_value(term);
        let root_val = self_.new_local();
        self_.set_local_to(root_val, root);
        self_.interfaces[i].inward.insert(root_val, Usage::GET);
        self_.interfaces[i].wires.insert((root_val, WireDir::Output));
        false
      })
    });

    swap(&mut self.cur, &mut self.stages[self.cur_id]);

    for (id, stage) in self.stages.iter_mut().enumerate() {
      stage.steps.extend(take(&mut stage.fin).into_iter().rev());

      let interface_id = stage.outer;
      let interface = &mut self.interfaces[interface_id];
      interface.stages.push(id);
      for step in &stage.steps {
        if let &Step::Call(inner, _) = step {
          self.interfaces[inner].parents.insert(interface_id);
        }
      }
    }

    init
  }

  fn lower_expr_value(&mut self, term: &Term) -> Port {
    match self.lower_expr(term, ExprCtx::Value) {
      Expr::Value(t) => t,
      _ => unreachable!(),
    }
  }

  fn lower_expr_place(&mut self, term: &Term) -> (Port, Port) {
    match self.lower_expr(term, ExprCtx::Place) {
      Expr::Place(t, u) => (t, u),
      _ => unreachable!(),
    }
  }

  fn lower_expr_space(&mut self, term: &Term) -> Port {
    match self.lower_expr(term, ExprCtx::Space) {
      Expr::Space(t) => t,
      _ => unreachable!(),
    }
  }

  fn lower_expr(&mut self, term: &Term, ctx: ExprCtx) -> Expr {
    match &term.kind {
      TermKind::Hole => Expr::Value(Port::Erase),
      TermKind::U32(num) => Expr::Value(Port::U32(*num)),
      TermKind::F32(num) => Expr::Value(Port::F32(*num)),
      TermKind::Path(path) => Expr::Value(Port::Global(path.to_string())),
      TermKind::Local(local) => match ctx {
        ExprCtx::Value => Expr::Value(self.get_local(*local)),
        ExprCtx::ValueClear => Expr::Value(self.move_local(*local)),
        ExprCtx::Place => Expr::Place(self.get_local(*local), self.set_local(*local)),
        ExprCtx::Space => Expr::Space(self.set_local(*local)),
        ExprCtx::SpaceClone => {
          let p = self.get_local(*local);
          let q = self.set_local(*local);
          let (x, y) = self.dup(q);
          self.net.link(x, p);
          Expr::Space(y)
        }
      },
      TermKind::Assign(s, v) => {
        let v = self.lower_expr_value(v);
        let s = self.lower_expr_space(s);
        self.net.link(v, s);
        Expr::Value(Port::Erase)
      }
      TermKind::Block(block) => Expr::Value(self.lower_block(block)),
      TermKind::Call(f, args) => {
        let f = self.lower_expr_value(f);
        Expr::Value(self.apply_combs("fn", f, args, Self::lower_expr_value))
      }
      TermKind::UnaryOp(op, rhs) => {
        let (f, lhs) = match op {
          UnaryOp::Neg => (ExtFnKind::sub, Port::U32(0)),
          UnaryOp::Not => (ExtFnKind::u32_xor, Port::U32(u32::MAX)),
        };
        let rhs = self.lower_expr_value(rhs);
        Expr::Value(self.ext_fn(f.into(), lhs, rhs))
      }
      TermKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.lower_expr_value(lhs);
        let rhs = self.lower_expr_value(rhs);
        Expr::Value(self.op(*op, lhs, rhs))
      }
      TermKind::BinaryOpAssign(op, lhs, rhs) => {
        let (lhs, out) = self.lower_expr_place(lhs);
        let rhs = self.lower_expr_value(rhs);
        let res = self.op(*op, lhs, rhs);
        self.net.link(out, res);
        Expr::Value(Port::Erase)
      }
      TermKind::ComparisonOp(init, cmps) => {
        let mut last_result = Port::U32(1);
        let mut lhs = self.lower_expr_value(init);
        for (i, (op, rhs)) in cmps.iter().enumerate() {
          let f = match op {
            ComparisonOp::Eq => ExtFn::from(ExtFnKind::eq),
            ComparisonOp::Ne => ExtFn::from(ExtFnKind::ne),
            ComparisonOp::Lt => ExtFn::from(ExtFnKind::lt),
            ComparisonOp::Gt => ExtFn::from(ExtFnKind::lt).swap(),
            ComparisonOp::Le => ExtFn::from(ExtFnKind::le),
            ComparisonOp::Ge => ExtFn::from(ExtFnKind::le).swap(),
          };
          let first = i == 0;
          let last = i == cmps.len() - 1;
          let rhs = self.lower_expr_value(rhs);
          let rhs = if last { (rhs, Port::Erase) } else { self.dup(rhs) };
          let result = self.ext_fn(f, lhs, rhs.0);
          lhs = rhs.1;
          last_result = if first {
            result
          } else {
            self.ext_fn(ExtFnKind::u32_and.into(), last_result, result)
          };
        }
        Expr::Value(last_result)
      }
      TermKind::Tuple(t) => match ctx {
        ExprCtx::Value => Expr::Value(self.tuple(t, Self::lower_expr_value)),
        ExprCtx::Space => Expr::Space(self.tuple(t, Self::lower_expr_space)),
        _ => todo!(),
      },
      TermKind::String(s) => {
        Expr::Value(self.list(s.chars().count(), s.chars(), |_, c| Port::U32(c as u32)))
      }
      TermKind::Ref(p) => Expr::Value(match self.lower_expr(p, ExprCtx::Place) {
        Expr::Value(t) => self.new_comb("ref", t, Port::Erase),
        Expr::Place(t, u) => self.new_comb("ref", t, u),
        _ => unreachable!(),
      }),
      TermKind::List(l) => Expr::Value(self.list(l.len(), l, Self::lower_expr_value)),
      TermKind::Move(t) => match t.kind {
        TermKind::Local(l) => {
          let v = self.net.new_wire();
          self.cur.steps.push_back(Step::Move(l, v.0));
          Expr::Value(v.1)
        }
        _ => {
          let (a, b) = self.lower_expr_place(t);
          self.erase(b);
          Expr::Value(a)
        }
      },
      TermKind::Inverse(x) => {
        match self.lower_expr(
          x,
          match ctx {
            ExprCtx::Value => ExprCtx::SpaceClone,
            ExprCtx::SpaceClone => ExprCtx::Value,
            ExprCtx::Place => ExprCtx::Place,
            ExprCtx::Space => ExprCtx::ValueClear,
            ExprCtx::ValueClear => ExprCtx::Space,
          },
        ) {
          Expr::Value(p) => Expr::Space(p),
          Expr::Place(p, q) => Expr::Place(q, p),
          Expr::Space(p) => Expr::Value(p),
        }
      }

      TermKind::Fn(params, body) => self.lower_fn(params, body),
      TermKind::Loop(body) => self.lower_loop(body),
      TermKind::While(cond, body) => self.lower_while(cond, body),
      TermKind::If(cond, then, els) => self.lower_if(cond, then, els),
      TermKind::Return(r) => self.lower_return(r),
      TermKind::Break => self.lower_break(),

      TermKind::Match(value, arms) => self.lower_match(value, arms),

      _ => todo!(),
    }
  }

  fn lower_pat_value(&mut self, t: &Term) -> Port {
    match &t.kind {
      TermKind::Hole => Port::Erase,
      TermKind::Local(local) => self.set_local(*local),
      TermKind::Tuple(t) => self.tuple(t, Self::lower_pat_value),
      TermKind::Ref(p) => {
        let (a, b) = self.lower_pat_place(p);
        self.new_comb("ref", a, b)
      }
      TermKind::Inverse(p) => self.lower_pat_space(p),
      _ => todo!(),
    }
  }

  fn lower_pat_place(&mut self, t: &Term) -> (Port, Port) {
    match &t.kind {
      TermKind::Hole => self.net.new_wire(),
      TermKind::Local(local) => {
        let x = self.set_local(*local);
        let y = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, y.0));
        (x, y.1)
      }
      TermKind::Move(t) => (self.lower_pat_value(t), Port::Erase),
      TermKind::Inverse(x) => {
        let (a, b) = self.lower_pat_place(x);
        (b, a)
      }
      _ => todo!(),
    }
  }

  fn lower_pat_space(&mut self, t: &Term) -> Port {
    match &t.kind {
      TermKind::Hole => Port::Erase,
      TermKind::Local(local) => {
        let x = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, x.0));
        x.1
      }
      TermKind::Inverse(x) => self.lower_pat_value(x),
      _ => todo!(),
    }
  }

  fn lower_block(&mut self, b: &Block) -> Port {
    let mut last = Port::Erase;
    for s in &b.stmts {
      self.erase(last);
      last = self.lower_stmt(s);
    }
    last
  }

  fn lower_block_erase(&mut self, block: &Block) {
    let out = self.lower_block(block);
    self.erase(out);
  }

  fn lower_stmt(&mut self, s: &Stmt) -> Port {
    match &s.kind {
      StmtKind::Let(l) => {
        let i = l.init.as_ref().map(|x| self.lower_expr_value(x)).unwrap_or(Port::Erase);
        let b = self.lower_pat_value(&l.bind);
        self.net.link(b, i);
        Port::Erase
      }
      StmtKind::Term(t, semi) => {
        let t = self.lower_expr_value(t);
        if *semi {
          self.erase(t);
          Port::Erase
        } else {
          t
        }
      }
      StmtKind::Item(_) => Port::Erase,
      StmtKind::Empty => Port::Erase,
    }
  }
}
