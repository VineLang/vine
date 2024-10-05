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
enum Result {
  Value(Port),
  Place(Port, Port),
  Space(Port),
}

impl Compiler<'_> {
  pub(super) fn build_stages(&mut self, locals: usize, name: String, expr: &Expr) -> StageId {
    self.local_count = locals;
    self.name = name;

    let i = self.new_interface();

    let init = self.new_fork(|self_| {
      self_.new_stage(i, |self_, _| {
        let root = self_.lower_expr_value(expr);
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

  fn lower_expr_value(&mut self, expr: &Expr) -> Port {
    match self.lower_expr(expr, ExprCtx::Value) {
      Result::Value(t) => t,
      _ => unreachable!(),
    }
  }

  fn lower_expr_place(&mut self, expr: &Expr) -> (Port, Port) {
    match self.lower_expr(expr, ExprCtx::Place) {
      Result::Place(t, u) => (t, u),
      _ => unreachable!(),
    }
  }

  fn lower_expr_space(&mut self, expr: &Expr) -> Port {
    match self.lower_expr(expr, ExprCtx::Space) {
      Result::Space(t) => t,
      _ => unreachable!(),
    }
  }

  fn lower_expr(&mut self, expr: &Expr, ctx: ExprCtx) -> Result {
    match &expr.kind {
      ExprKind::Hole => Result::Value(Port::Erase),
      ExprKind::U32(num) => Result::Value(Port::U32(*num)),
      ExprKind::F32(num) => Result::Value(Port::F32(*num)),
      ExprKind::Path(path) => Result::Value(Port::Global(path.to_string())),
      ExprKind::Local(local) => match ctx {
        ExprCtx::Value => Result::Value(self.get_local(*local)),
        ExprCtx::ValueClear => Result::Value(self.move_local(*local)),
        ExprCtx::Place => Result::Place(self.get_local(*local), self.set_local(*local)),
        ExprCtx::Space => Result::Space(self.set_local(*local)),
        ExprCtx::SpaceClone => {
          let p = self.get_local(*local);
          let q = self.set_local(*local);
          let (x, y) = self.dup(q);
          self.net.link(x, p);
          Result::Space(y)
        }
      },
      ExprKind::Assign(s, v) => {
        let v = self.lower_expr_value(v);
        let s = self.lower_expr_space(s);
        self.net.link(v, s);
        Result::Value(Port::Erase)
      }
      ExprKind::Block(block) => Result::Value(self.lower_block(block)),
      ExprKind::Call(f, args) => {
        let f = self.lower_expr_value(f);
        Result::Value(self.apply_combs("fn", f, args, Self::lower_expr_value))
      }
      ExprKind::UnaryOp(op, rhs) => {
        let (f, lhs) = match op {
          UnaryOp::Neg => (ExtFnKind::sub, Port::U32(0)),
          UnaryOp::Not => (ExtFnKind::u32_xor, Port::U32(u32::MAX)),
        };
        let rhs = self.lower_expr_value(rhs);
        Result::Value(self.ext_fn(f.into(), lhs, rhs))
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.lower_expr_value(lhs);
        let rhs = self.lower_expr_value(rhs);
        Result::Value(self.op(*op, lhs, rhs))
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let (lhs, out) = self.lower_expr_place(lhs);
        let rhs = self.lower_expr_value(rhs);
        let res = self.op(*op, lhs, rhs);
        self.net.link(out, res);
        Result::Value(Port::Erase)
      }
      ExprKind::ComparisonOp(init, cmps) => {
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
        Result::Value(last_result)
      }
      ExprKind::Tuple(t) => match ctx {
        ExprCtx::Value => Result::Value(self.tuple(t, Self::lower_expr_value)),
        ExprCtx::Space => Result::Space(self.tuple(t, Self::lower_expr_space)),
        _ => todo!(),
      },
      ExprKind::String(s) => {
        Result::Value(self.list(s.chars().count(), s.chars(), |_, c| Port::U32(c as u32)))
      }
      ExprKind::Ref(p) => Result::Value(match self.lower_expr(p, ExprCtx::Place) {
        Result::Value(t) => self.new_comb("ref", t, Port::Erase),
        Result::Place(t, u) => self.new_comb("ref", t, u),
        _ => unreachable!(),
      }),
      ExprKind::List(l) => Result::Value(self.list(l.len(), l, Self::lower_expr_value)),
      ExprKind::Move(t) => match t.kind {
        ExprKind::Local(l) => {
          let v = self.net.new_wire();
          self.cur.steps.push_back(Step::Move(l, v.0));
          Result::Value(v.1)
        }
        _ => {
          let (a, b) = self.lower_expr_place(t);
          self.erase(b);
          Result::Value(a)
        }
      },
      ExprKind::Inverse(x) => {
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
          Result::Value(p) => Result::Space(p),
          Result::Place(p, q) => Result::Place(q, p),
          Result::Space(p) => Result::Value(p),
        }
      }

      ExprKind::Fn(params, body) => self.lower_fn(params, body),
      ExprKind::Loop(body) => self.lower_loop(body),
      ExprKind::While(cond, body) => self.lower_while(cond, body),
      ExprKind::If(cond, then, els) => self.lower_if(cond, then, els),
      ExprKind::Return(r) => self.lower_return(r),
      ExprKind::Break => self.lower_break(),

      ExprKind::Match(value, arms) => self.lower_match(value, arms),

      _ => todo!(),
    }
  }

  fn lower_pat_value(&mut self, t: &Pat) -> Port {
    match &t.kind {
      PatKind::Hole => Port::Erase,
      PatKind::Local(local) => self.set_local(*local),
      PatKind::Tuple(t) => self.tuple(t, Self::lower_pat_value),
      PatKind::Ref(p) => {
        let (a, b) = self.lower_pat_place(p);
        self.new_comb("ref", a, b)
      }
      PatKind::Inverse(p) => self.lower_pat_space(p),
      _ => todo!(),
    }
  }

  fn lower_pat_place(&mut self, t: &Pat) -> (Port, Port) {
    match &t.kind {
      PatKind::Hole => self.net.new_wire(),
      PatKind::Local(local) => {
        let x = self.set_local(*local);
        let y = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, y.0));
        (x, y.1)
      }
      PatKind::Move(t) => (self.lower_pat_value(t), Port::Erase),
      PatKind::Inverse(x) => {
        let (a, b) = self.lower_pat_place(x);
        (b, a)
      }
      _ => todo!(),
    }
  }

  fn lower_pat_space(&mut self, t: &Pat) -> Port {
    match &t.kind {
      PatKind::Hole => Port::Erase,
      PatKind::Local(local) => {
        let x = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, x.0));
        x.1
      }
      PatKind::Inverse(x) => self.lower_pat_value(x),
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
      StmtKind::Expr(t, semi) => {
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
