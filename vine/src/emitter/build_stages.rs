use std::mem::{swap, take};

use ivm::ext::{ExtFn, ExtFnKind};
use vine_util::idx::Counter;

use crate::{ast::*, resolver::Def};

use super::{Emitter, Port, StageId, Step, Usage, WireDir};

mod control_flow;
mod net_utils;
mod pattern_matching;
mod stage_utils;

impl<'core> Emitter<'core, '_> {
  pub(super) fn build_stages(
    &mut self,
    locals: Counter<Local>,
    name: String,
    expr: &Expr<'core>,
  ) -> StageId {
    self.locals = locals;
    self.name = name;

    let i = self.new_interface();

    let init = self.new_fork(|self_| {
      self_.new_stage(i, |self_, _| {
        let root = self_.emit_expr_value(expr);
        let root_val = self_.new_local();
        self_.set_local_to(root_val, root);
        self_.interfaces[i].inward.insert(root_val, Usage::GET);
        self_.interfaces[i].wires.insert((root_val, WireDir::Output));
        false
      })
    });

    swap(&mut self.cur, &mut self.stages[self.cur_id]);

    for (id, stage) in self.stages.iter_mut() {
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

  fn emit_expr_value(&mut self, expr: &Expr<'core>) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !value] => unreachable!("{expr:?}"),

      ExprKind::DynFn(d) => {
        let (local, stage) = self.dyn_fns[d];
        self.goto(stage);
        self.move_local(local)
      }
      ExprKind::Paren(e) => self.emit_expr_value(e),
      ExprKind::U32(num) => Port::U32(*num),
      ExprKind::F32(num) => Port::F32(*num),
      ExprKind::Path(path) => Port::Global(path.path.to_string()),
      ExprKind::Assign(s, v) => {
        let v = self.emit_expr_value(v);
        let s = self.emit_expr_space(s);
        self.net.link(v, s);
        Port::Erase
      }
      ExprKind::Block(block) => self.emit_block(block),
      ExprKind::Call(f, args) => {
        let f = self.emit_expr_value(f);
        self.apply_combs("fn", f, args, Self::emit_expr_value)
      }
      ExprKind::Neg(rhs) => {
        let rhs = self.emit_expr_value(rhs);
        self.ext_fn(ExtFnKind::sub.into(), Port::U32(0), rhs)
      }
      ExprKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.emit_expr_value(lhs);
        let rhs = self.emit_expr_value(rhs);
        self.op(*op, lhs, rhs)
      }
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        let rhs = self.emit_expr_value(rhs);
        let (lhs, out) = self.emit_expr_place(lhs);
        let res = self.op(*op, lhs, rhs);
        self.net.link(out, res);
        Port::Erase
      }
      ExprKind::ComparisonOp(init, cmps) => {
        let mut last_result = Port::U32(1);
        let mut lhs = self.emit_expr_value(init);
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
          let rhs = self.emit_expr_value(rhs);
          let rhs = if last { (rhs, Port::Erase) } else { self.dup(rhs) };
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
      ExprKind::Tuple(t) => self.tuple(t, Self::emit_expr_value),
      ExprKind::String(s) => self.list(s.chars().count(), s.chars(), |_, c| Port::U32(c as u32)),
      ExprKind::Ref(p) => {
        let (t, u) = self.emit_expr_place(p);
        self.new_comb("ref", t, u)
      }
      ExprKind::List(l) => self.list(l.len(), l, Self::emit_expr_value),
      ExprKind::Move(t) => match t.kind {
        ExprKind::Local(l) => {
          let v = self.net.new_wire();
          self.cur.steps.push_back(Step::Move(l, v.0));
          v.1
        }
        _ => {
          let (a, b) = self.emit_expr_place(t);
          self.erase(b);
          a
        }
      },
      ExprKind::Inverse(x) => self.emit_expr_space(x),

      ExprKind::Fn(params, _, body) => self.emit_fn(params, body),
      ExprKind::Loop(body) => self.emit_loop(body),
      ExprKind::While(cond, body) => self.emit_while(cond, body),
      ExprKind::If(cond, then, els) => self.emit_if(cond, then, els),
      ExprKind::Return(r) => self.emit_return(r.as_deref()),
      ExprKind::Break(r) => self.emit_break(r.as_deref()),
      ExprKind::Continue => self.emit_continue(),

      ExprKind::Match(value, arms) => {
        let result = self.new_local();
        self.new_fork(|self_| {
          self_.emit_match(value, arms.iter().map(|(p, a)| (p, a)), |self_, expr| {
            let r = self_.emit_expr_value(expr);
            self_.set_local_to(result, r);
            true
          });
        });
        self.move_local(result)
      }

      ExprKind::CopyLocal(l) => self.get_local(*l),
      ExprKind::MoveLocal(l) => self.move_local(*l),

      ExprKind::Copy(e) => {
        let (v, s) = self.emit_expr_place(e);
        let (a, b) = self.dup(v);
        self.net.link(b, s);
        a
      }

      ExprKind![cond] => {
        let result = self.new_local();
        self.emit_cond(
          expr,
          &|self_| {
            self_.set_local_to(result, Port::U32(1));
            true
          },
          &|self_| {
            self_.set_local_to(result, Port::U32(0));
            true
          },
        );
        self.move_local(result)
      }

      ExprKind::For(..) => todo!(),
    }
  }

  fn emit_expr_place(&mut self, expr: &Expr<'core>) -> (Port, Port) {
    match &expr.kind {
      ExprKind![sugar || error || !place] => unreachable!(),
      ExprKind::Paren(e) => self.emit_expr_place(e),
      ExprKind::Local(l) => (self.get_local(*l), self.set_local(*l)),
      ExprKind::Inverse(e) => {
        let (a, b) = self.emit_expr_place(e);
        (b, a)
      }
      ExprKind::Temp(e) => (self.emit_expr_value(e), Port::Erase),
      ExprKind::Deref(p) => {
        let r = self.emit_expr_value(p);
        let x = self.net.new_wire();
        let y = self.net.new_wire();
        let s = self.new_comb("ref", x.0, y.0);
        self.net.link(r, s);
        (x.1, y.1)
      }
      ExprKind::Tuple(t) => self.tuple_pairs(t, Self::emit_expr_place),
    }
  }

  fn emit_expr_space(&mut self, expr: &Expr<'core>) -> Port {
    match &expr.kind {
      ExprKind![sugar || error || !space] => unreachable!(),
      ExprKind::Paren(e) => self.emit_expr_space(e),
      ExprKind::Hole => Port::Erase,
      ExprKind::Set(e) => {
        let (v, s) = self.emit_expr_place(e);
        self.erase(v);
        s
      }
      ExprKind::Inverse(e) => self.emit_expr_value(e),
      ExprKind::Tuple(t) => self.tuple(t, Self::emit_expr_space),
      ExprKind::SetLocal(l) => self.set_local(*l),
    }
  }

  fn emit_pat_value(&mut self, t: &Pat<'core>) -> Port {
    match &t.kind {
      PatKind![!value] => unreachable!(),

      PatKind::Paren(e) => self.emit_pat_value(e),
      PatKind::Hole | PatKind::Adt(_, None) => Port::Erase,
      PatKind::Local(local) => {
        self.declare_local(*local);
        self.set_local(*local)
      }
      PatKind::Tuple(t) | PatKind::Adt(_, Some(t)) => self.tuple(t, Self::emit_pat_value),
      PatKind::Ref(p) => {
        let (a, b) = self.emit_pat_place(p);
        self.new_comb("ref", a, b)
      }
      PatKind::Inverse(p) => self.emit_pat_space(p),
    }
  }

  fn emit_pat_place(&mut self, t: &Pat<'core>) -> (Port, Port) {
    match &t.kind {
      PatKind![!place] => unreachable!(),

      PatKind::Paren(e) => self.emit_pat_place(e),
      PatKind::Hole | PatKind::Adt(_, None) => self.net.new_wire(),
      PatKind::Local(local) => {
        self.declare_local(*local);
        let x = self.set_local(*local);
        let y = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, y.0));
        (x, y.1)
      }
      PatKind::Move(t) => (self.emit_pat_value(t), Port::Erase),
      PatKind::Inverse(x) => {
        let (a, b) = self.emit_pat_place(x);
        (b, a)
      }
      PatKind::Ref(p) => {
        let r = self.emit_pat_place(p);
        let w = self.net.new_wire();
        (self.new_comb("ref", r.0, w.0), self.new_comb("ref", r.1, w.1))
      }
      PatKind::Deref(p) => {
        let r = self.emit_pat_value(p);
        let x = self.net.new_wire();
        let y = self.net.new_wire();
        let s = self.new_comb("ref", x.0, y.0);
        self.net.link(r, s);
        (x.1, y.1)
      }
      PatKind::Tuple(t) | PatKind::Adt(_, Some(t)) => self.tuple_pairs(t, Self::emit_pat_place),
    }
  }

  fn emit_pat_space(&mut self, t: &Pat<'core>) -> Port {
    match &t.kind {
      PatKind![!space] => unreachable!(),

      PatKind::Paren(e) => self.emit_pat_space(e),
      PatKind::Hole | PatKind::Adt(_, None) => Port::Erase,
      PatKind::Local(local) => {
        self.declare_local(*local);
        let x = self.net.new_wire();
        self.cur.fin.push(Step::Move(*local, x.0));
        x.1
      }
      PatKind::Inverse(x) => self.emit_pat_value(x),
      PatKind::Tuple(t) | PatKind::Adt(_, Some(t)) => self.tuple(t, Self::emit_pat_space),
    }
  }

  fn emit_block(&mut self, b: &Block<'core>) -> Port {
    let mut last = Port::Erase;
    for s in &b.stmts {
      self.erase(last);
      last = self.emit_stmt(s);
    }
    last
  }

  fn emit_block_erase(&mut self, block: &Block<'core>) {
    let out = self.emit_block(block);
    self.erase(out);
  }

  fn emit_stmt(&mut self, s: &Stmt<'core>) -> Port {
    match &s.kind {
      StmtKind::Let(l) => {
        let i = l.init.as_ref().map(|x| self.emit_expr_value(x)).unwrap_or(Port::Erase);
        let b = self.emit_pat_value(&l.bind);
        self.net.link(b, i);
        Port::Erase
      }
      StmtKind::DynFn(d) => {
        let f = self.new_local();
        self.new_fork(|self_| {
          let i = self_.new_interface();
          let s = self_.new_stage(i, |self_, _| {
            let p = self_._emit_fn(&d.params, |self_| self_.emit_block(&d.body));
            self_.set_local_to(f, p);
            true
          });
          self_.dyn_fns.insert(d.id.unwrap(), (f, s));
        });
        Port::Erase
      }
      StmtKind::Expr(t, semi) => {
        let t = self.emit_expr_value(t);
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
