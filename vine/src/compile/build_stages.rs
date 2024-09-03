use std::mem::{swap, take};

use ivm::ext::{ExtFn, ExtFnKind};

use crate::{ast::*, resolve::Node};

use super::{
  stage_name, Agent, Compiler, Fork, ForkId, Interface, InterfaceId, Port, Stage, StageId, Step,
  Usage,
};

mod pattern_matching;

enum ExprCtx {
  Value,
  Place,
  Either,
}

#[must_use]
enum Expr {
  Value(Port),
  Place(Port, Port),
}

impl Compiler<'_> {
  pub(super) fn build_stages(&mut self, node: &Node, term: &Term) -> StageId {
    self.local_count = node.locals;
    self.name = node.canonical.to_string();

    let i = self.new_interface();

    self.cur_id = 0;
    self.forks.push(Fork { ends: Vec::new(), divergence: 0 });
    let init = self.start_stage(i);

    let root = self.build_expr_value(term);
    let root_val = self.new_local();
    self.cur.steps.push_back(Step::Set(root_val, root));
    self.interfaces[i].inward.insert(root_val, Usage::GET);

    self.forks.pop();
    debug_assert!(self.forks.is_empty());

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

  fn build_expr_value(&mut self, term: &Term) -> Port {
    match self.build_expr(term, ExprCtx::Value) {
      Expr::Value(t) => t,
      Expr::Place(..) => unreachable!(),
    }
  }

  fn build_expr_place(&mut self, term: &Term) -> (Port, Port) {
    match self.build_expr(term, ExprCtx::Place) {
      Expr::Place(t, u) => (t, u),
      Expr::Value(_) => unreachable!(),
    }
  }

  fn build_expr(&mut self, term: &Term, ctx: ExprCtx) -> Expr {
    match &term.kind {
      TermKind::Hole => Expr::Value(Port::Erase),
      TermKind::U32(num) => Expr::Value(Port::U32(*num)),
      TermKind::F32(num) => Expr::Value(Port::F32(*num)),
      TermKind::Path(path) => Expr::Value(Port::Global(path.to_string())),
      TermKind::Local(local) => match ctx {
        ExprCtx::Value => {
          let v = self.net.new_wire();
          self.cur.steps.push_back(Step::Get(*local, v.0));
          Expr::Value(v.1)
        }
        ExprCtx::Place | ExprCtx::Either => {
          let x = self.net.new_wire();
          let y = self.net.new_wire();
          self.cur.steps.push_back(Step::Get(*local, x.0));
          self.cur.steps.push_back(Step::Set(*local, y.0));
          Expr::Place(x.1, y.1)
        }
      },
      TermKind::Assign(p, e) => {
        let e = self.build_expr_value(e);
        let p = self.build_pat_value(p);
        self.net.link(e, p);
        Expr::Value(Port::Erase)
      }
      TermKind::Block(block) => Expr::Value(self.build_block(block)),
      TermKind::Fn(params, body) => {
        let func = self.net.new_wire();
        let mut cur = func.0;
        for param in params {
          let param = self.build_pat_value(param);
          cur = self.apply_comb("fn".to_owned(), cur, param);
        }
        let result = self.new_local();
        let orig = self.cur_id;
        let old_break = self.break_target.take();
        let old_return = self.return_target.replace((result, self.cur_fork()));
        let body = self.build_expr_value(body);
        self.return_target = old_return;
        self.break_target = old_break;
        self.cur.steps.push_back(Step::Set(result, body));
        if self.cur_id != orig {
          self.select_stage(orig);
        }
        self.cur.steps.push_back(Step::Get(result, cur));
        Expr::Value(func.1)
      }
      TermKind::Call(f, args) => {
        Expr::Value(args.iter().fold(self.build_expr_value(f), |func, arg| {
          let arg = self.build_expr_value(arg);
          self.apply_comb("fn".to_owned(), func, arg)
        }))
      }
      TermKind::UnaryOp(op, rhs) => {
        let (f, lhs) = match op {
          UnaryOp::Neg => (ExtFnKind::sub, Port::U32(0)),
          UnaryOp::Not => (ExtFnKind::u32_xor, Port::U32(u32::MAX)),
        };
        let rhs = self.build_expr_value(rhs);
        Expr::Value(self.ext_fn(f.into(), lhs, rhs))
      }
      TermKind::BinaryOp(op, lhs, rhs) => {
        let lhs = self.build_expr_value(lhs);
        let rhs = self.build_expr_value(rhs);
        Expr::Value(self.op(*op, lhs, rhs))
      }
      TermKind::BinaryOpAssign(op, lhs, rhs) => {
        let (lhs, out) = self.build_expr_place(lhs);
        let rhs = self.build_expr_value(rhs);
        let res = self.op(*op, lhs, rhs);
        self.net.link(out, res);
        Expr::Value(Port::Erase)
      }
      TermKind::ComparisonOp(init, cmps) => {
        let mut last_result = Port::U32(1);
        let mut lhs = self.build_expr_value(init);
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
          let rhs = self.build_expr_value(rhs);
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
      TermKind::Tuple(t) => {
        Expr::Value(self.tuple(t.iter().map(|x| |slf: &mut Self| slf.build_expr_value(x))))
      }
      TermKind::String(s) => Expr::Value(
        self.list(s.chars().count(), s.chars().map(|c| move |_: &mut Self| Port::U32(c as u32))),
      ),
      TermKind::Ref(p) => Expr::Value(match self.build_expr(p, ExprCtx::Either) {
        Expr::Value(t) => self.new_comb("ref".to_owned(), t, Port::Erase),
        Expr::Place(t, u) => self.new_comb("ref".to_owned(), t, u),
      }),
      TermKind::List(l) => Expr::Value(
        self.list(l.len(), l.iter().map(|el| |slf: &mut Self| slf.build_expr_value(el))),
      ),
      TermKind::Loop(body) => {
        self.fork(|slf| {
          let i = slf.new_interface();
          let s = slf.new_stage(i, move |slf, s| {
            let old = slf.break_target.replace(slf.cur_fork());
            let body = slf.build_block(body);
            slf.erase(body);
            slf.break_target = old;
            slf.goto(s);
            false
          });

          slf.goto(s);
        });

        Expr::Value(Port::Erase)
      }
      TermKind::While(cond, body) => {
        self.fork(|slf| {
          let old_break = slf.break_target.replace(slf.cur_fork());

          let i = slf.new_interface();
          let start = slf.new_stage(i, move |slf, start| {
            let cond = slf.build_expr_value(cond);

            slf.fork(|slf| {
              let j = slf.new_interface();
              let body = slf.new_stage(j, |slf, _| {
                let body = slf.build_block(body);
                slf.erase(body);
                let start = slf.stage_port(start);
                slf.cur.steps.push_back(Step::Call(i, start));
                false
              });

              let end = slf.new_stage(j, |_, _| true);

              let r = slf.net.new_wire();
              slf.cur.agents.push(Agent::Branch(
                cond,
                slf.stage_port(end),
                slf.stage_port(body),
                r.0,
              ));
              slf.cur.steps.push_back(Step::Call(j, r.1));
            });

            true
          });

          slf.break_target = old_break;

          slf.cur.steps.push_back(Step::Call(i, slf.stage_port(start)));
        });

        Expr::Value(Port::Erase)
      }
      TermKind::If(cond, then, els) => {
        let val = self.new_local();

        let cond = self.build_expr_value(cond);

        self.fork(|slf| {
          let i = slf.new_interface();
          let then = slf.new_stage(i, |slf, _| {
            let then = slf.build_block(then);
            slf.cur.steps.push_back(Step::Set(val, then));
            true
          });

          let els = slf.new_stage(i, |slf, _| {
            let els = slf.build_expr_value(els);
            slf.cur.steps.push_back(Step::Set(val, els));
            true
          });

          let r = slf.net.new_wire();
          slf.cur.agents.push(Agent::Branch(cond, slf.stage_port(els), slf.stage_port(then), r.0));
          slf.cur.steps.push_back(Step::Call(i, r.1));
        });

        let v = self.net.new_wire();
        self.cur.steps.push_back(Step::Get(val, v.0));
        Expr::Value(v.1)
      }
      TermKind::Move(t) => match t.kind {
        TermKind::Local(l) => {
          let v = self.net.new_wire();
          self.cur.steps.push_back(Step::Move(l, v.0));
          Expr::Value(v.1)
        }
        _ => {
          let (a, b) = self.build_expr_place(t);
          self.erase(b);
          Expr::Value(a)
        }
      },
      TermKind::Return(r) => {
        let r = self.build_expr_value(r);
        let ret = self.return_target.unwrap();
        self.cur.steps.push_back(Step::Set(ret.0, r));
        self.diverge(ret.1);
        Expr::Value(Port::Erase)
      }
      TermKind::Break => {
        self.diverge(self.break_target.unwrap());
        Expr::Value(Port::Erase)
      }
      TermKind::Match(value, arms) => self.build_match(value, arms),
      _ => todo!(),
    }
  }

  fn apply_comb(&mut self, label: String, to: Port, with: Port) -> Port {
    let out = self.net.new_wire();
    self.cur.agents.push(Agent::Comb(label, to, with, out.0));
    out.1
  }

  fn new_comb(&mut self, label: String, a: Port, b: Port) -> Port {
    let out = self.net.new_wire();
    self.cur.agents.push(Agent::Comb(label, out.0, a, b));
    out.1
  }

  fn op(&mut self, op: BinaryOp, lhs: Port, rhs: Port) -> Port {
    let f = match op {
      BinaryOp::Concat => {
        let x = Port::Global("::std::str::concat".to_owned());
        let x = self.apply_comb("fn".to_owned(), x, lhs);
        return self.apply_comb("fn".to_owned(), x, rhs);
      }
      BinaryOp::BitOr => ExtFnKind::u32_or,
      BinaryOp::BitXor => ExtFnKind::u32_xor,
      BinaryOp::BitAnd => ExtFnKind::u32_and,
      BinaryOp::Shl => ExtFnKind::u32_shl,
      BinaryOp::Shr => ExtFnKind::u32_shr,
      BinaryOp::Add => ExtFnKind::add,
      BinaryOp::Sub => ExtFnKind::sub,
      BinaryOp::Mul => ExtFnKind::mul,
      BinaryOp::Div => ExtFnKind::div,
      BinaryOp::Rem => ExtFnKind::rem,
      _ => todo!(),
    };
    self.ext_fn(f.into(), lhs, rhs)
  }

  fn build_pat_value(&mut self, t: &Term) -> Port {
    match &t.kind {
      TermKind::Hole => Port::Erase,
      TermKind::Local(local) => {
        let v = self.net.new_wire();
        self.cur.steps.push_back(Step::Set(*local, v.0));
        v.1
      }
      TermKind::Tuple(t) => self.tuple(t.iter().map(|x| |slf: &mut Self| slf.build_pat_value(x))),
      TermKind::Ref(p) => {
        let (a, b) = self.build_pat_place(p);
        let r = self.net.new_wire();
        self.cur.agents.push(Agent::Comb("ref".to_owned(), r.0, a, b));
        r.1
      }
      _ => todo!(),
    }
  }

  fn list<F: FnOnce(&mut Self) -> Port>(
    &mut self,
    len: usize,
    items: impl Iterator<Item = F>,
  ) -> Port {
    let root = self.net.new_wire();
    let buf = self.net.new_wire();
    let end = self.net.new_wire();

    self.cur.agents.push(Agent::Comb("tup".to_owned(), root.0, Port::U32(len as u32), buf.0));

    let mut cur = self.apply_comb("fn".to_owned(), buf.1, end.0);
    for item in items {
      let item = item(self);
      cur = self.apply_comb("tup".to_owned(), cur, item)
    }
    self.net.link(cur, end.1);

    root.1
  }

  fn tuple<F: FnOnce(&mut Self) -> Port>(&mut self, items: impl Iterator<Item = F>) -> Port {
    let mut items = items.into_iter();
    let Some(mut prev_item) = items.next() else { return Port::Erase };

    let root = self.net.new_wire();

    let mut cur = root.0;

    for item in items {
      let prev = prev_item(self);
      cur = self.apply_comb("tup".to_owned(), cur, prev);
      prev_item = item;
    }

    let last = prev_item(self);
    self.net.link(last, cur);

    root.1
  }

  fn build_pat_place(&mut self, t: &Term) -> (Port, Port) {
    match &t.kind {
      TermKind::Hole => self.net.new_wire(),
      TermKind::Local(local) => {
        let x = self.net.new_wire();
        let y = self.net.new_wire();
        self.cur.steps.push_back(Step::Set(*local, x.0));
        self.cur.fin.push(Step::Get(*local, y.0));
        (x.1, y.1)
      }
      TermKind::Move(t) => (self.build_pat_value(t), Port::Erase),
      _ => todo!(),
    }
  }

  fn build_block(&mut self, b: &Block) -> Port {
    let mut last = Port::Erase;
    for s in &b.stmts {
      self.erase(last);
      last = self.build_stmt(s);
    }
    last
  }

  fn build_stmt(&mut self, s: &Stmt) -> Port {
    match &s.kind {
      StmtKind::Let(l) => {
        let i = l.init.as_ref().map(|x| self.build_expr_value(x)).unwrap_or(Port::Erase);
        let b = self.build_pat_value(&l.bind);
        self.net.link(b, i);
        Port::Erase
      }
      StmtKind::Term(t, semi) => {
        let t = self.build_expr_value(t);
        if *semi {
          self.erase(t);
          Port::Erase
        } else {
          t
        }
      }
      StmtKind::Empty => Port::Erase,
    }
  }

  fn dup(&mut self, t: Port) -> (Port, Port) {
    let t = self.net.follow(t);
    if t.can_copy() {
      (t.clone(), t)
    } else {
      let a = self.net.new_wire();
      let b = self.net.new_wire();
      self.cur.agents.push(Agent::Comb("dup".to_owned(), t, a.0, b.0));
      (a.1, b.1)
    }
  }

  fn ext_fn(&mut self, f: ExtFn, lhs: Port, rhs: Port) -> Port {
    let o = self.net.new_wire();
    self.cur.agents.push(Agent::ExtFn(f, lhs, rhs, o.0));
    o.1
  }

  fn erase(&mut self, t: Port) {
    self.net.link(t, Port::Erase);
  }

  fn stage_port(&self, id: StageId) -> Port {
    Port::Global(stage_name(&self.name, id))
  }

  fn new_interface(&mut self) -> InterfaceId {
    let id = self.interfaces.len();
    self.interfaces.push(Interface::default());
    id
  }

  fn select_stage(&mut self, id: StageId) {
    swap(&mut self.cur, &mut self.stages[self.cur_id]);
    swap(&mut self.cur, &mut self.stages[id]);
    self.cur_id = id;
  }

  fn new_stage(&mut self, i: InterfaceId, f: impl FnOnce(&mut Self, StageId) -> bool) -> StageId {
    let old_id = self.cur_id;
    let id = self.start_stage(i);

    let end = f(self, id);

    let fork = self.forks.last_mut().unwrap();
    if end {
      fork.ends.push(self.cur_id);
    }
    fork.divergence = fork.divergence.min(self.cur.divergence);

    self.select_stage(old_id);

    id
  }

  fn start_stage(&mut self, interface: InterfaceId) -> usize {
    let id = self.stages.len();
    self.stages.push(Stage::default());
    self.select_stage(id);
    self.cur.outer = interface;
    self.cur.divergence = ForkId::MAX;
    id
  }

  fn diverge(&mut self, to: ForkId) {
    let divergence = self.cur.divergence.min(to);
    self.cur.divergence = divergence;
    self.forks[to].ends.push(self.cur_id);
    let i = self.new_interface();
    // this dummy stage could still affect interfaces?
    self.start_stage(i);
    self.cur.divergence = divergence;
  }

  fn fork(&mut self, f: impl FnOnce(&mut Self)) {
    self.forks.push(Fork { ends: Vec::new(), divergence: self.forks.len() });
    let l = self.forks.len();
    f(self);
    debug_assert!(l == self.forks.len());
    let fork = self.forks.pop().unwrap();
    if fork.divergence < self.forks.len() {
      self.diverge(fork.divergence);
      for &c in &fork.ends {
        let port = self.stage_port(self.cur_id);
        let i = self.cur.outer;
        self.stages[c].steps.push_back(Step::Call(i, port));
      }
    };
  }

  fn goto(&mut self, stage: StageId) {
    let interface = self.stages[stage].outer;
    let stage = self.stage_port(stage);
    self.cur.steps.push_back(Step::Call(interface, stage));
  }

  fn cur_fork(&self) -> ForkId {
    self.forks.len() - 1
  }
}
