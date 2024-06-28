use std::mem::{swap, take};

use ivm::ext::{ExtFn, ExtFnKind};

use crate::{ast::*, resolve::Node};

use super::{
  stage_name, Agent, Compiler, Interface, InterfaceId, Local, Port, Stage, StageId, Step, Usage,
};

enum ExprCtx {
  Value,
  Place,
  Either,
}

enum Expr {
  Value(Port),
  Place(Port, Port),
}

impl Compiler {
  pub(super) fn build_stages(&mut self, node: &Node, term: &Term) -> StageId {
    self.local_count = node.locals;
    self.name = node.canonical.to_string();
    let i = self.new_interface();
    self.new_stage(i, |slf, s| {
      slf.cur = Stage::default();
      let root = slf.build_expr_value(term);
      let root_val = slf.new_local();
      slf.cur.steps.push(Step::Set(root_val, root));
      slf.interfaces[i].inward.insert(root_val, Usage::GET);
      s
    })
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
          self.cur.steps.push(Step::Get(*local, v.0));
          Expr::Value(v.1)
        }
        ExprCtx::Place | ExprCtx::Either => {
          let x = self.net.new_wire();
          let y = self.net.new_wire();
          self.cur.steps.push(Step::Get(*local, x.0));
          self.cur.steps.push(Step::Set(*local, y.0));
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
        let result = self.net.new_wire();
        let mut cur = result.0;
        for param in params {
          let param = self.build_pat_value(param);
          cur = self.apply_comb("fn".to_owned(), cur, param);
        }
        let body = self.build_expr_value(body);
        self.net.link(cur, body);
        Expr::Value(result.1)
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
      TermKind::While(cond, body) => {
        let i = self.new_interface();
        let j = self.new_interface();

        let start = self.new_stage(i, |slf, start| {
          let cond = slf.build_expr_value(cond);

          let body = slf.new_stage(j, |slf, id| {
            let body = slf.build_block(body);
            slf.erase(body);
            let start = slf.stage_port(start);
            slf.cur.steps.push(Step::Call(i, start));
            id
          });

          let end = slf.new_stage(j, |_, id| id);

          let r = slf.net.new_wire();
          slf.cur.agents.push(Agent::Branch(cond, slf.stage_port(end), slf.stage_port(body), r.0));
          slf.cur.steps.push(Step::Call(j, r.1));

          start
        });

        self.cur.steps.push(Step::Call(i, self.stage_port(start)));

        Expr::Value(Port::Erase)
      }
      TermKind::If(cond, then, els) => {
        let val = self.new_local();
        let i = self.new_interface();

        let cond = self.build_expr_value(cond);

        let then = self.new_stage(i, |slf, id| {
          let then = slf.build_block(then);
          slf.cur.steps.push(Step::Set(val, then));
          id
        });

        let els = self.new_stage(i, |slf, id| {
          let els = slf.build_expr_value(els);
          slf.cur.steps.push(Step::Set(val, els));
          id
        });

        let r = self.net.new_wire();
        self.cur.agents.push(Agent::Branch(cond, self.stage_port(els), self.stage_port(then), r.0));
        self.cur.steps.push(Step::Call(i, r.1));

        let v = self.net.new_wire();
        self.cur.steps.push(Step::Get(val, v.0));
        Expr::Value(v.1)
      }
      TermKind::Move(t) => match t.kind {
        TermKind::Local(l) => {
          let v = self.net.new_wire();
          self.cur.steps.push(Step::Move(l, v.0));
          Expr::Value(v.1)
        }
        _ => {
          let (a, b) = self.build_expr_place(t);
          self.erase(b);
          Expr::Value(a)
        }
      },
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
        self.cur.steps.push(Step::Set(*local, v.0));
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
        self.cur.steps.push(Step::Set(*local, x.0));
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

  fn new_stage<T>(&mut self, interface: InterfaceId, f: impl FnOnce(&mut Self, StageId) -> T) -> T {
    let old_id = self.cur_id;
    let id = self.stages.len();
    self.stages.push(take(&mut self.cur));
    self.cur_id = id;
    self.cur.outer = interface;

    let res = f(self, id);

    self.cur.steps.extend(take(&mut self.cur.fin).into_iter().rev());

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
