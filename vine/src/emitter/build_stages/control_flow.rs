use crate::{
  ast::*,
  emitter::{Agent, Emitter, Port, Step},
};

impl Emitter<'_> {
  pub(super) fn emit_fn(&mut self, params: &[(Pat, Option<Ty>)], body: &Expr) -> Port {
    self._emit_fn(params, |self_| self_.emit_expr_value(body))
  }

  pub(super) fn _emit_fn(
    &mut self,
    params: &[(Pat, Option<Ty>)],
    body: impl FnOnce(&mut Self) -> Port,
  ) -> Port {
    let func = self.net.new_wire();
    let res = self.apply_combs("fn", func.0, params.iter().map(|x| &x.0), Self::emit_pat_value);
    let result = self.new_local();
    let orig = self.cur_id;
    let old_loop = self.loop_target.take();
    let old_return = self.return_target.replace((result, self.cur_fork()));
    let body = body(self);
    self.return_target = old_return;
    self.loop_target = old_loop;
    self.set_local_to(result, body);
    if self.cur_id != orig {
      self.select_stage(orig);
    }
    self.get_local_to(result, res);
    func.1
  }

  pub(super) fn emit_return(&mut self, r: Option<&Expr>) -> Port {
    let ret = self.return_target.unwrap();
    if let Some(r) = r {
      let r = self.emit_expr_value(r);
      self.set_local_to(ret.0, r);
    }

    self.diverge(ret.1);
    Port::Erase
  }

  pub(super) fn emit_if(&mut self, cond: &Expr, then: &Block, els: &Expr) -> Port {
    let val = self.new_local();

    self.new_fork(|self_| {
      self_.emit_cond(
        cond,
        &|self_| {
          let then = self_.emit_block(then);
          self_.set_local_to(val, then);
          true
        },
        &|self_| {
          let els = self_.emit_expr_value(els);
          self_.set_local_to(val, els);
          true
        },
      );
    });

    self.get_local(val)
  }

  pub(super) fn emit_loop(&mut self, body: &Block) -> Port {
    let local = self.new_local();

    self.new_fork(|self_| {
      let i = self_.new_interface();
      let s = self_.new_stage(i, move |self_, s| {
        let old_loop = self_.loop_target.replace((local, self_.cur_fork(), s));
        self_.emit_block_erase(body);
        self_.loop_target = old_loop;
        self_.goto(s);
        false
      });

      self_.goto(s);
    });

    self.get_local(local)
  }

  pub(super) fn emit_while(&mut self, cond: &Expr, body: &Block) -> Port {
    let local = self.new_local();

    self.new_fork(|self_| {
      let i = self_.new_interface();
      let start = self_.new_stage(i, move |self_, start| {
        self_.emit_cond(
          cond,
          &|self_| {
            let old_loop = self_.loop_target.replace((local, self_.cur_fork(), start));
            self_.emit_block_erase(body);
            self_.goto(start);
            self_.loop_target = old_loop;
            false
          },
          &|_| true,
        );

        false
      });

      self_.goto(start);
    });

    self.get_local(local)
  }

  pub(super) fn emit_break(&mut self, r: Option<&Expr>) -> Port {
    let (local, fork, _stage) = self.loop_target.unwrap();
    if let Some(r) = r {
      let r = self.emit_expr_value(r);
      self.set_local_to(local, r);
    }

    self.diverge(fork);
    Port::Erase
  }

  pub(super) fn emit_continue(&mut self) -> Port {
    let (_local, fork, stage) = self.loop_target.unwrap();
    self.diverge_to(fork, stage);
    Port::Erase
  }

  pub(super) fn emit_cond(
    &mut self,
    expr: &Expr,
    yay: &dyn Fn(&mut Self) -> bool,
    nay: &dyn Fn(&mut Self) -> bool,
  ) {
    match &expr.kind {
      ExprKind![!cond] => {
        let bool = self.emit_expr_value(expr);
        let i = self.new_interface();
        let yay = self.new_stage(i, |self_, _| yay(self_));
        let nay = self.new_stage(i, |self_, _| nay(self_));
        let r = self.net.new_wire();
        self.cur.agents.push(Agent::Branch(bool, self.stage_port(nay), self.stage_port(yay), r.0));
        self.cur.steps.push_back(Step::Call(i, r.1));
      }
      ExprKind::Is(expr, pat) => {
        self.emit_match(
          expr,
          [(&**pat, yay), (&Pat { span: Span::NONE, kind: PatKind::Hole }, nay)],
          |self_, f| f(self_),
        );
      }
      ExprKind::Not(cond) => {
        self.emit_cond(cond, nay, yay);
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        let nay = self.share_dyn_stage(nay);
        self.emit_cond(
          a,
          &|self_| {
            self_.emit_cond(b, yay, &nay);
            false
          },
          &nay,
        );
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        let yay = self.share_dyn_stage(yay);
        self.emit_cond(a, &yay, &|self_| {
          self_.emit_cond(b, &yay, nay);
          false
        });
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        let yay = self.share_dyn_stage(yay);
        self.emit_cond(
          a,
          &|self_| {
            self_.emit_cond(b, &yay, nay);
            false
          },
          &yay,
        );
      }
    }
  }

  fn share_dyn_stage(&mut self, nay: &dyn Fn(&mut Self) -> bool) -> impl Fn(&mut Self) -> bool {
    let i = self.new_interface();
    let stage = self.new_stage(i, |self_, _| nay(self_));
    move |self_| {
      self_.goto(stage);
      false
    }
  }
}