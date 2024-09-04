use crate::{
  ast::*,
  compile::{Agent, Compiler, Port, Step},
};

use super::Expr;

impl Compiler<'_> {
  pub(super) fn lower_fn(&mut self, params: &Vec<Term>, body: &Term) -> Expr {
    let func = self.net.new_wire();
    let res = self.apply_combs("fn", func.0, params, Self::lower_pat_value);
    let result = self.new_local();
    let orig = self.cur_id;
    let old_break = self.break_target.take();
    let old_return = self.return_target.replace((result, self.cur_fork()));
    let body = self.lower_expr_value(body);
    self.return_target = old_return;
    self.break_target = old_break;
    self.set_local_to(result, body);
    if self.cur_id != orig {
      self.select_stage(orig);
    }
    self.get_local_to(result, res);
    Expr::Value(func.1)
  }

  pub(super) fn lower_return(&mut self, r: &Term) -> Expr {
    let r = self.lower_expr_value(r);
    let ret = self.return_target.unwrap();
    self.set_local_to(ret.0, r);
    self.diverge(ret.1);
    Expr::Value(Port::Erase)
  }

  pub(super) fn lower_if(&mut self, cond: &Term, then: &Block, els: &Term) -> Expr {
    let val = self.new_local();

    let cond = self.lower_expr_value(cond);

    self.new_fork(|self_| {
      let i = self_.new_interface();
      let then = self_.new_stage(i, |self_, _| {
        let then = self_.lower_block(then);
        self_.set_local_to(val, then);
        true
      });

      let els = self_.new_stage(i, |self_, _| {
        let els = self_.lower_expr_value(els);
        self_.set_local_to(val, els);
        true
      });

      let r = self_.net.new_wire();
      self_.cur.agents.push(Agent::Branch(
        cond,
        self_.stage_port(els),
        self_.stage_port(then),
        r.0,
      ));
      self_.cur.steps.push_back(Step::Call(i, r.1));
    });

    Expr::Value(self.get_local(val))
  }

  pub(super) fn lower_loop(&mut self, body: &Block) -> Expr {
    self.new_fork(|self_| {
      let i = self_.new_interface();
      let s = self_.new_stage(i, move |self_, s| {
        let old = self_.break_target.replace(self_.cur_fork());
        self_.lower_block_erase(body);
        self_.break_target = old;
        self_.goto(s);
        false
      });

      self_.goto(s);
    });

    Expr::Value(Port::Erase)
  }

  pub(super) fn lower_while(&mut self, cond: &Term, body: &Block) -> Expr {
    self.new_fork(|self_| {
      let old_break = self_.break_target.replace(self_.cur_fork());

      let i = self_.new_interface();
      let start = self_.new_stage(i, move |self_, start| {
        let cond = self_.lower_expr_value(cond);

        self_.new_fork(|self_| {
          let j = self_.new_interface();
          let body = self_.new_stage(j, |self_, _| {
            self_.lower_block_erase(body);
            self_.goto(start);
            false
          });

          let end = self_.new_stage(j, |_, _| true);

          let r = self_.net.new_wire();
          self_.cur.agents.push(Agent::Branch(
            cond,
            self_.stage_port(end),
            self_.stage_port(body),
            r.0,
          ));
          self_.cur.steps.push_back(Step::Call(j, r.1));
        });

        true
      });

      self_.break_target = old_break;

      self_.goto(start);
    });

    Expr::Value(Port::Erase)
  }

  pub(super) fn lower_break(&mut self) -> Expr {
    self.diverge(self.break_target.unwrap());
    Expr::Value(Port::Erase)
  }
}
