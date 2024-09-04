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

    self.new_fork(|slf| {
      let i = slf.new_interface();
      let then = slf.new_stage(i, |slf, _| {
        let then = slf.lower_block(then);
        slf.set_local_to(val, then);
        true
      });

      let els = slf.new_stage(i, |slf, _| {
        let els = slf.lower_expr_value(els);
        slf.set_local_to(val, els);
        true
      });

      let r = slf.net.new_wire();
      slf.cur.agents.push(Agent::Branch(cond, slf.stage_port(els), slf.stage_port(then), r.0));
      slf.cur.steps.push_back(Step::Call(i, r.1));
    });

    Expr::Value(self.get_local(val))
  }

  pub(super) fn lower_loop(&mut self, body: &Block) -> Expr {
    self.new_fork(|slf| {
      let i = slf.new_interface();
      let s = slf.new_stage(i, move |slf, s| {
        let old = slf.break_target.replace(slf.cur_fork());
        slf.lower_block_erase(body);
        slf.break_target = old;
        slf.goto(s);
        false
      });

      slf.goto(s);
    });

    Expr::Value(Port::Erase)
  }

  pub(super) fn lower_while(&mut self, cond: &Term, body: &Block) -> Expr {
    self.new_fork(|slf| {
      let old_break = slf.break_target.replace(slf.cur_fork());

      let i = slf.new_interface();
      let start = slf.new_stage(i, move |slf, start| {
        let cond = slf.lower_expr_value(cond);

        slf.new_fork(|slf| {
          let j = slf.new_interface();
          let body = slf.new_stage(j, |slf, _| {
            slf.lower_block_erase(body);
            slf.goto(start);
            false
          });

          let end = slf.new_stage(j, |_, _| true);

          let r = slf.net.new_wire();
          slf.cur.agents.push(Agent::Branch(cond, slf.stage_port(end), slf.stage_port(body), r.0));
          slf.cur.steps.push_back(Step::Call(j, r.1));
        });

        true
      });

      slf.break_target = old_break;

      slf.goto(start);
    });

    Expr::Value(Port::Erase)
  }

  pub(super) fn lower_break(&mut self) -> Expr {
    self.diverge(self.break_target.unwrap());
    Expr::Value(Port::Erase)
  }
}
