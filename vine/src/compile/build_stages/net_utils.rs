use std::mem::{replace, take};

use ivm::ext::{ExtFn, ExtFnKind};
use ivy::ast::Net;

use crate::{
  ast::*,
  compile::{stage_name, Agent, Compiler, Local, Port, StageId, Step},
  resolve::{Adt, Node},
};

impl Compiler<'_> {
  pub(super) fn new_comb(&mut self, label: impl Into<String>, a: Port, b: Port) -> Port {
    let out = self.net.new_wire();
    self.cur.agents.push(Agent::Comb(label.into(), out.0, a, b));
    out.1
  }

  pub(super) fn apply_combs<T>(
    &mut self,
    label: &str,
    to: Port,
    args: impl IntoIterator<Item = T>,
    mut f: impl FnMut(&mut Self, T) -> Port,
  ) -> Port {
    let mut cur = to;
    for arg in args {
      let arg = f(self, arg);
      let out = self.net.new_wire();
      self.cur.agents.push(Agent::Comb(label.to_string(), cur, arg, out.0));
      cur = out.1;
    }
    cur
  }

  pub(super) fn erase(&mut self, t: Port) {
    self.net.link(t, Port::Erase);
  }

  pub(super) fn dup(&mut self, t: Port) -> (Port, Port) {
    let t = self.net.follow(t);
    if t.can_copy() {
      (t.clone(), t)
    } else {
      let a = self.net.new_wire();
      let b = self.net.new_wire();
      self.cur.agents.push(Agent::Comb("dup".into(), t, a.0, b.0));
      (a.1, b.1)
    }
  }

  pub(super) fn tuple<T>(
    &mut self,
    items: impl IntoIterator<Item = T>,
    mut f: impl FnMut(&mut Self, T) -> Port,
  ) -> Port {
    let mut items = items.into_iter();
    let Some(mut prev_item) = items.next() else { return Port::Erase };

    let root = self.net.new_wire();

    let end = self.apply_combs("tup", root.0, items, |self_, item| {
      let prev_item = replace(&mut prev_item, item);
      f(self_, prev_item)
    });

    let last = f(self, prev_item);
    self.net.link(last, end);

    root.1
  }

  pub(super) fn tuple_pairs<T>(
    &mut self,
    items: impl IntoIterator<Item = T>,
    mut f: impl FnMut(&mut Self, T) -> (Port, Port),
  ) -> (Port, Port) {
    let mut pairs = items.into_iter().map(|t| f(self, t)).collect::<Vec<_>>();

    (self.tuple(&mut pairs, |_, x| take(&mut x.0)), self.tuple(pairs, |_, x| x.1))
  }

  pub(super) fn list<T>(
    &mut self,
    len: usize,
    items: impl IntoIterator<Item = T>,
    f: impl FnMut(&mut Self, T) -> Port,
  ) -> Port {
    let buf = self.net.new_wire();
    let end = self.apply_combs("tup", buf.0, items, f);
    let pair = self.new_comb("tup", buf.1, end);
    self.new_comb("tup", Port::U32(len as u32), pair)
  }

  pub(super) fn op(&mut self, op: BinaryOp, lhs: Port, rhs: Port) -> Port {
    let f = match op {
      BinaryOp::Concat => {
        return self.apply_combs("fn", Port::Global("::std::str::concat".into()), [lhs, rhs], id)
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

  pub(super) fn ext_fn(&mut self, f: ExtFn, lhs: Port, rhs: Port) -> Port {
    let o = self.net.new_wire();
    self.cur.agents.push(Agent::ExtFn(f, lhs, rhs, o.0));
    o.1
  }

  pub(super) fn stage_port(&self, id: StageId) -> Port {
    Port::Global(stage_name(&self.name, id))
  }

  pub(super) fn make_enum<T>(
    &mut self,
    adt: &Adt,
    variant: usize,
    fields: impl IntoIterator<Item = T>,
    mut f: impl FnMut(&mut Self, T) -> Port,
  ) -> Port {
    let r = self.net.new_wire();
    let mut out = Port::Erase;
    let mut fields = fields.into_iter();
    let end = self.apply_combs("enum", r.0, 0..adt.variants.len(), |self_, i| {
      if i == variant {
        let x = self_.net.new_wire();
        out = self_.apply_combs("enum", x.0, &mut fields, &mut f);
        x.1
      } else {
        Port::Erase
      }
    });
    self.net.link(end, out);
    r.1
  }

  pub(in crate::compile) fn lower_adt_constructor(&mut self, node: &Node) -> Net {
    let variant = node.variant.as_ref().unwrap();
    let adt = self.nodes[variant.adt].adt.as_ref().unwrap();
    let root = self.net.new_wire();

    let fields = variant.fields.iter().map(|_| self.net.new_wire().0).collect::<Vec<_>>();
    let ret = self.apply_combs("fn", root.0, fields.iter().cloned(), id);

    let out = if adt.variants.len() == 1 {
      self.tuple(fields, id)
    } else {
      self.make_enum(adt, variant.variant, fields, id)
    };

    self.net.link(ret, out);
    let stage = take(&mut self.cur);
    self.net.agents = stage.agents;
    self.net.finish(root.1)
  }

  pub(super) fn get_local(&mut self, local: Local) -> Port {
    let x = self.net.new_wire();
    self.get_local_to(local, x.0);
    x.1
  }

  pub(super) fn get_local_to(&mut self, local: Local, port: Port) {
    self.cur.steps.push_back(Step::Get(local, port));
  }

  pub(super) fn set_local(&mut self, local: Local) -> Port {
    let x = self.net.new_wire();
    self.set_local_to(local, x.0);
    x.1
  }

  pub(super) fn set_local_to(&mut self, local: Local, port: Port) {
    self.cur.steps.push_back(Step::Set(local, port));
  }

  pub(super) fn move_local(&mut self, local: Local) -> Port {
    let x = self.net.new_wire();
    self.cur.steps.push_back(Step::Move(local, x.0));
    x.1
  }

  pub(super) fn fin_move_local(&mut self, local: Local) -> Port {
    let x = self.net.new_wire();
    self.cur.fin.push(Step::Move(local, x.0));
    x.1
  }
}

fn id(_: &mut Compiler<'_>, port: Port) -> Port {
  port
}
