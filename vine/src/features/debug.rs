use std::mem::replace;

use hedera::{
  name::NameId,
  net::{FlatNet, Wire},
};
use vine_util::nat::Nat;

use crate::{
  compiler::Guide,
  components::{emitter::Emitter, synthesizer::SyntheticItem},
  structures::{
    ast::Span,
    specializations::{Spec, SpecKind, SpecRels},
  },
};

impl Emitter<'_> {
  pub(crate) fn tap_debug(&mut self) -> Wire {
    let dbg_out = self.net.wire();
    let dbg_in = replace(&mut self.debug_state.as_mut().unwrap().0, dbg_out);
    self.net.make(self.guide.ref_, [dbg_in, dbg_out])
  }

  pub(crate) fn tap_debug_call(&mut self, span: Span) -> Wire {
    if self.fragment.frameless {
      return self.tap_debug();
    }
    let path = &self.fragment.path;
    let frame_name = self.guide.synthetic_frame.with_data(frame_data(path, span));
    let synthetic_item = SyntheticItem::Frame(path.clone(), span);
    self.specs.synthetic.entry((synthetic_item, vec![])).or_insert_with_key(|(item, _)| {
      self.specs.specs.push(Some(Spec {
        name: frame_name.clone(),
        rels: SpecRels::default(),
        kind: SpecKind::Synthetic(item.clone()),
      }))
    });
    let [io_in, stack_in, new_dbg_out, io_out, new_stack_out, frame_out, stack_out, dbg_out] =
      self.net.wires();
    let dbg_in = replace(&mut self.debug_state.as_mut().unwrap().0, dbg_out);
    self.net.add(self.guide.tuple, dbg_in, [io_in, stack_in]);
    let frame_in = self.net.make(frame_name, []);
    let new_stack_in = self.net.make(self.guide.tuple, [frame_in, stack_in]);
    let new_dbg_in = self.net.make(self.guide.tuple, [io_in, new_stack_in]);
    let ref_ = self.net.make(self.guide.ref_, [new_dbg_in, new_dbg_out]);
    self.net.add(self.guide.ref_, new_dbg_out, [io_out, new_stack_out]);
    self.net.add(self.guide.tuple, new_stack_out, [frame_out, stack_out]);
    self.net.add(self.guide.eraser, frame_out, []);
    self.net.add(self.guide.tuple, dbg_out, [io_out, stack_out]);
    ref_
  }

  pub fn with_debug(&mut self, inner: Wire) -> Wire {
    if self.debug {
      let dbg_in = self.net.wire();
      let dbg_out = self.net.wire();
      self.debug_state = Some((dbg_in, dbg_out));
      let ref_ = self.net.make(self.guide.ref_, [dbg_in, dbg_out]);
      self.net.make(self.guide.dbg, [ref_, inner])
    } else {
      inner
    }
  }
}

pub(crate) fn main_net_debug(main: NameId, guide: &Guide) -> FlatNet {
  let mut net = FlatNet::default();
  let [io_in, io_out, main_io_in, main_io_out, dbg_io_in, dbg_io_out] = net.wires();
  let free = net.make(guide.tuple, [io_in, io_out]);
  net.free.push(free);
  net.add(guide.io_split, io_in, [main_io_in, dbg_io_in]);
  net.add(guide.io_merge, main_io_out, [dbg_io_out, io_out]);
  let dbg_state_ref = {
    let stack_in = {
      let zero = net.make(guide.n32.with_data(0u32), []);
      let eraser = net.make(guide.eraser, []);
      let sentinel = net.make(guide.tuple, [zero, eraser]);
      let eraser = net.make(guide.eraser, []);
      net.make(guide.tuple, [sentinel, eraser])
    };
    let dbg_state_in = net.make(guide.tuple, [dbg_io_in, stack_in]);
    let stack_out = net.make(guide.eraser, []);
    let dbg_state_out = net.make(guide.tuple, [dbg_io_out, stack_out]);
    net.make(guide.ref_, [dbg_state_in, dbg_state_out])
  };
  let main_io_ref = net.make(guide.ref_, [main_io_in, main_io_out]);
  let main = net.make(guide.global.with_children([main]), []);
  let nil = net.make(guide.tuple, []);
  let call = net.make(guide.fn_, [main_io_ref, nil]);
  net.add(guide.dbg, main, [dbg_state_ref, call]);
  net
}

pub(crate) fn frame_data(path: &str, span: Span) -> Nat {
  Nat::encode_tuple([Nat::encode_string(path), Nat::new(vec![span.start as u32, span.end as u32])])
}
