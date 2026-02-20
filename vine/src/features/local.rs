use std::mem::take;

use hedera::net::{FlatNet, FlatNode, Wire};

use crate::{
  components::{distiller::Distiller, emitter::Emitter},
  structures::{
    ast::Span,
    tir::Local,
    types::{Inverted, Type},
    vir::{Invocation, Port, Stage, VirLocal},
  },
};

impl Distiller<'_> {
  pub(crate) fn distill_expr_value_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: Local,
  ) -> Port {
    stage.local_read(local, span, ty)
  }

  pub(crate) fn distill_expr_space_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: Local,
  ) -> Port {
    stage.local_write(local, span, ty)
  }

  pub(crate) fn distill_expr_place_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: Local,
  ) -> (Port, Port) {
    stage.local_read_write(local, span, ty)
  }

  pub(crate) fn distill_pat_value_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: Local,
  ) -> Port {
    stage.declarations.push(local);
    stage.local_barrier_write(local, span, ty)
  }

  pub(crate) fn distill_pat_space_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: Local,
  ) -> Port {
    stage.declarations.push(local);
    stage.local_read_barrier(local, span, ty)
  }

  pub(crate) fn distill_pat_place_local(
    &mut self,
    stage: &mut Stage,
    span: Span,
    ty: Type,
    local: &Local,
  ) -> (Port, Port) {
    stage.declarations.push(*local);
    let (a, b) = stage.local_read_write(*local, span, ty);
    (b, a)
  }

  pub(crate) fn distill_pat_uninit_local(&mut self, stage: &mut Stage, span: Span, local: Local) {
    stage.declarations.push(local);
    stage.local_barrier(local, span);
  }

  pub(crate) fn distill_pat_loop_local(&mut self, stage: &mut Stage, local: Local) {
    stage.declarations.push(local);
  }
}

#[derive(Debug)]
pub(crate) struct LocalEmissionState {
  inv: Inverted,
  past: Vec<(Vec<Wire>, Vec<Wire>)>,
  spaces: Vec<Wire>,
  values: Vec<Wire>,
}

impl Emitter<'_> {
  fn local(&mut self, local: Local) -> &mut LocalEmissionState {
    self.locals.entry(local).or_insert_with(|| LocalEmissionState {
      inv: self.vir.locals[local].inv,
      past: Vec::new(),
      spaces: Vec::new(),
      values: Vec::new(),
    })
  }

  pub(crate) fn emit_invocation(&mut self, local: &Local, invocation: &Invocation) {
    match invocation {
      Invocation::Barrier => self.local(*local).barrier(),
      Invocation::Read(port) => {
        let wire = self.emit_port(port);
        self.local(*local).read(wire)
      }
      Invocation::Write(port) => {
        let wire = self.emit_port(port);
        self.local(*local).write(wire)
      }
      Invocation::ReadBarrier(port) => {
        let wire = self.emit_port(port);
        self.local(*local).read_barrier(wire)
      }
      Invocation::BarrierWrite(port) => {
        let wire = self.emit_port(port);
        self.local(*local).barrier_write(wire)
      }
      Invocation::ReadWrite(a, b) => {
        let a = self.emit_port(a);
        let b = self.emit_port(b);
        self.local(*local).read_write(a, b)
      }
    }
  }

  pub(crate) fn emit_compound_invocation(
    &mut self,
    local: Local,
    read: bool,
    barrier: bool,
    write: bool,
  ) -> (Option<Wire>, Option<Wire>) {
    let read = read.then(|| {
      let w = self.net.wire();
      self.local(local).read(w);
      w
    });
    if barrier {
      self.local(local).barrier();
    }
    let write = write.then(|| {
      let w = self.net.wire();
      self.local(local).write(w);
      w
    });
    (read, write)
  }

  pub(crate) fn finish_local(&mut self, local: &VirLocal, mut state: LocalEmissionState) {
    if state.past.is_empty() {
      state.past.push((Vec::new(), Vec::new()));
    }
    let first = &mut state.past[0];
    state.spaces.append(&mut first.0);
    first.0 = state.spaces;
    state.values.append(&mut first.1);
    first.1 = state.values;
    if local.self_inverse {
      for port in state.past.into_iter().flat_map(|(x, y)| [x, y]).flatten() {
        self.net.push(FlatNode(self.guide.tuple, [port]));
      }
      return;
    }
    for (spaces, values) in state.past.into_iter() {
      let (mut sources, mut sinks) = if local.inv.0 { (spaces, values) } else { (values, spaces) };
      if local.inv.0 {
        sinks.reverse();
      }
      assert!(sources.len() <= 1);
      let source = sources.pop();
      if sinks.is_empty() {
        if let Some(source) = source {
          let drop_rel = local.drop.unwrap();
          let drop_tree = self.emit_fn_rel(drop_rel);
          self.pairs.push((drop_tree, Tree::n_ary("fn", [Tree::Erase, source, Tree::Erase])));
        }
      } else {
        let source = source.unwrap();
        if sinks.len() == 1 {
          self.pairs.push((source, sinks.pop().unwrap()));
        } else {
          let fork_rel = local.fork.unwrap();
          let sink = sinks
            .into_iter()
            .reduce(|former, latter| {
              let wire = self.new_wire();
              let fork_tree = self.emit_fn_rel(fork_rel);
              self.pairs.push((
                fork_tree,
                Tree::n_ary("fn", [Tree::Erase, Tree::n_ary("ref", [wire.0, latter]), former]),
              ));
              wire.1
            })
            .unwrap();
          self.pairs.push((source, sink));
        }
      }
    }
  }
}

impl LocalEmissionState {
  fn read(&mut self, wire: Wire) {
    self.spaces.push(wire);
    if self.inv.0 {
      self.barrier();
    }
  }

  fn read_barrier(&mut self, wire: Wire) {
    self.read(wire);
    self.barrier();
  }

  fn write(&mut self, wire: Wire) {
    if !self.inv.0 {
      self.barrier();
    }
    self.values.push(wire);
  }

  fn barrier_write(&mut self, wire: Wire) {
    self.barrier();
    self.write(wire);
  }

  fn read_write(&mut self, a: Wire, b: Wire) {
    self.read(a);
    self.write(b);
  }

  fn barrier(&mut self) {
    if self.past.is_empty() || !self.spaces.is_empty() || !self.values.is_empty() {
      self.past.push((take(&mut self.spaces), take(&mut self.values)));
    }
  }
}
