use std::mem::swap;

use crate::emitter_old::{Emitter, Fork, ForkId, Interface, InterfaceId, Stage, StageId, Step};

impl<'core> Emitter<'core, '_> {
  pub(super) fn new_interface(&mut self) -> InterfaceId {
    self.interfaces.push(Interface::default())
  }

  pub(super) fn select_stage(&mut self, id: StageId) {
    swap(&mut self.cur, &mut self.stages[self.cur_id]);
    swap(&mut self.cur, &mut self.stages[id]);
    self.cur_id = id;
  }

  pub(super) fn new_stage(
    &mut self,
    i: InterfaceId,
    f: impl FnOnce(&mut Self, StageId) -> bool,
  ) -> StageId {
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

  fn start_stage(&mut self, interface: InterfaceId) -> StageId {
    let id = self.stages.push(Stage::default());
    self.select_stage(id);
    self.cur.outer = interface;
    self.cur.divergence = ForkId(usize::MAX);
    id
  }

  pub(super) fn diverge(&mut self, to: ForkId) {
    let divergence = self.cur.divergence.min(to);
    self.cur.divergence = divergence;
    self.forks[to].ends.push(self.cur_id);
    let i = self.new_interface();
    // this dummy stage could still affect interfaces?
    self.start_stage(i);
    self.cur.divergence = divergence;
  }

  pub(super) fn diverge_to(&mut self, fork: ForkId, stage: StageId) {
    let divergence = self.cur.divergence.min(fork);
    self.cur.divergence = divergence;
    self.goto(stage);
    let i = self.new_interface();
    self.start_stage(i);
    self.cur.divergence = divergence;
  }

  pub(super) fn new_fork<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
    let l = self.forks.push(Fork { ends: Vec::new(), divergence: self.forks.next_index() });
    let out = f(self);
    let fork = self.forks.pop().unwrap();
    debug_assert!(l == self.forks.next_index());
    if fork.divergence < l {
      let divergence = self.cur.divergence.min(fork.divergence);
      self.cur.divergence = divergence;
      let i = self.new_interface();
      self.start_stage(i);
      self.cur.divergence = divergence;
      for &c in &fork.ends {
        let port = self.stage_port(self.cur_id);
        let i = self.cur.outer;
        self.stages[c].steps.push_back(Step::Call(i, port));
      }
    }
    out
  }

  pub(super) fn goto(&mut self, stage: StageId) {
    let interface = if stage == self.cur_id { self.cur.outer } else { self.stages[stage].outer };
    let stage = self.stage_port(stage);
    self.cur.steps.push_back(Step::Call(interface, stage));
  }

  pub(super) fn cur_fork(&self) -> ForkId {
    ForkId(self.forks.len() - 1)
  }
}
