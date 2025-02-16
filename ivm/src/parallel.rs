use core::{mem, ptr};
use std::{
  sync::{Condvar, Mutex, OnceLock},
  thread::{self, Thread},
  time::Instant,
};

use crate::{ivm::IVM, port::Port};

impl<'ivm, 'ext> IVM<'ivm, 'ext> {
  pub fn normalize_parallel(&mut self, threads: usize) {
    self.do_fast();

    let shared = Vec::from_iter((0..threads).map(|_| Shared::default()));
    let start = Instant::now();
    thread::scope(|s| {
      let workers = Vec::from_iter((0..threads).map(|i| {
        let dispatch = thread::current();
        let alloc = self.alloc_pool.pop().unwrap_or(self.alloc.fork(i, threads));
        let shared = &shared[i];
        let mut ivm = IVM::new_from_allocator(alloc, self.extrinsics);
        if i == 0 {
          ivm.active_slow = mem::take(&mut self.active_slow);
        }
        s.spawn(move || Worker { ivm, shared, dispatch }.execute());
        WorkerHandle { shared }
      }));
      Dispatch { active: workers, idle: vec![] }.execute();
    });

    self.stats.time_clock += start.elapsed();
    if self.stats.worker_min == 0 {
      self.stats.worker_min = u64::MAX;
    }
    for mut shared in shared {
      let ivm = shared.ivm_return.take().unwrap();
      self.stats.workers_spawned += 1;
      if ivm.stats.interactions() != 0 {
        self.alloc_pool.push(ivm.alloc);
        self.stats.workers_used += 1;
        self.stats.worker_max = self.stats.worker_max.max(ivm.stats.interactions());
        self.stats.worker_min = self.stats.worker_min.min(ivm.stats.interactions());
        self.stats += ivm.stats;
      }
    }
  }
}

#[derive(Default)]
struct Msg<'ivm> {
  kind: MsgKind,
  pairs: Pairs<'ivm>,
}

#[derive(Default)]
#[repr(align(256))]
struct Shared<'ivm, 'ext> {
  ivm_return: OnceLock<IVM<'ivm, 'ext>>,
  msg: Mutex<Msg<'ivm>>,
  condvar: Condvar,
}

type Pairs<'ivm> = Vec<(Port<'ivm>, Port<'ivm>)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum MsgKind {
  #[default]
  None,
  Dispatch,
  WorkerShare,
  WorkerIdle,
  Exit,
}

struct Worker<'w, 'ivm, 'ext> {
  ivm: IVM<'ivm, 'ext>,
  shared: &'w Shared<'ivm, 'ext>,
  dispatch: Thread,
}

const ERA_LENGTH: u32 = 512;

impl<'w, 'ivm, 'ext> Worker<'w, 'ivm, 'ext> {
  fn execute(mut self) {
    self.work();
    self.shared.ivm_return.set(self.ivm).ok().unwrap();
  }

  fn work(&mut self) {
    'work: loop {
      let mut watermark = self.ivm.active_slow.len();
      let start = Instant::now();
      for _ in 0..ERA_LENGTH {
        self.ivm.do_fast();
        if let Some((a, b)) = self.ivm.active_slow.pop() {
          watermark = watermark.min(self.ivm.active_slow.len());
          self.ivm.interact(a, b);
        } else {
          self.ivm.stats.time_total += start.elapsed();

          let mut msg = self.shared.msg.lock().unwrap();
          while msg.pairs.is_empty() {
            msg.kind = MsgKind::WorkerIdle;
            self.dispatch.unpark();
            msg = self.shared.condvar.wait(msg).unwrap();
            if msg.kind == MsgKind::Exit {
              return;
            }
          }
          msg.kind = MsgKind::None;
          mem::swap(&mut self.ivm.active_slow, &mut msg.pairs);

          continue 'work;
        }
      }
      self.ivm.stats.time_total += start.elapsed();

      if watermark != 0 && watermark != self.ivm.active_slow.len() {
        let mut msg = self.shared.msg.lock().unwrap();
        if msg.kind == MsgKind::None {
          debug_assert!(msg.pairs.is_empty());
          let move_count = self.ivm.active_slow.len() - watermark;
          self.ivm.stats.work_move += move_count as u64;
          debug_assert!(move_count != 0);
          msg.pairs.reserve(move_count);
          unsafe {
            ptr::copy_nonoverlapping(
              self.ivm.active_slow.as_mut_ptr().add(watermark),
              msg.pairs.as_mut_ptr(),
              move_count,
            );
            self.ivm.active_slow.set_len(watermark);
            msg.pairs.set_len(move_count);
          }
          debug_assert!(!msg.pairs.is_empty());
          mem::swap(&mut msg.pairs, &mut self.ivm.active_slow);
          msg.kind = MsgKind::WorkerShare;
          self.dispatch.unpark();
        }
      }
    }
  }
}

struct WorkerHandle<'w, 'ivm, 'ext> {
  shared: &'w Shared<'ivm, 'ext>,
}

struct Dispatch<'w, 'ivm, 'ext> {
  active: Vec<WorkerHandle<'w, 'ivm, 'ext>>,
  idle: Vec<WorkerHandle<'w, 'ivm, 'ext>>,
}

impl<'w, 'ivm, 'ext> Dispatch<'w, 'ivm, 'ext> {
  fn execute(mut self) {
    loop {
      let mut i = 0;
      let mut share_ready = false;
      while i < self.active.len() {
        let worker = &self.active[i];
        let mut msg = worker.shared.msg.lock().unwrap();
        if msg.kind == MsgKind::WorkerIdle {
          debug_assert!(msg.pairs.is_empty());
          drop(msg);
          self.idle.push(self.active.remove(i));
          continue;
        } else if msg.kind == MsgKind::WorkerShare {
          if self.idle.is_empty() {
            share_ready = true;
          } else {
            debug_assert!(!msg.pairs.is_empty());
            let waiting = self.idle.pop().unwrap();
            let mut idle_msg = waiting.shared.msg.lock().unwrap();
            debug_assert!(idle_msg.kind == MsgKind::WorkerIdle && idle_msg.pairs.is_empty());
            idle_msg.kind = MsgKind::Dispatch;
            mem::swap(&mut msg.pairs, &mut idle_msg.pairs);
            waiting.shared.condvar.notify_one();
            msg.kind = MsgKind::None;
            self.active.insert(i + 1, waiting);
            i += 1;
          }
        }
        i += 1;
      }

      if self.active.is_empty() {
        break;
      }

      if !self.idle.is_empty() && share_ready {
        continue;
      }

      thread::park()
    }

    for thread in self.idle {
      thread.shared.msg.lock().unwrap().kind = MsgKind::Exit;
      thread.shared.condvar.notify_one();
    }
  }
}
