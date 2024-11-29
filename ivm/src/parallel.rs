use core::{mem, ptr};
use std::{
  sync::{Condvar, Mutex, OnceLock},
  thread::{self, Thread},
  time::Instant,
};

use crate::{ivm::IVM, port::Port};

impl<'ivm> IVM<'ivm> {
  pub fn normalize_parallel(&mut self, threads: usize) {
    self.do_fast();

    let shared = Vec::from_iter((0..threads).map(|_| Shared::default()));
    let start = Instant::now();
    thread::scope(|s| {
      let workers = Vec::from_iter((0..threads).map(|i| {
        let dispatch = thread::current();
        let alloc = self.alloc_pool.pop().unwrap_or(self.alloc.fork(i, threads));
        let shared = &shared[i];
        let mut ivm = IVM::new_from_allocator(alloc);
        if i == 0 {
          ivm.active_slow = mem::take(&mut self.active_slow);
        }
        s.spawn(move || Worker { ivm, shared, dispatch }.execute());
        WorkerHandle { shared }
      }));
      Dispatch { working: workers, waiting: vec![] }.execute();
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
#[repr(align(256))]
struct Shared<'ivm> {
  ivm_return: OnceLock<IVM<'ivm>>,
  msg: Mutex<(Msg, Pairs<'ivm>)>,
  condvar: Condvar,
}

type Pairs<'ivm> = Vec<(Port<'ivm>, Port<'ivm>)>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Msg {
  #[default]
  None,
  Dispatch,
  Worker,
  WorkerStarve,
  Fin,
}

struct Worker<'w, 'ivm> {
  ivm: IVM<'ivm>,
  shared: &'w Shared<'ivm>,
  dispatch: Thread,
}

const ERA_LENGTH: u32 = 512;

impl<'w, 'ivm> Worker<'w, 'ivm> {
  fn execute(mut self) {
    self.work();
    self.shared.ivm_return.set(self.ivm).ok().unwrap();
  }

  fn work(&mut self) {
    let mut watermark = self.ivm.active_slow.len();
    let start = Instant::now();
    for _ in 0..ERA_LENGTH {
      self.ivm.do_fast();
      if let Some((a, b)) = self.ivm.active_slow.pop() {
        watermark = watermark.min(self.ivm.active_slow.len());
        self.ivm.interact(a, b);
      } else {
        return self.starve();
      }
    }
    self.ivm.stats.time_total += start.elapsed();
    self.pause(watermark)
  }

  fn starve(&mut self) {
    {
      let mut msg = self.shared.msg.lock().unwrap();
      while msg.1.is_empty() {
        msg.0 = Msg::WorkerStarve;
        self.dispatch.unpark();
        msg = self.shared.condvar.wait(msg).unwrap();
        if msg.0 == Msg::Fin {
          return;
        }
      }
      msg.0 = Msg::None;
      mem::swap(&mut self.ivm.active_slow, &mut msg.1);
    }
    self.work()
  }

  fn pause(&mut self, watermark: usize) {
    if watermark != 0 && watermark != self.ivm.active_slow.len() {
      let mut msg = self.shared.msg.lock().unwrap();
      if msg.0 == Msg::None {
        debug_assert!(msg.1.is_empty());
        let move_count = self.ivm.active_slow.len() - watermark;
        self.ivm.stats.work_move += move_count as u64;
        debug_assert!(move_count != 0);
        msg.1.reserve(move_count);
        unsafe {
          ptr::copy_nonoverlapping(
            self.ivm.active_slow.as_mut_ptr().add(watermark),
            msg.1.as_mut_ptr(),
            move_count,
          );
          self.ivm.active_slow.set_len(watermark);
          msg.1.set_len(move_count);
        }
        debug_assert!(!msg.1.is_empty());
        mem::swap(&mut msg.1, &mut self.ivm.active_slow);
        msg.0 = Msg::Worker;
        self.dispatch.unpark();
      }
    }
    self.work()
  }
}

struct WorkerHandle<'w, 'ivm> {
  shared: &'w Shared<'ivm>,
}

struct Dispatch<'w, 'ivm> {
  working: Vec<WorkerHandle<'w, 'ivm>>,
  waiting: Vec<WorkerHandle<'w, 'ivm>>,
}

impl<'w, 'ivm> Dispatch<'w, 'ivm> {
  fn execute(mut self) {
    loop {
      let mut i = 0;
      while i < self.working.len() {
        let worker = &self.working[i];
        let mut msg = worker.shared.msg.lock().unwrap();
        if msg.0 == Msg::WorkerStarve {
          debug_assert!(msg.1.is_empty());
          drop(msg);
          self.waiting.push(self.working.remove(i));
          continue;
        } else if msg.0 == Msg::Worker && !self.waiting.is_empty() {
          debug_assert!(!msg.1.is_empty());
          let waiting = self.waiting.pop().unwrap();
          let mut wmsg = waiting.shared.msg.lock().unwrap();
          debug_assert!(wmsg.0 == Msg::WorkerStarve && wmsg.1.is_empty());
          wmsg.0 = Msg::Dispatch;
          mem::swap(&mut msg.1, &mut wmsg.1);
          waiting.shared.condvar.notify_one();
          msg.0 = Msg::None;
          self.working.insert(i + 1, waiting);
        }
        i += 1;
      }

      if self.working.is_empty() {
        break;
      }

      thread::park()
    }

    for thread in self.waiting {
      thread.shared.msg.lock().unwrap().0 = Msg::Fin;
      thread.shared.condvar.notify_one();
    }
  }
}
