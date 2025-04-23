use core::{
  fmt::{self, Debug, Display, Write},
  ops::AddAssign,
  str,
  time::Duration,
};

/// Statistics about the execution of an IVM.
#[derive(Debug, Default, Clone, Copy)]
pub struct Stats {
  // Counts of various interaction types; each corresponds to a function in `interact.rs`.
  pub annihilate: u64,
  pub commute: u64,
  pub copy: u64,
  pub erase: u64,
  pub expand: u64,
  pub call: u64,
  pub branch: u64,

  /// Depth of the execution
  pub depth: u64,

  /// The size of the heap, in words; a high water mark of memory usage.
  pub mem_heap: u64,
  /// The number of words allocated over the course of execution.
  pub mem_alloc: u64,
  /// The number of words freed over the course of execution; should be equal to
  /// `alloc` when the net is normalized.
  pub mem_free: u64,

  /// The total time spent in `IVM::normalize`.
  pub time_total: Duration,
  pub time_clock: Duration,

  pub workers_used: u64,
  pub workers_spawned: u64,
  pub worker_min: u64,
  pub worker_max: u64,
  pub work_move: u64,
}

impl Stats {
  /// The total number of interactions.
  pub fn interactions(&self) -> u64 {
    self.annihilate + self.commute + self.copy + self.erase + self.expand + self.call + self.branch
  }

  /// The speed of the interactions, in interactions per second.
  pub fn speed(&self) -> u64 {
    (self.interactions() as f64 / self.time_total.as_secs_f64()) as u64
  }

  /// The speed of the interactions, in interactions per second.
  pub fn clock_speed(&self) -> u64 {
    (self.interactions() as f64 / self.time_clock.as_secs_f64()) as u64
  }
}

// This code is very complex, as it avoids doing any allocation. Premature
// optimization, to be sure, but will certainly be useful if this crate is ever
// `#![no_std]`.
impl Display for Stats {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut lines = vec![
      ("Interactions".to_string(), None),
      ("  Total".to_string(), Some((self.interactions(), ""))),
    ];

    if self.depth != 0 {
      lines.push(("  Depth".to_string(), Some((self.depth, ""))));
    }

    lines.extend([
      ("  Annihilate".to_string(), Some((self.annihilate, ""))),
      ("  Commute".to_string(), Some((self.commute, ""))),
      ("  Copy".to_string(), Some((self.copy, ""))),
      ("  Erase".to_string(), Some((self.erase, ""))),
      ("  Expand".to_string(), Some((self.expand, ""))),
      ("  Call".to_string(), Some((self.call, ""))),
      ("  Branch".to_string(), Some((self.branch, ""))),
      ("".to_string(), None),
    ]);

    if self.workers_used != 0 {
      let average_per_worker = ((self.interactions() as f64) / self.workers_used as f64) as u64;
      lines.extend([
        ("Workload".to_string(), None),
        ("  Workers".to_string(), Some((self.workers_spawned, ""))),
        ("  Active".to_string(), Some((self.workers_used, ""))),
        ("  Minimum".to_string(), Some((self.worker_min, ""))),
        ("  Average".to_string(), Some((average_per_worker, ""))),
        ("  Maximum".to_string(), Some((self.worker_max, ""))),
        ("  Moved".to_string(), Some((self.work_move, ""))),
        ("".to_string(), None),
      ]);
    }

    lines.extend([
      ("Memory".to_string(), None),
      ("  Heap".to_string(), Some((self.mem_heap * 8, "B"))),
      ("  Allocated".to_string(), Some((self.mem_alloc * 8, "B"))),
      ("  Freed".to_string(), Some((self.mem_free * 8, "B"))),
      ("".to_string(), None),
      ("Performance".to_string(), None),
      ("  Time".to_string(), Some((self.time_clock.as_millis() as u64, "ms"))),
      ("  Speed".to_string(), Some((self.clock_speed(), "IPS"))),
    ]);

    if self.workers_used != 0 {
      lines.extend([
        ("  Working".to_string(), Some((self.time_total.as_millis() as u64, "ms"))),
        ("  Rate".to_string(), Some((self.speed(), "IPS"))),
      ]);
    }

    let max_label_width = lines.iter().map(|x| x.0.len()).max().unwrap() + 1;
    let max_value = lines.iter().filter_map(|x| x.1).map(|x| x.0).max().unwrap().max(1_000_000_000);
    let max_value_width = measure_int(max_value);

    for (label, value) in lines {
      f.write_char('\n')?;
      f.write_str(&label)?;
      if let Some((mut value, unit)) = value {
        let value_width = measure_int(value);
        for _ in 0..(max_label_width + 2 + max_value_width - label.len() - value_width) {
          f.write_char(' ')?;
        }

        let mut text_buf = [0; measure_int(u64::MAX)];
        let mut index = text_buf.len();
        let mut digits = 0;
        while value != 0 || digits == 0 {
          if digits != 0 && digits % 3 == 0 {
            index -= 1;
            text_buf[index] = b'_';
          }
          index -= 1;
          text_buf[index] = b'0' + (value % 10) as u8;
          value /= 10;
          digits += 1;
        }
        f.write_str(unsafe { str::from_utf8_unchecked(&text_buf[index..]) })?;

        if !unit.is_empty() {
          f.write_char(' ')?;
          f.write_str(unit)?;
        }
      }
    }

    Ok(())
  }
}

/// Count the number of characters needed to pretty-print an integer.
const fn measure_int(int: u64) -> usize {
  if int == 0 {
    1
  } else {
    let digits = int.ilog10() + 1;
    (digits + (digits - 1) / 3) as usize
  }
}

impl AddAssign<Stats> for Stats {
  fn add_assign(&mut self, rhs: Stats) {
    self.annihilate += rhs.annihilate;
    self.commute += rhs.commute;
    self.copy += rhs.copy;
    self.erase += rhs.erase;
    self.expand += rhs.expand;
    self.call += rhs.call;
    self.branch += rhs.branch;
    self.mem_heap += rhs.mem_heap;
    self.mem_alloc += rhs.mem_alloc;
    self.mem_free += rhs.mem_free;
    self.time_total += rhs.time_total;
    self.work_move += rhs.work_move;
  }
}
