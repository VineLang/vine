#!/usr/bin/env -S cargo -Z script
---
package.edition = "2024"
---

// @snap programs/f64_ops/output.txt

trait AsF64 {
  fn as_f64(self) -> f64;
}

impl AsF64 for u64 {
  fn as_f64(self: u64) -> f64 {
    f64::from_bits(self)
  }
}

/// Returns `f64` args and the actual `f64`.
fn parse_f64s<'a>(mut parts: impl Iterator<Item = &'a str>) -> (Vec<f64>, f64) {
  let mut args = Vec::new();
  while let Some(bits) = parts.next() {
    args.push(f64::from_bits(bits.parse::<u64>().unwrap()))
  }
  let actual = args.pop().unwrap();
  (args, actual)
}

fn debug_f64(f: f64) -> String {
  format!("{:016x} ({f:?})", f.to_bits())
}

fn main() {
  let mut count = 0;
  let mut correct = 0;

  for line in std::io::stdin().lines() {
    let line = line.unwrap();
    if line.len() == 0 {
      continue;
    }
    count += 1;
    let mut parts = line.split_whitespace();
    let op = parts.next().unwrap();
    let (args, actual) = parse_f64s(parts);

    let (expected, max_ulp_diff) = match op {
      "abs" => (args[0].abs(), 0),
      "sqrt" => (args[0].sqrt(), 0),
      "exp" => (args[0].exp(), 1),
      "ln" => (args[0].ln(), 1),
      "sin" => (args[0].sin(), 1),
      "cos" => (args[0].cos(), 1),
      "atan" => (args[0].atan(), 1),

      "atan2" => (args[0].atan2(args[1]), 1),

      "pi" => (std::f64::consts::PI, 0),
      "eps" => (f64::EPSILON, 0),

      _ => panic!("unknown operation: {op}"),
    };
    let expected_bits = expected.to_bits();
    let ulp_diff = expected_bits.abs_diff(actual.to_bits());
    if expected.is_nan() && actual.is_nan() || ulp_diff <= max_ulp_diff {
      correct += 1;
    } else {
      eprintln!(
        "incorrect {op} [{args}]:\n  expect: {expect}\n  actual: {actual}\n  ulp_diff: {ulp_diff}",
        args = args.into_iter().map(debug_f64).collect::<Vec<_>>().join(", "),
        expect = debug_f64(expected),
        actual = debug_f64(actual),
      );
    }
  }

  eprintln!("checked {count}; {correct} correct");
  if correct != count {
    std::process::exit(1);
  }
}
