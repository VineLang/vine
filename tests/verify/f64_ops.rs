#!/usr/bin/env -S cargo -Z script
---
package.edition = "2024"
---

// @snap programs/f64_ops/output.txt

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
    let f_bits = parts.next().unwrap().parse::<u64>().unwrap();
    let actual_bits = parts.next().unwrap().parse::<u64>().unwrap();
    let f = f64::from_bits(f_bits);
    let actual = f64::from_bits(actual_bits);
    let (expected, max_ulp_diff) = match op {
      "sqrt" => (f.sqrt(), 0),
      "exp" => (f.exp(), 1),
      "ln" => (f.ln(), 0),
      _ => panic!("unknown operation: {op}"),
    };

    if expected.is_nan() && actual.is_nan() {
      correct += 1;
    } else if expected.to_bits().abs_diff(actual_bits) > max_ulp_diff {
      println!(
        "incorrect {op} {f:.17}: expected = {expected:.17} ({:x}), actual = {actual:.17} ({actual_bits:x})",
        expected.to_bits()
      );
    } else {
      correct += 1;
    }
  }

  println!("checked {count}; {correct} correct");
  if correct != count {
    std::process::exit(1);
  }
}
