#!/usr/bin/env -S cargo -Z script
---
package.edition = "2024"
dependencies.regex = "*"
---

// @snap programs/regex/output.txt

use regex::Regex;

fn main() {
  let mut re_str = String::new();
  let mut re = Regex::new("^$").unwrap();
  let mut count = 0;
  let mut matches = 0;
  let mut invalid = false;
  for line in std::io::stdin().lines() {
    let line = line.unwrap();
    let char = line.chars().next().unwrap();
    let str = &line[2..];
    if char == '/' {
      re_str = str.into();
      re = Regex::new(&format!("^{str}$")).unwrap();
    } else {
      if re.is_match(str) != (char == 'T') {
        println!("invalid: /{re_str}/ \"{str}\" {char}");
        invalid = true;
      }
      if char == 'T' {
        matches += 1;
      }
      count += 1;
    }
  }
  println!("\rchecked {count}; {matches} matches");
  if invalid {
    std::process::exit(1);
  }
}
