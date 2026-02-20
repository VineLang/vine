use std::{
  fmt::{self, Write},
  iter,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Nat(pub Vec<u32>);

impl Nat {
  pub const ZERO: Nat = Nat(Vec::new());

  pub fn new(mut parts: Vec<u32>) -> Self {
    while parts.last().is_some_and(|x| *x == 0) {
      parts.pop();
    }
    Self(parts)
  }

  pub fn add_u32(&mut self, mut n: u32) {
    for d in &mut self.0 {
      if n == 0 {
        return;
      }
      let (new, carry) = u32::overflowing_add(*d, n);
      *d = new;
      n = carry as u32;
    }
    if n != 0 {
      self.0.push(n);
    }
  }

  pub fn mul_u32(&mut self, n: u32) {
    let mut carry = 0;
    for d in &mut self.0 {
      let total = *d as u64 * n as u64 + carry;
      *d = total as u32;
      carry = total >> 32;
    }
    if carry != 0 {
      self.0.push(carry as u32);
    }
  }

  pub fn as_u32(&mut self) -> Option<u32> {
    match self.0[..] {
      [] => Some(0),
      [n] => Some(n),
      _ => None,
    }
  }

  pub fn as_u64(&mut self) -> Option<u64> {
    match self.0[..] {
      [] => Some(0),
      [n] => Some(n as u64),
      [n, m] => Some(n as u64 | (m as u64) << 32),
      _ => None,
    }
  }
}

impl fmt::Display for Nat {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut start = true;
    f.write_str("0x")?;
    for n in self.0.iter().rev() {
      for i in 0..8 {
        let n = n >> (i * 4) & 0xf;
        if !start || n != 0 {
          start = false;
          f.write_char(char::from_digit(n, 16).unwrap())?;
        }
      }
      start = false;
    }
    Ok(())
  }
}

impl From<u32> for Nat {
  fn from(n: u32) -> Nat {
    if n == 0 { Nat::ZERO } else { Nat(vec![n]) }
  }
}

impl From<u64> for Nat {
  fn from(n: u64) -> Nat {
    if n <= u32::MAX as u64 { (n as u32).into() } else { Nat(vec![n as u32, (n >> 32) as u32]) }
  }
}

impl From<usize> for Nat {
  fn from(n: usize) -> Nat {
    (n as u64).into()
  }
}

impl From<f32> for Nat {
  fn from(n: f32) -> Nat {
    n.to_bits().into()
  }
}

impl From<f64> for Nat {
  fn from(n: f64) -> Nat {
    n.to_bits().into()
  }
}

impl Nat {
  pub fn encode_string(str: &str) -> Nat {
    Nat::new(iter::once(str.len() as u32).chain(str.chars().map(|x| x as u32)).collect())
  }

  pub fn encode_tuple<const N: usize>(ns: [Nat; N]) -> Nat {
    Nat::new(ns.into_iter().flat_map(|n| iter::once(n.0.len() as u32).chain(n.0)).collect())
  }
}
