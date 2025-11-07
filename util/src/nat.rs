#[derive(Debug, Clone)]
pub struct Nat(pub Vec<u32>);

impl Nat {
  pub const ZERO: Nat = Nat(Vec::new());

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
}
