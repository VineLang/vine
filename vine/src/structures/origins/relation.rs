use std::{
  fmt::{self, Debug},
  num::NonZeroU8,
  ops::{Add, BitAnd, BitAndAssign},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Relation(NonZeroU8);

impl Relation {
  pub const LE: Self = Self::new(0b00_10);
  pub const LT: Self = Self::new(0b00_11);
  pub const GE: Self = Self::new(0b10_00);
  pub const GT: Self = Self::new(0b11_00);
  pub const EQ: Self = Self::new(0b10_10);

  const fn new(n: u8) -> Self {
    Self(NonZeroU8::new(n).unwrap())
  }

  fn val(self) -> u8 {
    self.0.into()
  }

  pub fn rev(self) -> Self {
    Self::new(((self.val() << 2) & 0b1111) | (self.val() >> 2))
  }

  pub fn lte_component(self) -> Option<Self> {
    Some(Self(NonZeroU8::new(self.val() & 0b00_11)?))
  }

  pub fn gte_component(self) -> Option<Self> {
    Some(Self(NonZeroU8::new(self.val() & 0b11_00)?))
  }

  pub fn not_equal(self) -> Self {
    Self::new(self.val() | ((self.val() & 0b1010) >> 1))
  }

  pub fn allows_equal(self) -> bool {
    (self.val() & 0b01_01) == 0
  }

  pub fn le(self) -> bool {
    (self.val() & 0b00_10) != 0
  }
}

impl BitAnd for Relation {
  type Output = Relation;

  fn bitand(self, rhs: Self) -> Self::Output {
    Self(self.0 | rhs.0)
  }
}

impl BitAndAssign for Relation {
  fn bitand_assign(&mut self, rhs: Self) {
    self.0 |= rhs.0;
  }
}

impl Add for Relation {
  type Output = Option<Relation>;

  fn add(self, rhs: Self) -> Self::Output {
    Some(Self(NonZeroU8::new(
      self.lte_component().and_then(|x| Some((x & rhs.lte_component()?).val())).unwrap_or(0)
        | self.gte_component().and_then(|x| Some((x & rhs.gte_component()?).val())).unwrap_or(0),
    )?))
  }
}

impl Debug for Relation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      Relation::LE => f.write_str("<="),
      Relation::LT => f.write_str("<"),
      Relation::GE => f.write_str(">="),
      Relation::GT => f.write_str(">"),
      Relation::EQ => f.write_str("=="),
      _ => write!(f, "{:?} & {:?}", self.lte_component().unwrap(), self.gte_component().unwrap()),
    }
  }
}
