use vine_util::trivial_visit;

use super::*;

#[derive_(Debug, Clone, Visit!)]
pub enum ExprLiteral {
  Char(LiteralChar),
  Nat(LiteralNat),
  Float(LiteralFloat),
}

trivial_visit!(LiteralChar, LiteralNat, LiteralFloat);

#[derive_(Debug, Clone)]
pub struct LiteralChar {
  pub escaped: bool,
  pub char: char,
}

#[derive_(Debug, Clone)]
pub struct LiteralNat {
  pub base: Base,
  pub digits: Vec<Digit>,
  pub exponent: usize,
}

#[derive_(Debug, Clone)]
pub struct LiteralFloat {
  pub base: Base,
  pub digits: Vec<Digit>,
  pub fraction: Vec<Digit>,
  pub exponent: isize,
}

/// Either a digit in the range `0..16` (stored as its value), or a `SEPARATOR`.
#[derive_(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Digit(u8);

impl Digit {
  pub const SEPARATOR: Digit = Digit(u8::MAX);

  pub fn char(self) -> char {
    match self {
      Digit(digit @ 0..16) => char::from_digit(digit as u32, 16).unwrap(),
      Digit::SEPARATOR => '_',
      _ => unreachable!(),
    }
  }
}

#[derive_(Debug, Clone, Copy)]
pub enum Base {
  Binary,
  Octal,
  Decimal,
  Hex,
}

impl Base {
  pub fn value(self) -> u8 {
    match self {
      Base::Binary => 2,
      Base::Octal => 8,
      Base::Decimal => 10,
      Base::Hex => 16,
    }
  }

  pub fn exponent(self) -> Exponent {
    match self {
      Base::Binary | Base::Octal | Base::Hex => Exponent::Binary,
      Base::Decimal => Exponent::Decimal,
    }
  }
}

#[derive_(Debug, Clone, Copy)]
pub enum Exponent {
  Binary,
  Decimal,
}

impl Exponent {
  pub fn value(self) -> u8 {
    match self {
      Exponent::Binary => 2,
      Exponent::Decimal => 10,
    }
  }
}

impl ParsePratt<Expr, ExprLiteral> for Parser<'_> {} // todo
