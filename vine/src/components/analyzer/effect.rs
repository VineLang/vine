#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Effect {
  P,
  B,
  PB,
  RP,
  RB,
  RPB,
  BW,
  PBW,
  RBW,
  RPBW,
  X,
}

use Effect::*;

#[rustfmt::skip]
const JOIN: &[[Effect; 11]; 11] = &[
  [P,    B,    PB,   RP,   RB,   RPB,  BW,   PBW,  RBW,  RPBW, X   ],
  [B,    B,    B,    B,    B,    B,    BW,   B,    BW,   B,    X   ],
  [PB,   B,    PB,   RPB,  RB,   RPB,  BW,   PB,   RBW,  RPB,  X   ],
  [RP,   RB,   RPB,  RP,   RB,   RPB,  RBW,  RPBW, RBW,  RPBW, X   ],
  [RB,   RB,   RB,   RB,   RB,   RB,   RBW,  RB,   RBW,  RB,   X   ],
  [RPB,  RB,   RPB,  RPB,  RB,   RPB,  RBW,  RPB,  RBW,  RPB,  X   ],
  [BW,   B,    B,    BW,   B,    B,    BW,   BW,   BW,   BW,   X   ],
  [PBW,  B,    PB,   RPBW, RB,   RPB,  BW,   PBW,  RBW,  RPBW, X   ],
  [RBW,  RB,   RB,   RBW,  RB,   RB,   RBW,  RBW,  RBW,  RBW,  X   ],
  [RPBW, RB,   RPB,  RPBW, RB,   RPB,  RBW,  RPBW, RBW,  RPBW, X   ],
  [X,    X,    X,    X,    X,    X,    X,    X,    X,    X,    X   ],
];

#[rustfmt::skip]
const UNION: &[[Effect; 11]; 11] = &[
  [P,    PB,   PB,   RP,   RPB,  RPB,  PBW,  PBW,  RPBW, RPBW, P   ],
  [PB,   B,    PB,   RPB,  RB,   RPB,  B,    PB,   RB,   RPB,  B   ],
  [PB,   PB,   PB,   RPB,  RPB,  RPB,  PB,   PB,   RPB,  RPB,  PB  ],
  [RP,   RPB,  RPB,  RP,   RPB,  RPB,  RPBW, RPBW, RPBW, RPBW, RP  ],
  [RPB,  RB,   RPB,  RPB,  RB,   RPB,  RB,   RPB,  RB,   RPB,  RB  ],
  [RPB,  RPB,  RPB,  RPB,  RPB,  RPB,  RPB,  RPB,  RPB,  RPB,  RPB ],
  [PBW,  B,    PB,   RPBW, RB,   RPB,  BW,   PBW,  RBW,  RPBW, BW  ],
  [PBW,  PB,   PB,   RPBW, RPB,  RPB,  PBW,  PBW,  RPBW, RPBW, PBW ],
  [RPBW, RB,   RPB,  RPBW, RB,   RPB,  RBW,  RPBW, RBW,  RPBW, RBW ],
  [RPBW, RPB,  RPB,  RPBW, RPB,  RPB,  RPBW, RPBW, RPBW, RPBW, RPBW],
  [P,    B,    PB,   RP,   RB,   RPB,  BW,   PBW,  RBW,  RPBW, X   ],
];

impl Effect {
  pub fn join(self: Effect, other: Effect) -> Effect {
    JOIN[self as usize][other as usize]
  }

  pub fn union(self: Effect, other: Effect) -> Effect {
    UNION[self as usize][other as usize]
  }

  pub fn read(self) -> bool {
    matches!(self, RP | RB | RPB | RBW | RPBW)
  }

  pub fn pass(self) -> bool {
    matches!(self, P | PB | RP | RPB | PBW | RPBW)
  }

  pub fn barrier(self) -> bool {
    matches!(self, B | PB | RB | RPB | BW | PBW | RBW | RPBW)
  }

  pub fn write(self) -> bool {
    matches!(self, BW | PBW | RBW | RPBW)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn effects() -> impl Iterator<Item = Effect> {
    [P, B, PB, RP, RB, RPB, BW, PBW, RBW, RPBW, X].into_iter()
  }

  #[test]
  fn join_identity() {
    for a in effects() {
      assert_eq!(a.join(P), a);
      assert_eq!(P.join(a), a);
    }
  }

  #[test]
  fn join_associative() {
    for a in effects() {
      for b in effects() {
        for c in effects() {
          assert_eq!(a.join(b).join(c), a.join(b.join(c)));
        }
      }
    }
  }

  #[test]
  fn join_idempotent() {
    for a in effects() {
      assert_eq!(a.join(a), a);
    }
  }

  #[test]
  fn union_identity() {
    for a in effects() {
      assert_eq!(a.union(X), a);
      assert_eq!(X.union(a), a);
    }
  }

  #[test]
  fn union_commutative() {
    for a in effects() {
      for b in effects() {
        assert_eq!(a.union(b), b.union(a));
      }
    }
  }

  #[test]
  fn union_associative() {
    for a in effects() {
      for b in effects() {
        for c in effects() {
          assert_eq!(a.union(b).union(c), a.union(b.union(c)));
        }
      }
    }
  }

  #[test]
  fn union_idempotent() {
    for a in effects() {
      assert_eq!(a.union(a), a);
    }
  }

  #[test]
  fn distributive() {
    for a in effects() {
      for b in effects() {
        for c in effects() {
          assert_eq!(a.join(b.union(c)), a.join(b).union(a.join(c)));
          assert_eq!(a.union(b).join(c), a.join(c).union(b.join(c)));
        }
      }
    }
  }
}
