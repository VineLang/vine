#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Usage {
  None,
  Erase,
  Mut,
  Set,
  Take,
  Get,
  Hedge,
  Wipe,
  Put,
  Req,
  Zero,
}

#[rustfmt::skip]
use Usage::{
  None  as N,
  Erase as E,
  Mut   as M,
  Set   as S,
  Take  as T,
  Get   as G,
  Hedge as H,
  Wipe  as W,
  Put   as P,
  Req   as R,
  Zero  as Z,
};

const UNION: [[Usage; 11]; 11] = [
  [N, W, M, P, R, G, H, W, P, R, N],
  [W, E, M, S, T, R, P, W, P, R, E],
  [M, M, M, M, M, M, M, M, M, M, M],
  [P, S, M, S, M, M, P, P, P, M, S],
  [R, T, M, M, T, R, M, R, M, R, T],
  [G, R, M, M, R, G, M, R, M, R, G],
  [H, P, M, P, M, M, H, P, P, M, H],
  [W, W, M, P, R, R, P, W, P, R, W],
  [P, P, M, P, M, M, P, P, P, M, P],
  [R, R, M, M, R, R, M, R, M, R, R],
  [N, E, M, S, T, G, H, W, P, R, Z],
];

const JOIN: [[Usage; 11]; 11] = [
  [N, E, M, S, T, G, H, W, P, R, Z],
  [E, E, S, S, E, E, S, E, S, E, Z],
  [M, T, M, M, T, M, M, M, M, M, Z],
  [S, E, S, S, E, S, S, S, S, S, Z],
  [T, T, M, M, T, T, M, T, M, T, Z],
  [G, T, M, M, T, G, M, R, M, R, Z],
  [H, E, M, S, T, M, H, P, P, M, Z],
  [W, E, M, S, T, R, P, W, P, R, Z],
  [P, E, M, S, T, M, P, P, P, M, Z],
  [R, T, M, M, T, R, M, R, M, R, Z],
  [Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z],
];

const INVERSE: [Usage; 11] = [N, E, M, T, S, H, G, W, R, P, Z];

const EFFECT: [[Usage; 11]; 11] = [
  [N, N, N, N, N, N, N, N, N, N, Z],
  [N, E, E, E, E, E, E, E, E, E, Z],
  [N, E, M, S, T, G, H, M, M, M, Z],
  [N, E, T, E, T, T, E, E, E, T, Z],
  [N, E, S, S, E, E, S, E, S, E, Z],
  [N, E, S, S, E, N, N, N, N, N, Z],
  [N, E, T, E, T, N, N, N, N, N, Z],
  [N, E, M, E, E, N, N, N, N, N, Z],
  [N, E, M, E, T, N, N, N, N, N, Z],
  [N, E, M, S, E, N, N, N, N, N, Z],
  [Z, Z, Z, Z, Z, Z, Z, Z, Z, Z, Z],
];

impl Usage {
  pub fn union(self: Usage, other: Usage) -> Usage {
    UNION[self as usize][other as usize]
  }

  pub fn join(self: Usage, other: Usage) -> Usage {
    JOIN[self as usize][other as usize]
  }

  pub fn inverse(self: Usage) -> Usage {
    INVERSE[self as usize]
  }

  pub fn effect(self: Usage, other: Usage) -> Usage {
    EFFECT[self as usize][other as usize]
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn usage() -> impl Iterator<Item = Usage> {
    [N, E, M, S, T, G, H, W, P, R, Z].into_iter()
  }

  #[test]
  fn union_identity() {
    for a in usage() {
      assert_eq!(a.union(a), a);
    }
  }

  #[test]
  fn union_commutativity() {
    for a in usage() {
      for b in usage() {
        assert_eq!(a.union(b), b.union(a));
      }
    }
  }

  #[test]
  fn union_associativity() {
    for a in usage() {
      for b in usage() {
        for c in usage() {
          assert_eq!(a.union(b).union(c), a.union(b.union(c)));
        }
      }
    }
  }

  #[test]
  fn join_identity() {
    for a in usage() {
      assert_eq!(a.join(N), a);
      assert_eq!(N.join(a), a);
      assert_eq!(a.join(a), a);
    }
  }

  #[test]
  fn join_associativity() {
    for a in usage() {
      for b in usage() {
        for c in usage() {
          assert_eq!(a.join(b).join(c), a.join(b.join(c)));
        }
      }
    }
  }

  #[test]
  fn distributivity() {
    for a in usage() {
      for b in usage() {
        for c in usage() {
          assert_eq!(a.join(b.union(c)), a.join(b).union(a.join(c)));
          assert_eq!(a.union(b).join(c), a.join(c).union(b.join(c)));
        }
      }
    }
  }

  #[test]
  fn inverse() {
    for a in usage() {
      assert_eq!(a.inverse().inverse(), a);
      for b in usage() {
        assert_eq!(a.union(b).inverse(), a.inverse().union(b.inverse()));
        assert_eq!(a.join(b).inverse(), b.inverse().join(a.inverse()));
      }
    }
  }

  #[test]
  fn effect() {
    for a in usage() {
      for b in usage() {
        let ab = a.effect(b);
        let ba = b.effect(a);
        assert!(matches!(ab, G | T | M) == matches!(ba, H | S | M));
        assert!(ab.inverse() == EFFECT[a.inverse() as usize][b.inverse() as usize]);
      }
    }
  }
}
