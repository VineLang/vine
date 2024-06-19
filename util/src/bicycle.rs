use std::{
  cell::Cell,
  fmt::{self, Debug},
};

#[derive(Default)]
#[repr(transparent)]
pub struct BicycleState(Cell<usize>);

impl Debug for BicycleState {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.0.get() {
      0 => write!(f, "Todo"),
      usize::MAX => write!(f, "Done"),
      d => write!(f, "Cycle({})", d - 1),
    }
  }
}

impl BicycleState {
  pub fn reset(&self) {
    self.0.set(0);
  }
}

pub trait Bicycle {
  type Node: Copy;
  fn state(&mut self, node: Self::Node) -> &BicycleState;
  fn visit(&mut self, node: Self::Node, recurse: impl FnMut(&mut Self, Self::Node));

  fn visit_all(&mut self, nodes: impl IntoIterator<Item = Self::Node> + Clone) {
    for node in nodes.clone() {
      self.state(node).reset();
    }
    for node in nodes {
      visit(self, node, 1);
    }
  }
}

fn visit<B: Bicycle + ?Sized>(bi: &mut B, node: B::Node, depth: usize) -> usize {
  let state = bi.state(node);
  match state.0.get() {
    0 => state.0.set(depth),
    usize::MAX => return usize::MAX,
    d if depth != 0 => return d,
    _ => state.0.set(usize::MAX),
  }

  let head_depth = _visit(bi, node, depth);

  let state = bi.state(node);
  if depth > head_depth {
    state.0.set(head_depth);
  } else {
    state.0.set(usize::MAX);
    if depth == head_depth {
      _visit(bi, node, 0);
    }
  }

  head_depth
}

fn _visit<B: Bicycle + ?Sized>(bi: &mut B, node: B::Node, depth: usize) -> usize {
  let mut head_depth = usize::MAX;
  let depth = if depth == 0 { depth } else { depth + 1 };
  bi.visit(node, |bi, child| head_depth = head_depth.min(visit(bi, child, depth)));
  head_depth
}
