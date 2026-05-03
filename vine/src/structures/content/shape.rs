use std::ops::{Add, Sub};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Length(pub u32);

impl Length {
  fn any(self) -> bool {
    self.0 != 0
  }
}

impl Add for Length {
  type Output = Length;
  fn add(self, rhs: Self) -> Self::Output {
    Length(self.0 + rhs.0)
  }
}

impl Sub for Length {
  type Output = Length;
  fn sub(self, rhs: Self) -> Self::Output {
    Length(self.0.saturating_sub(rhs.0))
  }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Segment(bool, Length, bool);

impl Segment {
  pub const EMPTY: Segment = Segment(false, Length(0), false);
  pub const SPACE: Segment = Segment(true, Length(0), true);
  pub const CHAR: Segment = Segment(false, Length(1), false);

  pub fn length(self) -> Length {
    self.1
  }
}

impl Add<Segment> for Segment {
  type Output = Segment;
  fn add(self, rhs: Segment) -> Segment {
    let Segment(a, b, c) = self;
    let Segment(x, y, z) = rhs;
    Segment(
      a || !b.any() && x,
      b + Length((b.any() && (c || x) && y.any()) as u32) + y,
      c && !y.any() || z,
    )
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Shape {
  Single(Segment),
  Multi(Segment, Segment),
}

impl Default for Shape {
  fn default() -> Self {
    Shape::EMPTY
  }
}

impl Shape {
  pub const EMPTY: Shape = Shape::Single(Segment::EMPTY);
  pub const SPACE: Shape = Shape::Single(Segment::SPACE);
  pub const CHAR: Shape = Shape::Single(Segment::CHAR);
  pub const LINE: Shape = Shape::Multi(Segment::EMPTY, Segment::EMPTY);

  pub fn of(str: &str) -> Self {
    Shape::Single(Segment(false, Length(str.chars().count() as u32), false))
  }

  pub fn is_multi(&self) -> bool {
    matches!(self, Shape::Multi(..))
  }

  fn max(self) -> Length {
    match self {
      Shape::Single(s) => s.length(),
      Shape::Multi(a, b) => a.length().max(b.length()),
    }
  }

  fn weight(self) -> Length {
    match self {
      Shape::Single(s) => s.length(),
      Shape::Multi(a, b) => a.length() + b.length(),
    }
  }

  pub fn leading(self) -> Segment {
    match self {
      Shape::Single(s) => s,
      Shape::Multi(s, _) => s,
    }
  }

  pub fn trailing(self) -> Segment {
    match self {
      Shape::Single(s) => s,
      Shape::Multi(_, s) => s,
    }
  }
}

impl Add for Shape {
  type Output = Shape;

  fn add(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Shape::Single(a), Shape::Single(b)) => Shape::Single(a + b),
      (Shape::Single(a), Shape::Multi(b, c)) => Shape::Multi(a + b, c),
      (Shape::Multi(a, b), Shape::Single(c)) => Shape::Multi(a, b + c),
      (Shape::Multi(a, _), Shape::Multi(_, b)) => Shape::Multi(a, b),
    }
  }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Shapes {
  pub min: Shape,
  pub max: Shape,
}

impl Add for Shapes {
  type Output = Shapes;

  fn add(self, rhs: Self) -> Self::Output {
    Shapes { min: self.min + rhs.min, max: self.max + rhs.max }
  }
}

impl Add<Shape> for Shapes {
  type Output = Shapes;

  fn add(self, rhs: Shape) -> Self::Output {
    self + Shapes::from(rhs)
  }
}

impl Add<Shapes> for Shape {
  type Output = Shapes;

  fn add(self, rhs: Shapes) -> Self::Output {
    Shapes::from(self) + rhs
  }
}

impl Shapes {
  pub fn quality(self) -> impl Ord {
    self.max.weight().0 as i32 - self.min.weight().0 as i32
  }
}

impl From<Shape> for Shapes {
  fn from(shape: Shape) -> Self {
    Shapes { min: shape, max: shape }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Surround {
  pub before: Segment,
  pub after: Segment,
}

impl From<()> for Shape {
  fn from((): ()) -> Self {
    Shape::EMPTY
  }
}

impl Surround {
  pub const EMPTY: Surround = Surround { before: Segment::EMPTY, after: Segment::EMPTY };

  pub fn permits(self, shape: Shape, width: Length) -> bool {
    (Shape::Single(self.before) + shape + Shape::Single(self.after)).max() <= width
  }

  pub fn new(before: impl Into<Shape>, after: impl Into<Shape>) -> Surround {
    Surround { before: before.into().trailing(), after: after.into().leading() }
  }

  pub fn inside(self, before: impl Into<Shape>, after: impl Into<Shape>) -> Surround {
    Surround {
      before: (Shape::Single(self.before) + before.into()).trailing(),
      after: (after.into() + Shape::Single(self.after)).leading(),
    }
  }

  pub fn max(width: Length) -> Self {
    Surround { before: Segment(false, width, false), after: Segment(false, width, false) }
  }
}
