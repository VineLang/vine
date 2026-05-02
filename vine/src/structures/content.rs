mod delimited;
mod shape;
mod writer;

use std::{iter, mem::replace};

pub use delimited::*;
pub use shape::*;
use vine_util::sum_tree::SumTree;
pub use writer::*;

use crate::structures::ast::Ident;

pub struct Content {
  children: Box<[(Shape, Box<dyn Element>, Shape)]>,
  shapes: Shapes,
}

pub enum BreakOrder {
  Even,
  Left,
  Right,
  Smart,
}

impl Content {
  pub fn even(content: impl IntoContent) -> Self {
    Content::new(BreakOrder::Even, content)
  }

  pub fn left(content: impl IntoContent) -> Self {
    Content::new(BreakOrder::Left, content)
  }

  pub fn right(content: impl IntoContent) -> Self {
    Content::new(BreakOrder::Right, content)
  }

  pub fn smart(content: impl IntoContent) -> Self {
    Content::new(BreakOrder::Smart, content)
  }

  pub fn new(break_order: BreakOrder, content: impl IntoContent) -> Self {
    let mut self_ = Content {
      children: content.elements().map(|el| (Shape::EMPTY, el, Shape::EMPTY)).collect(),
      shapes: Shapes::default(),
    };
    self_.initialize(break_order);
    self_
  }

  fn initialize(&mut self, break_order: BreakOrder) {
    if let BreakOrder::Smart = break_order {
      let mut element_shapes = self
        .children
        .iter_mut()
        .map(|(_, child, _)| {
          let shapes = child.measure();
          self.shapes = self.shapes + shapes;
          shapes
        })
        .enumerate()
        .collect::<Vec<_>>();
      let mut sum_tree = SumTree::new(element_shapes.iter().map(|(_, shapes)| shapes.max));
      element_shapes.sort_by_key(|(_, shapes)| shapes.quality());
      for (i, shapes) in element_shapes.into_iter().rev() {
        let len = self.children.len();
        let (before, _, after) = &mut self.children[i];
        *before = sum_tree.sum(0..i);
        *after = sum_tree.sum(i + 1..len);
        sum_tree.update(i, shapes.min);
      }
    } else {
      let mut acc = Shape::EMPTY;
      for (before, child, after) in &mut self.children {
        let shapes = child.measure();
        self.shapes = self.shapes + shapes;
        (*before, *after) = match break_order {
          BreakOrder::Smart => unreachable!(),
          BreakOrder::Even => (shapes.max, shapes.max),
          BreakOrder::Left => (shapes.min, shapes.max),
          BreakOrder::Right => (shapes.max, shapes.min),
        };
        acc = acc + replace(before, acc);
      }
      acc = Shape::EMPTY;
      for (_, _, after) in self.children.iter_mut().rev() {
        acc = replace(after, acc) + acc;
      }
    }
  }

  pub fn all<C: IntoContent>(iter: impl IntoIterator<Item = C>) -> impl IntoContent {
    struct All<I>(I);
    impl<I: Iterator<Item = Box<dyn Element>>> IntoContent for All<I> {
      fn elements(self) -> impl Iterator<Item = Box<dyn Element>> {
        self.0
      }
    }
    All(iter.into_iter().flat_map(|x| x.elements()))
  }
}

pub trait Element: 'static {
  fn measure(&mut self) -> Shapes;
  fn format(&mut self, writer: &mut Writer, surround: Surround);
}

impl Element for Content {
  fn measure(&mut self) -> Shapes {
    self.shapes
  }

  fn format(&mut self, writer: &mut Writer, surround: Surround) {
    for (before, child, after) in &mut self.children {
      child.format(writer, surround.inside(*before, *after));
    }
  }
}

pub trait IntoElement {
  fn into_element(self) -> Box<dyn Element>;
}

impl<T: Element> IntoElement for T {
  fn into_element(self) -> Box<dyn Element> {
    Box::new(self)
  }
}

impl IntoElement for Box<dyn Element> {
  fn into_element(self) -> Box<dyn Element> {
    self
  }
}

pub trait IntoContent: Sized {
  fn elements(self) -> impl Iterator<Item = Box<dyn Element>>;
}

impl<T: IntoElement> IntoContent for T {
  fn elements(self) -> impl Iterator<Item = Box<dyn Element>> {
    iter::once(self.into_element())
  }
}

macro_rules! impl_tuple {
  ($($A:ident)* | $B:ident $($C:ident)*) => {
    impl_tuple!($($A)* |);
    impl_tuple!($($A)* $B | $($C)*);
  };

  ($($T:ident)* |) => {
    #[allow(nonstandard_style)]
    impl<$($T: IntoContent,)*> IntoContent for ($($T,)*) {
      fn elements(self) -> impl Iterator<Item = Box<dyn Element>> {
        let ($($T,)*) = self;
        iter::empty()
          $(.chain($T.elements()))*
      }
    }
  }
}

impl_tuple!(| A B C D E F G H I J L M N O P Q R S T U V W X Y Z);

impl<C: IntoContent> IntoContent for Option<C> {
  fn elements(self) -> impl Iterator<Item = Box<dyn Element>> {
    self.map(|c| c.elements()).into_iter().flatten()
  }
}

pub struct Empty;

impl Element for Empty {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::EMPTY)
  }

  fn format(&mut self, _: &mut Writer, _: Surround) {}
}

impl Element for char {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::CHAR)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write_char(*self);
  }
}

impl Element for &'static str {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::of(self))
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write(self);
  }
}

impl Element for String {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::of(self))
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write(self);
  }
}

pub struct Colored<T>(pub Color, pub T);

impl Element for Colored<char> {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::CHAR)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write_color_char(self.0, self.1);
  }
}

impl Element for Colored<&'static str> {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::of(self.1))
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write_color(self.0, self.1);
  }
}

impl Element for Colored<String> {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::of(&self.1))
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.write_color(self.0, &self.1);
  }
}

pub struct Space;

impl Element for Space {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::SPACE)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.space();
  }
}

pub struct Line;

impl Element for Line {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::LINE)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.line();
  }
}

pub struct Blank;

impl Element for Blank {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::LINE)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.blank();
  }
}

pub struct Indent {
  child: Box<dyn Element>,
  shapes: Shapes,
  mode: IndentMode,
}

pub enum IndentMode {
  Always,
  Lazy,
  Eager,
}

impl Indent {
  pub fn always(child: impl IntoElement) -> Self {
    Indent::new(IndentMode::Always, child)
  }

  pub fn lazy(child: impl IntoElement) -> Self {
    Indent::new(IndentMode::Lazy, child)
  }

  pub fn eager(child: impl IntoElement) -> Self {
    Indent::new(IndentMode::Eager, child)
  }

  pub fn new(mode: IndentMode, child: impl IntoElement) -> Self {
    let mut child = child.into_element();
    let shapes = child.measure();
    Indent { child, shapes, mode }
  }
}

impl Element for Indent {
  fn measure(&mut self) -> Shapes {
    Shapes {
      min: Shape::LINE,
      max: match self.mode {
        IndentMode::Always => Shape::LINE,
        IndentMode::Lazy | IndentMode::Eager => self.shapes.max,
      },
    }
  }

  fn format(&mut self, writer: &mut Writer, surround: Surround) {
    let indent = match self.mode {
      IndentMode::Always => true,
      IndentMode::Lazy => !surround.permits(self.shapes.min, writer.width()),
      IndentMode::Eager => {
        self.shapes.max.is_multi() || !surround.permits(self.shapes.max, writer.width())
      }
    };
    if indent {
      writer.indent(|writer| self.child.format(writer, Surround::EMPTY))
    } else {
      self.child.format(writer, surround);
    }
  }
}

pub struct Keyword(pub &'static str);

impl IntoElement for Keyword {
  fn into_element(self) -> Box<dyn Element> {
    Box::new(Colored(Color::KEYWORD, self.0))
  }
}

pub struct Punct(pub &'static str);

impl IntoElement for Punct {
  fn into_element(self) -> Box<dyn Element> {
    Box::new(Colored(Color::VAGUE, self.0))
  }
}

pub struct Operator(pub &'static str);

impl IntoElement for Operator {
  fn into_element(self) -> Box<dyn Element> {
    Box::new(Colored(Color::KEYWORD, self.0))
  }
}

impl IntoElement for Ident {
  fn into_element(self) -> Box<dyn Element> {
    Box::new(self.0)
  }
}

pub struct Lines(Box<[Box<dyn Element>]>);

impl Lines {
  pub fn new<C: IntoElement>(lines: impl IntoIterator<Item = C>) -> Self {
    Lines(lines.into_iter().map(|c| c.into_element()).collect())
  }
}

impl Element for Lines {
  fn measure(&mut self) -> Shapes {
    Shapes::from(Shape::LINE)
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    writer.line();
    for child in &mut self.0 {
      child.format(writer, Surround::EMPTY);
      writer.line();
    }
  }
}
