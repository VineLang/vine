use crate::structures::content::{Color, Colored, IntoElement};

use super::{Element, Shape, Shapes, Surround, Writer};

/// A delimited sequence of elements.
pub struct Delimited {
  delims: &'static Delims,
  children: Box<[Box<dyn Element>]>,
  indent: bool,
  force_multi: bool,
  allow_final_multi: bool,
  break_final: bool,
}

impl Delimited {
  pub fn new<C: IntoElement>(
    delims: &'static Delims,
    children: impl IntoIterator<Item = C>,
  ) -> Self {
    let children = children.into_iter().map(|c| c.into_element()).collect();
    Delimited {
      delims,
      children,
      indent: true,
      force_multi: false,
      allow_final_multi: false,
      break_final: false,
    }
  }

  /// Indent the children when multi-line. Defaults to `true`.
  pub fn indent(self, indent: bool) -> Self {
    Delimited { indent, ..self }
  }

  /// Force this to be multi-line. Defaults to `false`.
  pub fn force_multi(self, force_multi: bool) -> Self {
    Delimited { force_multi, ..self }
  }

  /// Allow the last child to be multi-line without making the whole element
  /// multi-line. Defaults to `false`.
  pub fn allow_final_multi(self, allow_final_multi: bool) -> Self {
    Delimited { allow_final_multi, ..self }
  }

  /// Break the last child before making the whole element multi-line. Only
  /// relevant when `allow_final_multi` is `true`. Defaults to `false`.
  pub fn break_final(self, break_final: bool) -> Self {
    Delimited { break_final, ..self }
  }
}

pub struct Delims {
  pub empty: &'static str,
  pub single: (
    &'static str, // open
    &'static str, // separator
    &'static str, // close
  ),
  pub multi: (
    &'static str, // open
    &'static str, // prefix
    &'static str, // suffix
    &'static str, // close
  ),
  pub color: Color,
}

const VAGUE: Color = Color::VAGUE;
impl Delims {
  pub const TUPLE: &Delims =
    &Delims { empty: "()", single: ("(", ", ", ",)"), multi: ("(", "", ",", ")"), color: VAGUE };
  pub const BRACE: &Delims =
    &Delims { empty: "{}", single: ("{ ", " ", " }"), multi: ("{", "", "", "}"), color: VAGUE };
  pub const BRACE_COMMA: &Delims =
    &Delims { empty: "{}", single: ("{ ", ", ", " }"), multi: ("{", "", ",", "}"), color: VAGUE };
  pub const BRACE_COMMA_UNSPACED: &Delims =
    &Delims { empty: "{}", single: ("{", ", ", "}"), multi: ("{", "", ",", "}"), color: VAGUE };
  pub const PAREN_COMMA: &Delims =
    &Delims { empty: "()", single: ("(", ", ", ")"), multi: ("(", "", ",", ")"), color: VAGUE };
  pub const BRACKET_COMMA: &Delims =
    &Delims { empty: "[]", single: ("[", ", ", "]"), multi: ("[", "", ",", "]"), color: VAGUE };
  pub const NONE: &Delims =
    &Delims { empty: "", single: ("", "", ""), multi: ("", "", "", ""), color: Color::NORMAL };
}

impl IntoElement for Delimited {
  fn into_element(mut self) -> Box<dyn Element> {
    if self.children.is_empty() {
      return Box::new(Colored(self.delims.color, self.delims.empty));
    }
    let (open, separator, close) = self.delims.single;
    let mut initial_shape = Shape::of(open);
    let last = self.children.len() - 1;
    for child in &mut self.children[0..last] {
      initial_shape = initial_shape + child.measure().max + Shape::of(separator);
    }
    let final_shapes = self.children[last].measure();
    let base = DelimitedBase { delims: self.delims, children: self.children, indent: self.indent };
    if self.force_multi
      || initial_shape.is_multi()
      || (!self.allow_final_multi && final_shapes.max.is_multi())
    {
      Box::new(base)
    } else if self.allow_final_multi && self.break_final {
      Box::new(DelimitedBreakFinal { base, initial_shape, final_shapes })
    } else {
      Box::new(DelimitedStandard {
        base,
        shape: initial_shape + final_shapes.max + Shape::of(close),
      })
    }
  }
}

/// The most basic form of a delimited element, supporting only a multi-line
/// layout.
struct DelimitedBase {
  delims: &'static Delims,
  children: Box<[Box<dyn Element>]>,
  indent: bool,
}

impl Element for DelimitedBase {
  fn measure(&mut self) -> Shapes {
    let (open, _, _, close) = self.delims.multi;
    Shapes::from(Shape::of(open) + Shape::LINE + Shape::of(close))
  }

  fn format(&mut self, writer: &mut Writer, _: Surround) {
    let (open, prefix, suffix, close) = self.delims.multi;
    writer.write_color(self.delims.color, open);
    writer.indent_if(self.indent, |writer| {
      for child in &mut self.children {
        writer.write_color(self.delims.color, prefix);
        child.format(writer, Surround::new(Shape::of(prefix), Shape::of(suffix)));
        writer.write_color(self.delims.color, suffix);
        writer.line();
      }
    });
    writer.write_color(self.delims.color, close);
  }
}

/// A typical delimited element which can either be multi-line or single-line.
struct DelimitedStandard {
  base: DelimitedBase,
  /// The shape of this element when single-line.
  shape: Shape,
}

impl Element for DelimitedStandard {
  fn measure(&mut self) -> Shapes {
    Shapes { min: self.base.measure().min, max: self.shape }
  }

  fn format(&mut self, writer: &mut Writer, surround: Surround) {
    if surround.permits(self.shape, writer.width()) {
      let (open, separator, close) = self.base.delims.single;
      writer.write_color(self.base.delims.color, open);
      let mut first = true;
      for child in &mut self.base.children {
        if !first {
          writer.write_color(self.base.delims.color, separator);
        }
        first = false;
        child.format(writer, Surround::EMPTY);
      }
      writer.write_color(self.base.delims.color, close);
    } else {
      self.base.format(writer, surround);
    }
  }
}

/// A delimited element with `break_final` enabled.
struct DelimitedBreakFinal {
  base: DelimitedBase,
  /// The single-line shape of everything before the last child.
  initial_shape: Shape,
  /// The shapes of the last child.
  final_shapes: Shapes,
}

impl Element for DelimitedBreakFinal {
  fn measure(&mut self) -> Shapes {
    let (_, _, close) = self.base.delims.single;
    Shapes {
      min: self.base.measure().min,
      max: self.initial_shape + self.final_shapes.max + Shape::of(close),
    }
  }

  fn format(&mut self, writer: &mut Writer, surround: Surround) {
    let (open, separator, close) = self.base.delims.single;
    if surround
      .permits(self.initial_shape + self.final_shapes.min + Shape::of(close), writer.width())
    {
      writer.write_color(self.base.delims.color, open);
      let last = self.base.children.len() - 1;
      for child in &mut self.base.children[0..last] {
        child.format(writer, Surround::EMPTY);
        writer.write_color(self.base.delims.color, separator);
      }
      self.base.children[last]
        .format(writer, surround.inside(self.initial_shape, Shape::of(close)));
      writer.write_color(self.base.delims.color, close);
    } else {
      self.base.format(writer, surround);
    }
  }
}
