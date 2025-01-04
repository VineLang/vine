use std::{borrow::Cow, ops::Add};

use crate::ast::Ident;

const MAX_WIDTH: usize = 100;

#[derive(Debug)]
pub struct Doc<'src> {
  measure: Measure,
  kind: DocKind<'src>,
}

#[allow(non_snake_case)]
pub fn Doc<'src>(doc: impl Into<Doc<'src>>) -> Doc<'src> {
  doc.into()
}

impl<'src> Doc<'src> {
  pub const EMPTY: Self = Doc { measure: Measure::EMPTY, kind: DocKind::String(Cow::Borrowed("")) };
  pub const LINE: Self = Doc { measure: Measure::LINE, kind: DocKind::Line };

  fn collect(docs: impl IntoIterator<Item = Self>) -> Box<[Self]> {
    Vec::into_boxed_slice(docs.into_iter().collect())
  }

  pub fn concat(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc(DocKind::Concat(Self::collect(docs)))
  }

  pub fn concat_vec(docs: Vec<Self>) -> Self {
    Doc(DocKind::Concat(docs.into_boxed_slice()))
  }

  pub fn indent(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc(DocKind::Indent(Self::collect(docs)))
  }

  pub fn indent_vec(docs: Vec<Self>) -> Self {
    Doc(DocKind::Indent(docs.into_boxed_slice()))
  }

  pub fn group(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc(DocKind::Group(Self::collect(docs)))
  }

  pub fn lazy_group(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc(DocKind::LazyGroup(Self::collect(docs)))
  }

  pub fn interleave(docs: impl IntoIterator<Item = Self>, sep: Self) -> Self {
    Doc(DocKind::Interleave(Self::collect(docs), Box::new(sep)))
  }

  pub fn if_single(str: &'src str) -> Self {
    Doc(DocKind::IfSingle(str))
  }

  pub fn if_multi(str: &'src str) -> Self {
    Doc(DocKind::IfMulti(str))
  }

  pub fn soft_line(str: &'src str) -> Self {
    Doc(DocKind::SoftLine(str))
  }

  pub fn paren(doc: Self) -> Self {
    Doc::concat([Doc("("), Doc::group([doc]), Doc(")")])
  }

  #[allow(clippy::too_many_arguments)]
  pub fn delimited(
    empty: &'src str,
    open: &'src str,
    close: &'src str,
    force_multi: bool,
    force_trailing: bool,
    lazy_single: bool,
    space: bool,
    mut docs: impl ExactSizeIterator<Item = Self>,
  ) -> Self {
    if docs.len() == 0 {
      Doc(empty)
    } else {
      let trailing = if force_multi || force_trailing { Doc(",") } else { Doc::if_multi(",") };
      let sep = Doc::concat([Doc(","), if force_multi { Doc::LINE } else { Doc::soft_line(" ") }]);
      let space = || if space && !force_multi { Doc::if_single(" ") } else { Doc("") };
      let inner = if force_multi {
        Doc::indent([space(), Doc::interleave(docs, sep), trailing, space()])
      } else if lazy_single && docs.len() == 1 {
        Doc::lazy_group([space(), docs.next().unwrap(), trailing, space()])
      } else {
        Doc::group([space(), Doc::interleave(docs, sep), trailing, space()])
      };
      Doc::concat([Doc(open), inner, Doc(close)])
    }
  }

  pub fn paren_comma(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("()", "(", ")", false, false, true, false, docs)
  }

  pub fn tuple(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("()", "(", ")", false, docs.len() == 1, false, false, docs)
  }

  pub fn bracket_comma(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("[]", "[", "]", false, false, false, false, docs)
  }

  pub fn brace_comma(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("{}", "{", "}", false, false, false, false, docs)
  }

  pub fn brace_comma_space(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("{}", "{", "}", false, false, false, true, docs)
  }

  pub fn brace_comma_multiline(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Self::delimited("{}", "{", "}", true, false, false, false, docs)
  }

  pub fn brace_multiline(docs: impl ExactSizeIterator<Item = Self>) -> Self {
    if docs.len() == 0 {
      Doc("{}")
    } else {
      Doc::concat([Doc("{"), Doc::indent([Doc::interleave(docs, Doc::LINE)]), Doc("}")])
    }
  }
}

impl<'src> From<DocKind<'src>> for Doc<'src> {
  fn from(kind: DocKind<'src>) -> Self {
    Doc { measure: kind.measure(), kind }
  }
}

impl<'src> From<&'src str> for Doc<'src> {
  fn from(str: &'src str) -> Self {
    Doc { measure: Measure::string(str), kind: DocKind::String(str.into()) }
  }
}

impl<'src> From<String> for Doc<'src> {
  fn from(str: String) -> Self {
    Doc { measure: Measure::string(&str), kind: DocKind::String(str.into()) }
  }
}

impl<'core: 'src, 'src> From<Ident<'core>> for Doc<'src> {
  fn from(value: Ident<'core>) -> Self {
    Doc(value.0 .0)
  }
}

#[derive(Debug)]
enum DocKind<'src> {
  Line,
  String(Cow<'src, str>),
  Concat(Box<[Doc<'src>]>),
  Group(Box<[Doc<'src>]>),
  LazyGroup(Box<[Doc<'src>]>),
  Indent(Box<[Doc<'src>]>),
  SoftLine(&'src str),
  IfMulti(&'src str),
  IfSingle(&'src str),
  Interleave(Box<[Doc<'src>]>, Box<Doc<'src>>),
}

impl<'src> DocKind<'src> {
  fn measure(&self) -> Measure {
    match self {
      DocKind::Line | DocKind::Indent(_) => Measure::LINE,
      DocKind::String(s) => Measure::string(s),
      DocKind::SoftLine(s) | DocKind::IfSingle(s) => Measure::string(s),
      DocKind::IfMulti(_) => Measure::EMPTY,
      DocKind::Concat(x) | DocKind::Group(x) | DocKind::LazyGroup(x) => {
        x.iter().fold(Measure::EMPTY, |a, b| a + b.measure)
      }
      DocKind::Interleave(x, sep) => x.iter().enumerate().fold(Measure::EMPTY, |m, (i, d)| {
        m + d.measure + if i != x.len() - 1 { sep.measure } else { Measure::EMPTY }
      }),
    }
  }
}

#[derive(Default)]
pub struct Writer {
  pub out: String,
  indent: usize,
  written: usize,
}

impl Writer {
  pub fn write_doc(&mut self, doc: &Doc, multi: bool) {
    match &doc.kind {
      DocKind::Line => self.newline(),
      DocKind::String(str) => self.write_str(str),
      DocKind::Concat(docs) => self.write_all(docs, multi),
      DocKind::Indent(docs) => self.write_all_indented(docs, multi),
      DocKind::Group(docs) => {
        let multi = doc.measure.multi || doc.measure.width >= self.remaining();
        self.write_all_maybe_indented(docs, multi);
      }
      DocKind::LazyGroup(docs) => {
        let multi = doc.measure.leading >= self.remaining();
        self.write_all_maybe_indented(docs, multi);
      }
      DocKind::SoftLine(s) => {
        if multi {
          self.newline()
        } else {
          self.write_str(s)
        }
      }
      DocKind::IfMulti(s) => {
        if multi {
          self.write_str(s)
        }
      }
      DocKind::IfSingle(s) => {
        if !multi {
          self.write_str(s)
        }
      }
      DocKind::Interleave(docs, sep) => {
        let n = docs.len();
        for (i, doc) in docs.iter().enumerate() {
          self.write_doc(doc, multi);
          if i != n - 1 {
            self.write_doc(sep, multi);
          }
        }
      }
    }
  }

  fn write_all_maybe_indented(&mut self, docs: &[Doc], multi: bool) {
    if multi {
      self.write_all_indented(docs, multi);
    } else {
      self.write_all(docs, multi);
    }
  }

  fn write_all_indented(&mut self, docs: &[Doc], multi: bool) {
    self.indent();
    self.write_all(docs, multi);
    self.outdent();
  }

  fn write_all(&mut self, docs: &[Doc], multi: bool) {
    for doc in docs.iter() {
      self.write_doc(doc, multi);
    }
  }

  fn indent(&mut self) {
    self.indent += 2;
    self.newline();
  }

  fn outdent(&mut self) {
    self.indent -= 2;
    self.newline();
  }

  fn newline(&mut self) {
    self.out += "\n";
    self.written = 0;
  }

  fn write_str(&mut self, str: &str) {
    if str.is_empty() {
      return;
    }
    if self.written == 0 {
      for _ in 0..self.indent {
        self.out += " ";
      }
    }
    self.out += str;
    self.written += str.chars().count();
  }

  fn remaining(&self) -> usize {
    MAX_WIDTH.saturating_sub(self.written + self.indent)
  }
}

#[derive(Debug, Clone, Copy)]
struct Measure {
  multi: bool,
  width: usize,
  leading: usize,
}

impl Measure {
  const EMPTY: Self = Measure { multi: false, width: 0, leading: 0 };
  const LINE: Self = Measure { multi: true, width: 0, leading: 0 };

  fn string(str: &str) -> Self {
    let mut multi = false;
    let mut leading = 0;
    let mut width = 0;
    for char in str.chars() {
      if char == '\n' {
        if !multi {
          leading = width;
        }
        multi = true;
      } else {
        width += 1;
      }
    }
    Measure { multi, leading, width }
  }
}

impl Add<Measure> for Measure {
  type Output = Measure;

  fn add(self, rhs: Measure) -> Self::Output {
    Measure {
      multi: self.multi || rhs.multi,
      width: if self.multi { self.width } else { self.width + rhs.width },
      leading: if self.leading < self.width { self.leading } else { self.leading + rhs.leading },
    }
  }
}
