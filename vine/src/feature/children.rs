use super::*;

use crate::feature::comment::{Exclusion, Text};

use vine_util::visit::{Visitor, VisitorMut};

/// A list of child nodes, potentially interspersed with blank lines and
/// comments.
#[derive_(Debug, Clone)]
pub struct Children<T> {
  /// These entries should be normalized such that:
  /// - there are no two adjacent `Blank` entries
  /// - `entries` does not begin or end with a `Blank` entry
  /// - there are no two adjacent `Text` entries
  /// - no `Text` entry immediately precedes a `Child` entry (as it would be
  ///   incorporated into the `docs`)
  ///
  /// Note that a `Text` entry can immediately succeed a `Child` entry, as the
  /// `trailing` is only textual comments starting on the same line, not
  /// including subsequent lines.
  pub entries: Vec<ChildrenEntry<T>>,
}

#[derive_(Debug, Clone)]
pub enum ChildrenEntry<T> {
  Child {
    /// Textual comments on lines immediately preceding the child; these may be
    /// interpreted as documentation for the child.
    docs: Text,
    child: T,
    /// Textual comments following the child, on the same line.
    trailing: Text,
  },
  Blank,
  Text(Text),
  Exclusion(Exclusion),
}

impl<T> ChildrenEntry<T> {
  pub fn get(&self) -> Option<&T> {
    match self {
      ChildrenEntry::Child { child, .. } => Some(child),
      _ => None,
    }
  }

  pub fn get_mut(&mut self) -> Option<&mut T> {
    match self {
      ChildrenEntry::Child { child, .. } => Some(child),
      _ => None,
    }
  }

  pub fn with_docs(&self) -> Option<(&Text, &T)> {
    match self {
      ChildrenEntry::Child { docs, child, .. } => Some((docs, child)),
      _ => None,
    }
  }

  pub fn with_docs_mut(&mut self) -> Option<(&Text, &mut T)> {
    match self {
      ChildrenEntry::Child { docs, child, .. } => Some((docs, child)),
      _ => None,
    }
  }
}

impl<T> Children<T> {
  pub fn single(child: T) -> Self {
    Children { entries: vec![ChildrenEntry::from(child)] }
  }

  pub fn iter(&self) -> impl DoubleEndedIterator<Item = &T> {
    self.entries.iter().filter_map(ChildrenEntry::get)
  }

  pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut T> {
    self.entries.iter_mut().filter_map(ChildrenEntry::get_mut)
  }

  pub fn with_docs(&self) -> impl DoubleEndedIterator<Item = (&Text, &T)> {
    self.entries.iter().filter_map(ChildrenEntry::with_docs)
  }

  pub fn with_docs_mut(&mut self) -> impl DoubleEndedIterator<Item = (&Text, &mut T)> {
    self.entries.iter_mut().filter_map(ChildrenEntry::with_docs_mut)
  }
}

impl<T> From<T> for ChildrenEntry<T> {
  fn from(child: T) -> Self {
    ChildrenEntry::Child { docs: Text::default(), child, trailing: Text::default() }
  }
}

impl<T: Visit> Visit for Children<T> {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    for child in self.iter() {
      visitor.visit(child);
    }
  }

  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    for child in self.iter_mut() {
      visitor.visit(child);
    }
  }
}

impl<T> Default for Children<T> {
  fn default() -> Self {
    Self { entries: Default::default() }
  }
}

impl Parser<'_> {
  pub fn parse_children<T>(
    &mut self,
    open: Option<Token>,
    separator: Option<Token>,
    close: Option<Token>,
  ) -> Result<Children<T>, Diag>
  where
    Self: Parse<T>,
  {
    assert!(separator.is_some() || close.is_some());
    if let Some(open) = open {
      self.expect(open)?;
    }
    let mut entries = Vec::new();
    loop {
      if let Some(close) = close
        && self.check(close)
      {
        break;
      }
      let docs = Text { lines: vec![] }; // todo
      let trailing = Text { lines: vec![] };
      entries.push(ChildrenEntry::Child { child: self.parse()?, docs, trailing });
      if let Some(separator) = separator
        && !self.eat(separator)?
      {
        break;
      }
    }
    if let Some(close) = close {
      self.expect(close)?;
    }
    Ok(Children { entries })
  }
}
