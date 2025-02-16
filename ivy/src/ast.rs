use std::{
  fmt::{self, Display},
  iter,
  ops::{Deref, DerefMut},
};

use indexmap::IndexMap;

use vine_util::multi_iter;

#[derive(Default, Debug, Clone, PartialEq)]
pub enum Tree {
  #[default]
  Erase,
  Comb(String, Box<Tree>, Box<Tree>),
  ExtFn(String, bool, Box<Tree>, Box<Tree>),
  Branch(Box<Tree>, Box<Tree>, Box<Tree>),
  N32(u32),
  F32(f32),
  Var(String),
  Global(String),
}

#[derive(Debug, Clone)]
pub struct Net {
  pub root: Tree,
  pub pairs: Vec<(Tree, Tree)>,
}

#[derive(Debug, Default, Clone)]
pub struct Nets(IndexMap<String, Net>);

impl Deref for Nets {
  type Target = IndexMap<String, Net>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Nets {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Display for Tree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Tree::Erase => write!(f, "_"),
      Tree::Comb(n, a, b) => write!(f, "{n}({a} {b})"),
      Tree::ExtFn(e, swap, a, b) => write!(f, "@{e}{}({a} {b})", if *swap { "$" } else { "" }),
      Tree::Branch(a, b, c) => write!(f, "?({a} {b} {c})"),
      Tree::N32(n) => write!(f, "{n}"),
      Tree::F32(n) if n.is_nan() => write!(f, "+NaN"),
      Tree::F32(n) => write!(f, "{n:+?}"),
      Tree::Var(v) => write!(f, "{v}"),
      Tree::Global(g) => write!(f, "{g}"),
    }
  }
}

impl Display for Net {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.pairs.is_empty() {
      write!(f, "{{ {} }}", self.root)?;
    } else {
      write!(f, "{{\n  {}", self.root)?;
      for (a, b) in &self.pairs {
        write!(f, "\n  {a} = {b}")?;
      }
      write!(f, "\n}}")?;
    }
    Ok(())
  }
}

impl Display for Nets {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (name, net) in self.iter() {
      write!(f, "\n{name} {net}\n")?;
    }
    Ok(())
  }
}

impl Net {
  pub fn trees(&self) -> impl DoubleEndedIterator<Item = &Tree> + Clone {
    iter::once(&self.root).chain(self.pairs.iter().flat_map(|(a, b)| [a, b]))
  }
  pub fn trees_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Tree> {
    iter::once(&mut self.root).chain(self.pairs.iter_mut().flat_map(|(a, b)| [a, b]))
  }
}

impl Tree {
  pub fn n_ary(
    label: &str,
    ports: impl IntoIterator<IntoIter: DoubleEndedIterator<Item = Tree>>,
  ) -> Self {
    ports
      .into_iter()
      .rev()
      .reduce(|b, a| Tree::Comb(label.into(), Box::new(a), Box::new(b)))
      .unwrap_or(Tree::Erase)
  }

  pub fn children(&self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &Self> + Clone {
    multi_iter!(Children { Zero, Two, Three });
    match self {
      Tree::Erase | Tree::N32(_) | Tree::F32(_) | Tree::Var(_) | Tree::Global(_) => {
        Children::Zero([])
      }
      Tree::Comb(_, a, b) | Tree::ExtFn(_, _, a, b) => Children::Two([&**a, b]),
      Tree::Branch(a, b, c) => Children::Three([&**a, b, c]),
    }
  }

  pub fn children_mut(&mut self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &mut Self> {
    multi_iter!(Children { Zero, Two, Three });
    match self {
      Tree::Erase | Tree::N32(_) | Tree::F32(_) | Tree::Var(_) | Tree::Global(_) => {
        Children::Zero([])
      }
      Tree::Comb(_, a, b) | Tree::ExtFn(_, _, a, b) => Children::Two([&mut **a, b]),
      Tree::Branch(a, b, c) => Children::Three([&mut **a, b, c]),
    }
  }
}
