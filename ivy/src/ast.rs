use std::{
  fmt::{self, Display},
  iter,
  mem::replace,
  ops::{Deref, DerefMut},
};

use indexmap::IndexMap;

use ivm::ext::ExtFn;
use vine_util::multi_iter;

#[derive(Default, Debug, Clone)]
pub enum Tree {
  #[default]
  Erase,
  Comb(String, Box<Tree>, Box<Tree>),
  ExtFn(ExtFn, Box<Tree>, Box<Tree>),
  Branch(Box<Tree>, Box<Tree>, Box<Tree>),
  U32(u32),
  F32(f32),
  Var(String),
  Global(String),
  BlackBox(Box<Tree>),
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
      Tree::ExtFn(e, a, b) => write!(f, "@{e:?}({a} {b})"),
      Tree::Branch(a, b, c) => write!(f, "?({a} {b} {c})"),
      Tree::U32(n) => write!(f, "{n}"),
      Tree::F32(n) if n.is_nan() => write!(f, "+NaN"),
      Tree::F32(n) => write!(f, "{n:+?}"),
      Tree::Var(v) => write!(f, "{v}"),
      Tree::Global(g) => write!(f, "{g}"),
      Tree::BlackBox(a) => write!(f, "#black_box({a})"),
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
  pub fn n_ary(label: &str, children: impl IntoIterator<Item = Self>) -> Self {
    let mut children = children.into_iter();
    let Some(initial) = children.next() else { return Tree::Erase };

    let mut tree = initial;
    let mut cur = &mut tree;

    for child in children {
      let prev = replace(cur, Tree::Comb(label.to_owned(), Box::new(Tree::Erase), Box::new(child)));
      let Tree::Comb(_, l, r) = cur else { unreachable!() };
      **l = prev;
      cur = r;
    }

    tree
  }

  pub fn white_box(mut self: &Self) -> &Tree {
    while let Tree::BlackBox(x) = self {
      self = x;
    }
    self
  }

  pub fn children(&self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &Self> + Clone {
    multi_iter!(Children { Zero, One, Two, Three });
    match self {
      Tree::Erase | Tree::U32(_) | Tree::F32(_) | Tree::Var(_) | Tree::Global(_) => {
        Children::Zero([])
      }
      Tree::Comb(_, a, b) | Tree::ExtFn(_, a, b) => Children::Two([&**a, b]),
      Tree::Branch(a, b, c) => Children::Three([&**a, b, c]),
      Tree::BlackBox(a) => Children::One([&**a]),
    }
  }

  pub fn children_mut(&mut self) -> impl DoubleEndedIterator + ExactSizeIterator<Item = &mut Self> {
    multi_iter!(Children { Zero, One, Two, Three });
    match self {
      Tree::Erase | Tree::U32(_) | Tree::F32(_) | Tree::Var(_) | Tree::Global(_) => {
        Children::Zero([])
      }
      Tree::Comb(_, a, b) | Tree::ExtFn(_, a, b) => Children::Two([&mut **a, b]),
      Tree::Branch(a, b, c) => Children::Three([&mut **a, b, c]),
      Tree::BlackBox(a) => Children::One([&mut **a]),
    }
  }
}
