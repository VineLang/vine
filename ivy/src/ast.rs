use std::{
  fmt::{self, Display},
  mem::replace,
  ops::{Deref, DerefMut},
};

use indexmap::IndexMap;
use ivm::ext::{ExtFn, ExtFnKind};

#[derive(Default, Debug, Clone)]
pub enum Tree {
  #[default]
  Erase,
  Comb(String, Box<Tree>, Box<Tree>),
  ExtFn(ExtFn, Box<Tree>, Box<Tree>),
  Branch(Box<Tree>, Box<Tree>, Box<Tree>),
  U32(u32),
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
      Tree::ExtFn(e, a, b) => write!(f, "@{e:?}({a} {b})"),
      Tree::Branch(a, b, c) => write!(f, "?({a} {b} {c})"),
      Tree::U32(n) => write!(f, "{n}"),
      Tree::Var(v) => write!(f, "{v}"),
      Tree::Global(g) => write!(f, "{g}"),
    }
  }
}

impl Display for Net {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{{\n  {}", self.root)?;
    for (a, b) in &self.pairs {
      write!(f, "\n  {a} = {b}")?;
    }
    write!(f, "\n}}")?;
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
}

impl Nets {
  pub fn define_ext_fns(&mut self) {
    for &f in ExtFnKind::ALL {
      let net = Net {
        root: Tree::Comb(
          "lam".to_owned(),
          Box::new(Tree::ExtFn(
            f.into(),
            Box::new(Tree::Var("r".to_owned())),
            Box::new(Tree::Var("o".to_owned())),
          )),
          Box::new(Tree::Comb(
            "lam".to_owned(),
            Box::new(Tree::Var("r".to_owned())),
            Box::new(Tree::Var("o".to_owned())),
          )),
        ),
        pairs: Vec::new(),
      };
      self.insert(format!("::ext::{:?}", f), net);
      let net = Net {
        root: Tree::Comb(
          "lam".to_owned(),
          Box::new(Tree::Branch(
            Box::new(Tree::Var("n".to_owned())),
            Box::new(Tree::Var("p".to_owned())),
            Box::new(Tree::Var("o".to_owned())),
          )),
          Box::new(Tree::Comb(
            "lam".to_owned(),
            Box::new(Tree::Var("n".to_owned())),
            Box::new(Tree::Comb(
              "lam".to_owned(),
              Box::new(Tree::Var("p".to_owned())),
              Box::new(Tree::Var("o".to_owned())),
            )),
          )),
        ),
        pairs: Vec::new(),
      };
      self.insert("::ext::branch".to_owned(), net);
    }
  }
}
