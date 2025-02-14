use std::{
  collections::{hash_map::Entry, HashMap},
  mem::take,
};

use crate::ast::{Net, Tree};

#[derive(Debug, Default)]
pub(super) struct InlineVars {
  mappings: HashMap<String, Option<Tree>>,
}

impl InlineVars {
  //! Remove all `var = tree` pairs, inlining the `tree` into the usage of
  //! `var`.
  pub fn apply(&mut self, net: &mut Net) {
    net.pairs.retain_mut(|pair| self.extract_erase(pair));
    self.visit_tree(&mut net.root);
    net.pairs.retain_mut(|pair| self.visit_pair(pair));
    debug_assert!(self.mappings.values().all(|x| x.is_none()));
    self.mappings.clear();
    if let Tree::Var(v) = &net.root {
      if let Some((Tree::Var(u), t)) = net.pairs.get_mut(0) {
        if v == u {
          net.root = take(t);
          net.pairs.remove(0);
        }
      }
    }
  }

  fn extract_erase(&mut self, pair: &mut (Tree, Tree)) -> bool {
    match pair {
      (Tree::Var(v), Tree::Erase) | (Tree::Erase, Tree::Var(v)) => {
        match self.mappings.entry(take(v)) {
          Entry::Occupied(e) => {
            e.remove();
          }
          Entry::Vacant(e) => {
            e.insert(Some(Tree::Erase));
          }
        };
        false
      }
      (Tree::Erase, Tree::Erase) => false,
      _ => true,
    }
  }

  fn visit_pair(&mut self, (a, b): &mut (Tree, Tree)) -> bool {
    while let Tree::Var(v) = a {
      match self.mappings.entry(v.clone()) {
        Entry::Occupied(mut e) => {
          if let Some(new) = e.get_mut().take() {
            *a = new;
            continue;
          } else {
            break;
          }
        }
        Entry::Vacant(e) => {
          e.insert(Some(take(b)));
          return false;
        }
      }
    }
    while let Tree::Var(v) = b {
      match self.mappings.entry(v.clone()) {
        Entry::Occupied(mut e) => {
          if let Some(new) = e.get_mut().take() {
            *b = new;
            continue;
          } else {
            break;
          }
        }
        Entry::Vacant(e) => {
          e.insert(Some(take(a)));
          return false;
        }
      }
    }
    a.children_mut().for_each(|t| self.visit_tree(t));
    b.children_mut().for_each(|t| self.visit_tree(t));
    true
  }

  fn visit_tree(&mut self, tree: &mut Tree) {
    while let Tree::Var(var) = tree {
      match self.mappings.entry(var.clone()) {
        Entry::Vacant(e) => {
          e.insert(None);
          break;
        }
        Entry::Occupied(mut e) => {
          if let Some(new) = e.get_mut().take() {
            *tree = new;
            continue;
          } else {
            break;
          }
        }
      }
    }

    tree.children_mut().for_each(|t| self.visit_tree(t));
  }
}
