use std::{mem::replace, ptr::NonNull};

/// Stack your stack on the stack with StackStack!
pub enum StackStack<'a, T> {
  Nil,
  Cons(&'a mut T, StackStackRef<'a, T>),
}

impl<'a, T> StackStack<'a, T> {
  pub fn borrow<'b>(&'b mut self) -> StackStackRef<'b, T> {
    StackStackRef(NonNull::from_mut(self))
  }

  pub fn head(&mut self) -> Option<&mut T> {
    match self {
      StackStack::Nil => None,
      StackStack::Cons(head, _) => Some(head),
    }
  }

  pub fn tail(&mut self) -> Option<StackStackRef<'_, T>> {
    match self {
      StackStack::Nil => None,
      StackStack::Cons(_, tail) => Some(tail.fork()),
    }
  }

  pub fn fork(&mut self) -> StackStack<'_, T> {
    match self {
      StackStack::Nil => StackStack::Nil,
      StackStack::Cons(head, tail) => StackStack::Cons(head, tail.fork()),
    }
  }
}

/// Like `&'a mut StackStack<'a, T>`, but covariant on `'a`.
pub struct StackStackRef<'a, T>(NonNull<StackStack<'a, T>>);

impl<'a, T> StackStackRef<'a, T> {
  pub fn get(mut self) -> StackStack<'a, T> {
    unsafe { self.0.as_mut() }.fork()
  }

  pub fn fork(&mut self) -> StackStackRef<'_, T> {
    StackStackRef(self.0)
  }
}

impl<'a, T> Iterator for StackStack<'a, T> {
  type Item = &'a mut T;

  fn next(&mut self) -> Option<Self::Item> {
    match replace(self, StackStack::Nil) {
      StackStack::Nil => None,
      StackStack::Cons(head, tail) => {
        *self = tail.get();
        Some(head)
      }
    }
  }
}

#[test]
fn test() {
  #[derive(Debug, PartialEq, Eq)]
  struct Node(&'static str, u32);
  #[derive(Debug, PartialEq, Eq)]
  struct Tree(Node, Vec<Tree>);
  fn walk(tree: &mut Tree, mut stack: StackStack<'_, Node>, paths: &mut Vec<String>) {
    let mut stack = StackStack::Cons(&mut tree.0, stack.borrow());
    let mut path = String::new();
    for node in stack.fork() {
      path += node.0;
      node.1 += 1;
    }
    paths.push(path);
    for child in &mut tree.1 {
      walk(child, stack.fork(), paths);
    }
  }
  let mut tree = Tree(
    Node("a", 0),
    vec![
      Tree(
        Node("b", 0),
        vec![
          Tree(Node("c", 0), vec![]),
          Tree(Node("d", 0), vec![Tree(Node("e", 0), vec![Tree(Node("f", 0), vec![])])]),
        ],
      ),
      Tree(
        Node("g", 0),
        vec![
          Tree(Node("h", 0), vec![Tree(Node("i", 0), vec![]), Tree(Node("j", 0), vec![])]),
          Tree(Node("k", 0), vec![Tree(Node("l", 0), vec![]), Tree(Node("m", 0), vec![])]),
        ],
      ),
    ],
  );
  let mut paths = Vec::new();
  walk(&mut tree, StackStack::Nil, &mut paths);
  assert_eq!(
    paths,
    ["a", "ba", "cba", "dba", "edba", "fedba", "ga", "hga", "ihga", "jhga", "kga", "lkga", "mkga"]
  );
  assert_eq!(
    tree,
    Tree(
      Node("a", 13),
      vec![
        Tree(
          Node("b", 5),
          vec![
            Tree(Node("c", 1), vec![]),
            Tree(Node("d", 3), vec![Tree(Node("e", 2), vec![Tree(Node("f", 1), vec![])])]),
          ],
        ),
        Tree(
          Node("g", 7),
          vec![
            Tree(Node("h", 3), vec![Tree(Node("i", 1), vec![]), Tree(Node("j", 1), vec![])]),
            Tree(Node("k", 3), vec![Tree(Node("l", 1), vec![]), Tree(Node("m", 1), vec![])]),
          ],
        ),
      ],
    ),
  );
}
