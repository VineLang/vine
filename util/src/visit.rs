use std::{
  any::{Any, TypeId},
  fmt::Debug,
  mem::transmute,
  ops::Range,
};

pub trait Visitor<'a> {
  fn visit(&mut self, node: &'a impl Visit);
}

pub trait VisitorMut<'a> {
  fn visit(&mut self, node: &'a mut impl Visit);
}

pub trait Visit: Sized + Any + Debug {
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>);
  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>);

  fn cast<T: Visit>(&self) -> Option<&T> {
    if TypeId::of::<Self>() == TypeId::of::<T>() {
      Some(unsafe { transmute::<&Self, &T>(self) })
    } else {
      None
    }
  }

  fn cast_mut<T: Visit>(&mut self) -> Option<&mut T> {
    if TypeId::of::<Self>() == TypeId::of::<T>() {
      Some(unsafe { transmute::<&mut Self, &mut T>(self) })
    } else {
      None
    }
  }

  fn get_child<C: Visit>(&self) -> Option<&C> {
    struct ChildVisitor<'a, C>(Option<&'a C>);
    impl<'a, C: Visit> Visitor<'a> for ChildVisitor<'a, C> {
      #[inline]
      fn visit(&mut self, node: &'a impl Visit) {
        if self.0.is_none()
          && let Some(child) = node.cast::<C>()
        {
          self.0 = Some(child);
        }
      }
    }
    let mut visitor = ChildVisitor(None);
    self.visit_children(&mut visitor);
    visitor.0
  }

  fn get_child_mut<C: Visit>(&mut self) -> Option<&mut C> {
    struct ChildVisitor<'a, C>(Option<&'a mut C>);
    impl<'a, C: Visit> VisitorMut<'a> for ChildVisitor<'a, C> {
      #[inline]
      fn visit(&mut self, node: &'a mut impl Visit) {
        if self.0.is_none()
          && let Some(child) = node.cast_mut::<C>()
        {
          self.0 = Some(child);
        }
      }
    }
    let mut visitor = ChildVisitor(None);
    self.visit_children_mut(&mut visitor);
    visitor.0
  }
}

pub use crate::derive_visit as Visit;
use crate::idx::{Idx, IdxVec};

#[macro_export]
macro_rules! derive_visit {
  ($(#[$_0:meta])* $_1:vis struct $name:ident $({ $($(#[$_2:meta])* $_3:vis $key:ident: $_4:ty),* $(,)? })? $(;)?) => {
    impl $crate::visit::Visit for $name {
      #[inline]
      fn visit_children<'a>(&'a self, visitor: &mut impl $crate::visit::Visitor<'a>) {
        let $name $({ $($key),* })? = self;
        $($($crate::visit::Visitor::visit(visitor, $key);)*)?
      }

      #[inline]
      fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl $crate::visit::VisitorMut<'a>) {
        let $name $({ $($key),* })? = self;
        $($($crate::visit::VisitorMut::visit(visitor, $key);)*)?
      }
    }
  };

  ($(#[$_0:meta])* $_1:vis enum $name:ident { $(
    $(#[$_2:meta])*
    $variant:ident
      $({ $(#[$_3:meta])* $($key:ident: $_4:ty),* $(,)? })?
      $(($($ty0:ty $(, $($ty1:ty $(, $($ty2:ty $(, $($ty3:ty $(,)?)?)?)?)?)?)?)?))?
  ),* $(,)? }) => {
    impl $crate::visit::Visit for $name {
      #[inline]
      fn visit_children<'a>(&'a self, visitor: &mut impl $crate::visit::Visitor<'a>) {
        match self {
          $($name::$variant {
              $($($key,)*)?
              $($(0: $crate::macro_seq!(($ty0); _0),
              $($(1: $crate::macro_seq!(($ty1); _1),
              $($(2: $crate::macro_seq!(($ty2); _2),
              $($(3: $crate::macro_seq!(($ty3); _3),
              )?)?)?)?)?)?)?)?
          } => {
            $($($crate::visit::Visitor::visit(visitor, $key);)*)?
            $($($crate::visit::Visitor::visit(visitor, $crate::macro_seq!(($ty0); _0));
            $($($crate::visit::Visitor::visit(visitor, $crate::macro_seq!(($ty1); _1));
            $($($crate::visit::Visitor::visit(visitor, $crate::macro_seq!(($ty2); _2));
            $($($crate::visit::Visitor::visit(visitor, $crate::macro_seq!(($ty3); _3));
            )?)?)?)?)?)?)?)?
          })*
        }
      }

      #[inline]
      fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl $crate::visit::VisitorMut<'a>) {
        match self {
          $($name::$variant {
              $($($key,)*)?
              $($(0: $crate::macro_seq!(($ty0); _0),
              $($(1: $crate::macro_seq!(($ty1); _1),
              $($(2: $crate::macro_seq!(($ty2); _2),
              $($(3: $crate::macro_seq!(($ty3); _3),
              )?)?)?)?)?)?)?)?
          } => {
            $($($crate::visit::VisitorMut::visit(visitor, $key);)*)?
            $($($crate::visit::VisitorMut::visit(visitor, $crate::macro_seq!(($ty0); _0));
            $($($crate::visit::VisitorMut::visit(visitor, $crate::macro_seq!(($ty1); _1));
            $($($crate::visit::VisitorMut::visit(visitor, $crate::macro_seq!(($ty2); _2));
            $($($crate::visit::VisitorMut::visit(visitor, $crate::macro_seq!(($ty3); _3));
            )?)?)?)?)?)?)?)?
          })*
        }
      }
    }
  };
}

#[macro_export]
macro_rules! trivial_visit {
  ($($ty:ty),* $(,)?) => {
    $(impl $crate::visit::Visit for $ty {
      #[inline]
      fn visit_children<'a>(&'a self, _: &mut impl $crate::visit::Visitor<'a>) {}
      #[inline]
      fn visit_children_mut<'a>(&'a mut self, _: &mut impl $crate::visit::VisitorMut<'a>) {}
    })*
  };
}

impl<I: Idx + 'static, T: Visit> Visit for IdxVec<I, T> {
  #[inline]
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    for node in self.values() {
      visitor.visit(node);
    }
  }

  #[inline]
  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    for node in self.values_mut() {
      visitor.visit(node);
    }
  }
}

impl<T: Visit> Visit for Vec<T> {
  #[inline]
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    for node in self {
      visitor.visit(node);
    }
  }

  #[inline]
  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    for node in self {
      visitor.visit(node);
    }
  }
}

impl<T: Visit> Visit for Option<T> {
  #[inline]
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    if let Some(node) = self {
      visitor.visit(node);
    }
  }

  #[inline]
  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    if let Some(node) = self {
      visitor.visit(node);
    }
  }
}

impl<T: Visit> Visit for Box<T> {
  #[inline]
  fn visit_children<'a>(&'a self, visitor: &mut impl Visitor<'a>) {
    visitor.visit(&**self);
  }

  #[inline]
  fn visit_children_mut<'a>(&'a mut self, visitor: &mut impl VisitorMut<'a>) {
    visitor.visit(&mut **self);
  }
}

trivial_visit!(bool, usize, String, Range<usize>, (), !);

use macro_rules_attribute::derive;

#[derive(Debug, Visit!)]
struct Foo {
  name: String,
  children: Vec<Bar>,
}

#[derive(Debug, Visit!)]
struct Bar {
  index: usize,
  children: Vec<Foo>,
}

#[unsafe(no_mangle)]
fn foo_name(foo: &Foo) -> Option<&String> {
  foo.get_child::<String>()
}

#[unsafe(no_mangle)]
fn bar_name(bar: &Bar) -> Option<&String> {
  bar.get_child::<String>()
}

#[unsafe(no_mangle)]
fn foo_bools(foo: &Foo) {
  print_bools(foo);
}

fn print_bools<T: Visit>(node: &T) {
  struct PrintBools;
  impl Visitor<'_> for PrintBools {
    #[inline]
    fn visit(&mut self, node: &impl Visit) {
      if let Some(bool) = node.cast::<bool>() {
        println!("{bool}");
      }
      node.visit_children(self);
    }
  }
  PrintBools.visit(node);
}
