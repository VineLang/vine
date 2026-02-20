use std::{borrow::Cow, collections::HashMap};

use vine_util::{idx::IdxVec, nat::Nat, new_idx};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
  pub path: PathId,
  pub children: Vec<NameId>,
  pub data: Option<Nat>,
}

new_idx!(pub NameId);
new_idx!(pub PathId);

#[derive(Debug)]
pub struct Table {
  paths: IdxVec<PathId, String>,
  path_lookup: HashMap<String, PathId>,
  names: IdxVec<NameId, Name>,
  name_lookup: HashMap<Name, NameId>,
}

impl Table {
  pub fn path(&self, id: PathId) -> &str {
    &self.paths[id]
  }

  pub fn add_path<'a>(&mut self, path: impl Into<Cow<'a, str>>) -> PathId {
    let path = path.into();
    if let Some(&id) = self.path_lookup.get(&*path) {
      id
    } else {
      let path = path.into_owned();
      let id = self.paths.push(path.clone());
      self.path_lookup.insert(path, id);
      id
    }
  }

  pub fn name(&self, id: NameId) -> &Name {
    &self.names[id]
  }

  pub fn add_name(&mut self, name: Name) -> NameId {
    *self.name_lookup.entry(name).or_insert_with_key(|name| self.names.push(name.clone()))
  }
}

impl From<PathId> for Name {
  fn from(path: PathId) -> Self {
    path.to_name()
  }
}

impl PathId {
  pub fn to_name(self) -> Name {
    Name { path: self, children: Vec::new(), data: None }
  }

  pub fn with_data(self, data: impl Into<Nat>) -> Name {
    self.to_name().with_data(data)
  }

  pub fn with_children(self, children: impl IntoIterator<Item = NameId>) -> Name {
    self.to_name().with_children(children)
  }
}

impl Name {
  pub fn with_data(self, data: impl Into<Nat>) -> Name {
    Name { data: Some(data.into()), ..self }
  }

  pub fn with_children(self, children: impl IntoIterator<Item = NameId>) -> Name {
    Name { children: children.into_iter().collect(), ..self }
  }
}

pub trait FromTable<'a> {
  fn build(table: &'a mut Table) -> Self;
}

impl FromTable<'_> for () {
  fn build(_: &mut Table) {}
}

#[macro_export]
macro_rules! guide {
  ($vis:vis $name:ident { $($key:ident: $path:literal),* $(,)? }) => {
    $vis struct $name {
      $($vis $key: $crate::name::PathId,)*
    }

    impl $crate::name::FromTable<'_> for $name {
      fn build(table: &mut $crate::name::Table) -> Self {
        Self { $($key: table.add_path($path),)* }
      }
    }
  };

  ($vis:vis $name:ident { $($key:ident: $path:literal),* $(,)? ..$table:ident }) => {
    $vis struct $name<'a> {
      $($vis $key: $crate::name::PathId,)*
      $table: &'a mut $crate::name::Table,
    }

    impl<'a> $crate::name::FromTable<'a> for $name<'a> {
      fn build(table: &'a mut $crate::name::Table) -> Self {
        Self { $($key: table.add_path($path),)* $table }
      }
    }
  };
}
