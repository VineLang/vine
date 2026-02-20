use std::{borrow::Cow, collections::HashMap};

use vine_util::{idx::IdxVec, nat::Nat, new_idx};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
  pub path: PathId,
  pub data: Nat,
  pub children: Vec<NameId>,
}

new_idx!(pub NameId);
new_idx!(pub PathId);

#[derive(Debug, Default, Clone)]
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

  pub fn add_name(&mut self, name: impl Into<Name>) -> NameId {
    *self.name_lookup.entry(name.into()).or_insert_with_key(|name| self.names.push(name.clone()))
  }

  pub fn has_name(&self, name: &Name) -> Option<NameId> {
    self.name_lookup.get(name).copied()
  }

  pub fn import(&mut self, table: &Table) -> Importer {
    let mut importer = Importer { paths: IdxVec::new(), names: IdxVec::new() };
    for (id, path) in &table.paths {
      importer.paths.push_to(id, self.add_path(path));
    }
    for (id, name) in &table.names {
      importer.names.push_to(id, self.add_name(importer.import_name(name)));
    }
    importer
  }
}

impl From<PathId> for Name {
  fn from(path: PathId) -> Self {
    path.to_name()
  }
}

impl PathId {
  pub fn to_name(self) -> Name {
    Name { path: self, children: Vec::new(), data: Nat::ZERO }
  }

  pub fn with_data(self, data: impl Into<Nat>) -> Name {
    self.to_name().with_data(data)
  }

  pub fn with_children(self, children: impl IntoIterator<Item = NameId>) -> Name {
    self.to_name().with_children(children)
  }
}

impl Name {
  pub fn is_path(&self) -> bool {
    self.data.is_zero() && self.children.is_empty()
  }

  pub fn with_data(self, data: impl Into<Nat>) -> Name {
    Name { data: data.into(), ..self }
  }

  pub fn with_children(self, children: impl IntoIterator<Item = NameId>) -> Name {
    Name { children: children.into_iter().collect(), ..self }
  }
}

pub struct Importer {
  paths: IdxVec<PathId, PathId>,
  names: IdxVec<NameId, NameId>,
}

impl Importer {
  pub fn translate_path(&self, id: PathId) -> PathId {
    self.paths[id]
  }

  pub fn translate_name_id(&self, id: NameId) -> NameId {
    self.names[id]
  }

  pub fn import_name(&self, name: &Name) -> Name {
    Name {
      path: self.translate_path(name.path),
      children: name.children.iter().map(|&i| self.translate_name_id(i)).collect(),
      data: name.data.clone(),
    }
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
