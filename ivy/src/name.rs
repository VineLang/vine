use std::{borrow::Cow, collections::HashMap, fmt};

use vine_util::{idx::IdxVec, nat::Nat, new_idx};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
  pub path: PathId,
  pub payload: Nat,
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

  pub fn get_path(&self, path: &str) -> Option<PathId> {
    self.path_lookup.get(path).copied()
  }

  pub fn name(&self, id: NameId) -> &Name {
    &self.names[id]
  }

  pub fn show_name_id(&self, id: NameId) -> ShowName<'_> {
    ShowName { table: self, name: self.name(id) }
  }

  pub fn show_name<'a>(&'a self, name: &'a Name) -> ShowName<'a> {
    ShowName { table: self, name }
  }

  pub fn add_name(&mut self, name: impl Into<Name>) -> NameId {
    *self.name_lookup.entry(name.into()).or_insert_with_key(|name| self.names.push(name.clone()))
  }

  pub fn add_path_name<'a>(&mut self, path: impl Into<Cow<'a, str>>) -> NameId {
    let path = self.add_path(path);
    self.add_name(path)
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
    Name { path: self, children: Vec::new(), payload: Nat::ZERO }
  }

  pub fn with_payload(self, payload: impl Into<Nat>) -> Name {
    self.to_name().with_payload(payload)
  }

  pub fn with_children(self, children: impl IntoIterator<Item = NameId>) -> Name {
    self.to_name().with_children(children)
  }
}

impl Name {
  pub fn is_path(&self) -> bool {
    self.payload.is_zero() && self.children.is_empty()
  }

  pub fn with_payload(self, payload: impl Into<Nat>) -> Name {
    Name { payload: payload.into(), ..self }
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
      payload: name.payload.clone(),
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
  ($vis:vis $name:ident { $($key:ident: $type:ident = $str:literal),* $(,)? }) => {
    $vis struct $name {
      $($vis $key: $crate::name::$type,)*
    }

    impl $crate::name::FromTable<'_> for $name {
      fn build(table: &mut $crate::name::Table) -> Self {
        $(let $key = guide!(impl table $type $str);)*
        Self { $($key),* }
      }
    }
  };

  (type ) => { $crate::name::PathId };
  (impl $table:ident PathId $val:literal) => { $table.add_path($val) };
  (impl $table:ident NameId $val:literal) => { $table.add_path_name($val) };
}

pub struct ShowName<'a> {
  table: &'a Table,
  name: &'a Name,
}

impl fmt::Display for ShowName<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.table.path(self.name.path))?;
    if !self.name.payload.is_zero() {
      write!(f, "#{}", self.name.payload)?;
    }
    if !self.name.children.is_empty() {
      f.write_str("[")?;
      let mut first = true;
      for &child in &self.name.children {
        if !first {
          f.write_str(", ")?;
        }
        first = false;
        write!(f, "{}", ShowName { table: self.table, name: self.table.name(child) })?;
      }
      f.write_str("]")?;
    }
    Ok(())
  }
}

impl fmt::Debug for ShowName<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{}`", self)
  }
}
