use vine_util::register::Register;

use crate::{
  name::{Name, PathId, Table},
  net::FlatNode,
  translate::{Translate, Translator},
};

pub fn elide_unary<'a>(paths: impl IntoIterator<Item = PathId>) -> impl Register<Translator<'a>> {
  Translate(paths, |node, _, net| {
    let a = node.pri;
    let [b] = *node.aux else {
      return net.push(node);
    };
    net.link(a, b);
  })
}

pub fn chain_binary<'a>(paths: impl IntoIterator<Item = PathId>) -> impl Register<Translator<'a>> {
  Translate(paths, move |node, _, net| {
    if node.aux.len() <= 2 {
      return net.push(node);
    }
    assert!(node.name.is_path());
    let mut aux = node.aux;
    let mut end = aux.pop().unwrap();
    while let Some(port) = aux.pop() {
      end = net.make(node.name.path, [port, end]);
    }
    net.link(node.pri, end);
  })
}

pub fn replace_nilary<'a>(
  paths: impl IntoIterator<Item = PathId>,
  path: PathId,
) -> impl Register<Translator<'a>> {
  Translate(paths, move |node, _, net| {
    if !node.aux.is_empty() {
      return net.push(node);
    }
    assert!(node.name.is_path());
    net.push(FlatNode { name: path.to_name(), ..node });
  })
}

pub fn map_name<'a>(
  paths: impl IntoIterator<Item = PathId>,
  map: impl Fn(&mut Table, Name) -> Name + 'a,
) -> impl Register<Translator<'a>> {
  Translate(paths, move |node, table, net| {
    net.push(FlatNode { name: map(table, node.name), ..node });
  })
}

pub fn replace_name<'a>(
  paths: impl IntoIterator<Item = PathId>,
  new_name: impl Into<Name>,
) -> impl Register<Translator<'a>> {
  let new_name = new_name.into();
  map_name(paths, move |_, name| {
    assert!(name.is_path());
    new_name.clone()
  })
}

pub fn replace_path<'a>(
  paths: impl IntoIterator<Item = PathId>,
  new_path: PathId,
) -> impl Register<Translator<'a>> {
  map_name(paths, move |_, name| Name { path: new_path, ..name })
}
