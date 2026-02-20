use crate::{
  name::{Name, PathId, Table},
  net::FlatNode,
  translate::{Rule, Rules},
};

pub fn elide_unary(paths: impl IntoIterator<Item = PathId>) -> impl Rules<'static> {
  Rule(paths, |node, _, net| {
    let a = node.pri;
    let [b] = *node.aux else {
      return net.push(node);
    };
    net.link(a, b);
  })
}

pub fn chain_binary(paths: impl IntoIterator<Item = PathId>) -> impl Rules<'static> {
  Rule(paths, move |node, _, net| {
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

pub fn replace_nilary(
  paths: impl IntoIterator<Item = PathId>,
  path: PathId,
) -> impl Rules<'static> {
  Rule(paths, move |node, _, net| {
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
) -> impl Rules<'a> {
  Rule(paths, move |node, table, net| {
    net.push(FlatNode { name: map(table, node.name), ..node });
  })
}

pub fn replace_name(
  paths: impl IntoIterator<Item = PathId>,
  new_name: impl Into<Name>,
) -> impl Rules<'static> {
  let new_name = new_name.into();
  map_name(paths, move |_, name| {
    assert!(name.is_path());
    new_name.clone()
  })
}

pub fn replace_path(
  paths: impl IntoIterator<Item = PathId>,
  new_path: PathId,
) -> impl Rules<'static> {
  map_name(paths, move |_, name| Name { path: new_path, ..name })
}
