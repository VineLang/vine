use crate::ast::{Ident, Item, ItemKind, ModKind, Path, Term, TermKind, UseTree};

use super::{Adt, Node, NodeId, NodeValue, Resolver, Variant};

impl Resolver {
  pub fn build_graph(&mut self, root: ModKind) {
    let node = self.new_node(Path::ROOT, None);
    self.build_mod(root, node);
  }

  fn build_mod(&mut self, module: ModKind, node: NodeId) -> NodeId {
    let ModKind::Loaded(items) = module else { unreachable!("module not yet loaded") };
    for item in items {
      self.build_item(item, node);
    }
    node
  }

  fn build_item(&mut self, item: Item, parent: NodeId) {
    match item.kind {
      ItemKind::Fn(f) => {
        self.define_value(
          parent,
          f.name,
          NodeValue::Term(Term { kind: TermKind::Fn(f.params, Box::new(f.body)) }),
        );
      }
      ItemKind::Const(c) => {
        self.define_value(parent, c.name, NodeValue::Term(c.value));
      }
      ItemKind::Ivy(i) => {
        self.define_value(parent, i.name, NodeValue::Ivy(i.net));
      }
      ItemKind::Mod(m) => {
        let child = self.get_or_insert_child(parent, m.name).id;
        self.build_mod(m.kind, child);
      }
      ItemKind::Use(tree) => {
        Self::build_imports(
          tree,
          &mut self.nodes[parent],
          &mut Path { segments: Vec::new(), absolute: false, resolved: None },
        );
      }
      ItemKind::Struct(s) => {
        let child = self.get_or_insert_child(parent, s.name);
        if child.adt.is_some() || child.variant.is_some() || child.value.is_some() {
          panic!("duplicate definition of {:?}", child.canonical);
        }
        child.adt = Some(Adt { variants: vec![child.id] });
        child.variant = Some(Variant { adt: child.id, variant: 0, fields: s.fields });
        child.value = Some(NodeValue::AdtConstructor);
      }
      ItemKind::Enum(e) => {
        let child = self.get_or_insert_child(parent, e.name);
        if child.adt.is_some() {
          panic!("duplicate definition of {:?}", child.canonical);
        }
        let adt = child.id;
        let variants = e
          .variants
          .into_iter()
          .enumerate()
          .map(|(i, v)| {
            let variant = self.get_or_insert_child(adt, v.name);
            if variant.variant.is_some() || variant.value.is_some() {
              panic!("duplicate definition of {:?}", variant.canonical);
            }
            variant.variant = Some(Variant { adt, variant: i, fields: v.fields });
            variant.value = Some(NodeValue::AdtConstructor);
            variant.id
          })
          .collect();
        self.nodes[adt].adt = Some(Adt { variants });
      }
      ItemKind::Pattern(_) => todo!(),
    }
  }

  fn define_value(&mut self, parent: NodeId, name: Ident, value: NodeValue) {
    let child = self.get_or_insert_child(parent, name);
    if child.value.is_some() {
      panic!("duplicate definition of {:?}", child.canonical);
    }
    child.value = Some(value);
  }

  fn get_or_insert_child(&mut self, parent: NodeId, name: Ident) -> &mut Node {
    let next_child = self.next_node_id();
    let parent_node = &mut self.nodes[parent];
    let mut new = false;
    let child = *parent_node.children.entry(name).or_insert_with(|| {
      new = true;
      next_child
    });
    if new {
      let path = parent_node.canonical.extend(&[name]);
      self.new_node(path, Some(parent));
    }
    &mut self.nodes[child]
  }

  fn build_imports(tree: UseTree, node: &mut Node, path: &mut Path) {
    let initial_len = path.segments.len();
    path.segments.extend(&tree.path.segments);
    if let Some(children) = tree.children {
      for child in children {
        Self::build_imports(child, node, path);
      }
    } else {
      let name = *path.segments.last().unwrap();
      let prev = node.imports.insert(name, Some(path.clone()));
      if prev.is_some() {
        panic!("duplicate import of {:?} in {:?}", name, node.canonical);
      }
    }
    path.segments.truncate(initial_len);
  }

  fn next_node_id(&self) -> NodeId {
    self.nodes.len()
  }

  fn new_node(&mut self, mut canonical: Path, parent: Option<usize>) -> NodeId {
    let id = self.nodes.len();
    canonical.resolved = Some(id);
    let node = Node {
      id,
      canonical,
      children: Default::default(),
      imports: Default::default(),
      parent,
      locals: 0,
      value: None,
      adt: None,
      variant: None,
    };
    self.nodes.push(node);
    id
  }
}
