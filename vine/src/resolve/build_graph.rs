use std::{collections::hash_map::Entry, mem::replace};

use crate::{
  ast::{Expr, ExprKind, Ident, Item, ItemKind, ModKind, Path, Span, UseTree},
  diag::{Diag, DiagGroup},
  visit::{VisitMut, Visitee},
};

use super::{Adt, Member, Node, NodeId, NodeValue, Resolver, UseId, Variant};

impl Resolver {
  pub fn build_graph(&mut self, root: ModKind) {
    let node = self.new_node(Path::ROOT, None);
    debug_assert_eq!(node, 0);
    self.build_mod(root, node);
  }

  pub(crate) fn build_mod(&mut self, module: ModKind, node: NodeId) {
    let ModKind::Loaded(items) = module else { unreachable!("module not yet loaded") };
    for item in items {
      self.build_item(item, node);
    }
  }

  fn build_item(&mut self, item: Item, parent: NodeId) {
    match item.kind {
      ItemKind::Fn(f) => {
        self.define_value(
          item.span,
          parent,
          f.name,
          NodeValue::Expr(Expr { span: item.span, kind: ExprKind::Fn(f.params, Box::new(f.body)) }),
        );
      }
      ItemKind::Const(c) => {
        self.define_value(item.span, parent, c.name, NodeValue::Expr(c.value));
      }
      ItemKind::Ivy(i) => {
        self.define_value(item.span, parent, i.name, NodeValue::Ivy(i.net));
      }
      ItemKind::Mod(m) => {
        let child = self.get_or_insert_child(parent, m.name).id;
        self.build_mod(m.kind, child);
      }
      ItemKind::Use(tree) => {
        Self::build_imports(
          self.next_use_id,
          &mut self.diags,
          tree,
          &mut self.nodes[parent],
          &mut Path { span: Span::NONE, segments: Vec::new(), absolute: false, resolved: None },
        );
        self.next_use_id += 1;
      }
      ItemKind::Struct(s) => {
        let child = self.get_or_insert_child(parent, s.name);
        if child.adt.is_some() || child.variant.is_some() || child.value.is_some() {
          self.diags.add(Diag::DuplicateItem { span: item.span, name: s.name });
          return;
        }
        child.adt = Some(Adt { variants: vec![child.id] });
        child.variant = Some(Variant { adt: child.id, variant: 0, fields: s.fields });
        child.value = Some(NodeValue::AdtConstructor);
      }
      ItemKind::Enum(e) => {
        let child = self.get_or_insert_child(parent, e.name);
        if child.adt.is_some() {
          self.diags.add(Diag::DuplicateItem { span: item.span, name: e.name });
          return;
        }
        let adt = child.id;
        let variants = e
          .variants
          .into_iter()
          .enumerate()
          .filter_map(|(i, v)| {
            let variant = self.get_or_insert_child(adt, v.name);
            if variant.variant.is_some() || variant.value.is_some() {
              self.diags.add(Diag::DuplicateItem { span: item.span, name: v.name });
              return None;
            }
            variant.variant = Some(Variant { adt, variant: i, fields: v.fields });
            variant.value = Some(NodeValue::AdtConstructor);
            Some(variant.id)
          })
          .collect();
        self.nodes[adt].adt = Some(Adt { variants });
      }
      ItemKind::Pattern(_) => todo!(),
      ItemKind::Taken => {}
    }
  }

  fn define_value(&mut self, span: Span, parent: NodeId, name: Ident, mut value: NodeValue) {
    let child = self.get_or_insert_child(parent, name);
    if child.value.is_some() {
      self.diags.add(Diag::DuplicateItem { span, name });
      return;
    }
    let child = child.id;
    if let NodeValue::Expr(expr) = &mut value {
      self.extract_subitems(child, expr);
    }
    let child = &mut self.nodes[child];
    assert!(child.value.is_none());
    child.value = Some(value);
  }

  pub(crate) fn extract_subitems<'t>(&mut self, node: NodeId, visitee: &'t mut impl Visitee<'t>) {
    SubitemVisitor { resolver: self, node }.visit(visitee);
  }

  pub(crate) fn get_or_insert_child(&mut self, parent: NodeId, name: Ident) -> &mut Node {
    let next_child = self.next_node_id();
    let parent_node = &mut self.nodes[parent];
    let mut new = false;
    let member = parent_node.members.entry(name).or_insert_with(|| {
      new = true;
      Member::Child(next_child)
    });
    let child = match member {
      Member::Child(child) => *child,
      _ => {
        new = true;
        next_child
      }
    };
    if new {
      let path = parent_node.canonical.extend(&[name]);
      self.new_node(path, Some(parent));
    }
    &mut self.nodes[child]
  }

  fn build_imports(
    use_id: UseId,
    diags: &mut DiagGroup,
    tree: UseTree,
    node: &mut Node,
    path: &mut Path,
  ) {
    let initial_len = path.segments.len();
    path.segments.extend(&tree.path.segments);
    if let Some(children) = tree.children {
      for child in children {
        Self::build_imports(use_id, diags, child, node, path);
      }
    } else {
      let name = *path.segments.last().unwrap();
      let mut path = path.clone();
      path.span = tree.path.span;
      if let Entry::Vacant(e) = node.members.entry(name) {
        e.insert(Member::UnresolvedImport(Some(path), use_id));
      } else {
        diags.add(Diag::DuplicateItem { span: tree.path.span, name });
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
    let mut node = Node {
      id,
      canonical,
      members: Default::default(),
      parent,
      locals: 0,
      value: None,
      adt: None,
      variant: None,
    };
    if let Some(&name) = node.canonical.segments.last() {
      node.members.insert(name, Member::Child(id));
    }
    self.nodes.push(node);
    id
  }

  pub fn revert(&mut self, old_node_count: usize, old_use_count: usize) {
    self.nodes.truncate(old_node_count);
    for node in &mut self.nodes {
      node.members.retain(|_, m| match *m {
        Member::Child(id) => id < old_node_count,
        Member::ResolvedImport(_, id) | Member::UnresolvedImport(_, id) => id < old_use_count,
      });
    }
  }
}

struct SubitemVisitor<'a> {
  resolver: &'a mut Resolver,
  node: NodeId,
}

impl VisitMut<'_> for SubitemVisitor<'_> {
  fn visit_item(&mut self, item: &mut Item) {
    self
      .resolver
      .build_item(replace(item, Item { span: Span::NONE, kind: ItemKind::Taken }), self.node);
  }
}
