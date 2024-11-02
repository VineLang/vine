use std::{collections::hash_map::Entry, mem::replace};

use vine_util::interner::StringInterner;

use crate::{
  ast::{Expr, ExprKind, Ident, Item, ItemKind, ModKind, Path, Span, UseTree},
  diag::{Diag, DiagGroup},
  visit::{VisitMut, Visitee},
};

use super::{
  AdtDef, Def, DefId, Member, Resolver, TypeDef, UseId, ValueDef, ValueDefKind, VariantDef,
};

impl Resolver {
  pub fn build_graph(&mut self, interner: &StringInterner<'static>, root: ModKind) {
    let def = self.new_def(Path::ROOT, None);
    debug_assert_eq!(def, 0);
    self.build_prelude(interner);
    self.build_mod(root, def);
  }

  pub(crate) fn build_mod(&mut self, module: ModKind, def: DefId) {
    let ModKind::Loaded(items) = module else { unreachable!("module not yet loaded") };
    for item in items {
      self.build_item(item, def);
    }
  }

  fn build_item(&mut self, item: Item, parent: DefId) {
    match item.kind {
      ItemKind::Fn(f) => {
        self.define_value(
          item.span,
          parent,
          f.name,
          ValueDef {
            generics: f.generics,
            annotation: None,
            ty: None,
            locals: 0,
            kind: ValueDefKind::Expr(Expr {
              span: item.span,
              kind: ExprKind::Fn(f.params, Some(f.ret), Box::new(f.body)),
            }),
          },
        );
      }
      ItemKind::Const(c) => {
        self.define_value(
          item.span,
          parent,
          c.name,
          ValueDef {
            generics: c.generics,
            annotation: Some(c.ty),
            ty: None,
            locals: 0,
            kind: ValueDefKind::Expr(c.value),
          },
        );
      }
      ItemKind::Ivy(i) => {
        self.define_value(
          item.span,
          parent,
          i.name,
          ValueDef {
            generics: i.generics,
            annotation: Some(i.ty),
            ty: None,
            locals: 0,
            kind: ValueDefKind::Ivy(i.net),
          },
        );
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
          &mut self.defs[parent],
          &mut Path { span: Span::NONE, segments: Vec::new(), absolute: false, resolved: None },
        );
        self.next_use_id += 1;
      }
      ItemKind::Struct(s) => {
        let child = self.get_or_insert_child(parent, s.name);
        if child.type_def.is_some()
          || child.adt_def.is_some()
          || child.variant_def.is_some()
          || child.value_def.is_some()
        {
          self.diags.add(Diag::DuplicateItem { span: item.span, name: s.name });
          return;
        }
        child.type_def = Some(TypeDef { generics: s.generics.clone(), alias: None, ty: None });
        child.adt_def = Some(AdtDef { generics: s.generics.clone(), variants: vec![child.id] });
        child.variant_def = Some(VariantDef {
          generics: s.generics.clone(),
          adt: child.id,
          variant: 0,
          fields: s.fields,
          field_types: None,
        });
        child.value_def = Some(ValueDef {
          generics: s.generics,
          annotation: None,
          ty: None,
          locals: 0,
          kind: ValueDefKind::AdtConstructor,
        });
      }
      ItemKind::Enum(e) => {
        let child = self.get_or_insert_child(parent, e.name);
        if child.type_def.is_some() || child.adt_def.is_some() {
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
            if variant.variant_def.is_some() || variant.value_def.is_some() {
              self.diags.add(Diag::DuplicateItem { span: item.span, name: v.name });
              return None;
            }
            variant.variant_def = Some(VariantDef {
              generics: e.generics.clone(),
              adt,
              variant: i,
              fields: v.fields,
              field_types: None,
            });
            variant.value_def = Some(ValueDef {
              generics: e.generics.clone(),
              annotation: None,
              ty: None,
              locals: 0,
              kind: ValueDefKind::AdtConstructor,
            });
            Some(variant.id)
          })
          .collect();
        self.defs[adt].type_def =
          Some(TypeDef { generics: e.generics.clone(), alias: None, ty: None });
        self.defs[adt].adt_def = Some(AdtDef { generics: e.generics, variants });
      }
      ItemKind::Type(t) => {
        let child = self.get_or_insert_child(parent, t.name);
        if child.type_def.is_some() || child.adt_def.is_some() {
          self.diags.add(Diag::DuplicateItem { span: item.span, name: t.name });
          return;
        }
        child.type_def =
          Some(TypeDef { generics: t.generics.clone(), alias: Some(t.ty), ty: None });
      }
      ItemKind::Pattern(_) => todo!(),
      ItemKind::Taken => {}
    }
  }

  fn define_value(&mut self, span: Span, parent: DefId, name: Ident, mut value: ValueDef) {
    let child = self.get_or_insert_child(parent, name);
    if child.value_def.is_some() {
      self.diags.add(Diag::DuplicateItem { span, name });
      return;
    }
    let child = child.id;
    if let ValueDefKind::Expr(expr) = &mut value.kind {
      self.extract_subitems(child, expr);
    }
    let child = &mut self.defs[child];
    assert!(child.value_def.is_none());
    child.value_def = Some(value);
  }

  pub(crate) fn extract_subitems<'t>(&mut self, def: DefId, visitee: &'t mut impl Visitee<'t>) {
    SubitemVisitor { resolver: self, def }.visit(visitee);
  }

  pub(crate) fn get_or_insert_child(&mut self, parent: DefId, name: Ident) -> &mut Def {
    let next_child = self.next_def_id();
    let parent_def = &mut self.defs[parent];
    let mut new = false;
    let member = parent_def.members.entry(name).or_insert_with(|| {
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
      let path = parent_def.canonical.extend(&[name]);
      self.new_def(path, Some(parent));
    }
    &mut self.defs[child]
  }

  fn build_imports(
    use_id: UseId,
    diags: &mut DiagGroup,
    tree: UseTree,
    def: &mut Def,
    path: &mut Path,
  ) {
    let initial_len = path.segments.len();
    path.segments.extend(&tree.path.segments);
    if let Some(children) = tree.children {
      for child in children {
        Self::build_imports(use_id, diags, child, def, path);
      }
    } else {
      let name = *path.segments.last().unwrap();
      let mut path = path.clone();
      path.span = tree.path.span;
      if let Entry::Vacant(e) = def.members.entry(name) {
        e.insert(Member::UnresolvedImport(Some(path), use_id));
      } else {
        diags.add(Diag::DuplicateItem { span: tree.path.span, name });
      }
    }
    path.segments.truncate(initial_len);
  }

  fn next_def_id(&self) -> DefId {
    self.defs.len()
  }

  fn new_def(&mut self, mut canonical: Path, parent: Option<usize>) -> DefId {
    let id = self.defs.len();
    canonical.resolved = Some(id);
    let mut def = Def {
      id,
      canonical,
      members: Default::default(),
      parent,
      value_def: None,
      type_def: None,
      adt_def: None,
      variant_def: None,
    };
    if let Some(&name) = def.canonical.segments.last() {
      def.members.insert(name, Member::Child(id));
    }
    self.defs.push(def);
    id
  }

  pub fn revert(&mut self, old_def_count: usize, old_use_count: usize) {
    self.defs.truncate(old_def_count);
    for def in &mut self.defs {
      def.members.retain(|_, m| match *m {
        Member::Child(id) => id < old_def_count,
        Member::ResolvedImport(_, id) | Member::UnresolvedImport(_, id) => id < old_use_count,
      });
    }
  }
}

struct SubitemVisitor<'a> {
  resolver: &'a mut Resolver,
  def: DefId,
}

impl VisitMut<'_> for SubitemVisitor<'_> {
  fn visit_item(&mut self, item: &mut Item) {
    self
      .resolver
      .build_item(replace(item, Item { span: Span::NONE, kind: ItemKind::Taken }), self.def);
  }
}
