use std::{collections::hash_map::Entry, mem::take};

use vine_util::idx::Counter;

use crate::{
  ast::{
    AttrKind, Builtin, Expr, ExprKind, Ident, Item, ItemKind, ModKind, Path, Span, UseTree, Vis,
  },
  core::Core,
  diag::Diag,
  visit::{VisitMut, Visitee},
};

use super::{
  AdtDef, Def, DefId, Member, MemberKind, Resolver, TypeDef, UseId, ValueDef, ValueDefKind,
  VariantDef,
};

impl<'core> Resolver<'core> {
  pub fn build_graph(&mut self, root: ModKind<'core>) {
    let _root_def = self.new_def(Path::ROOT, None, DefId::ROOT);
    debug_assert_eq!(_root_def, DefId::ROOT);
    self.build_mod(DefId::ROOT, root, DefId::ROOT);
    if let Some(&prelude) = self.builtins.get(&Builtin::Prelude) {
      self.defs[prelude].parent = None;
      self.defs[DefId::ROOT].parent = Some(prelude);
    }
  }

  pub(crate) fn build_mod(&mut self, vis: DefId, module: ModKind<'core>, def: DefId) {
    let ModKind::Loaded(_, items) = module else { unreachable!("module not yet loaded") };
    for item in items {
      self.build_item(vis, item, def);
    }
  }

  fn build_item(&mut self, member_vis: DefId, item: Item<'core>, parent: DefId) {
    let span = item.span;
    let vis = self.resolve_vis(parent, item.vis);
    let member_vis = vis.max(member_vis);
    let def_id = match item.kind {
      ItemKind::Fn(f) => self.define_value(
        span,
        parent,
        f.name,
        ValueDef {
          vis,
          generics: f.generics,
          annotation: None,
          ty: None,
          locals: Counter::default(),
          kind: ValueDefKind::Expr(Expr {
            span,
            kind: ExprKind::Fn(
              f.params,
              Some(f.ret),
              Box::new(Expr { span: f.body.span, kind: ExprKind::Block(f.body) }),
            ),
          }),
        },
        member_vis,
      ),
      ItemKind::Const(c) => self.define_value(
        span,
        parent,
        c.name,
        ValueDef {
          vis,
          generics: c.generics,
          annotation: Some(c.ty),
          ty: None,
          locals: Counter::default(),
          kind: ValueDefKind::Expr(c.value),
        },
        member_vis,
      ),
      ItemKind::Ivy(i) => self.define_value(
        span,
        parent,
        i.name,
        ValueDef {
          vis,
          generics: i.generics,
          annotation: Some(i.ty),
          ty: None,
          locals: Counter::default(),
          kind: ValueDefKind::Ivy(i.net),
        },
        member_vis,
      ),
      ItemKind::Mod(m) => {
        let child = self.get_or_insert_child(parent, m.name, member_vis).id;
        self.build_mod(vis, m.kind, child);
        Some(child)
      }
      ItemKind::Use(u) => {
        Self::build_imports(
          self.use_id.next(),
          self.core,
          u.tree,
          &mut self.defs[parent],
          &mut Path { segments: Vec::new(), absolute: u.absolute, resolved: None },
          member_vis,
        );
        None
      }
      ItemKind::Struct(s) => {
        let child = self.get_or_insert_child(parent, s.name, member_vis);
        if child.type_def.is_some()
          || child.adt_def.is_some()
          || child.variant_def.is_some()
          || child.value_def.is_some()
        {
          self.core.report(Diag::DuplicateItem { span, name: s.name });
          return;
        }
        child.type_def = Some(TypeDef { vis, generics: s.generics.clone(), alias: None, ty: None });
        child.adt_def = Some(AdtDef { generics: s.generics.clone(), variants: vec![child.id] });
        child.variant_def = Some(VariantDef {
          vis,
          generics: s.generics.clone(),
          adt: child.id,
          variant: 0,
          fields: s.fields,
          field_types: None,
        });
        child.value_def = Some(ValueDef {
          vis,
          generics: s.generics,
          annotation: None,
          ty: None,
          locals: Counter::default(),
          kind: ValueDefKind::AdtConstructor,
        });
        Some(child.id)
      }
      ItemKind::Enum(e) => {
        let child = self.get_or_insert_child(parent, e.name, member_vis);
        if child.type_def.is_some() || child.adt_def.is_some() {
          self.core.report(Diag::DuplicateItem { span, name: e.name });
          return;
        }
        let adt = child.id;
        let variants = e
          .variants
          .into_iter()
          .enumerate()
          .filter_map(|(i, v)| {
            let variant = self.get_or_insert_child(adt, v.name, member_vis);
            if variant.variant_def.is_some() || variant.value_def.is_some() {
              self.core.report(Diag::DuplicateItem { span, name: v.name });
              return None;
            }
            variant.variant_def = Some(VariantDef {
              vis,
              generics: e.generics.clone(),
              adt,
              variant: i,
              fields: v.fields,
              field_types: None,
            });
            variant.value_def = Some(ValueDef {
              vis,
              generics: e.generics.clone(),
              annotation: None,
              ty: None,
              locals: Counter::default(),
              kind: ValueDefKind::AdtConstructor,
            });
            Some(variant.id)
          })
          .collect();
        self.defs[adt].type_def =
          Some(TypeDef { vis, generics: e.generics.clone(), alias: None, ty: None });
        self.defs[adt].adt_def = Some(AdtDef { generics: e.generics, variants });
        Some(adt)
      }
      ItemKind::Type(t) => {
        let child = self.get_or_insert_child(parent, t.name, member_vis);
        if child.type_def.is_some() || child.adt_def.is_some() {
          self.core.report(Diag::DuplicateItem { span, name: t.name });
          return;
        }
        child.type_def =
          Some(TypeDef { vis, generics: t.generics.clone(), alias: Some(t.ty), ty: None });
        Some(child.id)
      }
      ItemKind::Taken => None,
    };
    for attr in item.attrs {
      match attr.kind {
        AttrKind::Builtin(b) => {
          let Some(def_id) = def_id else {
            self.core.report(Diag::BadBuiltin { span });
            continue;
          };
          let old = self.builtins.insert(b, def_id);
          if old.is_some() {
            self.core.report(Diag::BadBuiltin { span });
          }
        }
      }
    }
  }

  fn resolve_vis(&mut self, base: DefId, vis: Vis) -> DefId {
    match vis {
      Vis::Private => base,
      Vis::Public => DefId::ROOT,
      Vis::PublicTo(span, name) => {
        let ancestors = &self.defs[base].ancestors;
        if let Some(&ancestor) =
          ancestors.iter().rev().find(|&&a| self.defs[a].canonical.segments.last() == Some(&name))
        {
          ancestor
        } else {
          self.core.report(Diag::BadVis { span });
          DefId::ROOT
        }
      }
    }
  }

  fn define_value(
    &mut self,
    span: Span,
    parent: DefId,
    name: Ident<'core>,
    mut value: ValueDef<'core>,
    vis: DefId,
  ) -> Option<DefId> {
    let child = self.get_or_insert_child(parent, name, vis);
    if child.value_def.is_some() {
      self.core.report(Diag::DuplicateItem { span, name });
      return None;
    }
    let child = child.id;
    if let ValueDefKind::Expr(expr) = &mut value.kind {
      self.extract_subitems(child, expr);
    }
    let child = &mut self.defs[child];
    assert!(child.value_def.is_none());
    child.value_def = Some(value);
    Some(child.id)
  }

  pub(crate) fn extract_subitems<'t>(&mut self, def: DefId, visitee: impl Visitee<'core, 't>) {
    SubitemVisitor { resolver: self, def }.visit(visitee);
  }

  pub(crate) fn get_or_insert_child(
    &mut self,
    parent: DefId,
    name: Ident<'core>,
    vis: DefId,
  ) -> &mut Def<'core> {
    let next_child = self.defs.next_index();
    let parent_def = &mut self.defs[parent];
    let mut new = false;
    let member = parent_def.members.entry(name).or_insert_with(|| {
      new = true;
      Member { vis, kind: MemberKind::Child(next_child) }
    });
    let child = match member.kind {
      MemberKind::Child(child) => child,
      _ => {
        new = true;
        next_child
      }
    };
    member.vis = member.vis.min(vis);
    if new {
      let path = parent_def.canonical.extend(&[name]);
      self.new_def(path, Some(parent), vis);
    }
    &mut self.defs[child]
  }

  fn build_imports(
    use_id: UseId,
    core: &Core<'core>,
    tree: UseTree<'core>,
    def: &mut Def<'core>,
    path: &mut Path<'core>,
    vis: DefId,
  ) {
    let initial_len = path.segments.len();
    path.segments.extend(&tree.path.segments);
    if let Some(children) = tree.children {
      for child in children {
        Self::build_imports(use_id, core, child, def, path, vis);
      }
    } else {
      let name = *path.segments.last().unwrap();
      let path = path.clone();
      if let Entry::Vacant(e) = def.members.entry(name) {
        e.insert(Member { vis, kind: MemberKind::UnresolvedImport(tree.span, Some(path), use_id) });
      } else {
        core.report(Diag::DuplicateItem { span: tree.span, name });
      }
    }
    path.segments.truncate(initial_len);
  }

  fn new_def(&mut self, mut canonical: Path<'core>, parent: Option<DefId>, vis: DefId) -> DefId {
    let id = self.defs.next_index();
    canonical.resolved = Some(id);
    let mut def = Def {
      id,
      canonical,
      members: Default::default(),
      parent,
      ancestors: Vec::new(),
      value_def: None,
      type_def: None,
      adt_def: None,
      variant_def: None,
    };
    if let Some(parent) = parent {
      def.ancestors = self.defs[parent].ancestors.iter().copied().chain([parent]).collect();
    }
    if let Some(&name) = def.canonical.segments.last() {
      def.members.insert(name, Member { vis, kind: MemberKind::Child(id) });
    }
    self.defs.push(def);
    id
  }

  pub fn revert(&mut self, old_def_count: DefId, old_use_count: UseId) {
    self.defs.truncate(old_def_count.0);
    for def in self.defs.values_mut() {
      def.members.retain(|_, m| match m.kind {
        MemberKind::Child(id) => id < old_def_count,
        MemberKind::ResolvedImport(_, id) | MemberKind::UnresolvedImport(_, _, id) => {
          id < old_use_count
        }
      });
    }
  }
}

struct SubitemVisitor<'core, 'a> {
  resolver: &'a mut Resolver<'core>,
  def: DefId,
}

impl<'core> VisitMut<'core, '_> for SubitemVisitor<'core, '_> {
  fn visit_item(&mut self, item: &mut Item<'core>) {
    if !matches!(item.vis, Vis::Private) {
      self.resolver.core.report(Diag::VisibleSubitem { span: item.span });
    }
    self.resolver.build_item(self.def, take(item), self.def);
  }
}
