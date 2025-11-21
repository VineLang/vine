use std::collections::{BTreeMap, hash_map::Entry};

use vine_util::parser::{Delimiters, Parser};

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{BRACE_COMMA, VineParser},
    resolver::Resolver,
  },
  structures::{
    ast::{Ident, ItemKind, Span, UseItem, UseTree},
    chart::{Binding, DefId, ImportDef, ImportId, ImportParent, MemberKind, VisId},
    diag::{Diag, ErrorGuaranteed},
    signatures::ImportState,
  },
  tools::fmt::{Formatter, doc::Doc},
};

impl VineParser<'_> {
  pub(crate) fn parse_use_item(&mut self) -> Result<(Span, ItemKind), Diag> {
    self.expect(Token::Use)?;
    let span = self.start_span();
    let mut relative = BTreeMap::new();
    let mut absolute = BTreeMap::new();
    self.parse_delimited(
      Delimiters { open: None, close: None, separator: Some(Token::Comma) },
      |self_| {
        let grouped = self_.check(Token::OpenBrace);
        self_.parse_delimited(
          if grouped {
            BRACE_COMMA
          } else {
            Delimiters { open: None, close: None, separator: None }
          },
          |self_| {
            let target = if self_.eat(Token::Hash)? { &mut absolute } else { &mut relative };
            self_.parse_use_tree(target)
          },
        )
      },
    )?;
    let span = self.end_span(span);
    self.eat(Token::Semi)?;
    relative.retain(|_, tree| tree.prune());
    absolute.retain(|_, tree| tree.prune());
    Ok((span, ItemKind::Use(UseItem { relative, absolute })))
  }

  fn parse_use_tree(&mut self, map: &mut BTreeMap<Ident, UseTree>) -> Result<(), Diag> {
    let span = self.span();
    let name = self.parse_ident()?;
    let tree = map.entry(name.clone()).or_insert(UseTree::empty(span));
    if self.eat(Token::ColonColon)? {
      if self.check(Token::Ident) {
        self.parse_use_tree(&mut tree.children)?;
      } else {
        self.parse_delimited(BRACE_COMMA, |self_| {
          if self_.state.lexer.slice() == &*name.0 {
            self_.expect(Token::Ident)?;
            self_.parse_use_tree_alias(name.clone(), tree)?;
          } else {
            self_.parse_use_tree(&mut tree.children)?;
          }
          Ok(())
        })?;
      }
    } else {
      self.parse_use_tree_alias(name, tree)?;
    }
    Ok(())
  }

  fn parse_use_tree_alias(&mut self, name: Ident, tree: &mut UseTree) -> Result<(), Diag> {
    if self.eat(Token::As)? {
      if self.eat(Token::Hole)? {
        tree.implicit = true;
      } else {
        let alias = self.parse_ident()?;
        tree.aliases.push(alias);
      }
    } else {
      tree.aliases.push(name);
    }
    Ok(())
  }
}

impl<'src> Formatter<'src> {
  pub(crate) fn fmt_use_item(&self, u: &UseItem) -> Doc<'src> {
    let mut trees = Vec::new();
    for (name, tree) in &u.absolute {
      trees.push(Doc::concat([Doc("#"), self.fmt_use_tree(name.clone(), tree)]));
    }
    for (name, tree) in &u.relative {
      trees.push(self.fmt_use_tree(name.clone(), tree));
    }
    Doc::concat([
      Doc("use "),
      if trees.len() == 1 { trees.pop().unwrap() } else { Doc::brace_comma(trees.into_iter()) },
      Doc(";"),
    ])
  }

  pub(crate) fn fmt_use_tree(&self, name: Ident, tree: &UseTree) -> Doc<'src> {
    let aliases =
      tree.implicit.then(|| Doc::concat([Doc(name.clone()), Doc(" as _")])).into_iter().chain(
        tree.aliases.iter().map(|alias| {
          if alias == &name {
            Doc(alias.clone())
          } else {
            Doc::concat([Doc(name.clone()), Doc(" as "), Doc(alias.clone())])
          }
        }),
      );
    let aliases_len = tree.implicit as usize + tree.aliases.len();
    let mut children =
      tree.children.iter().map(|(name, child)| self.fmt_use_tree(name.clone(), child));
    if aliases_len == 1 && children.len() == 0 {
      Doc::concat(aliases)
    } else {
      Doc::concat([
        Doc(name.clone()),
        Doc("::"),
        if aliases_len == 0 && children.len() == 1 {
          children.next().unwrap()
        } else {
          Doc::brace_comma(aliases.chain(children).collect::<Vec<_>>().into_iter())
        },
      ])
    }
  }
}

impl Charter<'_> {
  pub(crate) fn chart_use(&mut self, parent: DefId, vis: VisId, use_item: UseItem) {
    for (ident, use_tree) in use_item.absolute {
      self.chart_use_tree(parent, vis, ImportParent::Absolute, ident, use_tree);
    }
    for (ident, use_tree) in use_item.relative {
      self.chart_use_tree(parent, vis, ImportParent::Local, ident, use_tree);
    }
  }

  pub(crate) fn chart_use_tree(
    &mut self,
    def_id: DefId,
    vis: VisId,
    parent: ImportParent,
    ident: Ident,
    use_tree: UseTree,
  ) {
    let span = use_tree.span;
    let import = self.chart.imports.push(ImportDef { span, def: def_id, parent, ident });
    let def = &mut self.chart.defs[def_id];
    let member = Binding { span, vis, kind: MemberKind::Import(import) };
    if !use_tree.aliases.is_empty() {
      def.named_members.push(member);
    }
    for name in use_tree.aliases {
      if let Entry::Vacant(e) = def.members_lookup.entry(name.clone()) {
        e.insert(member);
      } else {
        self.diags.report(Diag::DuplicateItem { span, name });
      }
    }
    if use_tree.implicit {
      def.implicit_members.push(member);
    }
    for (ident, child) in use_tree.children {
      self.chart_use_tree(def_id, vis, ImportParent::Import(import), ident, child);
    }
  }
}

impl Resolver<'_> {
  pub(crate) fn resolve_import(&mut self, import_id: ImportId) -> Result<DefId, ErrorGuaranteed> {
    let import = &self.chart.imports[import_id];
    let state = self.sigs.imports.get_or_extend_with(import_id, || ImportState::Unresolved);
    match state {
      ImportState::Resolved(resolved) => *resolved,
      ImportState::Resolving => Err(self.diags.report(Diag::CircularImport { span: import.span })),
      ImportState::Unresolved => {
        *state = ImportState::Resolving;
        let import = import.clone();
        let resolved = self._resolve_import(import);
        self.sigs.imports[import_id] = ImportState::Resolved(resolved);
        resolved
      }
    }
  }

  fn _resolve_import(&mut self, import: ImportDef) -> Result<DefId, ErrorGuaranteed> {
    let ImportDef { span, def, parent, ident, .. } = import;
    match parent {
      ImportParent::Absolute => self.resolve_absolute(span, ident),
      ImportParent::Local => self.resolve_local(span, def, ident),
      ImportParent::Import(parent) => {
        let parent = self.resolve_import(parent)?;
        self.resolve_segment(span, def, parent, ident)
      }
    }
    .map_err(|diag| self.diags.report(diag))
  }
}
