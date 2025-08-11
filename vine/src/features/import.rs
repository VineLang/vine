use std::collections::hash_map::Entry;

use vine_util::parser::Parser;

use crate::{
  components::{
    charter::Charter,
    lexer::Token,
    parser::{VineParser, BRACE_COMMA},
    resolver::Resolver,
  },
  structures::{
    ast::{Ident, UseItem, UseTree},
    chart::{DefId, ImportDef, ImportId, ImportParent, MemberKind, WithVis},
    diag::{Diag, ErrorGuaranteed},
    signatures::ImportState,
  },
  tools::fmt::{doc::Doc, Formatter},
};

impl<'core> VineParser<'core, '_> {
  pub(crate) fn parse_use_item(&mut self) -> Result<UseItem<'core>, Diag<'core>> {
    self.expect(Token::Use)?;
    let absolute = self.eat(Token::ColonColon)?;
    let span = self.start_span();
    let mut tree = UseTree::empty(self.span());
    loop {
      self.parse_use_tree(None, &mut tree)?;
      if !self.eat(Token::Comma)? {
        break;
      }
    }
    tree.prune();
    tree.span = self.end_span(span);
    self.eat(Token::Semi)?;
    Ok(UseItem { absolute, tree })
  }

  fn parse_use_tree(
    &mut self,
    cur_name: Option<Ident<'core>>,
    tree: &mut UseTree<'core>,
  ) -> Result<(), Diag<'core>> {
    if self.check(Token::Ident) {
      let span = self.span();
      let ident = self.parse_ident()?;
      if cur_name == Some(ident) {
        if self.eat(Token::As)? {
          if self.eat(Token::Hole)? {
            tree.implicit = true;
          } else {
            let alias = self.parse_ident()?;
            tree.aliases.push(alias);
          }
        } else {
          tree.aliases.push(ident);
        }
      } else {
        let child = tree.children.entry(ident).or_insert(UseTree::empty(span));
        if self.eat(Token::ColonColon)? {
          let is_group = self.check(Token::OpenBrace);
          self.parse_use_tree(is_group.then_some(ident), child)?;
        } else if self.eat(Token::As)? {
          if self.eat(Token::Hole)? {
            child.implicit = true;
          } else {
            let alias = self.parse_ident()?;
            child.aliases.push(alias);
          }
        } else {
          child.aliases.push(ident);
        }
      }
    } else {
      self.parse_delimited(BRACE_COMMA, |self_| self_.parse_use_tree(cur_name, tree))?;
    }
    Ok(())
  }
}

impl<'core: 'src, 'src> Formatter<'src> {
  pub(crate) fn fmt_use_item(&self, u: &UseItem<'core>) -> Doc<'src> {
    Doc::concat([
      Doc(if u.absolute { "use ::" } else { "use " }),
      Self::fmt_use_tree(None, &u.tree),
      Doc(";"),
    ])
  }

  pub(crate) fn fmt_use_tree(name: Option<Ident<'core>>, tree: &UseTree<'core>) -> Doc<'src> {
    let prefix = name.iter().map(|&name| Doc::concat([Doc(name), Doc("::")]));
    let aliases =
      tree.implicit.then(|| Doc::concat([Doc(name.unwrap()), Doc(" as _")])).into_iter().chain(
        tree.aliases.iter().map(|&alias| {
          if Some(alias) == name {
            Doc(alias)
          } else {
            Doc::concat([Doc(name.unwrap()), Doc(" as "), Doc(alias)])
          }
        }),
      );
    let aliases_len = tree.implicit as usize + tree.aliases.len();
    let children = tree.children.iter().map(|(&name, child)| Self::fmt_use_tree(Some(name), child));
    let len = aliases_len + children.len();
    if len == 1 {
      if aliases_len == 1 {
        Doc::concat(aliases)
      } else {
        Doc::concat(prefix.chain(children))
      }
    } else {
      Doc::concat(
        prefix.chain([Doc::brace_comma(aliases.chain(children).collect::<Vec<_>>().into_iter())]),
      )
    }
  }
}

impl<'core> Charter<'core, '_> {
  pub(crate) fn chart_use(&mut self, parent: DefId, vis: DefId, use_item: UseItem<'core>) {
    let import_parent = if use_item.absolute { ImportParent::Root } else { ImportParent::Scope };
    for (ident, use_tree) in use_item.tree.children {
      self.chart_use_tree(parent, vis, import_parent, ident, use_tree);
    }
  }

  pub(crate) fn chart_use_tree(
    &mut self,
    def_id: DefId,
    vis: DefId,
    parent: ImportParent,
    ident: Ident<'core>,
    use_tree: UseTree<'core>,
  ) {
    let span = use_tree.span;
    let import = self.chart.imports.push(ImportDef { span, def: def_id, parent, ident });
    let def = &mut self.chart.defs[def_id];
    let member = WithVis { vis, kind: MemberKind::Import(import) };
    def.named_members.push(member);
    for name in use_tree.aliases {
      if let Entry::Vacant(e) = def.members_lookup.entry(name) {
        e.insert(member);
      } else {
        self.core.report(Diag::DuplicateItem { span, name });
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

impl<'core> Resolver<'core, '_> {
  pub(crate) fn resolve_import(&mut self, import_id: ImportId) -> Result<DefId, ErrorGuaranteed> {
    let import = &self.chart.imports[import_id];
    let state = self.sigs.imports.get_or_extend_with(import_id, || ImportState::Unresolved);
    match state {
      ImportState::Resolved(resolved) => *resolved,
      ImportState::Resolving => Err(self.core.report(Diag::CircularImport { span: import.span })),
      ImportState::Unresolved => {
        *state = ImportState::Resolving;
        let import = *import;
        let resolved = self._resolve_import(import);
        self.sigs.imports[import_id] = ImportState::Resolved(resolved);
        resolved
      }
    }
  }

  fn _resolve_import(&mut self, import: ImportDef<'core>) -> Result<DefId, ErrorGuaranteed> {
    let ImportDef { span, def, parent, ident, .. } = import;
    match parent {
      ImportParent::Root => self.resolve_segment(span, def, DefId::ROOT, ident),
      ImportParent::Scope => self.resolve_initial(span, def, ident),
      ImportParent::Import(parent) => {
        let parent = self.resolve_import(parent)?;
        self.resolve_segment(span, def, parent, ident)
      }
    }
    .map_err(|diag| self.core.report(diag))
  }
}
