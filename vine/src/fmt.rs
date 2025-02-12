mod doc;

use doc::{Doc, Writer};

use crate::{
  ast::{
    Block, ComparisonOp, Expr, ExprKind, GenericArgs, GenericParams, Generics, Ident, Impl,
    ImplKind, Item, ItemKind, Label, LogicalOp, ModKind, Pat, PatKind, Path, Span, Stmt, StmtKind,
    Trait, TraitKind, Ty, TyKind, UseTree, Vis,
  },
  core::Core,
  diag::Diag,
  parser::VineParser,
};

impl<'core> Core<'core> {
  pub fn fmt(&'core self, src: &str) -> Result<String, Diag<'core>> {
    let ast = VineParser::parse(self, src, 0)?;
    let fmt = Formatter { src };
    let doc = Doc::concat([
      Doc::LINE,
      Doc::concat_vec(fmt.line_break_separated(
        Span { file: 0, start: 0, end: src.len() },
        ast.iter().map(|x| (x.span, fmt.fmt_item(x))),
      )),
    ]);
    let mut writer = Writer::default();
    writer.write_doc(&doc, false);
    Ok(writer.out)
  }
}

struct Formatter<'src> {
  src: &'src str,
}

impl<'core: 'src, 'src> Formatter<'src> {
  fn fmt_item(&self, item: &Item<'core>) -> Doc<'src> {
    Doc::concat(item.attrs.iter().flat_map(|x| [self.fmt_verbatim(x.span), Doc::LINE]).chain([
      match &item.vis {
        Vis::Private => Doc::EMPTY,
        Vis::Public => Doc("pub "),
        Vis::PublicTo(_, name) => Doc::concat([Doc("pub("), Doc(*name), Doc(") ")]),
      },
      match &item.kind {
        ItemKind::Fn(f) => {
          let params = &f.params;
          Doc::concat([
            Doc("fn "),
            Doc(if f.method { "." } else { "" }),
            Doc(f.name),
            self.fmt_generic_params(&f.generics),
            Doc::paren_comma(params.iter().map(|p| self.fmt_pat(p))),
            self.fmt_return_ty(f.ret.as_ref()),
            match &f.body {
              Some(b) => Doc::concat([Doc(" "), self.fmt_block(b, true)]),
              None => Doc(";"),
            },
          ])
        }
        ItemKind::Const(c) => Doc::concat([
          Doc("const "),
          Doc(c.name),
          self.fmt_generic_params(&c.generics),
          Doc(": "),
          self.fmt_ty(&c.ty),
          match &c.value {
            Some(v) => Doc::concat([Doc(" = "), self.fmt_expr(v)]),
            None => Doc(""),
          },
          Doc(";"),
        ]),
        ItemKind::Struct(s) => Doc::concat([
          Doc("struct "),
          Doc(s.name),
          self.fmt_generic_params(&s.generics),
          if s.object {
            let TyKind::Object(e) = &s.fields[0].kind else { unreachable!() };
            Doc::concat([
              Doc(" "),
              Doc::brace_comma_multiline(
                e.iter().map(|(k, v)| Doc::concat([Doc(k.ident), Doc(": "), self.fmt_ty(v)])),
              ),
            ])
          } else {
            Doc::concat([Doc::paren_comma(s.fields.iter().map(|x| self.fmt_ty(x))), Doc(";")])
          },
        ]),
        ItemKind::Enum(e) => Doc::concat([
          Doc("enum "),
          Doc(e.name),
          self.fmt_generic_params(&e.generics),
          Doc(" "),
          Doc::brace_comma_multiline(e.variants.iter().map(|v| {
            Doc::concat([
              Doc(v.name),
              if v.fields.is_empty() {
                Doc::EMPTY
              } else {
                Doc::paren_comma(v.fields.iter().map(|x| self.fmt_ty(x)))
              },
            ])
          })),
        ]),
        ItemKind::Type(t) => Doc::concat([
          Doc("type "),
          Doc(t.name),
          self.fmt_generic_params(&t.generics),
          Doc(" = "),
          self.fmt_ty(&t.ty),
          Doc(";"),
        ]),
        ItemKind::Mod(m) => Doc::concat([
          Doc("mod "),
          Doc(m.name),
          match &m.kind {
            ModKind::Loaded(span, items) => Doc::concat([
              Doc(" "),
              self.fmt_block_like(*span, items.iter().map(|x| (x.span, self.fmt_item(x)))),
            ]),
            ModKind::Unloaded(span, _) => {
              Doc::concat([Doc(" = "), self.fmt_verbatim(*span), Doc(";")])
            }
            ModKind::Error(_) => unreachable!(),
          },
        ]),
        ItemKind::Trait(t) => Doc::concat([
          Doc("trait "),
          Doc(t.name),
          self.fmt_generic_params(&t.generics),
          Doc(" "),
          self.fmt_block_like(item.span, t.items.iter().map(|i| (i.span, self.fmt_item(i)))),
        ]),
        ItemKind::Impl(i) => Doc::concat([
          Doc("impl "),
          Doc(i.name),
          self.fmt_generic_params(&i.generics),
          Doc(": "),
          self.fmt_trait(&i.trait_),
          Doc(" "),
          self.fmt_block_like(item.span, i.items.iter().map(|i| (i.span, self.fmt_item(i)))),
        ]),
        ItemKind::Use(u) => Doc::concat([
          Doc(if u.absolute { "use ::" } else { "use " }),
          Self::fmt_use_tree(None, &u.tree),
          Doc(";"),
        ]),
        ItemKind::Ivy(_) => return self.fmt_verbatim(item.span),
        ItemKind::Taken => unreachable!(),
      },
    ]))
  }

  fn fmt_block_like(
    &self,
    span: Span,
    els: impl ExactSizeIterator<Item = (Span, Doc<'src>)>,
  ) -> Doc<'src> {
    if els.len() == 0 {
      let mut inner = vec![];
      self.get_line_break(false, false, span.start, span.end, &mut inner);
      inner.pop();
      if inner.is_empty() {
        Doc("{}")
      } else {
        Doc::concat([Doc("{"), Doc::indent_vec(inner), Doc("}")])
      }
    } else {
      Doc::concat([Doc("{"), Doc::indent(self.line_break_separated(span, els)), Doc("}")])
    }
  }

  fn line_break_separated(
    &self,
    span: Span,
    els: impl Iterator<Item = (Span, Doc<'src>)>,
  ) -> Vec<Doc<'src>> {
    let mut docs = Vec::new();
    let mut cur = span.start;
    for (i, (span, doc)) in els.enumerate() {
      if i != 0 {
        docs.push(Doc::LINE);
      }
      self.get_line_break(i != 0, true, cur, span.start, &mut docs);
      docs.push(doc);
      cur = span.end;
    }
    docs.push(Doc::LINE);
    self.get_line_break(true, false, cur, span.end, &mut docs);
    docs.pop();
    docs
  }

  fn get_line_break(
    &self,
    mut start_blank: bool,
    end_blank: bool,
    start: usize,
    end: usize,
    docs: &mut Vec<Doc<'src>>,
  ) {
    let mut str = &self.src[start..end];
    let mut line_count = 0;
    while !str.is_empty() {
      if str.starts_with("//") {
        if line_count > 1 && start_blank {
          docs.push(Doc::LINE);
        }
        start_blank = true;
        let end = str.find('\n').unwrap_or(str.len());
        docs.push(Doc(&str[..end]));
        docs.push(Doc::LINE);
        str = &str[end..];
        line_count = 0;
      } else {
        if str.starts_with("\n") {
          line_count += 1;
        }
        str = &str[str.chars().next().unwrap().len_utf8()..];
      }
    }
    if line_count > 1 && start_blank && end_blank {
      docs.push(Doc::LINE);
    }
  }

  fn fmt_block(&self, block: &Block<'core>, force_open: bool) -> Doc<'src> {
    if !force_open {
      if let [stmt] = &*block.stmts {
        if matches!(stmt.kind, StmtKind::Expr(_, false)) {
          let str = " ";
          return Doc::concat([
            Doc("{"),
            Doc::group([Doc::if_single(str), self.fmt_stmt(stmt), Doc::if_single(" ")]),
            Doc("}"),
          ]);
        }
      }
    }
    self.fmt_block_like(block.span, block.stmts.iter().map(|x| (x.span, self.fmt_stmt(x))))
  }

  fn fmt_stmt(&self, stmt: &Stmt<'core>) -> Doc<'src> {
    match &stmt.kind {
      StmtKind::Let(l) => Doc::concat([
        Doc("let "),
        self.fmt_pat(&l.bind),
        match &l.init {
          Some(e) => Doc::concat([Doc(" = "), self.fmt_expr(e)]),
          None => Doc::EMPTY,
        },
        match &l.else_block {
          Some(b) => Doc::concat([Doc(" else "), self.fmt_block(b, false)]),
          None => Doc::EMPTY,
        },
        Doc(";"),
      ]),
      StmtKind::DynFn(d) => Doc::concat([
        Doc("dyn fn "),
        Doc(d.name),
        Doc::paren_comma(d.params.iter().map(|p| self.fmt_pat(p))),
        self.fmt_return_ty(d.ret.as_ref()),
        Doc(" "),
        self.fmt_block(&d.body, true),
      ]),
      StmtKind::Expr(expr, false) => self.fmt_expr(expr),
      StmtKind::Expr(expr, true) => Doc::concat([self.fmt_expr(expr), Doc(";")]),
      StmtKind::Item(item) => self.fmt_item(item),
      StmtKind::Empty => Doc::EMPTY,
    }
  }

  fn fmt_use_tree(name: Option<Ident<'core>>, tree: &UseTree<'core>) -> Doc<'src> {
    let prefix = name.iter().map(|&name| Doc::concat([Doc(name), Doc("::")]));
    let aliases = tree.aliases.iter().map(|&alias| {
      if Some(alias) == name {
        Doc(alias)
      } else {
        Doc::concat([Doc(name.unwrap()), Doc(" as "), Doc(alias)])
      }
    });
    let children = tree.children.iter().map(|(&name, child)| Self::fmt_use_tree(Some(name), child));
    let len = aliases.len() + children.len();
    if len == 1 {
      if aliases.len() == 1 {
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

  fn fmt_generic_params(&self, generics: &GenericParams<'core>) -> Doc<'src> {
    self.fmt_generics(
      generics,
      |i| Doc(*i),
      |(i, t)| match i {
        Some(i) => Doc::concat([Doc(*i), Doc(": "), self.fmt_trait(t)]),
        None => self.fmt_trait(t),
      },
    )
  }

  fn fmt_generic_args(&self, generics: &GenericArgs<'core>) -> Doc<'src> {
    self.fmt_generics(generics, |t| self.fmt_ty(t), |p| self.fmt_impl(p))
  }

  fn fmt_impl(&self, impl_: &Impl<'core>) -> Doc<'src> {
    match &impl_.kind {
      ImplKind::Error(_) | ImplKind::Param(_) | ImplKind::Def(..) => unreachable!(),
      ImplKind::Hole => Doc("_"),
      ImplKind::Path(path) => self.fmt_path(path),
    }
  }

  fn fmt_trait(&self, trait_: &Trait<'core>) -> Doc<'src> {
    match &trait_.kind {
      TraitKind::Error(_) | TraitKind::Def(..) => unreachable!(),
      TraitKind::Path(path) => self.fmt_path(path),
    }
  }

  fn fmt_generics<T, I>(
    &self,
    generics: &Generics<T, I>,
    fmt_t: impl Fn(&T) -> Doc<'src>,
    fmt_i: impl Fn(&I) -> Doc<'src>,
  ) -> Doc<'src> {
    if generics.impls.is_empty() && generics.types.is_empty() {
      Doc::EMPTY
    } else {
      let trailing = || Doc::if_multi(",");
      let sep = || Doc::concat([Doc(","), Doc::soft_line(" ")]);
      Doc::concat([
        Doc("["),
        if generics.types.is_empty() {
          Doc::EMPTY
        } else {
          Doc::group([Doc::interleave(generics.types.iter().map(fmt_t), sep()), trailing()])
        },
        if generics.impls.is_empty() {
          Doc::EMPTY
        } else {
          Doc::concat([
            Doc(";"),
            Doc::group([
              Doc::if_single(" "),
              Doc::interleave(generics.impls.iter().map(fmt_i), sep()),
              trailing(),
            ]),
          ])
        },
        Doc("]"),
      ])
    }
  }

  fn fmt_expr(&self, expr: &Expr<'core>) -> Doc<'src> {
    match &expr.kind {
      ExprKind![synthetic || resolved || error] => unreachable!(),
      ExprKind::Paren(p) => Doc::paren(self.fmt_expr(p)),
      ExprKind::Hole => Doc("_"),
      ExprKind::Path(path) => self.fmt_path(path),
      ExprKind::Do(label, block) => {
        Doc::concat([Doc("do"), self.fmt_label(label), Doc(" "), self.fmt_block(block, false)])
      }
      ExprKind::Assign(i, s, v) => {
        Doc::concat([self.fmt_expr(s), Doc(if *i { " ~= " } else { " = " }), self.fmt_expr(v)])
      }
      ExprKind::Match(expr, arms) => Doc::concat([
        Doc("match "),
        self.fmt_expr(expr),
        Doc(" "),
        Doc::brace_multiline(
          arms
            .iter()
            .map(|(p, e)| Doc::concat([self.fmt_pat(p), Doc(" "), self.fmt_block(e, false)])),
        ),
      ]),
      ExprKind::If(arms, leg) => Doc::interleave(
        arms
          .iter()
          .map(|(cond, block)| {
            Doc::concat([Doc("if "), self.fmt_expr(cond), Doc(" "), self.fmt_block(block, true)])
          })
          .chain(leg.iter().map(|block| self.fmt_block(block, true))),
        Doc(" else "),
      ),
      ExprKind::While(l, c, b) => Doc::concat([
        Doc("while"),
        self.fmt_label(l),
        Doc(" "),
        self.fmt_expr(c),
        Doc(" "),
        self.fmt_block(b, true),
      ]),
      ExprKind::Loop(l, b) => {
        Doc::concat([Doc("loop"), self.fmt_label(l), Doc(" "), self.fmt_block(b, true)])
      }
      ExprKind::Fn(p, _, b) => Doc::concat([
        Doc("fn"),
        Doc::paren_comma(p.iter().map(|p| self.fmt_pat(p))),
        Doc(" "),
        self.fmt_block(b, false),
      ]),
      ExprKind::Return(Some(x)) => Doc::concat([Doc("return "), self.fmt_expr(x)]),
      ExprKind::Break(label, Some(x)) => {
        Doc::concat([Doc("break"), self.fmt_label(label), Doc(" "), self.fmt_expr(x)])
      }
      ExprKind::Return(None) => Doc("return"),
      ExprKind::Break(label, None) => Doc::concat([Doc("break"), self.fmt_label(label)]),
      ExprKind::Continue(label) => Doc::concat([Doc("continue"), self.fmt_label(label)]),
      ExprKind::Ref(x, false) => Doc::concat([Doc("&"), self.fmt_expr(x)]),
      ExprKind::Deref(x, false) => Doc::concat([Doc("*"), self.fmt_expr(x)]),
      ExprKind::Move(x, false) => Doc::concat([Doc("move "), self.fmt_expr(x)]),
      ExprKind::Inverse(x, false) => Doc::concat([Doc("~"), self.fmt_expr(x)]),
      ExprKind::Ref(x, true) => Doc::concat([self.fmt_expr(x), Doc(".&")]),
      ExprKind::Deref(x, true) => Doc::concat([self.fmt_expr(x), Doc(".*")]),
      ExprKind::Move(x, true) => Doc::concat([self.fmt_expr(x), Doc(".move")]),
      ExprKind::Inverse(x, true) => Doc::concat([self.fmt_expr(x), Doc(".~")]),
      ExprKind::Place(v, s) => {
        Doc::concat([Doc("("), self.fmt_expr(v), Doc("; "), self.fmt_expr(s), Doc(")")])
      }
      ExprKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_expr(x))),
      ExprKind::Object(o) => Doc::brace_comma_space(o.iter().map(|(k, v)| {
        if let ExprKind::Path(path) = &v.kind {
          if let Some(i) = path.as_ident() {
            if k.ident == i {
              return Doc(k.ident);
            }
          }
        }
        Doc::concat([Doc(k.ident), Doc(": "), self.fmt_expr(v)])
      })),
      ExprKind::List(l) => Doc::bracket_comma(l.iter().map(|x| self.fmt_expr(x))),
      ExprKind::TupleField(e, i, _) => {
        Doc::concat([self.fmt_expr(e), Doc("."), Doc(format!("{i}"))])
      }
      ExprKind::ObjectField(e, k) => Doc::concat([self.fmt_expr(e), Doc("."), Doc(k.ident)]),
      ExprKind::Method(e, i, g, a) => Doc::concat([
        self.fmt_expr(e),
        Doc("."),
        Doc(*i),
        self.fmt_generic_args(g),
        Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x))),
      ]),
      ExprKind::Call(f, a) => {
        Doc::concat([self.fmt_expr(f), Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x)))])
      }
      ExprKind::Neg(x) => Doc::concat([Doc("-"), self.fmt_expr(x)]),
      ExprKind::Bool(b) => Doc(if *b { "true" } else { "false" }),
      ExprKind::Not(x) => Doc::concat([Doc("!"), self.fmt_expr(x)]),
      ExprKind::Is(e, p) => Doc::concat([self.fmt_expr(e), Doc(" is "), self.fmt_pat(p)]),
      ExprKind::LogicalOp(op, a, b) => Doc::concat([
        self.fmt_expr(a),
        Doc(match op {
          LogicalOp::And => " && ",
          LogicalOp::Or => " || ",
          LogicalOp::Implies => " => ",
        }),
        self.fmt_expr(b),
      ]),
      ExprKind::ComparisonOp(e, v) => {
        Doc::concat([self.fmt_expr(e)].into_iter().chain(v.iter().flat_map(|(op, x)| {
          [
            Doc(match op {
              ComparisonOp::Eq => " == ",
              ComparisonOp::Ne => " != ",
              ComparisonOp::Lt => " < ",
              ComparisonOp::Gt => " > ",
              ComparisonOp::Le => " <= ",
              ComparisonOp::Ge => " >= ",
            }),
            self.fmt_expr(x),
          ]
        })))
      }
      ExprKind::BinaryOp(op, a, b) => {
        Doc::concat([self.fmt_expr(a), Doc(" "), Doc(op.as_str()), Doc(" "), self.fmt_expr(b)])
      }
      ExprKind::BinaryOpAssign(op, a, b) => {
        Doc::concat([self.fmt_expr(a), Doc(" "), Doc(op.as_str()), Doc("= "), self.fmt_expr(b)])
      }
      ExprKind::N32(_) | ExprKind::F32(_) | ExprKind::Char(_) => self.fmt_verbatim(expr.span),
      ExprKind::String(init, rest) => {
        Doc::concat([self.fmt_verbatim(init.span)].into_iter().chain(rest.iter().flat_map(
          |(expr, seg)| [Doc::group([self.fmt_expr(expr)]), self.fmt_verbatim(seg.span)],
        )))
      }
    }
  }

  fn fmt_label(&self, label: &Label<'core>) -> Doc<'src> {
    let Label::Ident(label) = label else { unreachable!() };
    if let Some(label) = label {
      Doc::concat([Doc("."), Doc(*label)])
    } else {
      Doc("")
    }
  }

  fn fmt_pat(&self, pat: &Pat<'core>) -> Doc<'src> {
    match &pat.kind {
      PatKind::Local(_) | PatKind::Adt(..) | PatKind::Error(_) => unreachable!(),
      PatKind::Hole => Doc("_"),
      PatKind::Paren(p) => Doc::paren(self.fmt_pat(p)),
      PatKind::PathCall(p, None) => self.fmt_path(p),
      PatKind::PathCall(p, Some(x)) => {
        Doc::concat([self.fmt_path(p), Doc::paren_comma(x.iter().map(|x| self.fmt_pat(x)))])
      }
      PatKind::Ref(p) => Doc::concat([Doc("&"), self.fmt_pat(p)]),
      PatKind::Deref(p) => Doc::concat([Doc("*"), self.fmt_pat(p)]),
      PatKind::Inverse(p) => Doc::concat([Doc("~"), self.fmt_pat(p)]),
      PatKind::Annotation(p, t) => Doc::concat([self.fmt_pat(p), Doc(": "), self.fmt_ty(t)]),
      PatKind::Object(o) => Doc::brace_comma_space(o.iter().map(|(key, pat)| {
        let (pat, ty) = match &pat.kind {
          PatKind::Annotation(p, t) => (&**p, Some(t)),
          _ => (pat, None),
        };
        let pat = if let PatKind::PathCall(path, None) = &pat.kind {
          if let Some(i) = path.as_ident() {
            if key.ident == i {
              None
            } else {
              Some(pat)
            }
          } else {
            Some(pat)
          }
        } else {
          Some(pat)
        };
        match (pat, ty) {
          (None, None) => Doc(key.ident),
          (Some(pat), None) => Doc::concat([Doc(key.ident), Doc(": "), self.fmt_pat(pat)]),
          (None, Some(ty)) => Doc::concat([Doc(key.ident), Doc(":: "), self.fmt_ty(ty)]),
          (Some(pat), Some(ty)) => {
            Doc::concat([Doc(key.ident), Doc(": "), self.fmt_pat(pat), Doc(": "), self.fmt_ty(ty)])
          }
        }
      })),
      PatKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_pat(x))),
    }
  }

  fn fmt_ty(&self, ty: &Ty<'core>) -> Doc<'src> {
    match &ty.kind {
      TyKind::Hole => Doc("_"),
      TyKind::Paren(p) => Doc::paren(self.fmt_ty(p)),
      TyKind::Fn(a, r) => Doc::concat([
        Doc("fn"),
        Doc::paren_comma(a.iter().map(|x| self.fmt_ty(x))),
        self.fmt_return_ty(r.as_deref()),
      ]),
      TyKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_ty(x))),
      TyKind::Ref(t) => Doc::concat([Doc("&"), self.fmt_ty(t)]),
      TyKind::Inverse(t) => Doc::concat([Doc("~"), self.fmt_ty(t)]),
      TyKind::Path(p) => self.fmt_path(p),
      TyKind::Object(o) => Doc::brace_comma_space(
        o.iter().map(|(k, t)| Doc::concat([Doc(k.ident), Doc(": "), self.fmt_ty(t)])),
      ),
      TyKind::Param(_) | TyKind::Def(..) | TyKind::Error(_) => unreachable!(),
    }
  }

  fn fmt_path(&self, path: &Path<'core>) -> Doc<'src> {
    let mut docs = Vec::<Doc>::new();
    if path.absolute {
      docs.push(Doc("::"));
    }
    let mut first = true;
    for &seg in &path.segments {
      if !first {
        docs.push(Doc("::"));
      }
      docs.push(Doc(seg));
      first = false;
    }
    if let Some(generics) = &path.generics {
      docs.push(self.fmt_generic_args(generics));
    }
    Doc::concat_vec(docs)
  }

  fn fmt_return_ty(&self, r: Option<&Ty<'core>>) -> Doc<'src> {
    match r {
      Some(t) => Doc::concat([Doc(" -> "), self.fmt_ty(t)]),
      None => Doc::EMPTY,
    }
  }

  fn fmt_verbatim(&self, span: Span) -> Doc<'src> {
    Doc(&self.src[span.start..span.end])
  }
}
