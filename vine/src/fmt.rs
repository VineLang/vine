use vine_util::interner::StringInterner;

mod doc;
use doc::{Doc, Writer};

use crate::{
  ast::{
    Block, ComparisonOp, Expr, ExprKind, GenericPath, Ident, Item, ItemKind, LogicalOp, ModKind,
    Pat, PatKind, Path, Span, Stmt, StmtKind, Ty, TyKind, UseTree,
  },
  diag::Diag,
  parser::VineParser,
};

pub fn fmt(interner: &StringInterner<'static>, src: &str) -> Result<String, Diag> {
  let ast = VineParser::parse(interner, src, 0)?;
  let fmt = Formatter { src };
  let doc = Doc::concat([
    Doc::LINE,
    fmt.line_break_separated(
      Span { file: 0, start: 0, end: src.len() },
      ast.iter().map(|x| (x.span, fmt.fmt_item(x))),
    ),
  ]);
  let mut writer = Writer::default();
  writer.write_doc(&doc, false);
  Ok(writer.out)
}

struct Formatter<'src> {
  src: &'src str,
}

impl<'src> Formatter<'src> {
  fn fmt_item(&self, item: &Item) -> Doc<'src> {
    Doc::concat(item.attrs.iter().flat_map(|x| [self.fmt_verbatim(x.span), Doc::LINE]).chain([
      match &item.kind {
        ItemKind::Fn(f) => {
          let params = &f.params;
          Doc::concat([
            Doc("fn "),
            Doc(f.name),
            self.fmt_generics(&f.generics),
            self.fmt_params(params),
            self.fmt_return_ty(f.ret.as_ref()),
            Doc(" "),
            self.fmt_block(&f.body, true),
          ])
        }
        ItemKind::Const(c) => Doc::concat([
          Doc("const "),
          Doc(c.name),
          self.fmt_generics(&c.generics),
          Doc(": "),
          self.fmt_ty(&c.ty),
          Doc(" = "),
          self.fmt_expr(&c.value),
          Doc(";"),
        ]),
        ItemKind::Struct(s) => Doc::concat([
          Doc("struct "),
          Doc(s.name),
          self.fmt_generics(&s.generics),
          Doc::paren_comma(s.fields.iter().map(|x| self.fmt_ty(x))),
          Doc(";"),
        ]),
        ItemKind::Enum(e) => Doc::concat([
          Doc("enum "),
          Doc(e.name),
          self.fmt_generics(&e.generics),
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
          self.fmt_generics(&t.generics),
          Doc(" = "),
          self.fmt_ty(&t.ty),
          Doc(";"),
        ]),
        ItemKind::Mod(m) => Doc::concat([
          Doc("mod "),
          Doc(m.name),
          match &m.kind {
            ModKind::Loaded(items) => Doc::concat([
              Doc(" {"),
              Doc::indent([self.line_break_separated(
                item.span,
                items.iter().map(|x| (x.span, self.fmt_item(x))),
              )]),
              Doc("}"),
            ]),
            ModKind::Unloaded(span, _) => {
              Doc::concat([Doc(" = "), self.fmt_verbatim(*span), Doc(";")])
            }
            ModKind::Error(_) => unreachable!(),
          },
        ]),
        ItemKind::Use(u) => Doc::concat([
          Doc(if u.absolute { "use ::" } else { "use " }),
          self.fmt_use_tree(&u.tree),
          Doc(";"),
        ]),
        ItemKind::Ivy(_) => self.fmt_verbatim(item.span),
        ItemKind::Pattern(_) => todo!(),
        ItemKind::Taken => unreachable!(),
      },
    ]))
  }

  fn line_break_separated(
    &self,
    span: Span,
    els: impl Iterator<Item = (Span, Doc<'src>)>,
  ) -> Doc<'src> {
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
    self.get_line_break(true, false, cur, span.end, &mut docs);
    Doc::concat_vec(docs)
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

  fn fmt_block(&self, block: &Block, force_open: bool) -> Doc<'src> {
    if block.stmts.is_empty() {
      return Doc("{}");
    }
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
    Doc::concat([
      Doc("{"),
      Doc::indent([self
        .line_break_separated(block.span, block.stmts.iter().map(|x| (x.span, self.fmt_stmt(x))))]),
      Doc("}"),
    ])
  }

  fn fmt_stmt(&self, stmt: &Stmt) -> Doc<'src> {
    match &stmt.kind {
      StmtKind::Let(l) => Doc::concat([
        Doc("let "),
        self.fmt_pat(&l.bind),
        match &l.ty {
          Some(t) => Doc::concat([Doc(": "), self.fmt_ty(t)]),
          None => Doc::EMPTY,
        },
        match &l.init {
          Some(e) => Doc::concat([Doc(" = "), self.fmt_expr(e)]),
          None => Doc::EMPTY,
        },
        Doc(";"),
      ]),
      StmtKind::DynFn(d) => Doc::concat([
        Doc("dyn fn "),
        Doc(d.name),
        self.fmt_params(&d.params),
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

  fn fmt_use_tree(&self, use_tree: &UseTree) -> Doc<'src> {
    if let Some(children) = &use_tree.children {
      Doc::concat([
        self.fmt_path(&use_tree.path),
        Doc("::"),
        Doc::brace_comma(children.iter().map(|x| self.fmt_use_tree(x))),
      ])
    } else {
      self.fmt_path(&use_tree.path)
    }
  }

  fn fmt_generics(&self, generics: &[Ident]) -> Doc<'src> {
    if generics.is_empty() {
      Doc::EMPTY
    } else {
      Doc::bracket_comma(generics.iter().map(|x| Doc(*x)))
    }
  }

  fn fmt_expr(&self, expr: &Expr) -> Doc<'src> {
    match &expr.kind {
      ExprKind![synthetic || error] | ExprKind::Local(_) | ExprKind::DynFn(_) => unreachable!(),
      ExprKind::Paren(p) => Doc::paren(self.fmt_expr(p)),
      ExprKind::Hole => Doc("_"),
      ExprKind::Path(path) => self.fmt_generic_path(path),
      ExprKind::Block(block) => self.fmt_block(block, false),
      ExprKind::Assign(s, v) => Doc::concat([self.fmt_expr(s), Doc(" = "), self.fmt_expr(v)]),
      ExprKind::Match(expr, arms) => Doc::concat([
        Doc("match "),
        self.fmt_expr(expr),
        Doc(" "),
        Doc::brace_comma_multiline(
          arms.iter().map(|(p, e)| Doc::concat([self.fmt_pat(p), Doc(" => "), self.fmt_expr(e)])),
        ),
      ]),
      ExprKind::If(c, t, e) => Doc::concat([
        Doc("if "),
        self.fmt_expr(c),
        Doc(" "),
        self.fmt_block(t, true),
        match &e.kind {
          ExprKind::Block(b) if b.stmts.is_empty() => Doc::EMPTY,
          ExprKind::Block(b) => Doc::concat([Doc(" else "), self.fmt_block(b, true)]),
          _ => Doc::concat([Doc(" else "), self.fmt_expr(e)]),
        },
      ]),
      ExprKind::While(c, b) => {
        Doc::concat([Doc("while "), self.fmt_expr(c), Doc(" "), self.fmt_block(b, true)])
      }
      ExprKind::Loop(b) => Doc::concat([Doc("loop "), self.fmt_block(b, true)]),
      ExprKind::For(p, e, b) => Doc::concat([
        Doc("for "),
        self.fmt_pat(p),
        Doc(" in "),
        self.fmt_expr(e),
        Doc(" "),
        self.fmt_block(b, true),
      ]),
      ExprKind::Fn(p, _, b) => {
        Doc::concat([Doc("fn"), self.fmt_params(p), Doc(" "), self.fmt_expr(b)])
      }
      ExprKind::Return(Some(x)) => Doc::concat([Doc("return "), self.fmt_expr(x)]),
      ExprKind::Break(Some(x)) => Doc::concat([Doc("break "), self.fmt_expr(x)]),
      ExprKind::Return(None) => Doc("return"),
      ExprKind::Break(None) => Doc("break"),
      ExprKind::Continue => Doc("continue"),
      ExprKind::Ref(x) => Doc::concat([Doc("&"), self.fmt_expr(x)]),
      ExprKind::Deref(x) => Doc::concat([Doc("*"), self.fmt_expr(x)]),
      ExprKind::Move(x) => Doc::concat([Doc("move "), self.fmt_expr(x)]),
      ExprKind::Inverse(x) => Doc::concat([Doc("~"), self.fmt_expr(x)]),
      ExprKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_expr(x))),
      ExprKind::List(l) => Doc::bracket_comma(l.iter().map(|x| self.fmt_expr(x))),
      ExprKind::Field(e, p) => Doc::concat([self.fmt_expr(e), Doc("."), self.fmt_generic_path(p)]),
      ExprKind::Method(e, p, a) => Doc::concat([
        self.fmt_expr(e),
        Doc("."),
        self.fmt_generic_path(p),
        Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x))),
      ]),
      ExprKind::Call(f, a) => {
        Doc::concat([self.fmt_expr(f), Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x)))])
      }
      ExprKind::Neg(x) => Doc::concat([Doc("-"), self.fmt_expr(x)]),
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
      ExprKind::U32(_) | ExprKind::F32(_) | ExprKind::String(_) => self.fmt_verbatim(expr.span),
    }
  }

  fn fmt_pat(&self, pat: &Pat) -> Doc<'src> {
    match &pat.kind {
      PatKind::Local(_) | PatKind::Error(_) => unreachable!(),
      PatKind::Hole => Doc("_"),
      PatKind::Paren(p) => Doc::paren(self.fmt_pat(p)),
      PatKind::Adt(p, None) => self.fmt_generic_path(p),
      PatKind::Adt(p, Some(x)) => {
        Doc::concat([self.fmt_generic_path(p), Doc::paren_comma(x.iter().map(|x| self.fmt_pat(x)))])
      }
      PatKind::Ref(p) => Doc::concat([Doc("&"), self.fmt_pat(p)]),
      PatKind::Deref(p) => Doc::concat([Doc("*"), self.fmt_pat(p)]),
      PatKind::Move(p) => Doc::concat([Doc("move "), self.fmt_pat(p)]),
      PatKind::Inverse(p) => Doc::concat([Doc("~"), self.fmt_pat(p)]),
      PatKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_pat(x))),
    }
  }

  fn fmt_ty(&self, ty: &Ty) -> Doc<'src> {
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
      TyKind::Path(p) => self.fmt_generic_path(p),
      TyKind::Generic(_) | TyKind::Error(_) => unreachable!(),
    }
  }

  fn fmt_generic_path(&self, path: &GenericPath) -> Doc<'src> {
    if let Some(gens) = &path.generics {
      Doc::concat([
        self.fmt_path(&path.path),
        Doc::bracket_comma(gens.iter().map(|x| self.fmt_ty(x))),
      ])
    } else {
      self.fmt_path(&path.path)
    }
  }

  fn fmt_path(&self, path: &Path) -> Doc<'src> {
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
    Doc::concat_vec(docs)
  }

  fn fmt_params(&self, params: &[(Pat, Option<Ty>)]) -> Doc<'src> {
    Doc::paren_comma(params.iter().map(|(p, t)| match t {
      Some(t) => Doc::concat([self.fmt_pat(p), Doc(": "), self.fmt_ty(t)]),
      None => self.fmt_pat(p),
    }))
  }

  fn fmt_return_ty(&self, r: Option<&Ty>) -> Doc<'src> {
    match r {
      Some(t) => Doc::concat([Doc(" -> "), self.fmt_ty(t)]),
      None => Doc::EMPTY,
    }
  }

  fn fmt_verbatim(&self, span: Span) -> Doc<'src> {
    Doc(&self.src[span.start..span.end])
  }
}
