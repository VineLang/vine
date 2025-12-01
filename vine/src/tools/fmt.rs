pub mod doc;

use doc::{Doc, Writer};

use crate::{
  components::{loader::FileId, parser::VineParser},
  structures::{
    ast::{
      Expr, ExprKind, Flex, Impl, ImplKind, Item, ItemKind, Pat, PatKind, Span, Stmt, StmtKind,
      Trait, TraitKind, Ty, TyKind, Vis,
    },
    diag::Diag,
  },
};

pub struct Formatter<'src> {
  src: &'src str,
}

impl<'src> Formatter<'src> {
  pub fn fmt(src: &str) -> Result<String, Diag> {
    let ast = VineParser::parse(src, FileId(0))?;
    let fmt = Formatter { src };
    let doc = Doc::concat_vec(
      fmt.line_break_separated(
        Span { file: FileId(0), start: 0, end: src.len() },
        [(Span { file: FileId(0), start: 0, end: 0 }, Doc::EMPTY)]
          .into_iter()
          .chain(ast.iter().map(|x| (x.span, fmt.fmt_item(x)))),
      ),
    );
    let mut writer = Writer::default();
    writer.write_doc(&doc, false);
    Ok(writer.out)
  }

  pub(crate) fn fmt_item(&self, item: &Item) -> Doc<'src> {
    Doc::concat(
      [].into_iter()
        .chain(item.docs.iter().flat_map(|d| [Doc(d.clone()), Doc::LINE]))
        .chain(item.attrs.iter().flat_map(|x| [self.fmt_verbatim(x.span), Doc::LINE]))
        .chain([
          self.fmt_vis(&item.vis),
          match &item.kind {
            ItemKind::Fn(f) => self.fmt_fn_item(f),
            ItemKind::Const(c) => self.fmt_const_item(c),
            ItemKind::Struct(s) => self.fmt_struct_item(s),
            ItemKind::Enum(e) => self.fmt_enum_item(e),
            ItemKind::Type(t) => self.fmt_type_item(t),
            ItemKind::Mod(m) => self.fmt_mod_item(m),
            ItemKind::Trait(t) => self.fmt_trait_item(item.span, t),
            ItemKind::Impl(i) => self.fmt_impl_item(item.span, i),
            ItemKind::Use(u) => self.fmt_use_item(u),
            ItemKind::Taken => unreachable!(),
          },
        ]),
    )
  }

  pub(crate) fn fmt_vis(&self, vis: &Vis) -> Doc<'src> {
    Doc::concat([self._fmt_vis(vis), Doc(if !matches!(vis, Vis::Private) { " " } else { "" })])
  }

  pub(crate) fn _fmt_vis(&self, vis: &Vis) -> Doc<'src> {
    match vis {
      Vis::Private => Doc::EMPTY,
      Vis::Public => Doc("pub"),
      Vis::PublicTo(_, name) => Doc::concat([Doc("pub."), Doc(name.clone())]),
    }
  }

  pub(crate) fn fmt_block_like(
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

  pub(crate) fn fmt_stmt(&self, stmt: &Stmt) -> Doc<'src> {
    match &stmt.kind {
      StmtKind::Assert(a) => self.fmt_stmt_assert(a),
      StmtKind::Let(l) => self.fmt_stmt_let(l),
      StmtKind::LetFn(d) => self.fmt_stmt_let_fn(d),
      StmtKind::Expr(expr, false) => self.fmt_expr(expr),
      StmtKind::Expr(expr, true) => Doc::concat([self.fmt_expr(expr), Doc(";")]),
      StmtKind::Item(item) => self.fmt_item(item),
      StmtKind::Return(expr) => self.fmt_stmt_return(expr),
      StmtKind::Break(label, expr) => self.fmt_stmt_break(label.clone(), expr),
      StmtKind::Continue(label) => self.fmt_stmt_continue(label.clone()),
      StmtKind::Empty => Doc::EMPTY,
    }
  }

  pub(crate) fn fmt_flex(&self, flex: Flex) -> Doc<'src> {
    Doc(flex.sigil())
  }

  pub(crate) fn fmt_impl(&self, impl_: &Impl) -> Doc<'src> {
    match &*impl_.kind {
      ImplKind::Error(_) => unreachable!(),
      ImplKind::Hole => Doc("_"),
      ImplKind::Path(path) => self.fmt_path(path),
      ImplKind::Fn(path) => Doc::concat([Doc("fn "), self.fmt_path(path)]),
    }
  }

  pub(crate) fn fmt_trait(&self, trait_: &Trait) -> Doc<'src> {
    match &*trait_.kind {
      TraitKind::Error(_) => unreachable!(),
      TraitKind::Path(path) => self.fmt_path(path),
      TraitKind::Fn(receiver, args, ret) => Doc::concat([
        Doc("fn "),
        self.fmt_ty(receiver),
        Doc::paren_comma(args.iter().map(|a| self.fmt_ty(a))),
        match ret {
          Some(ty) => Doc::concat([Doc(" -> "), self.fmt_ty(ty)]),
          None => Doc(""),
        },
      ]),
    }
  }

  pub(crate) fn fmt_expr(&self, expr: &Expr) -> Doc<'src> {
    match &*expr.kind {
      ExprKind::Error(_) => unreachable!(),
      ExprKind::Paren(p) => Doc::paren(self.fmt_expr(p)),
      ExprKind::Hole => Doc("_"),
      ExprKind::Path(path, args) => self.fmt_expr_path(path, args),
      ExprKind::Do(label, ty, body) => self.fmt_expr_do(label.clone(), ty, body),
      ExprKind::Assign(inverted, space, value) => self.fmt_expr_assign(*inverted, space, value),
      ExprKind::Match(expr, ty, arms) => self.fmt_expr_match(expr, ty, arms),
      ExprKind::If(cond, ty, then, else_) => self.fmt_expr_if(cond, ty, then, else_),
      ExprKind::When(label, ty, arms, leg) => self.fmt_expr_when(label.clone(), ty, arms, leg),
      ExprKind::While(label, cond, ty, body, else_) => {
        self.fmt_expr_while(label.clone(), cond, ty, body, else_)
      }
      ExprKind::Loop(label, ty, body) => self.fmt_expr_loop(label.clone(), ty, body),
      ExprKind::For(label, pat, expr, ty, block, else_) => {
        self.fmt_expr_for(label.clone(), pat, expr, ty, block, else_)
      }
      ExprKind::Fn(flex, params, ty, body) => self.fmt_expr_fn(flex, params, ty, body),
      ExprKind::Ref(expr, postfix) => self.fmt_expr_ref(expr, *postfix),
      ExprKind::Deref(expr, postfix) => self.fmt_expr_deref(expr, *postfix),
      ExprKind::Inverse(expr, postfix) => self.fmt_expr_inverse(expr, *postfix),
      ExprKind::Cast(expr, ty, postfix) => self.fmt_expr_cast(expr, ty, *postfix),
      ExprKind::Unwrap(expr) => self.fmt_expr_unwrap(expr),
      ExprKind::Try(expr) => self.fmt_expr_try(expr),
      ExprKind::RangeExclusive(start, end) => self.fmt_expr_range_exclusive(start, end),
      ExprKind::RangeInclusive(start, end) => self.fmt_expr_range_inclusive(start, end),
      ExprKind::Place(value, space) => self.fmt_expr_place(value, space),
      ExprKind::Tuple(elements) => self.fmt_expr_tuple(elements),
      ExprKind::Object(entries) => self.fmt_expr_object(entries),
      ExprKind::List(elements) => self.fmt_expr_list(elements),
      ExprKind::TupleField(expr, index) => self.fmt_expr_tuple_field(expr, *index),
      ExprKind::ObjectField(expr, key) => self.fmt_expr_object_field(expr, key),
      ExprKind::Method(receiver, _, name, generics, args) => {
        self.fmt_expr_method(receiver, name, generics, args)
      }
      ExprKind::Call(func, args) => self.fmt_expr_call(func, args),
      ExprKind::Sign(sign, expr) => self.fmt_expr_sign(*sign, expr),
      ExprKind::Bool(bool) => self.fmt_expr_bool(*bool),
      ExprKind::Not(expr) => self.fmt_expr_not(expr),
      ExprKind::Is(expr, pat) => self.fmt_expr_is(expr, pat),
      ExprKind::LogicalOp(op, lhs, rhs) => self.fmt_expr_logical_op(op, lhs, rhs),
      ExprKind::ComparisonOp(init, cmps) => self.fmt_expr_comparison_op(init, cmps),
      ExprKind::BinaryOp(op, lhs, rhs) => self.fmt_expr_binary_op(op, lhs, rhs),
      ExprKind::BinaryOpAssign(op, lhs, rhs) => self.fmt_expr_binary_op_assign(op, lhs, rhs),
      ExprKind::N32(_) | ExprKind::F32(_) | ExprKind::Char(_) => self.fmt_verbatim(expr.span),
      ExprKind::Nat(span, _, ty) => self.fmt_expr_nat(*span, ty),
      ExprKind::String(init, rest) => self.fmt_expr_string(init, rest),
      ExprKind::InlineIvy(binds, ty, net_span, _) => self.fmt_expr_inline_ivy(binds, ty, net_span),
    }
  }

  pub(crate) fn fmt_pat(&self, pat: &Pat) -> Doc<'src> {
    match &*pat.kind {
      PatKind::Error(_) => unreachable!(),
      PatKind::Hole => Doc("_"),
      PatKind::Paren(pat) => Doc::paren(self.fmt_pat(pat)),
      PatKind::Path(path, args) => self.fmt_pat_path(path, args),
      PatKind::Ref(pat) => self.fmt_pat_ref(pat),
      PatKind::Deref(pat) => self.fmt_pat_deref(pat),
      PatKind::Inverse(pat) => self.fmt_pat_inverse(pat),
      PatKind::Annotation(pat, ty) => self.fmt_pat_annotation(pat, ty),
      PatKind::Object(entries) => self.fmt_pat_object(entries),
      PatKind::Tuple(elements) => self.fmt_pat_tuple(elements),
    }
  }

  pub(crate) fn fmt_ty(&self, ty: &Ty) -> Doc<'src> {
    match &*ty.kind {
      TyKind::Error(_) => unreachable!(),
      TyKind::Hole => Doc("_"),
      TyKind::Never => Doc("!"),
      TyKind::Paren(pat) => Doc::paren(self.fmt_ty(pat)),
      TyKind::Tuple(elements) => self.fmt_ty_tuple(elements),
      TyKind::Ref(ty) => self.fmt_ty_ref(ty),
      TyKind::Inverse(ty) => self.fmt_ty_inverse(ty),
      TyKind::Path(path) => self.fmt_path(path),
      TyKind::Fn(path) => Doc::concat([Doc("fn "), self.fmt_path(path)]),
      TyKind::Object(object) => self.fmt_ty_object(object),
    }
  }

  pub(crate) fn fmt_arrow_ty(&self, ty: &Option<Ty>) -> Doc<'src> {
    match ty {
      Some(ty) => Doc::concat([Doc(" -> "), self.fmt_ty(ty)]),
      None => Doc(""),
    }
  }

  pub(crate) fn fmt_verbatim(&self, span: Span) -> Doc<'src> {
    Doc(&self.src[span.start..span.end])
  }
}
