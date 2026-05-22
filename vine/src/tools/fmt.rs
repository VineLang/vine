use crate::{
  components::{loader::FileId, parser::Parser},
  structures::{
    ast::{
      BinaryOp, Expr, ExprKind, Flex, Impl, ImplKind, Item, ItemKind, LogicalOp, Pat, PatKind,
      Span, Stmt, StmtKind, Trait, TraitKind, Ty, TyKind, Vis,
    },
    content::{
      Blank, Color, Colored, Content, Delimited, Delims, Element, Indent, IntoElement, Keyword,
      Length, Line, Lines, Operator, Punct, Space, Surround, Writer,
    },
    diag::Diag,
  },
};

pub struct Formatter<'src> {
  src: &'src str,
}

pub const DEFAULT_MAX_WIDTH: u16 = 100;
impl<'src> Formatter<'src> {
  pub fn fmt(src: &str, max_width: u16) -> Result<String, Diag> {
    let mut output = String::new();
    Self::_fmt(src)?.format(&mut Writer::new(Length(max_width), &mut output), Surround::EMPTY);
    Ok(output)
  }

  pub fn _fmt(src: &str) -> Result<Content, Diag> {
    let ast = Parser::parse(FileId(0), src)?;
    let fmt = Formatter { src };
    Ok(Content::even(Lines::new(
      fmt.line_break_separated(
        Span { file: FileId(0), start: 0, end: src.len() },
        [(Span { file: FileId(0), start: 0, end: 0 }, Content::even(Blank))]
          .into_iter()
          .chain(ast.iter().map(|x| (x.span, fmt.fmt_item(x)))),
      ),
    )))
  }

  pub(crate) fn fmt_item(&self, item: &Item) -> Content {
    Content::even((
      Lines::new(item.docs.iter().map(|d| Colored(Color::COMMENT, d.clone()))),
      Lines::new(item.attrs.iter().map(|x| self.fmt_verbatim(Color::VAGUE, x.span))),
      self.fmt_vis(&item.vis),
      item.unsafe_.then_some((Keyword("unsafe"), Space)),
      match &item.kind {
        ItemKind::Fn(f) => self.fmt_fn_item(f),
        ItemKind::Const(c) => self.fmt_const_item(c),
        ItemKind::Struct(s) => self.fmt_struct_item(s),
        ItemKind::Enum(e) => self.fmt_enum_item(e),
        ItemKind::Type(t) => self.fmt_type_item(t),
        ItemKind::Mod(m) => self.fmt_mod_item(m),
        ItemKind::Trait(t) => self.fmt_trait_item(t),
        ItemKind::Impl(i) => self.fmt_impl_item(i),
        ItemKind::Use(u) => self.fmt_use_item(u),
        ItemKind::OuterMod => Content::even((Keyword("mod"), Punct(";"))),
        ItemKind::Taken => unreachable!(),
      },
    ))
  }

  pub(crate) fn fmt_vis(&self, vis: &Vis) -> Content {
    match vis {
      Vis::Private => Content::even(()),
      Vis::Public => Content::even((Keyword("pub"), Space)),
      Vis::PublicTo(_, name) => Content::even((Keyword("pub"), Punct("."), name.clone(), Space)),
    }
  }

  pub(crate) fn fmt_block_like(
    &self,
    span: Span,
    force_multi: bool,
    els: impl ExactSizeIterator<Item = (Span, Content)>,
  ) -> Content {
    let mut lines = self.line_break_separated(span, els);
    if lines.is_empty() {
      Content::even(Punct("{}"))
    } else if !force_multi && lines.len() == 1 {
      Content::even((Punct("{"), Space, Indent::eager(lines.pop().unwrap()), Space, Punct("}")))
    } else {
      Content::even((Punct("{"), Indent::always(Lines::new(lines)), Punct("}")))
    }
  }

  fn line_break_separated(
    &self,
    span: Span,
    els: impl Iterator<Item = (Span, Content)>,
  ) -> Vec<Box<dyn Element>> {
    let mut lines = Vec::new();
    let mut cur = span.start;
    for (i, (span, content)) in els.enumerate() {
      self.get_line_break(i != 0, true, cur, span.start, &mut lines);
      lines.push(content.into_element());
      cur = span.end;
    }
    self.get_line_break(true, false, cur, span.end, &mut lines);
    lines
  }

  fn get_line_break(
    &self,
    mut start_blank: bool,
    end_blank: bool,
    start: usize,
    end: usize,
    lines: &mut Vec<Box<dyn Element>>,
  ) {
    let mut str = &self.src[start..end];
    let mut line_count = 0;
    while !str.is_empty() {
      if str.starts_with("//") {
        if line_count > 1 && start_blank {
          lines.push(Blank.into_element());
        }
        start_blank = true;
        let end = str.find('\n').unwrap_or(str.len());
        lines.push(Colored(Color::COMMENT, str[..end].to_owned()).into_element());
        lines.push(Line.into_element());
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
      lines.push(Blank.into_element());
    }
  }

  pub(crate) fn fmt_stmt(&self, stmt: &Stmt) -> Content {
    match &stmt.kind {
      StmtKind::Assert(a) => self.fmt_stmt_assert(a),
      StmtKind::Let(l) => self.fmt_stmt_let(l),
      StmtKind::LetFn(d) => self.fmt_stmt_let_fn(d),
      StmtKind::Expr(expr, semi) => {
        Content::even((self._chain_expr(expr, true).finish(), semi.then_some(Punct(";"))))
      }
      StmtKind::Item(item) => self.fmt_item(item),
      StmtKind::Return(expr) => self.fmt_stmt_return(expr),
      StmtKind::Break(label, expr) => self.fmt_stmt_break(label.clone(), expr),
      StmtKind::Continue(label) => self.fmt_stmt_continue(label.clone()),
      StmtKind::Empty => Content::even(()),
    }
  }

  pub(crate) fn fmt_flex(&self, flex: Flex) -> Content {
    Content::even(Operator(flex.sigil()))
  }

  pub(crate) fn fmt_impl(&self, impl_: &Impl) -> Content {
    match &*impl_.kind {
      ImplKind::Error(_) => unreachable!(),
      ImplKind::Hole => Content::even(Punct("_")),
      ImplKind::Safe(_, impl_) => Content::even((Keyword("safe"), Space, self.fmt_impl(impl_))),
      ImplKind::Path(path) => self.fmt_path(Color::NORMAL, path),
      ImplKind::Fn(path) => {
        Content::even((Keyword("fn"), Space, self.fmt_path(Color::SPECIAL, path)))
      }
    }
  }

  pub(crate) fn fmt_trait(&self, trait_: &Trait) -> Content {
    match &*trait_.kind {
      TraitKind::Error(_) => unreachable!(),
      TraitKind::Path(path) => self.fmt_path(Color::SPECIAL, path),
      TraitKind::Fn(receiver, args, ret) => Content::smart((
        Keyword("fn"),
        Space,
        self.fmt_ty(receiver),
        Delimited::new(Delims::PAREN_COMMA, args.iter().map(|a| self.fmt_ty(a)))
          .omit_unary_separators(true),
        self.fmt_arrow_ty(ret),
      )),
    }
  }

  pub(crate) fn fmt_expr(&self, expr: &Expr) -> Content {
    self.chain_expr(expr).finish()
  }

  pub(crate) fn chain_expr(&self, expr: &Expr) -> Chain {
    self._chain_expr(expr, false)
  }

  pub(crate) fn _chain_expr(&self, expr: &Expr, is_stmt: bool) -> Chain {
    match &*expr.kind {
      ExprKind::Error(_) => unreachable!(),
      ExprKind::Paren(p) => {
        Chain::new(Content::even((Punct("("), Indent::lazy(self.fmt_expr(p)), Punct(")"))))
      }
      ExprKind::Safe(_, expr_) => {
        Chain::new(Content::even((Keyword("safe"), Space, self.fmt_expr(expr_))))
      }
      ExprKind::Hole(ty) => Chain::new(self.fmt_expr_hole(ty)),
      ExprKind::Path(path, args) => Chain::new(self.fmt_expr_path(path, args)),
      ExprKind::Do(label, ty, body) => Chain::new(self.fmt_expr_do(label.clone(), ty, body)),
      ExprKind::Assign(inverted, space, value) => {
        Chain::new(self.fmt_expr_assign(*inverted, space, value))
      }
      ExprKind::Match(expr, ty, arms) => Chain::new(self.fmt_expr_match(expr, ty, arms)),
      ExprKind::If(cond, ty, then, else_) => {
        Chain::new(self.fmt_expr_if(cond, ty, then, else_, is_stmt))
      }
      ExprKind::IfConst(cond, then, else_) => {
        Chain::new(self.fmt_expr_if_const(cond, then, else_, is_stmt))
      }
      ExprKind::When(label, ty, arms, leg) => {
        Chain::new(self.fmt_expr_when(label.clone(), ty, arms, leg))
      }
      ExprKind::While(label, cond, ty, body, else_) => {
        Chain::new(self.fmt_expr_while(label.clone(), cond, ty, body, else_))
      }
      ExprKind::Loop(label, ty, body) => Chain::new(self.fmt_expr_loop(label.clone(), ty, body)),
      ExprKind::For(label, pat, expr, ty, block, else_) => {
        Chain::new(self.fmt_expr_for(label.clone(), pat, expr, ty, block, else_))
      }
      ExprKind::Fn(flex, params, ty, body) => Chain::new(self.fmt_expr_fn(flex, params, ty, body)),
      ExprKind::Bench(flex, params, ty, body) => {
        Chain::new(self.fmt_expr_bench(flex, params, ty, body))
      }
      ExprKind::Ref(expr, postfix) => self.fmt_expr_ref(expr, *postfix),
      ExprKind::Deref(expr, postfix) => self.fmt_expr_deref(expr, *postfix),
      ExprKind::Inverse(expr, postfix) => self.fmt_expr_inverse(expr, *postfix),
      ExprKind::Cast(expr, ty, postfix) => self.fmt_expr_cast(expr, ty, *postfix),
      ExprKind::Unwrap(expr) => self.fmt_expr_unwrap(expr),
      ExprKind::Try(expr) => self.fmt_expr_try(expr),
      ExprKind::Index(expr, index) => self.fmt_expr_index(expr, index),
      ExprKind::RangeExclusive(start, end) => Chain::new(self.fmt_expr_range_exclusive(start, end)),
      ExprKind::RangeInclusive(start, end) => Chain::new(self.fmt_expr_range_inclusive(start, end)),
      ExprKind::Place(value, space) => Chain::new(self.fmt_expr_place(value, space)),
      ExprKind::Tuple(elements) => Chain::new(self.fmt_expr_tuple(elements)),
      ExprKind::Object(entries) => Chain::new(self.fmt_expr_object(entries)),
      ExprKind::List(elements) => Chain::new(self.fmt_expr_list(elements)),
      ExprKind::TupleField(expr, index) => self.fmt_expr_tuple_field(expr, *index),
      ExprKind::ObjectField(expr, key) => self.fmt_expr_object_field(expr, key),
      ExprKind::Method(receiver, _, name, generics, args) => {
        self.fmt_expr_method(receiver, name, generics, args)
      }
      ExprKind::Call(func, args) => self.fmt_expr_call(func, args),
      ExprKind::Sign(sign, expr) => Chain::new(self.fmt_expr_sign(*sign, expr)),
      ExprKind::Bool(bool) => Chain::new(self.fmt_expr_bool(*bool)),
      ExprKind::Not(expr) => Chain::new(self.fmt_expr_not(expr)),
      ExprKind::Is(expr, pat) => self.fmt_expr_is(expr, pat),
      ExprKind::LogicalOp(op, lhs, rhs) => self.fmt_expr_logical_op(op, lhs, rhs),
      ExprKind::ComparisonOp(init, cmps) => self.fmt_expr_comparison_op(init, cmps),
      ExprKind::BinaryOp(op, lhs, rhs) => self.fmt_expr_binary_op(op, lhs, rhs),
      ExprKind::BinaryOpAssign(op, lhs, rhs) => {
        Chain::new(self.fmt_expr_binary_op_assign(op, lhs, rhs))
      }
      ExprKind::N32(_) | ExprKind::F32(_) => {
        Chain::new(self.fmt_verbatim(Color::KEYWORD, expr.span))
      }
      ExprKind::Char(_) => Chain::new(self.fmt_verbatim(Color::STRING, expr.span)),
      ExprKind::Nat(span, _, ty) => Chain::new(self.fmt_expr_nat(*span, ty)),
      ExprKind::Float(span, _, ty) => Chain::new(self.fmt_expr_float(*span, ty)),
      ExprKind::String(init, rest) => Chain::new(self.fmt_expr_string(init, rest)),
      ExprKind::InlineIvy(table, net) => Chain::new(self.fmt_expr_inline_ivy(table, net)),
    }
  }

  pub(crate) fn fmt_pat(&self, pat: &Pat) -> Content {
    match &*pat.kind {
      PatKind::Error(_) => unreachable!(),
      PatKind::Hole => Content::even(Punct("_")),
      PatKind::Paren(pat) => {
        Content::even((Punct("("), Indent::lazy(self.fmt_pat(pat)), Punct(")")))
      }
      PatKind::Safe(_, pat_) => Content::even((Keyword("safe"), Space, self.fmt_pat(pat_))),
      PatKind::Path(path, args) => self.fmt_pat_path(path, args),
      PatKind::Ref(pat) => self.fmt_pat_ref(pat),
      PatKind::Deref(pat) => self.fmt_pat_deref(pat),
      PatKind::Inverse(pat) => self.fmt_pat_inverse(pat),
      PatKind::Annotation(pat, ty) => self.fmt_pat_annotation(pat, ty),
      PatKind::Object(entries) => self.fmt_pat_object(entries),
      PatKind::Tuple(elements) => self.fmt_pat_tuple(elements),
    }
  }

  pub(crate) fn fmt_ty(&self, ty: &Ty) -> Content {
    match &*ty.kind {
      TyKind::Error(_) => unreachable!(),
      TyKind::Hole => Content::even(Punct("_")),
      TyKind::Never => Content::even(Punct("!")),
      TyKind::Paren(pat) => Content::even((Punct("("), Indent::lazy(self.fmt_ty(pat)), Punct(")"))),
      TyKind::Tuple(elements) => self.fmt_ty_tuple(elements),
      TyKind::Ref(ty) => self.fmt_ty_ref(ty),
      TyKind::Key(ident) => Content::even((Punct("."), ident.clone())),
      TyKind::Inverse(ty) => self.fmt_ty_inverse(ty),
      TyKind::Path(path) => self.fmt_path(Color::SPECIAL, path),
      TyKind::Fn(path) => {
        Content::even((Keyword("fn"), Space, self.fmt_path(Color::SPECIAL, path)))
      }
      TyKind::Object(object) => self.fmt_ty_object(object),
      TyKind::IfConst(cond, then, else_) => self.fmt_ty_if_const(cond, then, else_),
    }
  }

  pub(crate) fn fmt_arrow_ty(&self, ty: &Option<Ty>) -> Content {
    Content::even(ty.as_ref().map(|ty| (Space, Punct("->"), Space, Indent::eager(self.fmt_ty(ty)))))
  }

  pub(crate) fn fmt_verbatim(&self, color: Color, span: Span) -> Content {
    Content::even(Colored(color, self.src[span.start..span.end].to_owned()))
  }
}

pub struct Chain {
  head: Box<dyn Element>,
  chains: Vec<Box<dyn Element>>,
  kind: Option<ChainKind>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChainKind {
  Postfix,
  BinaryOp(BinaryOp),
  LogicalOp(LogicalOp),
  Comparison,
  As,
  Is,
}

impl Chain {
  pub fn new(head: impl IntoElement) -> Self {
    Chain { head: head.into_element(), chains: Vec::new(), kind: None }
  }

  pub fn chain(mut self, kind: ChainKind, chain: impl IntoElement) -> Self {
    if self.kind.is_some_and(|k| k != kind) {
      return Chain::new(self.finish()).chain(kind, chain);
    }
    self.kind = Some(kind);
    self.chains.push(chain.into_element());
    self
  }

  pub fn chains<C: IntoElement>(
    mut self,
    kind: ChainKind,
    chains: impl IntoIterator<Item = C>,
  ) -> Self {
    if self.kind.is_some_and(|k| k != kind) {
      return Chain::new(self.finish()).chains(kind, chains);
    }
    self.kind = Some(kind);
    self.chains.extend(chains.into_iter().map(|c| c.into_element()));
    self
  }

  pub fn finish(self) -> Content {
    if self.chains.is_empty() {
      Content::even(self.head)
    } else {
      let len = self.chains.len();
      Content::smart((
        self.head,
        Delimited::new(Delims::NONE, self.chains)
          .allow_final_multi(self.kind == Some(ChainKind::Postfix))
          .break_final(len == 1),
      ))
    }
  }
}
