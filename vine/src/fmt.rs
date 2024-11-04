use std::ops::Add;

use vine_util::interner::StringInterner;

use crate::{
  ast::{
    Block, ComparisonOp, Expr, ExprKind, GenericPath, Ident, Item, ItemKind, LogicalOp, ModKind,
    Pat, PatKind, Span, Stmt, StmtKind, Ty, TyKind,
  },
  diag::Diag,
  parser::VineParser,
};

pub fn fmt(interner: &StringInterner<'static>, src: &str) -> Result<String, Diag> {
  let ast = VineParser::parse(interner, src, 0)?;
  let mut fmt = Formatter { source: src };
  let doc = Doc::concat([
    Doc::LINE,
    Doc::interleave(ast.iter().map(|x| fmt.fmt_item(x)), Doc::concat([Doc::LINE, Doc::LINE])),
  ]);
  let mut writer = WriteDoc { out: String::new(), indent: 0, written: 0 };
  writer.write_doc(&doc, false);
  Ok(writer.out)
}

struct Formatter<'src> {
  source: &'src str,
}

impl<'src> Formatter<'src> {
  fn fmt_block(&mut self, block: &Block) -> Doc<'src> {
    if block.stmts.is_empty() {
      return Doc::from("{}");
    }
    if let [stmt] = &*block.stmts {
      if matches!(stmt.kind, StmtKind::Expr(_, false)) {
        return Doc::concat([
          Doc::from("{"),
          Doc::group([
            Doc::from(DocKind::IfSingle(" ")),
            self.fmt_stmt(stmt),
            Doc::from(DocKind::IfSingle(" ")),
          ]),
          Doc::from("}"),
        ]);
      }
    }
    Doc::concat([
      Doc::from("{"),
      Doc::indent([Doc::interleave(block.stmts.iter().map(|x| self.fmt_stmt(x)), Doc::LINE)]),
      Doc::from("}"),
    ])
  }

  fn fmt_stmt(&mut self, stmt: &Stmt) -> Doc<'src> {
    match &stmt.kind {
      StmtKind::Let(l) => Doc::concat([
        Doc::from("let "),
        self.fmt_pat(&l.bind),
        match &l.ty {
          Some(t) => Doc::concat([": ".into(), self.fmt_ty(t)]),
          None => Doc::EMPTY,
        },
        match &l.init {
          Some(e) => Doc::concat([" = ".into(), self.fmt_expr(e)]),
          None => Doc::EMPTY,
        },
        ";".into(),
      ]),
      StmtKind::Expr(expr, false) => self.fmt_expr(expr),
      StmtKind::Expr(expr, true) => Doc::concat([self.fmt_expr(expr), ";".into()]),
      StmtKind::Item(item) => self.fmt_item(item),
      StmtKind::Empty => Doc::EMPTY,
    }
  }

  fn fmt_item(&mut self, item: &Item) -> Doc<'src> {
    match &item.kind {
      ItemKind::Fn(f) => Doc::concat([
        "fn ".into(),
        f.name.into(),
        self.fmt_generics(&f.generics),
        Doc::paren_comma(f.params.iter().map(|(p, t)| match t {
          Some(t) => Doc::concat([self.fmt_pat(p), ": ".into(), self.fmt_ty(t)]),
          None => self.fmt_pat(p),
        })),
        self.fmt_return_ty(f.ret.as_ref()),
        " ".into(),
        self.fmt_expr(&f.body),
      ]),
      ItemKind::Const(c) => Doc::concat([
        "const ".into(),
        c.name.into(),
        self.fmt_generics(&c.generics),
        ": ".into(),
        self.fmt_ty(&c.ty),
        " = ".into(),
        self.fmt_expr(&c.value),
        ";".into(),
      ]),
      ItemKind::Struct(s) => Doc::concat([
        "struct ".into(),
        s.name.into(),
        self.fmt_generics(&s.generics),
        Doc::paren_comma(s.fields.iter().map(|x| self.fmt_ty(x))),
        ";".into(),
      ]),
      ItemKind::Enum(e) => Doc::concat([
        "enum ".into(),
        e.name.into(),
        self.fmt_generics(&e.generics),
        "{".into(),
        Doc::indent([Doc::interleave(
          e.variants.iter().map(|v| {
            Doc::concat([
              v.name.into(),
              Doc::paren_comma(v.fields.iter().map(|x| self.fmt_ty(x))),
              Doc::from(","),
            ])
          }),
          Doc::LINE,
        )]),
        "}".into(),
      ]),
      ItemKind::Type(t) => Doc::concat([
        "type ".into(),
        t.name.into(),
        self.fmt_generics(&t.generics),
        " = ".into(),
        self.fmt_ty(&t.ty),
        ";".into(),
      ]),
      ItemKind::Mod(m) => Doc::concat([
        "mod ".into(),
        m.name.into(),
        match &m.kind {
          ModKind::Loaded(items) => Doc::concat([
            "{".into(),
            Doc::interleave(
              items.iter().map(|x| self.fmt_item(x)),
              Doc::concat([Doc::LINE, Doc::LINE]),
            ),
            "}".into(),
          ]),
          ModKind::Unloaded(span, _) => Doc::concat([" = ".into(), self.fmt_span(*span)]),
          ModKind::Error(_) => unreachable!(),
        },
      ]),
      ItemKind::Use(_) => self.fmt_span(item.span),
      ItemKind::Ivy(_) => self.fmt_span(item.span),
      ItemKind::Pattern(_) => todo!(),
      ItemKind::Taken => unreachable!(),
    }
  }

  fn fmt_return_ty(&mut self, r: Option<&Ty>) -> Doc<'src> {
    match r {
      Some(t) => Doc::concat([" -> ".into(), self.fmt_ty(t)]),
      None => Doc::EMPTY,
    }
  }

  fn fmt_generics(&mut self, generics: &[Ident]) -> Doc<'src> {
    if generics.is_empty() {
      Doc::EMPTY
    } else {
      Doc::bracket_comma(generics.iter().map(|x| Doc::from(*x)))
    }
  }

  fn fmt_pat(&mut self, pat: &Pat) -> Doc<'src> {
    match &pat.kind {
      PatKind::Local(_) | PatKind::Error(_) => unreachable!(),
      PatKind::Hole => "_".into(),
      PatKind::Adt(p, None) => self.fmt_path(p),
      PatKind::Adt(p, Some(x)) => {
        Doc::concat([self.fmt_path(p), Doc::paren_comma(x.iter().map(|x| self.fmt_pat(x)))])
      }
      PatKind::Ref(p) => Doc::concat(["&".into(), self.fmt_pat(p)]),
      PatKind::Deref(p) => Doc::concat(["*".into(), self.fmt_pat(p)]),
      PatKind::Move(p) => Doc::concat(["move ".into(), self.fmt_pat(p)]),
      PatKind::Inverse(p) => Doc::concat(["~".into(), self.fmt_pat(p)]),
      PatKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_pat(x))),
    }
  }

  fn fmt_ty(&mut self, ty: &Ty) -> Doc<'src> {
    match &ty.kind {
      TyKind::Hole => "_".into(),
      TyKind::Fn(a, r) => Doc::concat([
        "fn".into(),
        Doc::paren_comma(a.iter().map(|x| self.fmt_ty(x))),
        self.fmt_return_ty(r.as_deref()),
      ]),
      TyKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_ty(x))),
      TyKind::Ref(t) => Doc::concat(["&".into(), self.fmt_ty(t)]),
      TyKind::Inverse(t) => Doc::concat(["~".into(), self.fmt_ty(t)]),
      TyKind::Path(p) => self.fmt_path(p),
      TyKind::Generic(_) | TyKind::Error(_) => unreachable!(),
    }
  }

  fn fmt_expr(&mut self, expr: &Expr) -> Doc<'src> {
    match &expr.kind {
      ExprKind![synthetic || error] | ExprKind::Local(_) => unreachable!(),
      ExprKind::Hole => "_".into(),
      ExprKind::Path(path) => self.fmt_path(path),
      ExprKind::Block(block) => self.fmt_block(block),
      ExprKind::Assign(s, v) => Doc::concat([self.fmt_expr(s), " = ".into(), self.fmt_expr(v)]),
      ExprKind::Match(expr, arms) => Doc::concat([
        "match ".into(),
        self.fmt_expr(expr),
        Doc::brace_comma(
          arms.iter().map(|(p, e)| Doc::concat([self.fmt_pat(p), " => ".into(), self.fmt_expr(e)])),
        ),
      ]),
      ExprKind::If(c, t, e) => Doc::concat([
        "if ".into(),
        self.fmt_expr(c),
        " ".into(),
        self.fmt_block(t),
        match &e.kind {
          ExprKind::Block(b) if b.stmts.is_empty() => Doc::EMPTY,
          _ => Doc::concat([" else ".into(), self.fmt_expr(e)]),
        },
      ]),
      ExprKind::While(c, b) => {
        Doc::concat(["while ".into(), self.fmt_expr(c), " ".into(), self.fmt_block(b)])
      }
      ExprKind::Loop(b) => Doc::concat(["loop ".into(), self.fmt_block(b)]),
      ExprKind::For(p, e, b) => Doc::concat([
        "for ".into(),
        self.fmt_pat(p),
        " in ".into(),
        self.fmt_expr(e),
        " ".into(),
        self.fmt_block(b),
      ]),
      ExprKind::Fn(p, _, b) => Doc::concat([
        "fn".into(),
        Doc::paren_comma(p.iter().map(|(p, t)| match t {
          Some(t) => Doc::concat([self.fmt_pat(p), ": ".into(), self.fmt_ty(t)]),
          None => self.fmt_pat(p),
        })),
        " ".into(),
        self.fmt_expr(b),
      ]),
      ExprKind::Return(Some(x)) => Doc::concat(["return ".into(), self.fmt_expr(x)]),
      ExprKind::Break(Some(x)) => Doc::concat([
        "break
"
        .into(),
        self.fmt_expr(x),
      ]),
      ExprKind::Return(None) => "return".into(),
      ExprKind::Break(None) => "break".into(),
      ExprKind::Continue => "continue".into(),
      ExprKind::Ref(x) => Doc::concat(["&".into(), self.fmt_expr(x)]),
      ExprKind::Deref(x) => Doc::concat(["*".into(), self.fmt_expr(x)]),
      ExprKind::Move(x) => Doc::concat(["move ".into(), self.fmt_expr(x)]),
      ExprKind::Inverse(x) => Doc::concat(["~".into(), self.fmt_expr(x)]),
      ExprKind::Tuple(t) => Doc::tuple(t.iter().map(|x| self.fmt_expr(x))),
      ExprKind::List(l) => Doc::bracket_comma(l.iter().map(|x| self.fmt_expr(x))),
      ExprKind::Field(e, p) => Doc::concat([self.fmt_expr(e), ".".into(), self.fmt_path(p)]),
      ExprKind::Method(e, p, a) => Doc::concat([
        self.fmt_expr(e),
        ".".into(),
        self.fmt_path(p),
        Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x))),
      ]),
      ExprKind::Call(f, a) => {
        Doc::concat([self.fmt_expr(f), Doc::paren_comma(a.iter().map(|x| self.fmt_expr(x)))])
      }
      ExprKind::Neg(x) => Doc::concat(["-".into(), self.fmt_expr(x)]),
      ExprKind::Not(x) => Doc::concat(["!".into(), self.fmt_expr(x)]),
      ExprKind::Is(e, p) => Doc::concat([self.fmt_expr(e), " is ".into(), self.fmt_pat(p)]),
      ExprKind::LogicalOp(op, a, b) => Doc::concat([
        self.fmt_expr(a),
        match op {
          LogicalOp::And => " && ",
          LogicalOp::Or => " || ",
          LogicalOp::Implies => " => ",
        }
        .into(),
        self.fmt_expr(b),
      ]),
      ExprKind::ComparisonOp(e, v) => {
        Doc::concat([self.fmt_expr(e)].into_iter().chain(v.iter().flat_map(|(op, x)| {
          [
            match op {
              ComparisonOp::Eq => " == ",
              ComparisonOp::Ne => " != ",
              ComparisonOp::Lt => " < ",
              ComparisonOp::Gt => " > ",
              ComparisonOp::Le => " <= ",
              ComparisonOp::Ge => " >= ",
            }
            .into(),
            self.fmt_expr(x),
          ]
        })))
      }
      ExprKind::BinaryOp(op, a, b) => Doc::concat([
        self.fmt_expr(a),
        " ".into(),
        op.as_str().into(),
        " ".into(),
        self.fmt_expr(b),
      ]),
      ExprKind::BinaryOpAssign(op, a, b) => Doc::concat([
        self.fmt_expr(a),
        " ".into(),
        op.as_str().into(),
        "= ".into(),
        self.fmt_expr(b),
      ]),
      ExprKind::U32(_) | ExprKind::F32(_) | ExprKind::String(_) => self.fmt_span(expr.span),
    }
  }

  fn fmt_span(&mut self, span: Span) -> Doc<'src> {
    self.source[span.start..span.end].into()
  }

  fn fmt_path(&mut self, path: &GenericPath) -> Doc<'src> {
    let mut docs = Vec::<Doc>::new();
    if path.path.absolute {
      docs.push("::".into());
    }
    let mut first = true;
    for &seg in &path.path.segments {
      if !first {
        docs.push("::".into());
      }
      docs.push(seg.into());
      first = false;
    }
    if let Some(gens) = &path.generics {
      docs.push(Doc::bracket_comma(gens.iter().map(|x| self.fmt_ty(x))));
    }
    Doc::from(DocKind::Concat(docs.into_boxed_slice()))
  }
}

#[derive(Debug)]
struct Doc<'src> {
  measure: Measure,
  kind: DocKind<'src>,
}

impl<'src> Doc<'src> {
  const EMPTY: Self = Doc { measure: Measure::EMPTY, kind: DocKind::String("") };
  const LINE: Self = Doc { measure: Measure::LINE, kind: DocKind::Line };

  fn collect(docs: impl IntoIterator<Item = Self>) -> Box<[Self]> {
    Vec::into_boxed_slice(docs.into_iter().collect())
  }

  fn concat(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::from(DocKind::Concat(Self::collect(docs)))
  }

  fn indent(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::from(DocKind::Indent(Self::collect(docs)))
  }

  fn group(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::from(DocKind::Group(Self::collect(docs)))
  }

  fn lazy_group(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::from(DocKind::LazyGroup(Self::collect(docs)))
  }

  fn interleave(docs: impl IntoIterator<Item = Self>, sep: Self) -> Self {
    Doc::from(DocKind::Interleave(Self::collect(docs), Box::new(sep)))
  }

  fn paren_comma(mut docs: impl ExactSizeIterator<Item = Self>) -> Self {
    Doc::concat([
      Doc::from("("),
      if docs.len() == 1 {
        Doc::lazy_group([docs.next().unwrap(), Doc::from(DocKind::IfMulti(","))])
      } else {
        Doc::group([
          Doc::interleave(docs, Doc::concat([Doc::from(","), Doc::from(DocKind::SoftLine(" "))])),
          Doc::from(DocKind::IfMulti(",")),
        ])
      },
      Doc::from(")"),
    ])
  }

  fn tuple(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::concat([
      Doc::from("("),
      Doc::group([
        Doc::from(DocKind::Interleave(
          docs.into_iter().collect::<Vec<_>>().into_boxed_slice(),
          Box::new(Doc::concat([Doc::from(","), Doc::from(DocKind::SoftLine(" "))])),
        )),
        Doc::from(DocKind::String(",")),
      ]),
      Doc::from(")"),
    ])
  }

  fn bracket_comma(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::concat([
      Doc::from("["),
      Doc::group([
        Doc::interleave(docs, Doc::concat([Doc::from(","), Doc::from(DocKind::SoftLine(" "))])),
        Doc::from(DocKind::IfMulti(",")),
      ]),
      Doc::from("]"),
    ])
  }

  fn brace_comma(docs: impl IntoIterator<Item = Self>) -> Self {
    Doc::concat([
      Doc::from("{"),
      Doc::group([
        Doc::interleave(docs, Doc::concat([Doc::from(","), Doc::from(DocKind::SoftLine(" "))])),
        Doc::from(DocKind::IfMulti(",")),
      ]),
      Doc::from("}"),
    ])
  }
}

impl<'src> From<DocKind<'src>> for Doc<'src> {
  fn from(kind: DocKind<'src>) -> Self {
    Doc { measure: kind.measure(), kind }
  }
}

#[derive(Debug, Clone, Copy)]
struct Measure {
  multi: bool,
  width: usize,
  leading: usize,
}

impl Measure {
  const EMPTY: Self = Measure { multi: false, width: 0, leading: 0 };
  const LINE: Self = Measure { multi: true, width: 0, leading: 0 };

  fn string(str: &str) -> Self {
    let mut multi = false;
    let mut leading = 0;
    let mut width = 0;
    for char in str.chars() {
      if char == '\n' {
        if !multi {
          leading = width;
        }
        multi = true;
      } else {
        width += 1;
      }
    }
    Measure { multi, leading, width }
  }
}

impl Add<Measure> for Measure {
  type Output = Measure;

  fn add(self, rhs: Measure) -> Self::Output {
    Measure {
      multi: self.multi || rhs.multi,
      width: if self.multi { self.width } else { self.width + rhs.width },
      leading: if self.leading < self.width { self.leading } else { self.leading + rhs.leading },
    }
  }
}

impl<'src> DocKind<'src> {
  fn measure(&self) -> Measure {
    match self {
      DocKind::Line | DocKind::Indent(_) => Measure::LINE,
      DocKind::String(s) | DocKind::SoftLine(s) | DocKind::IfSingle(s) => Measure::string(s),
      DocKind::IfMulti(_) => Measure::EMPTY,
      DocKind::Concat(x) | DocKind::Group(x) | DocKind::LazyGroup(x) => {
        x.iter().fold(Measure::EMPTY, |a, b| a + b.measure)
      }
      DocKind::Interleave(x, sep) => x.iter().enumerate().fold(Measure::EMPTY, |m, (i, d)| {
        m + d.measure + if i != x.len() - 1 { sep.measure } else { Measure::EMPTY }
      }),
    }
  }
}

#[derive(Debug)]
enum DocKind<'src> {
  Line,
  String(&'src str),
  Concat(Box<[Doc<'src>]>),
  Group(Box<[Doc<'src>]>),
  LazyGroup(Box<[Doc<'src>]>),
  Indent(Box<[Doc<'src>]>),
  SoftLine(&'src str),
  IfMulti(&'src str),
  IfSingle(&'src str),
  Interleave(Box<[Doc<'src>]>, Box<Doc<'src>>),
}

struct WriteDoc {
  out: String,
  indent: usize,
  written: usize,
}

impl WriteDoc {
  fn write_doc(&mut self, doc: &Doc, multi: bool) {
    match &doc.kind {
      DocKind::Line => self.newline(),
      DocKind::String(str) => self.write_str(&str),
      DocKind::Concat(docs) => {
        for doc in docs.iter() {
          self.write_doc(doc, multi);
        }
      }
      DocKind::Group(docs) => {
        let multi = doc.measure.multi || doc.measure.width >= self.remaining();
        if multi {
          self.indent();
        }
        for doc in docs.iter() {
          self.write_doc(doc, multi);
        }
        if multi {
          self.outdent();
        }
      }
      DocKind::LazyGroup(docs) => {
        let multi = doc.measure.leading >= self.remaining();
        if multi {
          self.indent();
        }
        for doc in docs.iter() {
          self.write_doc(doc, multi);
        }
        if multi {
          self.outdent();
        }
      }
      DocKind::Indent(docs) => {
        self.indent();
        for doc in docs.iter() {
          self.write_doc(doc, multi);
        }
        self.outdent();
      }
      DocKind::SoftLine(s) => {
        if multi {
          self.newline()
        } else {
          self.write_str(s)
        }
      }
      DocKind::IfMulti(s) => {
        if multi {
          self.write_str(s)
        }
      }
      DocKind::IfSingle(s) => {
        if !multi {
          self.write_str(s)
        }
      }
      DocKind::Interleave(docs, sep) => {
        let n = docs.len();
        for (i, doc) in docs.iter().enumerate() {
          self.write_doc(doc, multi);
          if i != n - 1 {
            self.write_doc(&sep, multi);
          }
        }
      }
    }
  }

  fn indent(&mut self) {
    self.indent += 2;
    self.newline();
  }

  fn outdent(&mut self) {
    self.indent -= 2;
    self.newline();
  }

  fn newline(&mut self) {
    self.out += "\n";
    self.written = 0;
  }

  fn write_str(&mut self, str: &str) {
    if str.is_empty() {
      return;
    }
    if self.written == 0 {
      self.write_indent();
    }
    self.out += str;
    self.written += str.chars().count();
  }

  fn write_indent(&mut self) {
    for _ in 0..self.indent {
      self.out += " ";
    }
  }

  fn remaining(&self) -> usize {
    MAX_WIDTH.saturating_sub(self.written + self.indent)
  }
}

impl<'src> From<&'src str> for Doc<'src> {
  fn from(str: &'src str) -> Self {
    Doc { measure: Measure::string(str), kind: DocKind::String(str) }
  }
}

impl From<Ident> for Doc<'_> {
  fn from(value: Ident) -> Self {
    value.0 .0.into()
  }
}

const MAX_WIDTH: usize = 100;
