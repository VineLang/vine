mod resolve_path;

use std::{
  collections::{BTreeMap, HashMap},
  mem::{replace, take},
};

use vine_util::idx::Counter;

use crate::{
  ast::{
    Expr, ExprKind, Ident, Impl, ImplKind, Label, LabelId, LetFnId, Local, LogicalOp, Pat, PatKind,
    Span, Stmt, StmtKind, Trait, TraitKind, Ty, TyKind,
  },
  chart::{
    Chart, ChartCheckpoint, DefId, EnumDef, GenericsId, ImplDef, ImplDefKind, PatternDefKind,
    StructDef, TraitDef, TraitDefKind, TraitSubitemKind, TypeDef, TypeDefKind, ValueDef,
    ValueDefKind,
  },
  core::Core,
  diag::Diag,
  visit::{VisitMut, Visitee},
};

#[derive(Debug)]
pub struct Resolver<'core, 'a> {
  pub core: &'core Core<'core>,
  pub chart: &'a mut Chart<'core>,
}

impl<'core> Resolver<'core, '_> {
  pub fn resolve_all(&mut self) {
    self.resolve_since(&ChartCheckpoint::default());
  }

  pub fn resolve_since(&mut self, checkpoint: &ChartCheckpoint) {
    ResolveVisitor {
      resolver: self,
      def: Default::default(),
      type_params: Default::default(),
      impl_params: Default::default(),
      scope: Default::default(),
      scope_depth: Default::default(),
      locals: Default::default(),
      let_fns: Default::default(),
      labels: Default::default(),
      loops: Default::default(),
      label_id: Default::default(),
    }
    .resolve_since(checkpoint);
  }

  pub(crate) fn resolve_custom<'t>(
    &mut self,
    def: DefId,
    initial: &BTreeMap<Local, Ident<'core>>,
    local_count: &mut Counter<Local>,
    visitee: impl Visitee<'core, 't>,
  ) -> impl Iterator<Item = (Ident<'core>, Local)> {
    let mut visitor = ResolveVisitor {
      resolver: self,
      def,
      type_params: HashMap::new(),
      impl_params: HashMap::new(),
      scope: initial
        .iter()
        .map(|(&l, &i)| (i, vec![ScopeEntry { depth: 0, binding: Binding::Local(l) }]))
        .collect(),
      scope_depth: 0,
      locals: *local_count,
      let_fns: Counter::default(),
      labels: Default::default(),
      loops: Default::default(),
      label_id: Default::default(),
    };
    visitor.visit(visitee);
    *local_count = visitor.locals;
    visitor.scope.into_iter().filter_map(|(k, e)| {
      let entry = e.first()?;
      if entry.depth == 0 {
        if let Binding::Local(l) = entry.binding {
          Some((k, l))
        } else {
          None
        }
      } else {
        None
      }
    })
  }
}

#[derive(Debug)]
struct ResolveVisitor<'core, 'a, 'b> {
  resolver: &'a mut Resolver<'core, 'b>,
  def: DefId,
  type_params: HashMap<Ident<'core>, usize>,
  impl_params: HashMap<Ident<'core>, usize>,
  scope: HashMap<Ident<'core>, Vec<ScopeEntry>>,
  scope_depth: usize,
  locals: Counter<Local>,
  let_fns: Counter<LetFnId>,
  labels: HashMap<Ident<'core>, (LabelId, bool)>,
  loops: Vec<LabelId>,
  label_id: Counter<LabelId>,
}

#[derive(Debug)]
struct ScopeEntry {
  depth: usize,
  binding: Binding,
}

#[derive(Debug, Clone, Copy)]
enum Binding {
  Local(Local),
  LetFn(LetFnId),
}

impl<'core> ResolveVisitor<'core, '_, '_> {
  fn resolve_since(&mut self, checkpoint: &ChartCheckpoint) {
    for id in self.resolver.chart.values.keys_from(checkpoint.values) {
      let ValueDef { def, generics, ref mut kind, .. } = self.resolver.chart.values[id];
      let mut kind = take(kind);
      self.initialize(def, generics);
      match &mut kind {
        ValueDefKind::Taken => unreachable!(),
        ValueDefKind::Const { ty, value } => {
          self.visit(ty);
          self.visit(value);
        }
        ValueDefKind::Fn { params, ret, body } => {
          self.visit(params);
          self.visit(ret);
          self.visit(body);
        }
        ValueDefKind::Struct(..) | ValueDefKind::Enum(..) | ValueDefKind::TraitSubitem(..) => {}
      }
      let value_def = &mut self.resolver.chart.values[id];
      value_def.kind = kind;
      value_def.locals = self.locals;
    }

    for id in self.resolver.chart.types.keys_from(checkpoint.types) {
      let TypeDef { def, generics, ref mut kind, .. } = self.resolver.chart.types[id];
      let mut kind = take(kind);
      self.initialize(def, generics);
      match &mut kind {
        TypeDefKind::Taken => unreachable!(),
        TypeDefKind::Alias(ty) => self.visit(ty),
        TypeDefKind::Struct(_) | TypeDefKind::Enum(_) | TypeDefKind::Opaque => {}
      }
      self.resolver.chart.types[id].kind = kind;
    }

    for id in self.resolver.chart.traits.keys_from(checkpoint.traits) {
      let TraitDef { def, generics, ref mut kind, .. } = self.resolver.chart.traits[id];
      let mut kind = take(kind);
      self.initialize(def, generics);
      match &mut kind {
        TraitDefKind::Taken => unreachable!(),
        TraitDefKind::Trait { subitems } => {
          for subitem in subitems.values_mut() {
            match &mut subitem.kind {
              TraitSubitemKind::Fn(params, ty) => {
                self.visit(params);
                self.visit(ty);
              }
              TraitSubitemKind::Const(ty) => self.visit_type(ty),
            }
          }
        }
      }
      self.resolver.chart.traits[id].kind = kind;
    }

    for id in self.resolver.chart.impls.keys_from(checkpoint.impls) {
      let ImplDef { def, generics, ref mut kind, .. } = self.resolver.chart.impls[id];
      let mut kind = take(kind);
      self.initialize(def, generics);
      match &mut kind {
        ImplDefKind::Taken => unreachable!(),
        ImplDefKind::Impl { trait_, .. } => self.visit(trait_),
      }
      self.resolver.chart.impls[id].kind = kind;
    }

    for id in self.resolver.chart.structs.keys_from(checkpoint.structs) {
      let StructDef { def, generics, ref mut data, .. } = self.resolver.chart.structs[id];
      let mut data = replace(data, Ty { span: Span::NONE, kind: TyKind::Hole });
      self.initialize(def, generics);
      self.visit(&mut data);
      self.resolver.chart.structs[id].data = data;
    }

    for id in self.resolver.chart.enums.keys_from(checkpoint.enums) {
      let EnumDef { def, generics, ref mut variants, .. } = self.resolver.chart.enums[id];
      let mut variants = take(variants);
      self.initialize(def, generics);
      for variant in variants.values_mut() {
        self.visit(&mut variant.data);
      }
      self.resolver.chart.enums[id].variants = variants;
    }

    for id in self.resolver.chart.generics.keys_from(checkpoint.generics) {
      let def = self.resolver.chart.generics[id].def;
      self.initialize(def, id);
      let mut generics = take(&mut self.resolver.chart.generics[id]);
      if self.type_params.len() != generics.type_params.len() {
        self.resolver.core.report(Diag::DuplicateTypeParam { span: generics.span });
      }
      if self.impl_params.len() != generics.impl_params.iter().filter(|x| x.name.is_some()).count()
      {
        self.resolver.core.report(Diag::DuplicateImplParam { span: generics.span });
      }
      self.visit(generics.impl_params.iter_mut().map(|p| &mut p.trait_));
      self.resolver.chart.generics[id] = generics;
    }

    for id in self.resolver.chart.imports.keys_from(checkpoint.imports) {
      _ = self.resolver.resolve_import(id);
    }
  }

  fn initialize(&mut self, def: DefId, generics: GenericsId) {
    self.type_params.clear();
    self.impl_params.clear();
    self.scope.clear();
    debug_assert_eq!(self.scope_depth, 0);
    self.locals.reset();
    self.let_fns.reset();
    self.labels.clear();
    debug_assert!(self.loops.is_empty());
    self.label_id.reset();

    self.def = def;
    let generics = &self.resolver.chart.generics[generics];
    self.type_params.extend(generics.type_params.iter().enumerate().map(|(i, &p)| (p.name, i)));
    self
      .impl_params
      .extend(generics.impl_params.iter().enumerate().filter_map(|(i, p)| Some((p.name?, i))));
  }
}

impl<'core> VisitMut<'core, '_> for ResolveVisitor<'core, '_, '_> {
  fn enter_scope(&mut self) {
    self.scope_depth += 1;
  }

  fn exit_scope(&mut self) {
    self.scope_depth -= 1;
    for stack in self.scope.values_mut() {
      if stack.last().is_some_and(|x| x.depth > self.scope_depth) {
        stack.pop();
      }
    }
  }

  fn visit_stmt(&mut self, stmt: &mut Stmt<'core>) {
    self._visit_stmt(stmt);
    if let StmtKind::LetFn(d) = &mut stmt.kind {
      let id = self.let_fns.next();
      self.bind(d.name, Binding::LetFn(id));
      d.id = Some(id);
    }
  }

  fn visit_pat(&mut self, pat: &mut Pat<'core>) {
    self._visit_pat(pat);
    let span = pat.span;
    if let PatKind::Path(path, data) = &mut pat.kind {
      let resolved = self.resolver.resolve_path_to(self.def, path, "pattern", |d| d.pattern_def);
      pat.kind = match resolved {
        Ok(id) => match self.resolver.chart.patterns[id].kind {
          PatternDefKind::Struct(struct_id) => PatKind::Struct(
            struct_id,
            path.take_generics(),
            match data.take() {
              Some(mut args) => Box::new(if args.len() == 1 {
                args.pop().unwrap()
              } else {
                Pat { span, kind: PatKind::Tuple(args) }
              }),
              None => Box::new(Pat {
                span,
                kind: PatKind::Error(
                  self.resolver.core.report(Diag::ExpectedDataSubpattern { span }),
                ),
              }),
            },
          ),
          PatternDefKind::Enum(enum_id, variant) => PatKind::Enum(
            enum_id,
            variant,
            path.take_generics(),
            data.take().map(|mut args| {
              Box::new(if args.len() == 1 {
                args.pop().unwrap()
              } else {
                Pat { span, kind: PatKind::Tuple(args) }
              })
            }),
          ),
        },
        Err(diag) => {
          if let (Some(ident), None) = (path.as_ident(), data) {
            let local = self.locals.next();
            let binding = Binding::Local(local);
            self.bind(ident, binding);
            PatKind::Local(local)
          } else {
            PatKind::Error(self.resolver.core.report(diag))
          }
        }
      };
    }
  }

  fn visit_expr(&mut self, expr: &mut Expr<'core>) {
    let span = expr.span;
    match &mut expr.kind {
      ExprKind![cond] => {
        self.enter_scope();
        self.visit_cond(expr);
        self.exit_scope();
      }
      ExprKind::If(arms, leg) => {
        for (cond, block) in arms {
          self.enter_scope();
          self.visit_cond(cond);
          self.visit_block(block);
          self.exit_scope();
        }
        if let Some(leg) = leg {
          self.visit_block(leg);
        }
      }
      ExprKind::While(label, cond, block) => {
        self.bind_label(label, true, |self_| {
          self_.enter_scope();
          self_.visit_cond(cond);
          self_.visit_block(block);
          self_.exit_scope();
        });
      }
      ExprKind::Loop(label, block) => {
        self.bind_label(label, true, |self_| {
          self_.visit_block(block);
        });
      }
      ExprKind::Do(label, block) => {
        self.bind_label(label, false, |self_| {
          self_.visit_block(block);
        });
      }
      ExprKind::Break(label, value) => {
        let id = if let Label::Ident(Some(label)) = *label {
          self.labels.get(&label).map(|x| x.0).ok_or(Diag::UnboundLabel { span, label })
        } else {
          self.loops.last().copied().ok_or(Diag::NoLoopBreak { span })
        };
        match id {
          Ok(id) => {
            *label = Label::Resolved(id);
          }
          Err(d) => {
            *label = Label::Error(self.resolver.core.report(d));
          }
        }
        if let Some(value) = value {
          self.visit_expr(value);
        }
      }
      ExprKind::Continue(label) => {
        let id = if let Label::Ident(Some(label)) = *label {
          self
            .labels
            .get(&label)
            .map(|x| if x.1 { Ok(x.0) } else { Err(Diag::NoContinueLabel { span, label }) })
            .ok_or(Diag::UnboundLabel { span, label })
        } else {
          self.loops.last().copied().map(Ok).ok_or(Diag::NoLoopBreak { span })
        };
        match id {
          Ok(Ok(id)) => {
            *label = Label::Resolved(id);
          }
          Ok(Err(d)) | Err(d) => {
            *label = Label::Error(self.resolver.core.report(d));
          }
        }
      }
      ExprKind::Fn(..) => {
        let labels = take(&mut self.labels);
        let loops = take(&mut self.loops);
        self._visit_expr(expr);
        self.labels = labels;
        self.loops = loops;
      }

      ExprKind::Path(path, args) => {
        self.visit(&mut path.generics);
        self.visit(&mut *args);
        let args = args.take();
        if let Some(ident) = path.as_ident() {
          if let Some(bind) = self.scope.get(&ident).and_then(|x| x.last()) {
            expr.kind = bind.binding.into();
            if let Some(args) = args {
              *expr = Expr { span, kind: ExprKind::Call(Box::new(take(expr)), args) };
            }
            return;
          }
        }
        let value = match self.resolver.resolve_path_to(self.def, path, "value", |d| d.value_def) {
          Ok(id) => id,
          Err(diag) => {
            expr.kind = ExprKind::Error(self.resolver.core.report(diag));
            return;
          }
        };

        match self.resolver.chart.values[value].kind {
          ValueDefKind::Struct(struct_id) => {
            if let Some(mut args) = args {
              expr.kind = ExprKind::Struct(
                struct_id,
                path.take_generics(),
                Box::new(if args.len() != 1 {
                  Expr { span, kind: ExprKind::Tuple(args) }
                } else {
                  args.pop().unwrap()
                }),
              )
            } else {
              expr.kind =
                ExprKind::Error(self.resolver.core.report(Diag::ExpectedDataExpr { span }))
            }
          }
          ValueDefKind::Enum(enum_id, variant) => {
            expr.kind = ExprKind::Enum(
              enum_id,
              variant,
              path.take_generics(),
              args.map(|mut args| {
                Box::new(if args.len() != 1 {
                  Expr { span, kind: ExprKind::Tuple(args) }
                } else {
                  args.pop().unwrap()
                })
              }),
            )
          }
          _ => {
            expr.kind = ExprKind::Def(value, path.take_generics());
            if let Some(args) = args {
              *expr = Expr { span, kind: ExprKind::Call(Box::new(take(expr)), args) };
            }
          }
        }
      }

      _ => self._visit_expr(expr),
    }
  }

  fn visit_type(&mut self, ty: &mut Ty<'core>) {
    self._visit_type(ty);
    if let TyKind::Path(path) = &mut ty.kind {
      if let Some(ident) = path.as_ident() {
        if let Some(&i) = self.type_params.get(&ident) {
          ty.kind = TyKind::Param(i);
          return;
        }
      }
      ty.kind = match self.resolver.resolve_path_to(self.def, path, "type", |d| d.type_def) {
        Ok(id) => TyKind::Def(id, path.take_generics()),
        Err(diag) => TyKind::Error(self.resolver.core.report(diag)),
      };
    }
  }

  fn visit_impl(&mut self, impl_: &mut Impl<'core>) {
    self._visit_impl(impl_);
    if let ImplKind::Path(path) = &mut impl_.kind {
      if let Some(ident) = path.as_ident() {
        if let Some(&i) = self.impl_params.get(&ident) {
          impl_.kind = ImplKind::Param(i);
          return;
        }
      }
      impl_.kind = match self.resolver.resolve_path_to(self.def, path, "impl", |d| d.impl_def) {
        Ok(id) => ImplKind::Def(id, path.take_generics()),
        Err(diag) => ImplKind::Error(self.resolver.core.report(diag)),
      };
    }
  }

  fn visit_trait(&mut self, trait_: &mut Trait<'core>) {
    self._visit_trait(trait_);
    if let TraitKind::Path(path) = &mut trait_.kind {
      trait_.kind = match self.resolver.resolve_path_to(self.def, path, "trait", |d| d.trait_def) {
        Ok(id) => TraitKind::Def(id, path.take_generics()),
        Err(diag) => TraitKind::Error(self.resolver.core.report(diag)),
      };
    }
  }
}

impl<'core> ResolveVisitor<'core, '_, '_> {
  fn bind(&mut self, ident: Ident<'core>, binding: Binding) {
    let stack = self.scope.entry(ident).or_default();
    let top = stack.last_mut();
    if top.as_ref().is_some_and(|x| x.depth == self.scope_depth) {
      top.unwrap().binding = binding;
    } else {
      stack.push(ScopeEntry { depth: self.scope_depth, binding })
    }
  }

  fn visit_cond(&mut self, cond: &mut Expr<'core>) {
    match &mut cond.kind {
      ExprKind![!cond] => self.visit_expr(cond),
      ExprKind::Bool(_) => {}
      ExprKind::Not(e) => {
        self.enter_scope();
        self.visit_cond(e);
        self.exit_scope();
      }
      ExprKind::Is(e, p) => {
        self.visit_expr(e);
        self.visit_pat(p);
      }
      ExprKind::LogicalOp(LogicalOp::And, a, b) => {
        self.visit_cond(a);
        self.visit_cond(b);
      }
      ExprKind::LogicalOp(LogicalOp::Or, a, b) => {
        self.enter_scope();
        self.visit_cond(a);
        self.exit_scope();
        self.enter_scope();
        self.visit_cond(b);
        self.exit_scope();
      }
      ExprKind::LogicalOp(LogicalOp::Implies, a, b) => {
        self.enter_scope();
        self.visit_cond(a);
        self.visit_cond(b);
        self.exit_scope();
      }
    }
  }

  fn bind_label(&mut self, label: &mut Label<'core>, cont: bool, f: impl FnOnce(&mut Self)) {
    let id = self.label_id.next();
    if cont {
      self.loops.push(id);
    }
    if let Label::Ident(Some(label)) = label {
      let old = self.labels.insert(*label, (id, cont));
      f(self);
      if let Some(old) = old {
        self.labels.insert(*label, old);
      } else {
        self.labels.remove(label);
      }
    } else {
      f(self);
    }
    if cont {
      self.loops.pop();
    }
    *label = Label::Resolved(id);
  }
}

impl From<Binding> for ExprKind<'_> {
  fn from(val: Binding) -> Self {
    match val {
      Binding::Local(l) => ExprKind::Local(l),
      Binding::LetFn(d) => ExprKind::LetFn(d),
    }
  }
}
