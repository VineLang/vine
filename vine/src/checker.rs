use std::{
  collections::{BTreeMap, HashMap},
  mem::{swap, take},
};

use vine_util::idx::{Counter, IdxVec, RangeExt};

use crate::{
  ast::{
    Block, GenericArgs, Generics, Ident, Impl, ImplKind, Key, Label, LabelId, LetFnId, Local, Pat,
    PatKind, Span, StmtKind, Trait, TraitKind, Ty, TyKind,
  },
  chart::{
    checkpoint::ChartCheckpoint, Chart, ConcreteConstId, ConcreteFnId, DefId, DefImplKind,
    DefPatternKind, DefTraitKind, DefTypeKind, EnumId, GenericsId, ImplId, ImplSubitemKind,
    StructId, TraitId, TypeAliasId,
  },
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  finder::Finder,
  signatures::{
    ConstSig, EnumSig, FnSig, GenericsSig, ImplSig, Signatures, StructSig, TraitSig, TypeAliasSig,
  },
  types::{ImplType, Type, TypeCtx, TypeKind, Types},
};

mod check_expr;
mod check_pat;
mod resolve_path;

#[derive(Debug)]
pub struct Checker<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a mut Chart<'core>,
  sigs: &'a mut Signatures<'core>,
  types: Types<'core>,
  return_ty: Option<Type>,
  cur_def: DefId,
  cur_generics: GenericsId,

  type_param_lookup: HashMap<Ident<'core>, usize>,
  impl_param_lookup: HashMap<Ident<'core>, usize>,

  scope: HashMap<Ident<'core>, Vec<ScopeEntry>>,
  scope_depth: usize,
  locals: Counter<Local>,
  let_fns: Counter<LetFnId>,
  labels: HashMap<Ident<'core>, LabelInfo>,
  loops: Vec<LabelInfo>,
  label_id: Counter<LabelId>,
}

#[derive(Debug)]
struct ScopeEntry {
  depth: usize,
  binding: Binding,
}

#[derive(Debug, Clone, Copy)]
struct LabelInfo {
  id: LabelId,
  is_loop: bool,
  break_ty: Type,
}

#[derive(Debug, Clone, Copy)]
enum Binding {
  Local(Local, Type),
  LetFn(LetFnId, Type),
}

impl<'core, 'a> Checker<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a mut Chart<'core>,
    sigs: &'a mut Signatures<'core>,
  ) -> Self {
    Checker {
      core,
      chart,
      sigs,
      types: Types::default(),
      locals: Default::default(),
      let_fns: Default::default(),
      return_ty: None,
      labels: Default::default(),
      cur_def: DefId::ROOT,
      cur_generics: GenericsId::NONE,
      type_param_lookup: Default::default(),
      impl_param_lookup: Default::default(),
      scope: Default::default(),
      scope_depth: Default::default(),
      loops: Default::default(),
      label_id: Default::default(),
    }
  }

  pub fn check_all(&mut self) {
    self.check_since(&ChartCheckpoint::default());
  }

  pub(crate) fn check_since(&mut self, checkpoint: &ChartCheckpoint) {
    for id in self.chart.imports.keys_from(checkpoint.imports) {
      _ = self.resolve_import(id);
    }
    for id in self.chart.type_aliases.keys_from(checkpoint.type_aliases) {
      self.resolve_type_alias(id);
    }
    for id in self.chart.structs.keys_from(checkpoint.structs) {
      self.resolve_struct_sig(id);
    }
    for id in self.chart.enums.keys_from(checkpoint.enums) {
      self.resolve_enum_sig(id);
    }
    for id in self.chart.traits.keys_from(checkpoint.traits) {
      self.resolve_trait_sig(id);
    }
    for id in self.chart.generics.keys_from(checkpoint.generics) {
      self.resolve_generics_sig(id);
    }
    for id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      self.resolve_const_sig(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.resolve_fn_sig(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.resolve_impl_sig(id);
    }
    for id in self.chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      self.check_const_def(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.check_fn_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.check_impl_def(id);
    }
  }

  fn initialize(&mut self, def_id: DefId, generics_id: GenericsId) {
    assert!(self.return_ty.is_none());
    self.labels.clear();
    self.types.reset();

    self.type_param_lookup.clear();
    self.impl_param_lookup.clear();
    self.scope.clear();
    debug_assert_eq!(self.scope_depth, 0);
    self.locals.reset();
    self.let_fns.reset();
    self.labels.clear();
    debug_assert!(self.loops.is_empty());
    self.label_id.reset();

    self.cur_def = def_id;
    self.cur_generics = generics_id;
    let generics = &self.chart.generics[generics_id];
    self
      .type_param_lookup
      .extend(generics.type_params.iter().enumerate().map(|(i, &p)| (p.name, i)));
    self
      .impl_param_lookup
      .extend(generics.impl_params.iter().enumerate().filter_map(|(i, p)| Some((p.name?, i))));
  }

  fn resolve_type_alias(&mut self, alias_id: TypeAliasId) {
    if let Some(Some(_)) = self.sigs.type_aliases.get(alias_id) {
      return;
    }
    let mut alias_def = self.chart.type_aliases[alias_id].clone();
    self.initialize(alias_def.def, alias_def.generics);
    let ty = self.hydrate_type(&mut alias_def.ty, false);
    self.chart.type_aliases[alias_id] = alias_def;
    let slot = self.sigs.type_aliases.get_or_extend(alias_id);
    if slot.is_none() {
      *slot = Some(self.types.export(|t| TypeAliasSig { ty: t.transfer(&ty) }));
    }
  }

  fn check_const_def(&mut self, const_id: ConcreteConstId) {
    let mut const_def = self.chart.concrete_consts[const_id].clone();
    self.initialize(const_def.def, const_def.generics);
    let ty = self.types.import(&self.sigs.concrete_consts[const_id], None).ty;
    self.check_expr_form_type(&mut const_def.value, Form::Value, ty);
    const_def.locals = self.locals;
    self.chart.concrete_consts[const_id] = const_def;
  }

  fn check_fn_def(&mut self, fn_id: ConcreteFnId) {
    let mut fn_def = self.chart.concrete_fns[fn_id].clone();
    self.initialize(fn_def.def, fn_def.generics);
    let ret_ty = self
      .types
      .import_with(&self.sigs.concrete_fns[fn_id], None, |t, sig| t.transfer(&sig.ret_ty));
    for pat in fn_def.params.iter_mut() {
      self.check_pat(pat, Form::Value, false);
    }
    self.return_ty = Some(ret_ty);
    self.check_block_type(&mut fn_def.body, ret_ty);
    self.return_ty = None;
    fn_def.locals = self.locals;
    self.chart.concrete_fns[fn_id] = fn_def;
  }

  pub(crate) fn _check_custom(
    &mut self,
    def_id: DefId,
    types: &mut Types<'core>,
    locals: &BTreeMap<Local, (Ident<'core>, Type)>,
    local_count: &mut Counter<Local>,
    block: &mut Block<'core>,
  ) -> (Type, impl Iterator<Item = (Ident<'core>, Local, Type)>) {
    self.cur_def = def_id;
    self.scope = locals
      .iter()
      .map(|(&local, &(name, ty))| {
        (name, vec![ScopeEntry { depth: 0, binding: Binding::Local(local, ty) }])
      })
      .collect();
    swap(types, &mut self.types);
    self.locals = *local_count;
    let ty = self._check_block(block);
    *local_count = self.locals;
    swap(types, &mut self.types);
    (
      ty,
      take(&mut self.scope).into_iter().filter_map(|(name, entries)| {
        let entry = entries.first()?;
        if entry.depth == 0 {
          if let Binding::Local(local, ty) = entry.binding {
            Some((name, local, ty))
          } else {
            None
          }
        } else {
          None
        }
      }),
    )
  }

  fn check_block(&mut self, block: &mut Block<'core>) -> Type {
    self.enter_scope();
    let ty = self._check_block(block);
    self.exit_scope();
    ty
  }

  fn _check_block(&mut self, block: &mut Block<'core>) -> Type {
    let mut ty = self.types.nil();
    for stmt in block.stmts.iter_mut() {
      ty = self.types.nil();
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let refutable = l.else_block.is_some();
          let expr_info = if let Some(value) = &mut l.init {
            Some((value.span, self.check_expr_form(value, Form::Value)))
          } else {
            None
          };
          let pat_ty = self.check_pat(&mut l.bind, Form::Value, refutable);
          if let Some((expr_span, expr_ty)) = expr_info {
            if self.types.unify(expr_ty, pat_ty).is_failure() {
              self.core.report(Diag::ExpectedTypeFound {
                span: expr_span,
                expected: self.types.show(self.chart, pat_ty),
                found: self.types.show(self.chart, expr_ty),
              });
            }
          }
          if let Some(block) = &mut l.else_block {
            let never = self.types.new(TypeKind::Never);
            self.check_block_type(block, never);
          }
        }
        StmtKind::LetFn(d) => {
          let old_labels = take(&mut self.labels);
          let old_loops = take(&mut self.loops);
          self.enter_scope();
          let params = d.params.iter_mut().map(|p| self.check_pat(p, Form::Value, false)).collect();
          let ret = d
            .ret
            .as_mut()
            .map(|t| self.hydrate_type(t, true))
            .unwrap_or_else(|| self.types.new_var(d.body.span));
          let old_return_ty = self.return_ty.replace(ret);
          self.check_block_type(&mut d.body, ret);
          self.exit_scope();
          self.labels = old_labels;
          self.loops = old_loops;
          self.return_ty = old_return_ty;
          let id = self.let_fns.next();
          let ty = self.types.new(TypeKind::Fn(params, ret));
          self.bind(d.name, Binding::LetFn(id, ty));
          d.id = Some(id);
        }
        StmtKind::Expr(e, semi) => {
          ty = self.check_expr_form(e, Form::Value);
          if *semi {
            ty = self.types.nil();
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => {}
      }
    }
    ty
  }

  fn check_block_type(&mut self, block: &mut Block<'core>, ty: Type) {
    let found = self.check_block(block);
    if self.types.unify(found, ty).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span: block.span,
        expected: self.types.show(self.chart, ty),
        found: self.types.show(self.chart, found),
      });
    }
  }

  fn hydrate_type(&mut self, ty: &mut Ty<'core>, inference: bool) -> Type {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.types.new_var(span),
      TyKind::Paren(t) => self.hydrate_type(t, inference),
      TyKind::Hole => self.types.error(self.core.report(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => {
        let params = args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect();
        let ret =
          ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(self.types.nil());
        self.types.new(TypeKind::Fn(params, ret))
      }
      TyKind::Tuple(tys) => {
        let tys = tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect();
        self.types.new(TypeKind::Tuple(tys))
      }
      TyKind::Object(entries) => {
        self.build_object_type(entries, |self_, t| self_.hydrate_type(t, inference))
      }
      TyKind::Ref(inner) => {
        let inner = self.hydrate_type(inner, inference);
        self.types.new(TypeKind::Ref(inner))
      }
      TyKind::Inverse(inner) => {
        let inner = self.hydrate_type(inner, inference);
        inner.inverse()
      }
      TyKind::Path(path) => {
        if let Some(ident) = path.as_ident() {
          if let Some(&i) = self.type_param_lookup.get(&ident) {
            return self.types.new(TypeKind::Param(i, ident));
          }
        }
        let resolved = self.resolve_path_to(self.cur_def, path, "type", |d| d.type_kind);
        let _generics = &mut Generics::default();
        let generics = path.generics.as_mut().unwrap_or(_generics);
        match resolved {
          Ok(DefTypeKind::Opaque(opaque_id)) => {
            let generics_id = self.chart.opaque_types[opaque_id].generics;
            let type_params = self.check_generics(generics, generics_id, inference);
            self.types.new(TypeKind::Opaque(opaque_id, type_params))
          }
          Ok(DefTypeKind::Struct(struct_id)) => {
            let generics_id = self.chart.structs[struct_id].generics;
            let type_params = self.check_generics(generics, generics_id, inference);
            self.types.new(TypeKind::Struct(struct_id, type_params))
          }
          Ok(DefTypeKind::Enum(enum_id)) => {
            let generics_id = self.chart.enums[enum_id].generics;
            let type_params = self.check_generics(generics, generics_id, inference);
            self.types.new(TypeKind::Enum(enum_id, type_params))
          }
          Ok(DefTypeKind::Alias(type_alias_id)) => {
            let generics_id = self.chart.type_aliases[type_alias_id].generics;
            let type_params = self.check_generics(generics, generics_id, inference);
            Checker::new(self.core, self.chart, self.sigs).resolve_type_alias(type_alias_id);
            self
              .types
              .import(self.sigs.type_aliases[type_alias_id].as_ref().unwrap(), Some(&type_params))
              .ty
          }
          Err(diag) => self.types.error(self.core.report(diag)),
        }
      }
      TyKind::Error(e) => self.types.error(*e),
    }
  }

  fn hydrate_param(&mut self, pat: &mut Pat<'core>) -> Type {
    let span = pat.span;
    match &mut pat.kind {
      PatKind::Struct(..) | PatKind::Enum(..) => unreachable!(),
      PatKind::Paren(inner) => self.hydrate_param(inner),
      PatKind::Annotation(_, ty) => self.hydrate_type(ty, false),
      PatKind::Path(path, _) => {
        let resolved = self.resolve_path_to(self.cur_def, path, "pattern", |d| d.pattern_kind);
        let _generics = &mut Generics::default();
        let generics = path.generics.as_mut().unwrap_or(_generics);
        match resolved {
          Ok(DefPatternKind::Struct(struct_id)) => {
            let type_params =
              self.check_generics(generics, self.chart.structs[struct_id].generics, false);
            self.types.new(TypeKind::Struct(struct_id, type_params))
          }
          Ok(DefPatternKind::Enum(enum_id, _)) => {
            let type_params =
              self.check_generics(generics, self.chart.enums[enum_id].generics, false);
            self.types.new(TypeKind::Enum(enum_id, type_params))
          }
          Err(_) => self.types.error(self.core.report(Diag::ItemTypeHole { span })),
        }
      }
      PatKind::Ref(inner) => {
        let inner = self.hydrate_param(inner);
        self.types.new(TypeKind::Ref(inner))
      }
      PatKind::Inverse(inner) => {
        let inner = self.hydrate_param(inner);
        inner.inverse()
      }
      PatKind::Tuple(els) => {
        let els = els.iter_mut().map(|p| self.hydrate_param(p)).collect();
        self.types.new(TypeKind::Tuple(els))
      }
      PatKind::Object(entries) => self.build_object_type(entries, Self::hydrate_param),
      PatKind::Hole | PatKind::Local(_) | PatKind::Deref(_) => {
        self.types.error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => self.types.error(*e),
    }
  }

  fn check_impl_def(&mut self, impl_id: ImplId) {
    let mut impl_def = self.chart.impls[impl_id].clone();
    self.initialize(impl_def.def, impl_def.generics);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    if let ImplType::Trait(trait_id, type_params) = ty {
      let trait_def = &self.chart.traits[trait_id];
      let trait_sig = &self.sigs.traits[trait_id];
      let consts = IdxVec::from(Vec::from_iter(trait_sig.consts.iter().map(|(id, sig)| {
        let name = trait_def.consts[id].name;
        let Some(subitem) = impl_def.subitems.iter().find(|i| i.name == name) else {
          return Err(self.core.report(Diag::IncompleteImpl { span, name }));
        };
        let span = subitem.span;
        let ImplSubitemKind::Const(const_id) = subitem.kind else {
          return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "const" }));
        };
        let expected_ty = self.types.import(sig, Some(&type_params)).ty;
        let const_sig = &self.sigs.concrete_consts[const_id];
        let found_ty = self.types.import(const_sig, None).ty;
        if self.types.unify(expected_ty, found_ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show(self.chart, expected_ty),
            found: self.types.show(self.chart, found_ty),
          });
        }
        Ok(const_id)
      })));
      let fns = IdxVec::from(Vec::from_iter(trait_sig.fns.iter().map(|(id, sig)| {
        let name = trait_def.fns[id].name;
        let Some(subitem) = impl_def.subitems.iter().find(|i| i.name == name) else {
          return Err(self.core.report(Diag::IncompleteImpl { span, name }));
        };
        let span = subitem.span;
        let ImplSubitemKind::Fn(fn_id) = subitem.kind else {
          return Err(self.core.report(Diag::WrongImplSubitemKind { span, expected: "fn" }));
        };
        let expected_ty = {
          let sig = self.types.import(sig, Some(&type_params));
          self.types.new(TypeKind::Fn(sig.params, sig.ret_ty))
        };
        let fn_sig = &self.sigs.concrete_fns[fn_id];
        let found_ty = {
          let sig = self.types.import(fn_sig, None);
          self.types.new(TypeKind::Fn(sig.params, sig.ret_ty))
        };
        if self.types.unify(expected_ty, found_ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show(self.chart, expected_ty),
            found: self.types.show(self.chart, found_ty),
          });
        }
        Ok(fn_id)
      })));
      for item in impl_def.subitems.iter() {
        if trait_def.consts.values().all(|x| x.name != item.name)
          && trait_def.fns.values().all(|x| x.name != item.name)
        {
          self.core.report(Diag::ExtraneousImplItem { span: item.span, name: item.name });
        }
      }
      impl_def.consts = consts;
      impl_def.fns = fns;
    }
    self.chart.impls[impl_id] = impl_def;
  }

  fn build_object_type<T>(
    &mut self,
    entries: &mut Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut T) -> Type,
  ) -> Type {
    let mut fields = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = fields.insert(key.ident, f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    if let Err(err) = duplicate {
      self.types.error(err)
    } else {
      self.types.new(TypeKind::Object(fields))
    }
  }

  fn resolve_struct_sig(&mut self, struct_id: StructId) {
    assert_eq!(self.sigs.structs.next_index(), struct_id);
    let mut struct_def = self.chart.structs[struct_id].clone();
    self.initialize(struct_def.def, struct_def.generics);
    let data = self.hydrate_type(&mut struct_def.data, false);
    self.sigs.structs.push(TypeCtx { types: take(&mut self.types), inner: StructSig { data } });
    self.chart.structs[struct_id] = struct_def;
  }

  fn resolve_enum_sig(&mut self, enum_id: EnumId) {
    assert_eq!(self.sigs.enums.next_index(), enum_id);
    let mut enum_def = self.chart.enums[enum_id].clone();
    self.initialize(enum_def.def, enum_def.generics);
    let variant_data = enum_def
      .variants
      .values_mut()
      .map(|variant| variant.data.as_mut().map(|ty| self.hydrate_type(ty, false)))
      .collect::<Vec<_>>()
      .into();
    self.sigs.enums.push(TypeCtx { types: take(&mut self.types), inner: EnumSig { variant_data } });
    self.chart.enums[enum_id] = enum_def;
  }

  fn resolve_trait_sig(&mut self, trait_id: TraitId) {
    assert_eq!(self.sigs.traits.next_index(), trait_id);
    let mut trait_def = self.chart.traits[trait_id].clone();
    self.initialize(trait_def.def, trait_def.generics);
    let sig = TraitSig {
      consts: IdxVec::from(Vec::from_iter(trait_def.consts.values_mut().map(|trait_const| {
        let ty = self.hydrate_type(&mut trait_const.ty, false);
        TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } }
      }))),
      fns: IdxVec::from(Vec::from_iter(trait_def.fns.values_mut().map(|trait_fn| {
        let (params, ret_ty) = self.assess_fn_sig(&mut trait_fn.params, &mut trait_fn.ret_ty);
        TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } }
      }))),
    };
    self.sigs.traits.push(sig);
    self.chart.traits[trait_id] = trait_def;
  }

  fn resolve_generics_sig(&mut self, generics_id: GenericsId) {
    assert_eq!(self.sigs.generics.next_index(), generics_id);
    let mut generics_def = self.chart.generics[generics_id].clone();
    self.initialize(generics_def.def, generics_id);
    let mut impl_param_types = vec![];
    impl_param_types.extend(
      generics_def
        .type_params
        .iter()
        .enumerate()
        .flat_map(|(i, param)| {
          let span = param.span;
          let ty = self.types.new(TypeKind::Param(i, param.name));
          [
            param.flex.fork().then(|| {
              if let Some(fork) = self.chart.builtins.fork {
                ImplType::Trait(fork, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Fork" }))
              }
            }),
            param.flex.drop().then(|| {
              if let Some(drop) = self.chart.builtins.drop {
                ImplType::Trait(drop, vec![ty])
              } else {
                ImplType::Error(self.core.report(Diag::MissingBuiltin { span, builtin: "Drop" }))
              }
            }),
          ]
        })
        .flatten(),
    );
    impl_param_types
      .extend(generics_def.impl_params.iter_mut().map(|p| self.assess_trait(&mut p.trait_)));
    self.sigs.generics.push(TypeCtx {
      types: take(&mut self.types),
      inner: GenericsSig { impl_params: impl_param_types },
    });
    if self.type_param_lookup.len() != generics_def.type_params.len() {
      self.core.report(Diag::DuplicateTypeParam { span: generics_def.span });
    }
    if self.impl_param_lookup.len()
      != generics_def.impl_params.iter().filter(|x| x.name.is_some()).count()
    {
      self.core.report(Diag::DuplicateImplParam { span: generics_def.span });
    }
    self.chart.generics[generics_id] = generics_def;
  }

  fn resolve_const_sig(&mut self, const_id: ConcreteConstId) {
    assert_eq!(self.sigs.concrete_consts.next_index(), const_id);
    let mut const_def = self.chart.concrete_consts[const_id].clone();
    self.initialize(const_def.def, const_def.generics);
    let ty = self.hydrate_type(&mut const_def.ty, false);
    self
      .sigs
      .concrete_consts
      .push(TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } });
    self.chart.concrete_consts[const_id] = const_def;
  }

  fn resolve_fn_sig(&mut self, fn_id: ConcreteFnId) {
    assert_eq!(self.sigs.concrete_fns.next_index(), fn_id);
    let mut fn_def = self.chart.concrete_fns[fn_id].clone();
    self.initialize(fn_def.def, fn_def.generics);
    let (params, ret_ty) = self.assess_fn_sig(&mut fn_def.params, &mut fn_def.ret_ty);
    self
      .sigs
      .concrete_fns
      .push(TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } });
    self.chart.concrete_fns[fn_id] = fn_def;
  }

  fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    assert_eq!(self.sigs.impls.next_index(), impl_id);
    let mut impl_def = self.chart.impls[impl_id].clone();
    self.initialize(impl_def.def, impl_def.generics);
    let ty = self.assess_trait(&mut impl_def.trait_);
    self.sigs.impls.push(TypeCtx { types: take(&mut self.types), inner: ImplSig { ty } });
    self.chart.impls[impl_id] = impl_def;
  }

  fn assess_fn_sig(
    &mut self,
    params: &mut [Pat<'core>],
    ret: &mut Option<Ty<'core>>,
  ) -> (Vec<Type>, Type) {
    let params = params.iter_mut().map(|p| self.hydrate_param(p)).collect();
    let ret = ret.as_mut().map(|t| self.hydrate_type(t, false)).unwrap_or(self.types.nil());
    (params, ret)
  }

  fn assess_trait(&mut self, trait_: &mut Trait<'core>) -> ImplType {
    match &mut trait_.kind {
      TraitKind::Path(path) => {
        match self.resolve_path_to(self.cur_def, path, "trait", |d| d.trait_kind) {
          Ok(DefTraitKind::Trait(id)) => {
            let mut generics = path.take_generics();
            let type_params =
              self.check_generics(&mut generics, self.chart.traits[id].generics, false);
            ImplType::Trait(id, type_params)
          }
          Err(diag) => ImplType::Error(self.core.report(diag)),
        }
      }
      TraitKind::Error(e) => ImplType::Error(*e),
    }
  }

  fn check_impl_type(&mut self, impl_: &mut Impl<'core>, ty: &ImplType) {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Hole => {
        *impl_ = self.find_impl(span, ty);
      }
      _ => {
        let found = self.check_impl(impl_);
        if self.types.unify_impl_type(&found, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span: impl_.span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &found),
          });
        }
      }
    }
  }

  fn check_impl(&mut self, impl_: &mut Impl<'core>) -> ImplType {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Param(_) | ImplKind::Def(..) => unreachable!(),
      ImplKind::Path(path) => {
        if let Some(ident) = path.as_ident() {
          if let Some(&i) = self.impl_param_lookup.get(&ident) {
            impl_.kind = ImplKind::Param(i);
            return self.types.import_with(
              &self.sigs.generics[self.cur_generics],
              None,
              |t, sig| t.transfer(&sig.impl_params[i]),
            );
          }
        }
        match self.resolve_path_to(self.cur_def, path, "impl", |d| d.impl_kind) {
          Ok(DefImplKind::Impl(id)) => {
            let mut generics = path.take_generics();
            let type_params =
              self.check_generics(&mut generics, self.chart.impls[id].generics, true);
            impl_.kind = ImplKind::Def(id, generics);
            self.types.import(&self.sigs.impls[id], Some(&type_params)).ty
          }
          Err(diag) => {
            let err = self.core.report(diag);
            impl_.kind = ImplKind::Error(err);
            ImplType::Error(err)
          }
        }
      }
      ImplKind::Hole => ImplType::Error(self.core.report(Diag::UnspecifiedImpl { span })),
      ImplKind::Error(e) => ImplType::Error(*e),
    }
  }

  pub fn check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
  ) -> Vec<Type> {
    self._check_generics(args, params_id, inference, None)
  }

  fn _check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
    type_params: Option<Vec<Type>>,
  ) -> Vec<Type> {
    let params = &self.chart.generics[params_id];
    let check_count = |got, expected, kind| {
      if got != expected {
        self.core.report(Diag::BadGenericCount {
          span: args.span,
          path: self.chart.defs[params.def].path,
          expected,
          got,
          kind,
        });
      }
    };
    if !inference || !args.types.is_empty() {
      check_count(args.types.len(), params.type_params.len(), "type");
    }
    let impl_param_count =
      // Things with no inference cannot have implementation parameters; skip
      // checking the `GenericsSig` as this may not have been resolved yet.
      if !inference { 0 } else { self.sigs.generics[params_id].inner.impl_params.len() };
    if !args.impls.is_empty() {
      check_count(args.impls.len(), impl_param_count, "impl");
    }
    let has_impl_params = impl_param_count != 0;
    let type_param_count = params.type_params.len();
    let type_params = if let Some(type_params) = type_params {
      for (a, b) in type_params.iter().zip(args.types.iter_mut()) {
        let b = self.hydrate_type(b, inference);
        _ = self.types.unify(*a, b);
      }
      type_params
    } else {
      (0..type_param_count)
        .iter()
        .map(|i| {
          args.types.get_mut(i).map(|t| self.hydrate_type(t, inference)).unwrap_or_else(|| {
            if inference {
              self.types.new_var(args.span)
            } else {
              self.types.error(ErrorGuaranteed::new_unchecked())
            }
          })
        })
        .collect::<Vec<_>>()
    };
    if has_impl_params {
      let impl_params_types =
        self.types.import(&self.sigs.generics[params_id], Some(&type_params)).impl_params;
      if args.impls.is_empty() {
        args.impls =
          impl_params_types.into_iter().map(|ty| self.find_impl(args.span, &ty)).collect();
      } else {
        for (impl_, param_type) in args.impls.iter_mut().zip(impl_params_types.into_iter()) {
          self.check_impl_type(impl_, &param_type);
        }
      }
    }
    type_params
  }

  fn finder(&self, span: Span) -> Finder<'core, '_> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
  }

  fn find_impl(&mut self, span: Span, ty: &ImplType) -> Impl<'core> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
      .find_impl(&mut self.types, ty)
  }

  fn bind(&mut self, ident: Ident<'core>, binding: Binding) {
    let stack = self.scope.entry(ident).or_default();
    let top = stack.last_mut();
    if top.as_ref().is_some_and(|x| x.depth == self.scope_depth) {
      top.unwrap().binding = binding;
    } else {
      stack.push(ScopeEntry { depth: self.scope_depth, binding })
    }
  }

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

  fn bind_label<T>(
    &mut self,
    label: &mut Label<'core>,
    is_loop: bool,
    break_ty: Type,
    f: impl FnOnce(&mut Self) -> T,
  ) -> T {
    let id = self.label_id.next();
    let info = LabelInfo { id, is_loop, break_ty };
    if is_loop {
      self.loops.push(info);
    }
    let result;
    if let Label::Ident(Some(label)) = label {
      let old = self.labels.insert(*label, info);
      result = f(self);
      if let Some(old) = old {
        self.labels.insert(*label, old);
      } else {
        self.labels.remove(label);
      }
    } else {
      result = f(self);
    }
    if is_loop {
      self.loops.pop();
    }
    *label = Label::Resolved(id);
    result
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Form {
  Value,
  Place,
  Space,
  Error(ErrorGuaranteed),
}

impl Form {
  fn inverse(self) -> Self {
    match self {
      Form::Value => Form::Space,
      Form::Place => Form::Place,
      Form::Space => Form::Value,
      Form::Error(e) => Form::Error(e),
    }
  }
}
