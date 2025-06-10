use std::{
  collections::{BTreeMap, HashMap},
  mem::{swap, take},
};

use vine_util::idx::{Counter, IdxVec, RangeExt};

use crate::{
  ast::{
    Block, GenericArgs, Generics, Ident, Impl, ImplKind, Key, Pat, PatKind, Path, Span, Stmt,
    StmtKind, Trait, TraitKind, Ty, TyKind,
  },
  chart::{
    checkpoint::ChartCheckpoint, Chart, ConcreteConstId, ConcreteFnId, ConstId, DefId, DefImplKind,
    DefPatternKind, DefTraitKind, DefTypeKind, EnumId, FnId, GenericsId, ImplId, ImplSubitemKind,
    StructId, TraitConstId, TraitFnId, TraitId, TypeAliasId,
  },
  core::Core,
  diag::{Diag, ErrorGuaranteed},
  finder::Finder,
  signatures::{
    ConstSig, EnumSig, FnSig, GenericsSig, ImplSig, Signatures, StructSig, TraitSig, TypeAliasSig,
  },
  tir::{
    ClosureId, ConstRelId, FnRelId, Form, LabelId, Local, Tir, TirClosure, TirExpr, TirExprKind,
    TirImpl, TirPat, TirPatKind,
  },
  types::{ImplType, Type, TypeCtx, TypeKind, Types},
};

mod resolve_expr;
mod resolve_pat;
mod resolve_path;

#[derive(Debug)]
pub struct Resolver<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a Chart<'core>,
  sigs: &'a mut Signatures<'core>,
  resolutions: &'a mut Resolutions,
  types: Types<'core>,
  return_ty: Option<Type>,
  cur_def: DefId,
  cur_generics: GenericsId,

  type_param_lookup: HashMap<Ident<'core>, usize>,
  impl_param_lookup: HashMap<Ident<'core>, usize>,

  scope: HashMap<Ident<'core>, Vec<ScopeEntry>>,
  scope_depth: usize,
  locals: Counter<Local>,
  labels: HashMap<Ident<'core>, LabelInfo>,
  loops: Vec<LabelInfo>,
  label_id: Counter<LabelId>,

  const_rels: IdxVec<ConstRelId, (ConstId, Vec<TirImpl>)>,
  fn_rels: IdxVec<FnRelId, (FnId, Vec<TirImpl>)>,
  closures: IdxVec<ClosureId, TirClosure>,
}

#[derive(Debug, Default)]
pub struct Resolutions {
  pub consts: IdxVec<ConcreteConstId, Tir>,
  pub fns: IdxVec<ConcreteFnId, Tir>,
  pub impls: IdxVec<ImplId, Result<ResolvedImpl, ErrorGuaranteed>>,
}

#[derive(Debug, Default)]
pub struct ResolvedImpl {
  pub consts: IdxVec<TraitConstId, Result<ConcreteConstId, ErrorGuaranteed>>,
  pub fns: IdxVec<TraitFnId, Result<ConcreteFnId, ErrorGuaranteed>>,
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
  Closure(ClosureId, Type),
}

impl<'core, 'a> Resolver<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a Chart<'core>,
    sigs: &'a mut Signatures<'core>,
    resolutions: &'a mut Resolutions,
  ) -> Self {
    Resolver {
      core,
      chart,
      sigs,
      resolutions,
      types: Types::default(),
      locals: Default::default(),
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
      const_rels: Default::default(),
      fn_rels: Default::default(),
      closures: Default::default(),
    }
  }

  pub fn resolve_all(&mut self) {
    self.resolve_since(&ChartCheckpoint::default());
  }

  pub(crate) fn resolve_since(&mut self, checkpoint: &ChartCheckpoint) {
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
      self.resolve_const_def(id);
    }
    for id in self.chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      self.resolve_fn_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.resolve_impl_def(id);
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
    let alias_def = &self.chart.type_aliases[alias_id];
    self.initialize(alias_def.def, alias_def.generics);
    let ty = self.resolve_type(&alias_def.ty, false);
    let slot = self.sigs.type_aliases.get_or_extend(alias_id);
    if slot.is_none() {
      *slot = Some(self.types.export(|t| TypeAliasSig { ty: t.transfer(&ty) }));
    }
  }

  fn resolve_const_def(&mut self, const_id: ConcreteConstId) {
    let const_def = &self.chart.concrete_consts[const_id];
    self.initialize(const_def.def, const_def.generics);
    let ty = self.types.import(&self.sigs.concrete_consts[const_id], None).ty;
    let root = self.resolve_expr_form_type(&const_def.value, Form::Value, ty);
    let tir = self.finish_tir(root);
    assert_eq!(self.resolutions.consts.next_index(), const_id);
    self.resolutions.consts.push(tir);
  }

  fn resolve_fn_def(&mut self, fn_id: ConcreteFnId) {
    let fn_def = &self.chart.concrete_fns[fn_id];
    self.initialize(fn_def.def, fn_def.generics);
    let (ty, closure_id) =
      self.resolve_closure(&fn_def.params, &fn_def.ret_ty, &fn_def.body, false);
    let root = TirExpr {
      span: fn_def.span,
      ty,
      form: Form::Value,
      kind: Box::new(TirExprKind::Closure(closure_id)),
    };
    let tir = self.finish_tir(root);
    assert_eq!(self.resolutions.fns.next_index(), fn_id);
    self.resolutions.fns.push(tir);
  }

  pub(crate) fn _resolve_custom(
    &mut self,
    def_id: DefId,
    types: &mut Types<'core>,
    locals: &BTreeMap<Local, (Ident<'core>, Type)>,
    local_count: &mut Counter<Local>,
    block: &mut Block<'core>,
  ) -> (Tir, impl Iterator<Item = (Ident<'core>, Local, Type)>) {
    self.cur_def = def_id;
    self.scope = locals
      .iter()
      .map(|(&local, &(name, ty))| {
        (name, vec![ScopeEntry { depth: 0, binding: Binding::Local(local, ty) }])
      })
      .collect();
    swap(types, &mut self.types);
    self.locals = *local_count;
    let ty = self.types.new_var(block.span);
    let root = self.resolve_stmts_type(block.span, &block.stmts, ty);
    *local_count = self.locals;
    swap(types, &mut self.types);
    (
      self.finish_tir(root),
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

  fn resolve_block(&mut self, block: &Block<'core>) -> TirExpr {
    let ty = self.types.new_var(block.span);
    self.resolve_block_type(block, ty)
  }

  fn resolve_block_type(&mut self, block: &Block<'core>, ty: Type) -> TirExpr {
    self.enter_scope();
    let block = self.resolve_stmts_type(block.span, &block.stmts, ty);
    self.exit_scope();
    block
  }

  fn resolve_stmts_type(&mut self, span: Span, stmts: &[Stmt<'core>], ty: Type) -> TirExpr {
    TirExpr {
      span,
      ty,
      form: Form::Value,
      kind: Box::new(self._resolve_stmts_type(span, stmts, ty)),
    }
  }

  fn _resolve_stmts_type(&mut self, span: Span, stmts: &[Stmt<'core>], ty: Type) -> TirExprKind {
    let [stmt, rest @ ..] = stmts else {
      let nil = self.types.nil();
      return if self.types.unify(ty, nil).is_failure() {
        TirExprKind::Error(
          self.core.report(Diag::MissingBlockExpr { span, ty: self.types.show(self.chart, ty) }),
        )
      } else {
        TirExprKind::Composite(Vec::new())
      };
    };
    match &stmt.kind {
      StmtKind::Let(l) => {
        let init = l.init.as_ref().map(|init| self.resolve_expr_form(init, Form::Value));
        let else_block = l.else_block.as_ref().map(|block| {
          let never = self.types.new(TypeKind::Never);
          self.resolve_block_type(block, never)
        });
        let bind = self.resolve_pat(&l.bind, Form::Value, l.else_block.is_some());
        if let Some(init) = &init {
          self.expect_type(init.span, init.ty, bind.ty);
        }
        if let Some(else_block) = else_block {
          TirExprKind::LetElse(
            bind,
            init.unwrap(),
            else_block,
            self.resolve_stmts_type(span, rest, ty),
          )
        } else {
          TirExprKind::Let(bind, init, self.resolve_stmts_type(span, rest, ty))
        }
      }
      StmtKind::LetFn(l) => {
        let (fn_ty, id) = self.resolve_closure(&l.params, &l.ret, &l.body, true);
        self.bind(l.name, Binding::Closure(id, fn_ty));
        self._resolve_stmts_type(span, rest, ty)
      }
      StmtKind::Expr(expr, semi) => {
        if !semi && rest.is_empty() {
          *self.resolve_expr_form_type(expr, Form::Value, ty).kind
        } else {
          let expr = self.resolve_expr_form(expr, Form::Value);
          TirExprKind::Seq(expr, self.resolve_stmts_type(span, rest, ty))
        }
      }
      StmtKind::Item(_) | StmtKind::Empty => self._resolve_stmts_type(span, rest, ty),
    }
  }

  fn resolve_closure(
    &mut self,
    params: &[Pat<'core>],
    ret: &Option<Ty<'core>>,
    body: &Block<'core>,
    inferred_ret: bool,
  ) -> (Type, ClosureId) {
    let old_labels = take(&mut self.labels);
    let old_loops = take(&mut self.loops);
    self.enter_scope();
    let params = params.iter().map(|p| self.resolve_pat(p, Form::Value, false)).collect::<Vec<_>>();
    let ret = ret.as_ref().map(|t| self.resolve_type(t, true)).unwrap_or_else(|| {
      if inferred_ret {
        self.types.new_var(body.span)
      } else {
        self.types.nil()
      }
    });
    let old_return_ty = self.return_ty.replace(ret);
    let body = self.resolve_block_type(body, ret);
    self.exit_scope();
    self.labels = old_labels;
    self.loops = old_loops;
    self.return_ty = old_return_ty;
    let ty = self.types.new(TypeKind::Fn(params.iter().map(|x| x.ty).collect(), ret));
    let id = self.closures.push(TirClosure { params, body });
    (ty, id)
  }

  fn resolve_type(&mut self, ty: &Ty<'core>, inference: bool) -> Type {
    let span = ty.span;
    match &ty.kind {
      TyKind::Hole if inference => self.types.new_var(span),
      TyKind::Paren(t) => self.resolve_type(t, inference),
      TyKind::Hole => self.types.error(self.core.report(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => {
        let params = args.iter().map(|arg| self.resolve_type(arg, inference)).collect();
        let ret =
          ret.as_ref().map(|ret| self.resolve_type(ret, inference)).unwrap_or(self.types.nil());
        self.types.new(TypeKind::Fn(params, ret))
      }
      TyKind::Tuple(tys) => {
        let tys = tys.iter().map(|arg| self.resolve_type(arg, inference)).collect();
        self.types.new(TypeKind::Tuple(tys))
      }
      TyKind::Object(entries) => {
        self.build_object_type(entries, |self_, t| self_.resolve_type(t, inference))
      }
      TyKind::Ref(inner) => {
        let inner = self.resolve_type(inner, inference);
        self.types.new(TypeKind::Ref(inner))
      }
      TyKind::Inverse(inner) => {
        let inner = self.resolve_type(inner, inference);
        inner.inverse()
      }
      TyKind::Path(path) => {
        if let Some(ident) = path.as_ident() {
          if let Some(&i) = self.type_param_lookup.get(&ident) {
            return self.types.new(TypeKind::Param(i, ident));
          }
        }
        let resolved = self.resolve_path_to(self.cur_def, path, "type", |d| d.type_kind);
        match resolved {
          Ok(DefTypeKind::Opaque(opaque_id)) => {
            let generics_id = self.chart.opaque_types[opaque_id].generics;
            let (type_params, _) = self.resolve_generics(path, generics_id, inference);
            self.types.new(TypeKind::Opaque(opaque_id, type_params))
          }
          Ok(DefTypeKind::Struct(struct_id)) => {
            let generics_id = self.chart.structs[struct_id].generics;
            let (type_params, _) = self.resolve_generics(path, generics_id, inference);
            self.types.new(TypeKind::Struct(struct_id, type_params))
          }
          Ok(DefTypeKind::Enum(enum_id)) => {
            let generics_id = self.chart.enums[enum_id].generics;
            let (type_params, _) = self.resolve_generics(path, generics_id, inference);
            self.types.new(TypeKind::Enum(enum_id, type_params))
          }
          Ok(DefTypeKind::Alias(type_alias_id)) => {
            let generics_id = self.chart.type_aliases[type_alias_id].generics;
            let (type_params, _) = self.resolve_generics(path, generics_id, inference);
            Resolver::new(self.core, self.chart, self.sigs, self.resolutions)
              .resolve_type_alias(type_alias_id);
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

  fn resolve_pat_sig(&mut self, pat: &Pat<'core>) -> Type {
    let span = pat.span;
    match &pat.kind {
      PatKind::Paren(inner) => self.resolve_pat_sig(inner),
      PatKind::Annotation(_, ty) => self.resolve_type(ty, false),
      PatKind::Path(path, _) => {
        let resolved = self.resolve_path_to(self.cur_def, path, "pattern", |d| d.pattern_kind);
        match resolved {
          Ok(DefPatternKind::Struct(struct_id)) => {
            let (type_params, _) =
              self.resolve_generics(path, self.chart.structs[struct_id].generics, false);
            self.types.new(TypeKind::Struct(struct_id, type_params))
          }
          Ok(DefPatternKind::Enum(enum_id, _)) => {
            let (type_params, _) =
              self.resolve_generics(path, self.chart.enums[enum_id].generics, false);
            self.types.new(TypeKind::Enum(enum_id, type_params))
          }
          Err(_) => self.types.error(self.core.report(Diag::ItemTypeHole { span })),
        }
      }
      PatKind::Ref(inner) => {
        let inner = self.resolve_pat_sig(inner);
        self.types.new(TypeKind::Ref(inner))
      }
      PatKind::Inverse(inner) => {
        let inner = self.resolve_pat_sig(inner);
        inner.inverse()
      }
      PatKind::Tuple(elements) => {
        let elements = elements.iter().map(|element| self.resolve_pat_sig(element)).collect();
        self.types.new(TypeKind::Tuple(elements))
      }
      PatKind::Object(entries) => self.build_object_type(entries, Self::resolve_pat_sig),
      PatKind::Hole | PatKind::Deref(_) => {
        self.types.error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => self.types.error(*e),
    }
  }

  fn resolve_impl_def(&mut self, impl_id: ImplId) {
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics);
    let span = impl_def.span;
    let ty = self.types.import(&self.sigs.impls[impl_id], None).ty;
    let resolved = match ty {
      ImplType::Trait(trait_id, type_params) => {
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
        Ok(ResolvedImpl { consts, fns })
      }
      ImplType::Error(err) => Err(err),
    };
    assert_eq!(self.resolutions.impls.next_index(), impl_id);
    self.resolutions.impls.push(resolved);
  }

  fn build_object_type<T>(
    &mut self,
    entries: &Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &T) -> Type,
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

  fn build_object<T, U>(
    &mut self,
    entries: &Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &T) -> U,
  ) -> Result<BTreeMap<Ident<'core>, U>, ErrorGuaranteed> {
    let mut object = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = object.insert(key.ident, f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    duplicate?;
    Ok(object)
  }

  fn resolve_struct_sig(&mut self, struct_id: StructId) {
    assert_eq!(self.sigs.structs.next_index(), struct_id);
    let struct_def = &self.chart.structs[struct_id];
    self.initialize(struct_def.def, struct_def.generics);
    let data = self.resolve_type(&struct_def.data, false);
    self.sigs.structs.push(TypeCtx { types: take(&mut self.types), inner: StructSig { data } });
  }

  fn resolve_enum_sig(&mut self, enum_id: EnumId) {
    assert_eq!(self.sigs.enums.next_index(), enum_id);
    let enum_def = &self.chart.enums[enum_id];
    self.initialize(enum_def.def, enum_def.generics);
    let variant_data = enum_def
      .variants
      .values()
      .map(|variant| variant.data.as_ref().map(|ty| self.resolve_type(ty, false)))
      .collect::<Vec<_>>()
      .into();
    self.sigs.enums.push(TypeCtx { types: take(&mut self.types), inner: EnumSig { variant_data } });
  }

  fn resolve_trait_sig(&mut self, trait_id: TraitId) {
    assert_eq!(self.sigs.traits.next_index(), trait_id);
    let trait_def = &self.chart.traits[trait_id];
    self.initialize(trait_def.def, trait_def.generics);
    let sig = TraitSig {
      consts: IdxVec::from(Vec::from_iter(trait_def.consts.values().map(|trait_const| {
        let ty = self.resolve_type(&trait_const.ty, false);
        TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } }
      }))),
      fns: IdxVec::from(Vec::from_iter(trait_def.fns.values().map(|trait_fn| {
        let (params, ret_ty) = self._resolve_fn_sig(&trait_fn.params, &trait_fn.ret_ty);
        TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } }
      }))),
    };
    self.sigs.traits.push(sig);
  }

  fn resolve_generics_sig(&mut self, generics_id: GenericsId) {
    assert_eq!(self.sigs.generics.next_index(), generics_id);
    let generics_def = &self.chart.generics[generics_id];
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
    impl_param_types.extend(generics_def.impl_params.iter().map(|p| self.resolve_trait(&p.trait_)));
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
  }

  fn resolve_const_sig(&mut self, const_id: ConcreteConstId) {
    assert_eq!(self.sigs.concrete_consts.next_index(), const_id);
    let const_def = &self.chart.concrete_consts[const_id];
    self.initialize(const_def.def, const_def.generics);
    let ty = self.resolve_type(&const_def.ty, false);
    self
      .sigs
      .concrete_consts
      .push(TypeCtx { types: take(&mut self.types), inner: ConstSig { ty } });
  }

  fn resolve_fn_sig(&mut self, fn_id: ConcreteFnId) {
    assert_eq!(self.sigs.concrete_fns.next_index(), fn_id);
    let fn_def = &self.chart.concrete_fns[fn_id];
    self.initialize(fn_def.def, fn_def.generics);
    let (params, ret_ty) = self._resolve_fn_sig(&fn_def.params, &fn_def.ret_ty);
    self
      .sigs
      .concrete_fns
      .push(TypeCtx { types: take(&mut self.types), inner: FnSig { params, ret_ty } });
  }

  fn resolve_impl_sig(&mut self, impl_id: ImplId) {
    assert_eq!(self.sigs.impls.next_index(), impl_id);
    let impl_def = &self.chart.impls[impl_id];
    self.initialize(impl_def.def, impl_def.generics);
    let ty = self.resolve_trait(&impl_def.trait_);
    self.sigs.impls.push(TypeCtx { types: take(&mut self.types), inner: ImplSig { ty } });
  }

  fn _resolve_fn_sig(
    &mut self,
    params: &[Pat<'core>],
    ret: &Option<Ty<'core>>,
  ) -> (Vec<Type>, Type) {
    let params = params.iter().map(|p| self.resolve_pat_sig(p)).collect();
    let ret = ret.as_ref().map(|t| self.resolve_type(t, false)).unwrap_or(self.types.nil());
    (params, ret)
  }

  fn resolve_trait(&mut self, trait_: &Trait<'core>) -> ImplType {
    match &trait_.kind {
      TraitKind::Path(path) => {
        match self.resolve_path_to(self.cur_def, path, "trait", |d| d.trait_kind) {
          Ok(DefTraitKind::Trait(trait_id)) => {
            let (type_params, _) =
              self.resolve_generics(path, self.chart.traits[trait_id].generics, false);
            ImplType::Trait(trait_id, type_params)
          }
          Err(diag) => ImplType::Error(self.core.report(diag)),
        }
      }
      TraitKind::Error(e) => ImplType::Error(*e),
    }
  }

  fn resolve_impl_type(&mut self, impl_: &Impl<'core>, ty: &ImplType) -> TirImpl {
    let span = impl_.span;
    match &impl_.kind {
      ImplKind::Hole => self.find_impl(span, ty),
      _ => {
        let (found_ty, impl_) = self.resolve_impl(impl_);
        if self.types.unify_impl_type(&found_ty, ty).is_failure() {
          self.core.report(Diag::ExpectedTypeFound {
            span,
            expected: self.types.show_impl_type(self.chart, ty),
            found: self.types.show_impl_type(self.chart, &found_ty),
          });
        }
        impl_
      }
    }
  }

  fn resolve_impl(&mut self, impl_: &Impl<'core>) -> (ImplType, TirImpl) {
    let span = impl_.span;
    match &impl_.kind {
      ImplKind::Path(path) => {
        if let Some(ident) = path.as_ident() {
          if let Some(&i) = self.impl_param_lookup.get(&ident) {
            let ty =
              self.types.import_with(&self.sigs.generics[self.cur_generics], None, |t, sig| {
                t.transfer(&sig.impl_params[i])
              });
            return (ty, TirImpl::Param(i));
          }
        }
        match self.resolve_path_to(self.cur_def, path, "impl", |d| d.impl_kind) {
          Ok(DefImplKind::Impl(id)) => {
            let (type_params, impl_params) =
              self.resolve_generics(path, self.chart.impls[id].generics, true);
            let ty = self.types.import(&self.sigs.impls[id], Some(&type_params)).ty;
            (ty, TirImpl::Def(id, impl_params))
          }
          Err(diag) => {
            let err = self.core.report(diag);
            (ImplType::Error(err), TirImpl::Error(err))
          }
        }
      }
      ImplKind::Hole => {
        let err = self.core.report(Diag::UnspecifiedImpl { span });
        (ImplType::Error(err), TirImpl::Error(err))
      }
      ImplKind::Error(err) => (ImplType::Error(*err), TirImpl::Error(*err)),
    }
  }

  pub fn resolve_generics(
    &mut self,
    path: &Path<'core>,
    params_id: GenericsId,
    inference: bool,
  ) -> (Vec<Type>, Vec<TirImpl>) {
    self._resolve_generics(
      path.generics.as_ref().unwrap_or(&Generics { span: path.span, ..Default::default() }),
      params_id,
      inference,
      None,
    )
  }

  fn _resolve_generics(
    &mut self,
    args: &GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
    type_params: Option<Vec<Type>>,
  ) -> (Vec<Type>, Vec<TirImpl>) {
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
      for (a, b) in type_params.iter().zip(args.types.iter()) {
        let b = self.resolve_type(b, inference);
        _ = self.types.unify(*a, b);
      }
      type_params
    } else {
      (0..type_param_count)
        .iter()
        .map(|i| {
          args.types.get(i).map(|t| self.resolve_type(t, inference)).unwrap_or_else(|| {
            if inference {
              self.types.new_var(args.span)
            } else {
              self.types.error(ErrorGuaranteed::new_unchecked())
            }
          })
        })
        .collect::<Vec<_>>()
    };
    let impl_params = if has_impl_params {
      let impl_params_types =
        self.types.import(&self.sigs.generics[params_id], Some(&type_params)).impl_params;
      if args.impls.is_empty() {
        impl_params_types.into_iter().map(|ty| self.find_impl(args.span, &ty)).collect()
      } else {
        args
          .impls
          .iter()
          .zip(impl_params_types)
          .map(|(impl_, ty)| self.resolve_impl_type(impl_, &ty))
          .collect()
      }
    } else {
      Vec::new()
    };
    (type_params, impl_params)
  }

  fn finder(&self, span: Span) -> Finder<'core, '_> {
    Finder::new(self.core, self.chart, self.sigs, self.cur_def, self.cur_generics, span)
  }

  fn find_impl(&mut self, span: Span, ty: &ImplType) -> TirImpl {
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
    label: &Option<Ident<'core>>,
    is_loop: bool,
    break_ty: Type,
    f: impl FnOnce(&mut Self) -> T,
  ) -> (LabelId, T) {
    let id = self.label_id.next();
    let info = LabelInfo { id, is_loop, break_ty };
    if is_loop {
      self.loops.push(info);
    }
    let result;
    if let Some(label) = label {
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
    (id, result)
  }

  fn error_expr(&mut self, span: Span, diag: Diag<'core>) -> TirExpr {
    let err = self.core.report(diag);
    TirExpr {
      span,
      ty: self.types.error(err),
      form: Form::Error(err),
      kind: Box::new(TirExprKind::Error(err)),
    }
  }

  fn error_pat(&mut self, span: Span, diag: Diag<'core>) -> TirPat {
    let err = self.core.report(diag);
    TirPat {
      span,
      ty: self.types.error(err),
      form: Form::Error(err),
      kind: Box::new(TirPatKind::Error(err)),
    }
  }

  fn expect_type(&mut self, span: Span, found: Type, expected: Type) {
    if self.types.unify(found, expected).is_failure() {
      self.core.report(Diag::ExpectedTypeFound {
        span,
        expected: self.types.show(self.chart, expected),
        found: self.types.show(self.chart, found),
      });
    }
  }

  fn finish_tir(&mut self, root: TirExpr) -> Tir {
    Tir {
      locals: self.locals,
      closures: take(&mut self.closures),
      const_rels: take(&mut self.const_rels),
      fn_rels: take(&mut self.fn_rels),
      root,
    }
  }
}

impl Resolutions {
  pub fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    let Resolutions { consts, fns, impls } = self;
    consts.truncate(checkpoint.concrete_consts.0);
    fns.truncate(checkpoint.concrete_fns.0);
    impls.truncate(checkpoint.impls.0);
  }
}
