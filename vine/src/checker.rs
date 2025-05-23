use std::{
  collections::{BTreeMap, HashMap},
  mem::{replace, swap, take},
};

use vine_util::{
  idx::{IdxVec, IntMap, RangeExt},
  multi_iter, new_idx,
};

use crate::{
  ast::{
    Block, DynFnId, GenericArgs, Ident, Impl, ImplKind, Key, LabelId, Local, Pat, PatKind, Span,
    StmtKind, Trait, TraitKind, Ty, TyKind,
  },
  chart::{
    Chart, ChartCheckpoint, DefId, EnumDef, EnumId, GenericsDef, GenericsId, ImplDef, ImplDefId,
    ImplDefKind, StructDef, StructId, SubitemId, TraitDef, TraitDefId, TraitDefKind,
    TraitSubitemKind, TypeDefId, TypeDefKind, ValueDef, ValueDefId, ValueDefKind, VariantId,
  },
  core::Core,
  diag::{report, Diag, ErrorGuaranteed},
  finder::Finder,
  unifier::Unifier,
};

mod check_expr;
mod check_pat;
mod display_type;

#[derive(Debug)]
pub struct Checker<'core, 'a> {
  core: &'core Core<'core>,
  chart: &'a mut Chart<'core>,
  types: &'a mut ChartTypes<'core>,
  unifier: Unifier<'core>,
  locals: IntMap<Local, Var>,
  dyn_fns: IntMap<DynFnId, Type<'core>>,
  return_ty: Option<Type<'core>>,
  labels: IdxVec<LabelId, Option<Type<'core>>>,
  cur_def: DefId,
  cur_generics: GenericsId,
}

#[derive(Debug, Default)]
pub struct ChartTypes<'core> {
  pub type_defs: IdxVec<TypeDefId, Option<Type<'core>>>,
  pub struct_types: IdxVec<StructId, Type<'core>>,
  pub enum_types: IdxVec<EnumId, IdxVec<VariantId, Option<Type<'core>>>>,
  pub trait_types: IdxVec<TraitDefId, TraitInfo<'core>>,
  pub impl_param_types: IdxVec<GenericsId, Vec<Type<'core>>>,
  pub value_types: IdxVec<ValueDefId, Type<'core>>,
  pub impl_def_types: IdxVec<ImplDefId, Type<'core>>,
}

#[derive(Debug)]
pub struct TraitInfo<'core> {
  lookup: HashMap<Ident<'core>, SubitemId>,
  subitem_types: IdxVec<SubitemId, Type<'core>>,
}

impl<'core, 'a> Checker<'core, 'a> {
  pub fn new(
    core: &'core Core<'core>,
    chart: &'a mut Chart<'core>,
    types: &'a mut ChartTypes<'core>,
  ) -> Self {
    Checker {
      core,
      chart,
      types,
      unifier: Unifier::new(core),
      locals: Default::default(),
      dyn_fns: Default::default(),
      return_ty: None,
      labels: Default::default(),
      cur_def: DefId::ROOT,
      cur_generics: GenericsId::NONE,
    }
  }

  pub fn check_all(&mut self) {
    self.check_since(&ChartCheckpoint::default());
  }

  pub(crate) fn check_since(&mut self, checkpoint: &ChartCheckpoint) {
    for id in self.chart.types.keys_from(checkpoint.types) {
      self.assess_type_def(id);
    }
    for id in self.chart.structs.keys_from(checkpoint.structs) {
      self.assess_struct_type(id);
    }
    for id in self.chart.enums.keys_from(checkpoint.enums) {
      self.assess_enum_type(id);
    }
    for id in self.chart.traits.keys_from(checkpoint.traits) {
      self.assess_trait_type(id);
    }
    for id in self.chart.generics.keys_from(checkpoint.generics) {
      self.assess_impl_params(id);
    }
    for id in self.chart.values.keys_from(checkpoint.values) {
      self.assess_value_type(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.assess_impl_type(id);
    }
    for id in self.chart.values.keys_from(checkpoint.values) {
      self.check_value_def(id);
    }
    for id in self.chart.impls.keys_from(checkpoint.impls) {
      self.check_impl_def(id);
    }
  }

  fn initialize(&mut self, def_id: DefId, generics_id: GenericsId) {
    assert!(self.return_ty.is_none());
    self.labels.clear();
    self.unifier.reset();
    self.locals.clear();
    self.dyn_fns.clear();

    self.cur_def = def_id;
    self.cur_generics = generics_id;
  }

  fn assess_type_def(&mut self, type_id: TypeDefId) -> &Type<'core> {
    if let Some(Some(_)) = self.types.type_defs.get(type_id) {
      return self.types.type_defs[type_id].as_ref().unwrap();
    }
    let type_def = &mut self.chart.types[type_id];
    let prev_def = replace(&mut self.cur_def, type_def.def);
    let mut kind = take(&mut type_def.kind);
    let ty = match &mut kind {
      TypeDefKind::Taken => {
        Type::Error(self.core.report(Diag::RecursiveTypeAlias { span: type_def.span }))
      }
      TypeDefKind::Opaque => Type::Opaque(
        type_id,
        (0..self.chart.generics[type_def.generics].type_params.len()).map(Type::Param).collect(),
      ),
      TypeDefKind::Alias(ty) => self.hydrate_type(ty, false),
      TypeDefKind::Struct(struct_id) => Type::Struct(
        *struct_id,
        (0..self.chart.generics[type_def.generics].type_params.len()).map(Type::Param).collect(),
      ),
      TypeDefKind::Enum(enum_id) => Type::Enum(
        *enum_id,
        (0..self.chart.generics[type_def.generics].type_params.len()).map(Type::Param).collect(),
      ),
    };
    self.chart.types[type_id].kind = kind;
    let slot = self.types.type_defs.get_or_extend(type_id);
    if slot.is_none() {
      *slot = Some(ty);
    }
    self.cur_def = prev_def;
    slot.as_ref().unwrap()
  }

  fn check_value_def(&mut self, value_id: ValueDefId) {
    let ValueDef { def, generics, ref mut kind, .. } = self.chart.values[value_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    match &mut kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { value, .. } => {
        self.check_expr_form_type(
          value,
          Form::Value,
          &mut self.types.value_types[value_id].clone(),
        );
      }
      ValueDefKind::Fn { params, body, .. } => {
        for pat in params {
          self.check_pat(pat, Form::Value, false);
        }
        let Type::Fn(_, ret) = &self.types.value_types[value_id] else { unreachable!() };
        let mut ret = (**ret).clone();
        let old = self.return_ty.replace(ret.clone());
        self.check_block_type(body, &mut ret);
        self.return_ty = old;
      }
      ValueDefKind::Struct(..) | ValueDefKind::Enum(..) | ValueDefKind::TraitSubitem(..) => {}
    }
    self.chart.values[value_id].kind = kind;
  }

  pub(crate) fn _check_custom(
    &mut self,
    def_id: DefId,
    unifier: &mut Unifier<'core>,
    locals: &mut IntMap<Local, Var>,
    block: &mut Block<'core>,
  ) -> Type<'core> {
    self.cur_def = def_id;
    swap(unifier, &mut self.unifier);
    swap(locals, &mut self.locals);
    let ty = self.check_block(block);
    swap(unifier, &mut self.unifier);
    swap(locals, &mut self.locals);
    ty
  }

  fn check_block(&mut self, block: &mut Block<'core>) -> Type<'core> {
    let mut ty = Type::NIL;
    for stmt in block.stmts.iter_mut() {
      ty = Type::NIL;
      match &mut stmt.kind {
        StmtKind::Let(l) => {
          let refutable = l.else_block.is_some();
          let mut let_ty = self.check_pat(&mut l.bind, Form::Value, refutable);
          if let Some(value) = &mut l.init {
            self.check_expr_form_type(value, Form::Value, &mut let_ty);
          }
          if let Some(block) = &mut l.else_block {
            self.check_block_type(block, &mut Type::Never);
          }
        }
        StmtKind::DynFn(d) => {
          let params = d.params.iter_mut().map(|p| self.check_pat(p, Form::Value, false)).collect();
          let mut ret = d
            .ret
            .as_mut()
            .map(|t| self.hydrate_type(t, true))
            .unwrap_or_else(|| self.unifier.new_var(d.body.span));
          let old = self.return_ty.replace(ret.clone());
          self.check_block_type(&mut d.body, &mut ret);
          self.return_ty = old;
          self.dyn_fns.insert(d.id.unwrap(), Type::Fn(params, Box::new(ret)));
        }
        StmtKind::Expr(e, semi) => {
          ty = self.check_expr_form(e, Form::Value);
          if *semi {
            ty = Type::NIL;
          }
        }
        StmtKind::Item(_) | StmtKind::Empty => {}
      }
    }
    ty
  }

  fn check_block_type(&mut self, block: &mut Block<'core>, ty: &mut Type<'core>) {
    let mut found = self.check_block(block);
    if !self.unifier.unify(&mut found, ty) {
      self.core.report(Diag::ExpectedTypeFound {
        span: block.span,
        expected: self.display_type(ty),
        found: self.display_type(&found),
      });
    }
  }

  fn hydrate_type(&mut self, ty: &mut Ty<'core>, inference: bool) -> Type<'core> {
    let span = ty.span;
    match &mut ty.kind {
      TyKind::Hole if inference => self.unifier.new_var(span),
      TyKind::Paren(t) => self.hydrate_type(t, inference),
      TyKind::Hole => Type::Error(self.core.report(Diag::ItemTypeHole { span })),
      TyKind::Fn(args, ret) => Type::Fn(
        args.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect(),
        Box::new(ret.as_mut().map(|ret| self.hydrate_type(ret, inference)).unwrap_or(Type::NIL)),
      ),
      TyKind::Tuple(tys) => {
        Type::Tuple(tys.iter_mut().map(|arg| self.hydrate_type(arg, inference)).collect())
      }
      TyKind::Object(entries) => {
        report!(self.core, ty.kind; self.build_object_type(entries, |self_, t| self_.hydrate_type(t, inference)))
      }
      TyKind::Ref(t) => Type::Ref(Box::new(self.hydrate_type(t, inference))),
      TyKind::Inverse(t) => Type::Inverse(Box::new(self.hydrate_type(t, inference))),
      TyKind::Path(_) => unreachable!(),
      TyKind::Def(type_def_id, generics) => {
        let type_params =
          self.check_generics(generics, self.chart.types[*type_def_id].generics, inference);
        self.assess_type_def(*type_def_id).instantiate(&type_params)
      }
      TyKind::Param(n) => Type::Param(*n),
      TyKind::Error(e) => Type::Error(*e),
    }
  }

  fn hydrate_param(&mut self, pat: &mut Pat<'core>) -> Type<'core> {
    let span = pat.span;
    match &mut pat.kind {
      PatKind::Path(..) => unreachable!(),
      PatKind::Paren(inner) => self.hydrate_param(inner),
      PatKind::Annotation(_, ty) => self.hydrate_type(ty, false),
      PatKind::Struct(struct_id, generics, _) => Type::Struct(
        *struct_id,
        self.check_generics(generics, self.chart.structs[*struct_id].generics, false),
      ),
      PatKind::Enum(enum_id, _, generics, _) => Type::Enum(
        *enum_id,
        self.check_generics(generics, self.chart.enums[*enum_id].generics, false),
      ),
      PatKind::Ref(p) => Type::Ref(Box::new(self.hydrate_param(p))),
      PatKind::Inverse(p) => Type::Inverse(Box::new(self.hydrate_param(p))),
      PatKind::Tuple(t) => Type::Tuple(t.iter_mut().map(|p| self.hydrate_param(p)).collect()),
      PatKind::Object(entries) => {
        report!(self.core, pat.kind; self.build_object_type(entries, Self::hydrate_param))
      }
      PatKind::Hole | PatKind::Local(_) | PatKind::Deref(_) => {
        Type::Error(self.core.report(Diag::ItemTypeHole { span }))
      }
      PatKind::Error(e) => Type::Error(*e),
    }
  }

  fn check_impl_def(&mut self, impl_id: ImplDefId) {
    let ImplDef { span, def, generics, ref mut kind, .. } = self.chart.impls[impl_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    match &mut kind {
      ImplDefKind::Taken => unreachable!(),
      ImplDefKind::Impl { subitems, .. } => {
        let ty = self.types.impl_def_types[impl_id].clone();
        if let Type::Trait(trait_id, trait_type_params) = ty {
          let trait_subitems = &self.types.trait_types[trait_id];
          subitems.vec.sort_by_key(|s| trait_subitems.lookup.get(&s.name));
          if trait_subitems.lookup.len() > subitems.len() {
            self.core.report(Diag::IncompleteImpl { span });
          }
          for subitem in subitems.values_mut() {
            let trait_subitems = &self.types.trait_types[trait_id];
            if let Some(&subitem_id) = trait_subitems.lookup.get(&subitem.name) {
              let mut trait_ty =
                trait_subitems.subitem_types[subitem_id].instantiate(&trait_type_params);
              let mut ty = self.types.value_types[subitem.value].clone();
              if !self.unifier.unify(&mut ty, &mut trait_ty) {
                self.core.report(Diag::ExpectedTypeFound {
                  span: subitem.span,
                  expected: self.display_type(&trait_ty),
                  found: self.display_type(&ty),
                });
              }
            } else {
              self.core.report(Diag::ExtraneousImplItem { span: subitem.span, name: subitem.name });
            }
          }
        }
      }
    }
    self.chart.impls[impl_id].kind = kind;
  }

  fn build_object_type<T>(
    &mut self,
    entries: &mut Vec<(Key<'core>, T)>,
    mut f: impl FnMut(&mut Self, &mut T) -> Type<'core>,
  ) -> Result<Type<'core>, Diag<'core>> {
    let mut fields = BTreeMap::new();
    let mut duplicate = Ok(());
    for (key, value) in entries {
      let old = fields.insert(key.ident, f(self, value));
      if old.is_some() {
        duplicate = Err(self.core.report(Diag::DuplicateKey { span: key.span }));
      }
    }
    duplicate?;
    Ok(Type::Object(fields))
  }

  fn assess_struct_type(&mut self, struct_id: StructId) {
    assert_eq!(self.types.struct_types.next_index(), struct_id);
    let StructDef { def, generics, ref mut data, .. } = self.chart.structs[struct_id];
    let mut data = take(data);
    self.initialize(def, generics);
    let data_ty = self.hydrate_type(&mut data, false);
    self.types.struct_types.push(data_ty);
    self.chart.structs[struct_id].data = data;
  }

  fn assess_enum_type(&mut self, enum_id: EnumId) {
    assert_eq!(self.types.enum_types.next_index(), enum_id);
    let EnumDef { def, generics, ref mut variants, .. } = self.chart.enums[enum_id];
    let mut variants = take(variants);
    self.initialize(def, generics);
    let tys = variants
      .values_mut()
      .map(|variant| variant.data.as_mut().map(|ty| self.hydrate_type(ty, false)))
      .collect::<Vec<_>>()
      .into();
    self.types.enum_types.push(tys);
    self.chart.enums[enum_id].variants = variants;
  }

  fn assess_trait_type(&mut self, trait_id: TraitDefId) {
    assert_eq!(self.types.trait_types.next_index(), trait_id);
    let TraitDef { def, generics, ref mut kind, .. } = self.chart.traits[trait_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let tys = match &mut kind {
      TraitDefKind::Taken => unreachable!(),
      TraitDefKind::Trait { subitems } => TraitInfo {
        lookup: subitems.iter_mut().map(|(id, subitem)| (subitem.name, id)).collect(),
        subitem_types: subitems
          .values_mut()
          .map(|subitem| match &mut subitem.kind {
            TraitSubitemKind::Fn(params, ret) => self.assess_fn_sig(params, ret),
            TraitSubitemKind::Const(ty) => self.hydrate_type(ty, false),
          })
          .collect::<Vec<_>>()
          .into(),
      },
    };
    self.types.trait_types.push(tys);
    self.chart.traits[trait_id].kind = kind;
  }

  fn assess_impl_params(&mut self, generics_id: GenericsId) {
    assert_eq!(self.types.impl_param_types.next_index(), generics_id);
    let GenericsDef { def, ref mut impl_params, .. } = self.chart.generics[generics_id];
    let mut impl_params = take(impl_params);
    self.initialize(def, generics_id);
    let impl_param_types = impl_params.iter_mut().map(|(_, t)| self.assess_trait(t)).collect();
    self.types.impl_param_types.push(impl_param_types);
    self.chart.generics[generics_id].impl_params = impl_params;
  }

  fn assess_value_type(&mut self, value_id: ValueDefId) {
    assert_eq!(self.types.value_types.next_index(), value_id);
    let ValueDef { def, generics, ref mut kind, .. } = self.chart.values[value_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let ty = match &mut kind {
      ValueDefKind::Taken => unreachable!(),
      ValueDefKind::Const { ty, .. } => self.hydrate_type(ty, false),
      ValueDefKind::Fn { params, ret, .. } => self.assess_fn_sig(params, ret),
      ValueDefKind::Struct(struct_id) => {
        let struct_def = &self.chart.structs[*struct_id];
        let generics = &self.chart.generics[struct_def.generics];
        let data = &self.types.struct_types[*struct_id];
        Type::Fn(
          vec![data.clone()],
          Box::new(Type::Struct(
            *struct_id,
            (0..generics.type_params.len()).map(Type::Param).collect(),
          )),
        )
      }
      ValueDefKind::Enum(enum_id, variant_id) => {
        let enum_def = &self.chart.enums[*enum_id];
        let generics = &self.chart.generics[enum_def.generics];
        let data = &self.types.enum_types[*enum_id][*variant_id];
        let enum_ty =
          Type::Enum(*enum_id, (0..generics.type_params.len()).map(Type::Param).collect());
        if let Some(data) = data {
          Type::Fn(vec![data.clone()], Box::new(enum_ty))
        } else {
          enum_ty
        }
      }
      ValueDefKind::TraitSubitem(trait_def_id, subitem_id) => {
        self.types.trait_types[*trait_def_id].subitem_types[*subitem_id].clone()
      }
    };
    self.types.value_types.push(ty);
    self.chart.values[value_id].kind = kind;
  }

  fn assess_impl_type(&mut self, impl_id: ImplDefId) {
    assert_eq!(self.types.impl_def_types.next_index(), impl_id);
    let ImplDef { def, generics, ref mut kind, .. } = self.chart.impls[impl_id];
    let mut kind = take(kind);
    self.initialize(def, generics);
    let ty = match &mut kind {
      ImplDefKind::Taken => unreachable!(),
      ImplDefKind::Impl { trait_, .. } => self.assess_trait(trait_),
    };
    self.types.impl_def_types.push(ty);
    self.chart.impls[impl_id].kind = kind;
  }

  fn assess_fn_sig(
    &mut self,
    params: &mut [Pat<'core>],
    ret: &mut Option<Ty<'core>>,
  ) -> Type<'core> {
    Type::Fn(
      params.iter_mut().map(|p| self.hydrate_param(p)).collect(),
      Box::new(ret.as_mut().map(|t| self.hydrate_type(t, false)).unwrap_or(Type::NIL)),
    )
  }

  fn assess_trait(&mut self, trait_: &mut Trait<'core>) -> Type<'core> {
    match &mut trait_.kind {
      TraitKind::Path(_) => unreachable!(),
      TraitKind::Def(id, generics) => {
        let type_params = self.check_generics(generics, self.chart.traits[*id].generics, false);
        Type::Trait(*id, type_params)
      }
      TraitKind::Error(e) => Type::Error(*e),
    }
  }

  fn check_impl_type(&mut self, impl_: &mut Impl<'core>, ty: &mut Type<'core>) {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Hole => {
        *impl_ = self.find_impl(span, ty);
      }
      _ => {
        let mut found = self.check_impl(impl_);
        if !self.unifier.unify(&mut found, ty) {
          self.core.report(Diag::ExpectedTypeFound {
            span: impl_.span,
            expected: self.display_type(ty),
            found: self.display_type(&found),
          });
        }
      }
    }
  }

  fn check_impl(&mut self, impl_: &mut Impl<'core>) -> Type<'core> {
    let span = impl_.span;
    match &mut impl_.kind {
      ImplKind::Path(_) => unreachable!(),
      ImplKind::Hole => Type::Error(self.core.report(Diag::UnspecifiedImpl { span })),
      ImplKind::Param(n) => self.types.impl_param_types[self.cur_generics][*n].clone(),
      ImplKind::Def(id, generics) => {
        let type_params = self.check_generics(generics, self.chart.impls[*id].generics, true);
        self.types.impl_def_types[*id].instantiate(&type_params)
      }
      ImplKind::Error(e) => Type::Error(*e),
    }
  }

  pub fn check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
  ) -> Vec<Type<'core>> {
    self._check_generics(args, params_id, inference, None)
  }

  fn _check_generics(
    &mut self,
    args: &mut GenericArgs<'core>,
    params_id: GenericsId,
    inference: bool,
    pre_type_params: Option<Vec<Type<'core>>>,
  ) -> Vec<Type<'core>> {
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
    if !args.impls.is_empty() {
      check_count(args.impls.len(), params.impl_params.len(), "impl");
    }
    let has_impl_params = !params.impl_params.is_empty();
    let mut type_params = (0..params.type_params.len())
      .iter()
      .map(|i| {
        args.types.get_mut(i).map(|t| self.hydrate_type(t, inference)).unwrap_or_else(|| {
          if inference {
            self.unifier.new_var(args.span)
          } else {
            Type::Error(ErrorGuaranteed::new_unchecked())
          }
        })
      })
      .collect::<Vec<_>>();
    if let Some(mut type_param_hints) = pre_type_params {
      self.unifier.import(args.span, &mut type_param_hints);
      for (mut hint, ty) in type_param_hints.into_iter().zip(type_params.iter_mut()) {
        _ = self.unifier.unify(&mut hint, ty);
      }
    }
    if has_impl_params {
      let impl_params_types = self.types.impl_param_types[params_id]
        .iter()
        .map(|t| t.instantiate(&type_params))
        .collect::<Vec<_>>();
      if args.impls.is_empty() {
        args.impls =
          impl_params_types.into_iter().map(|mut ty| self.find_impl(args.span, &mut ty)).collect();
      } else {
        for (impl_, mut param_type) in args.impls.iter_mut().zip(impl_params_types.into_iter()) {
          self.check_impl_type(impl_, &mut param_type);
        }
      }
    }
    type_params
  }

  fn finder(&mut self, span: Span) -> Finder<'core, '_> {
    Finder {
      chart: self.chart,
      initial_checkpoint: self.unifier.checkpoint(),
      unifier: &mut self.unifier,
      types: self.types,
      span,
      source: self.cur_def,
      generics: self.cur_generics,
      steps: 0,
    }
  }

  fn find_impl(&mut self, span: Span, ty: &mut Type<'core>) -> Impl<'core> {
    let results = self.finder(span).find_impl(ty);
    let diag = if let Ok(mut results) = results {
      if results.len() == 1 {
        let mut result = results.pop().unwrap();
        _ = self.unifier.unify(ty, &mut result.1);
        return result.0;
      } else if results.is_empty() {
        Diag::CannotFindImpl { span, ty: self.display_type(ty) }
      } else {
        Diag::AmbiguousImpl { span, ty: self.display_type(ty) }
      }
    } else {
      Diag::SearchLimit { span, ty: self.display_type(ty) }
    };
    Impl { span, kind: ImplKind::Error(self.core.report(diag)) }
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

new_idx!(pub Var);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'core> {
  Tuple(Vec<Type<'core>>),
  Object(BTreeMap<Ident<'core>, Type<'core>>),
  Fn(Vec<Type<'core>>, Box<Type<'core>>),
  Ref(Box<Type<'core>>),
  Inverse(Box<Type<'core>>),
  Struct(StructId, Vec<Type<'core>>),
  Enum(EnumId, Vec<Type<'core>>),
  Trait(TraitDefId, Vec<Type<'core>>),
  Opaque(TypeDefId, Vec<Type<'core>>),
  Param(usize),
  Var(Var),
  Fresh(Var),
  Never,
  Error(ErrorGuaranteed),
}

impl<'core> Type<'core> {
  pub const NIL: Type<'static> = Type::Tuple(Vec::new());

  pub(crate) fn instantiate(&self, params: &[Type<'core>]) -> Type<'core> {
    match self {
      Type::Tuple(tys) => Type::Tuple(tys.iter().map(|t| t.instantiate(params)).collect()),
      Type::Object(entries) => {
        Type::Object(entries.iter().map(|(&k, t)| (k, t.instantiate(params))).collect())
      }
      Type::Fn(tys, ret) => Type::Fn(
        tys.iter().map(|t| t.instantiate(params)).collect(),
        Box::new(ret.instantiate(params)),
      ),
      Type::Ref(t) => Type::Ref(Box::new(t.instantiate(params))),
      Type::Inverse(t) => Type::Inverse(Box::new(t.instantiate(params))),
      Type::Opaque(id, tys) => {
        Type::Opaque(*id, tys.iter().map(|t| t.instantiate(params)).collect())
      }
      Type::Struct(def, tys) => {
        Type::Struct(*def, tys.iter().map(|t| t.instantiate(params)).collect())
      }
      Type::Enum(def, tys) => Type::Enum(*def, tys.iter().map(|t| t.instantiate(params)).collect()),
      Type::Trait(def, tys) => {
        Type::Trait(*def, tys.iter().map(|t| t.instantiate(params)).collect())
      }
      Type::Param(n) => params[*n].clone(),
      Type::Error(e) => Type::Error(*e),
      Type::Never => Type::Never,
      Type::Var(_) | Type::Fresh(_) => unreachable!(),
    }
  }

  pub(crate) fn children_mut(&mut self) -> impl Iterator<Item = &mut Type<'core>> {
    multi_iter! { Iter { Zero, One, Vec, Object, Fn } }
    match self {
      Type::Param(_) | Type::Var(_) | Type::Fresh(_) | Type::Never | Type::Error(_) => {
        Iter::Zero([])
      }
      Type::Ref(inner) | Type::Inverse(inner) => Iter::One([&mut **inner]),
      Type::Tuple(tys)
      | Type::Opaque(_, tys)
      | Type::Struct(_, tys)
      | Type::Enum(_, tys)
      | Type::Trait(_, tys) => Iter::Vec(tys),
      Type::Object(fields) => Iter::Object(fields.values_mut()),
      Type::Fn(params, ret) => Iter::Fn(params.iter_mut().chain([&mut **ret])),
    }
  }

  fn inverse(self) -> Self {
    match self {
      Type::Inverse(t) => *t,
      _ => Type::Inverse(Box::new(self)),
    }
  }

  pub fn invert_if(self, invert: bool) -> Self {
    if invert {
      self.inverse()
    } else {
      self
    }
  }

  pub fn get_mod(&self, chart: &Chart) -> Result<Option<DefId>, ErrorGuaranteed> {
    Ok(match self {
      Type::Opaque(type_id, _) => Some(chart.types[*type_id].def),
      Type::Struct(struct_id, _) => Some(chart.structs[*struct_id].def),
      Type::Enum(enum_id, _) => Some(chart.enums[*enum_id].def),
      Type::Trait(trait_id, _) => Some(chart.traits[*trait_id].def),
      Type::Tuple(_)
      | Type::Object(_)
      | Type::Fn(..)
      | Type::Ref(_)
      | Type::Inverse(_)
      | Type::Param(_)
      | Type::Never => None,
      Type::Error(e) => Err(*e)?,
      Type::Var(_) | Type::Fresh(_) => None,
    })
  }

  pub(crate) fn receiver(&self) -> Option<&Type<'core>> {
    match self {
      Type::Fn(args, _) => args.first().map(|t| match t {
        Type::Ref(t) => t,
        _ => t,
      }),
      _ => None,
    }
  }
}

impl Default for Type<'_> {
  fn default() -> Self {
    Self::NIL
  }
}

impl From<ErrorGuaranteed> for Type<'_> {
  fn from(value: ErrorGuaranteed) -> Self {
    Type::Error(value)
  }
}

impl<'core> ChartTypes<'core> {
  pub fn revert(&mut self, chart: &ChartCheckpoint) {
    self.type_defs.truncate(chart.types.0);
    self.struct_types.truncate(chart.structs.0);
    self.enum_types.truncate(chart.enums.0);
    self.trait_types.truncate(chart.traits.0);
    self.impl_param_types.truncate(chart.generics.0);
    self.value_types.truncate(chart.values.0);
    self.impl_def_types.truncate(chart.impls.0);
  }
}
