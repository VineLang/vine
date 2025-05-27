use vine_util::idx::Idx;

use crate::chart::ConstKind;

use super::{
  Builtins, Chart, ConcreteConstId, ConcreteFnId, Def, DefId, DefImplKind, DefPatternKind,
  DefTraitKind, DefTypeKind, DefValueKind, EnumId, FnKind, GenericsId, ImplId, ImportId,
  MemberKind, OpaqueTypeId, StructId, TraitId, TypeAliasId,
};

#[derive(Default, Debug)]
pub struct ChartCheckpoint {
  pub defs: DefId,
  pub imports: ImportId,
  pub generics: GenericsId,
  pub concrete_consts: ConcreteConstId,
  pub concrete_fns: ConcreteFnId,
  pub opaque_types: OpaqueTypeId,
  pub type_aliases: TypeAliasId,
  pub structs: StructId,
  pub enums: EnumId,
  pub traits: TraitId,
  pub impls: ImplId,
}

impl<'core> Chart<'core> {
  pub fn checkpoint(&self) -> ChartCheckpoint {
    ChartCheckpoint {
      defs: self.defs.next_index(),
      imports: self.imports.next_index(),
      generics: self.generics.next_index(),
      concrete_consts: self.concrete_consts.next_index(),
      concrete_fns: self.concrete_fns.next_index(),
      opaque_types: self.opaque_types.next_index(),
      type_aliases: self.type_aliases.next_index(),
      structs: self.structs.next_index(),
      enums: self.enums.next_index(),
      traits: self.traits.next_index(),
      impls: self.impls.next_index(),
    }
  }

  pub fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    let Chart {
      defs,
      imports,
      generics,
      concrete_consts: consts,
      concrete_fns: fns,
      opaque_types,
      type_aliases,
      structs,
      enums,
      traits,
      impls,
      builtins,
    } = self;
    defs.truncate(checkpoint.defs.0);
    imports.truncate(checkpoint.imports.0);
    generics.truncate(checkpoint.generics.0);
    consts.truncate(checkpoint.concrete_consts.0);
    fns.truncate(checkpoint.concrete_fns.0);
    opaque_types.truncate(checkpoint.opaque_types.0);
    type_aliases.truncate(checkpoint.type_aliases.0);
    structs.truncate(checkpoint.structs.0);
    enums.truncate(checkpoint.enums.0);
    traits.truncate(checkpoint.traits.0);
    impls.truncate(checkpoint.impls.0);

    for def in defs.values_mut() {
      def.revert(checkpoint);
    }

    builtins.revert(checkpoint);
  }
}

impl<'core> Def<'core> {
  fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    self.members.retain(|_, member| match member.kind {
      MemberKind::Child(id) => id < checkpoint.defs,
      MemberKind::Import(id) => id < checkpoint.imports,
    });
    if self.value_kind.is_some_and(|k| k.kind.after(checkpoint)) {
      self.value_kind = None;
    }
    if self.type_kind.is_some_and(|k| k.kind.after(checkpoint)) {
      self.type_kind = None;
    }
    if self.pattern_kind.is_some_and(|k| k.kind.after(checkpoint)) {
      self.pattern_kind = None;
    }
    if self.trait_kind.is_some_and(|k| k.kind.after(checkpoint)) {
      self.trait_kind = None;
    }
    if self.impl_kind.is_some_and(|k| k.kind.after(checkpoint)) {
      self.impl_kind = None;
    }
  }
}

impl DefValueKind {
  fn after(&self, checkpoint: &ChartCheckpoint) -> bool {
    match *self {
      DefValueKind::Const(ConstKind::Concrete(const_id)) => const_id >= checkpoint.concrete_consts,
      DefValueKind::Const(ConstKind::Abstract(trait_id, _)) => trait_id >= checkpoint.traits,
      DefValueKind::Fn(FnKind::Concrete(fn_id)) => fn_id >= checkpoint.concrete_fns,
      DefValueKind::Fn(FnKind::Abstract(trait_id, _)) => trait_id >= checkpoint.traits,
      DefValueKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefValueKind::Enum(enum_id, _) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefTypeKind {
  fn after(&self, checkpoint: &ChartCheckpoint) -> bool {
    match *self {
      DefTypeKind::Opaque(opaque_type_id) => opaque_type_id >= checkpoint.opaque_types,
      DefTypeKind::Alias(type_alias_id) => type_alias_id >= checkpoint.type_aliases,
      DefTypeKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefTypeKind::Enum(enum_id) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefPatternKind {
  fn after(&self, checkpoint: &ChartCheckpoint) -> bool {
    match *self {
      DefPatternKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefPatternKind::Enum(enum_id, _) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefTraitKind {
  fn after(&self, checkpoint: &ChartCheckpoint) -> bool {
    match *self {
      DefTraitKind::Trait(trait_id) => trait_id >= checkpoint.traits,
    }
  }
}

impl DefImplKind {
  fn after(&self, checkpoint: &ChartCheckpoint) -> bool {
    match *self {
      DefImplKind::Impl(impl_id) => impl_id >= checkpoint.impls,
    }
  }
}

impl Builtins {
  fn revert(&mut self, checkpoint: &ChartCheckpoint) {
    let Builtins {
      prelude,
      bool,
      n32,
      i32,
      f32,
      char,
      io,
      list,
      string,
      result,
      neg,
      not,
      bool_not,
      cast,
      binary_ops,
      comparison_ops,
      fork,
      drop,
      range,
      bound_exclusive,
      bound_inclusive,
      bound_unbounded,
    } = self;
    revert_idx(prelude, checkpoint.defs);
    revert_idx(bool, checkpoint.opaque_types);
    revert_idx(n32, checkpoint.opaque_types);
    revert_idx(f32, checkpoint.opaque_types);
    revert_idx(i32, checkpoint.opaque_types);
    revert_idx(char, checkpoint.opaque_types);
    revert_idx(io, checkpoint.opaque_types);
    revert_idx(list, checkpoint.structs);
    revert_idx(string, checkpoint.structs);
    revert_idx(result, checkpoint.enums);
    revert_fn(neg, checkpoint);
    revert_fn(not, checkpoint);
    revert_idx(bool_not, checkpoint.impls);
    revert_fn(cast, checkpoint);
    binary_ops.values_mut().for_each(|op| revert_fn(op, checkpoint));
    comparison_ops.values_mut().for_each(|op| revert_fn(op, checkpoint));
    revert_idx(fork, checkpoint.traits);
    revert_idx(drop, checkpoint.traits);
    revert_idx(range, checkpoint.structs);
    revert_idx(bound_exclusive, checkpoint.structs);
    revert_idx(bound_inclusive, checkpoint.structs);
    revert_idx(bound_unbounded, checkpoint.structs);
  }
}

fn revert_idx<T: Idx>(option: &mut Option<T>, checkpoint: T) {
  if option.is_some_and(|id| id >= checkpoint) {
    *option = None;
  }
}

fn revert_fn(builtin: &mut Option<FnKind>, checkpoint: &ChartCheckpoint) {
  match *builtin {
    Some(FnKind::Concrete(fn_id)) if fn_id >= checkpoint.concrete_fns => *builtin = None,
    Some(FnKind::Abstract(trait_id, _)) if trait_id >= checkpoint.traits => *builtin = None,
    _ => {}
  }
}
