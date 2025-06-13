use vine_util::idx::Idx;

use crate::{
  compiler::Compiler,
  components::{
    resolver::Resolutions,
    specializer::{SpecId, Specializations},
  },
  structures::{
    chart::{
      Builtins, Chart, ConcreteConstId, ConcreteFnId, ConstId, Def, DefId, DefImplKind,
      DefPatternKind, DefTraitKind, DefTypeKind, DefValueKind, EnumId, FnId, GenericsId, ImplId,
      ImportId, MemberKind, OpaqueTypeId, StructId, TraitId, TypeAliasId,
    },
    signatures::Signatures,
  },
};

#[derive(Default, Debug)]
pub struct Checkpoint {
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
  pub specs: SpecId,
}

impl<'core> Compiler<'core> {
  pub fn checkpoint(&self) -> Checkpoint {
    Checkpoint {
      defs: self.chart.defs.next_index(),
      imports: self.chart.imports.next_index(),
      generics: self.chart.generics.next_index(),
      concrete_consts: self.chart.concrete_consts.next_index(),
      concrete_fns: self.chart.concrete_fns.next_index(),
      opaque_types: self.chart.opaque_types.next_index(),
      type_aliases: self.chart.type_aliases.next_index(),
      structs: self.chart.structs.next_index(),
      enums: self.chart.enums.next_index(),
      traits: self.chart.traits.next_index(),
      impls: self.chart.impls.next_index(),
      specs: self.specs.specs.next_index(),
    }
  }

  pub fn revert(&mut self, checkpoint: &Checkpoint) {
    let Compiler { core: _, loader: _, chart, sigs, resolutions, specs, const_vir, fn_vir } = self;
    chart.revert(checkpoint);
    sigs.revert(checkpoint);
    resolutions.revert(checkpoint);
    specs.revert(checkpoint);
    const_vir.truncate(checkpoint.concrete_consts.0);
    fn_vir.truncate(checkpoint.concrete_fns.0);
  }
}

impl<'core> Chart<'core> {
  fn revert(&mut self, checkpoint: &Checkpoint) {
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
      main_mod,
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

    revert_idx(main_mod, checkpoint.defs);
  }
}

impl<'core> Def<'core> {
  fn revert(&mut self, checkpoint: &Checkpoint) {
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
  fn after(&self, checkpoint: &Checkpoint) -> bool {
    match *self {
      DefValueKind::Const(ConstId::Concrete(const_id)) => const_id >= checkpoint.concrete_consts,
      DefValueKind::Const(ConstId::Abstract(trait_id, _)) => trait_id >= checkpoint.traits,
      DefValueKind::Fn(FnId::Concrete(fn_id)) => fn_id >= checkpoint.concrete_fns,
      DefValueKind::Fn(FnId::Abstract(trait_id, _)) => trait_id >= checkpoint.traits,
      DefValueKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefValueKind::Enum(enum_id, _) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefTypeKind {
  fn after(&self, checkpoint: &Checkpoint) -> bool {
    match *self {
      DefTypeKind::Opaque(opaque_type_id) => opaque_type_id >= checkpoint.opaque_types,
      DefTypeKind::Alias(type_alias_id) => type_alias_id >= checkpoint.type_aliases,
      DefTypeKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefTypeKind::Enum(enum_id) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefPatternKind {
  fn after(&self, checkpoint: &Checkpoint) -> bool {
    match *self {
      DefPatternKind::Struct(struct_id) => struct_id >= checkpoint.structs,
      DefPatternKind::Enum(enum_id, _) => enum_id >= checkpoint.enums,
    }
  }
}

impl DefTraitKind {
  fn after(&self, checkpoint: &Checkpoint) -> bool {
    match *self {
      DefTraitKind::Trait(trait_id) => trait_id >= checkpoint.traits,
    }
  }
}

impl DefImplKind {
  fn after(&self, checkpoint: &Checkpoint) -> bool {
    match *self {
      DefImplKind::Impl(impl_id) => impl_id >= checkpoint.impls,
    }
  }
}

impl Builtins {
  fn revert(&mut self, checkpoint: &Checkpoint) {
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

impl<'core> Signatures<'core> {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Signatures {
      imports,
      generics,
      concrete_consts,
      concrete_fns,
      type_aliases,
      structs,
      enums,
      traits,
      impls,
    } = self;
    imports.truncate(checkpoint.imports.0);
    generics.truncate(checkpoint.generics.0);
    concrete_consts.truncate(checkpoint.concrete_consts.0);
    concrete_fns.truncate(checkpoint.concrete_fns.0);
    type_aliases.truncate(checkpoint.type_aliases.0);
    structs.truncate(checkpoint.structs.0);
    enums.truncate(checkpoint.enums.0);
    traits.truncate(checkpoint.traits.0);
    impls.truncate(checkpoint.impls.0);
  }
}

impl Resolutions {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Resolutions { consts, fns, impls, main } = self;
    consts.truncate(checkpoint.concrete_consts.0);
    fns.truncate(checkpoint.concrete_fns.0);
    impls.truncate(checkpoint.impls.0);
    revert_idx(main, checkpoint.concrete_fns);
  }
}

impl Specializations {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Specializations { consts, fns, specs } = self;
    specs.truncate(checkpoint.specs.0);
    consts.truncate(checkpoint.concrete_consts.0);
    fns.truncate(checkpoint.concrete_fns.0);
    consts.values_mut().for_each(|info| info.specs.retain(|_, s| *s < checkpoint.specs));
    fns.values_mut().for_each(|info| info.specs.retain(|_, s| *s < checkpoint.specs));
  }
}

fn revert_idx<T: Idx>(option: &mut Option<T>, checkpoint: T) {
  if option.is_some_and(|id| id >= checkpoint) {
    *option = None;
  }
}

fn revert_fn(builtin: &mut Option<FnId>, checkpoint: &Checkpoint) {
  match *builtin {
    Some(FnId::Concrete(fn_id)) if fn_id >= checkpoint.concrete_fns => *builtin = None,
    Some(FnId::Abstract(trait_id, _)) if trait_id >= checkpoint.traits => *builtin = None,
    _ => {}
  }
}
