use vine_util::idx::Idx;

use crate::{
  compiler::Compiler,
  components::{
    finder::{FinderCache, candidates::CandidateSetId},
    loader::FileId,
  },
  features::builtin::Builtins,
  structures::{
    annotations::Annotations,
    chart::{
      Chart, ConcreteConstId, ConcreteFnId, ConstId, Def, DefId, DefImplKind, DefPatternKind,
      DefTraitKind, DefTypeKind, DefValueKind, EnumId, FnId, GenericsId, ImplId, ImportId,
      MemberKind, OpaqueTypeId, StructId, TraitId, TypeAliasId,
    },
    resolutions::{FragmentId, Resolutions},
    signatures::Signatures,
    specializations::{SpecId, Specializations},
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
  pub fragments: FragmentId,
  pub specs: SpecId,
  pub files: FileId,
  pub candidate_sets: CandidateSetId,
  pub errors: usize,
  pub warnings: usize,
}

impl Compiler {
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
      fragments: self.fragments.next_index(),
      specs: self.specs.specs.next_index(),
      files: self.loader.files.next_index(),
      candidate_sets: self.finder_cache.candidates.sets.next_index(),
      errors: self.diags.errors.len(),
      warnings: self.diags.warnings.len(),
    }
  }

  pub fn revert(&mut self, checkpoint: &Checkpoint) {
    let Compiler {
      debug: _,
      config: _,
      loader,
      chart,
      sigs,
      resolutions,
      annotations,
      specs,
      fragments,
      vir,
      templates,
      diags,
      finder_cache,
    } = self;
    chart.revert(checkpoint);
    sigs.revert(checkpoint);
    resolutions.revert(checkpoint);
    annotations.revert(checkpoint);
    specs.revert(checkpoint);
    loader.revert(checkpoint);
    diags.revert(checkpoint);
    finder_cache.revert(checkpoint);
    fragments.truncate(checkpoint.fragments.0);
    vir.truncate(checkpoint.fragments.0);
    templates.truncate(checkpoint.fragments.0);
  }
}

impl Chart {
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
      top_level,
      tests,
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
    top_level.retain(|_, v| *v < checkpoint.defs);
    tests.retain(|concrete_fn_id| *concrete_fn_id < checkpoint.concrete_fns);
  }
}

impl Def {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    self.members_lookup.retain(|_, member| match member.kind {
      MemberKind::Child(id) => id < checkpoint.defs,
      MemberKind::Import(id) => id < checkpoint.imports,
    });
    self.named_members.retain(|member| match member.kind {
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
      nat,
      char,
      io,
      list,
      string,
      option,
      result,
      pos,
      neg,
      not,
      bool_not,
      cast,
      binary_ops,
      comparison_ops,
      fn_,
      fork,
      drop,
      duplicate,
      erase,
      range,
      bound_exclusive,
      bound_inclusive,
      bound_unbounded,
      advance,
      iter,
      tuple,
      object,
      struct_,
      enum_,
      variant,
      debug_state,
      show,
      show_to_string,
    } = self;
    revert_idx(prelude, checkpoint.defs);
    revert_idx(bool, checkpoint.opaque_types);
    revert_idx(n32, checkpoint.opaque_types);
    revert_idx(f32, checkpoint.opaque_types);
    revert_idx(i32, checkpoint.opaque_types);
    revert_idx(nat, checkpoint.structs);
    revert_idx(char, checkpoint.opaque_types);
    revert_idx(io, checkpoint.opaque_types);
    revert_idx(list, checkpoint.structs);
    revert_idx(string, checkpoint.structs);
    revert_idx(option, checkpoint.enums);
    revert_idx(result, checkpoint.enums);
    revert_fn(pos, checkpoint);
    revert_fn(neg, checkpoint);
    revert_fn(not, checkpoint);
    revert_idx(bool_not, checkpoint.impls);
    revert_fn(cast, checkpoint);
    binary_ops.values_mut().for_each(|op| revert_fn(op, checkpoint));
    comparison_ops.values_mut().for_each(|op| revert_fn(op, checkpoint));
    revert_idx(fn_, checkpoint.traits);
    revert_idx(fork, checkpoint.traits);
    revert_idx(drop, checkpoint.traits);
    revert_idx(duplicate, checkpoint.impls);
    revert_idx(erase, checkpoint.impls);
    revert_idx(range, checkpoint.structs);
    revert_idx(bound_exclusive, checkpoint.structs);
    revert_idx(bound_inclusive, checkpoint.structs);
    revert_idx(bound_unbounded, checkpoint.structs);
    revert_fn(advance, checkpoint);
    revert_fn(iter, checkpoint);
    revert_idx(tuple, checkpoint.traits);
    revert_idx(object, checkpoint.traits);
    revert_idx(struct_, checkpoint.traits);
    revert_idx(enum_, checkpoint.traits);
    revert_idx(variant, checkpoint.enums);
    revert_fn(debug_state, checkpoint);
    revert_idx(show, checkpoint.traits);
    revert_fn(show_to_string, checkpoint);
  }
}

impl Signatures {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Signatures {
      imports,
      type_params,
      impl_params,
      concrete_consts,
      concrete_fns,
      type_aliases,
      structs,
      enums,
      traits,
      impls,
    } = self;
    imports.truncate(checkpoint.imports.0);
    type_params.truncate(checkpoint.generics.0);
    impl_params.truncate(checkpoint.generics.0);
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
    revert_idx(main, checkpoint.fragments);
  }
}

impl Annotations {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Annotations { references, definitions, hovers } = self;
    for map in [references, definitions] {
      map.retain(|k, v| {
        if k.file < checkpoint.files {
          v.retain(|s| s.file < checkpoint.files);
          true
        } else {
          false
        }
      });
    }
    hovers.retain(|k, _| k.file < checkpoint.files);
  }
}

impl Specializations {
  fn revert(&mut self, checkpoint: &Checkpoint) {
    let Specializations { lookup, specs, synthetic } = self;
    specs.truncate(checkpoint.specs.0);
    lookup.truncate(checkpoint.fragments.0);
    lookup.values_mut().for_each(|map| map.retain(|_, s| *s < checkpoint.specs));
    synthetic.retain(|_, s| *s < checkpoint.specs);
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

impl FinderCache {
  pub(crate) fn revert(&mut self, checkpoint: &Checkpoint) {
    self.candidates.by_def.retain(|(def, _), _| *def < checkpoint.defs);
    self.candidates.sets.truncate(checkpoint.candidate_sets.0);
  }
}
