use std::{collections::HashMap, fmt, mem::take};

use ivy::{
  guide,
  name::{FromTable, NameId, Table},
  net::FlatNet,
  translate::Translator,
};
use vine_util::idx::IdxVec;

use crate::{
  components::{
    analyzer::analyze,
    charter::Charter,
    distiller::Distiller,
    emitter::{emit, main_net},
    finder::FinderCache,
    loader::{FileId, Module},
    normalizer::normalize,
    resolver::Resolver,
    specializer::Specializer,
    synthesizer::synthesize,
  },
  structures::{
    annotations::Annotations,
    ast::Span,
    chart::{Chart, VisId},
    checkpoint::Checkpoint,
    diag::{Diag, Diags, ErrorGuaranteed, FileInfo},
    resolutions::{Fragment, FragmentId, Resolutions},
    signatures::Signatures,
    specializations::{SpecKind, Specializations},
    template::Template,
    vir::Vir,
  },
};

#[derive(Default)]
pub struct Compiler {
  pub debug: bool,
  pub config: HashMap<String, String>,
  pub files: IdxVec<FileId, FileInfo>,
  pub loaded: Vec<Module>,
  pub chart: Chart,
  pub sigs: Signatures,
  pub resolutions: Resolutions,
  pub annotations: Annotations,
  pub specs: Specializations,
  pub fragments: IdxVec<FragmentId, Fragment>,
  pub vir: IdxVec<FragmentId, Vir>,
  pub templates: IdxVec<FragmentId, Template>,
  pub finder_cache: FinderCache,
  pub diags: Diags,
}

impl Compiler {
  pub fn new(debug: bool, config: HashMap<String, String>) -> Self {
    Compiler { debug, config, ..Default::default() }
  }

  pub fn check(&mut self, hooks: impl Hooks) -> Result<(), ErrorGuaranteed> {
    let checkpoint = self.checkpoint();
    self._check(hooks, &checkpoint)
  }

  pub fn compile(
    &mut self,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
    hooks: impl Hooks,
  ) -> Result<(), ErrorGuaranteed> {
    let checkpoint = self.checkpoint();
    self._compile(table, nets, hooks, &checkpoint)
  }

  fn _compile(
    &mut self,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
    hooks: impl Hooks,
    checkpoint: &Checkpoint,
  ) -> Result<(), ErrorGuaranteed> {
    self._check(hooks, checkpoint)?;
    self.nets_from(table, nets, checkpoint);
    Ok(())
  }

  fn _check(
    &mut self,
    mut hooks: impl Hooks,
    checkpoint: &Checkpoint,
  ) -> Result<(), ErrorGuaranteed> {
    let loaded = take(&mut self.loaded);

    self.diags.bail()?;

    let chart = &mut self.chart;

    let mut charter = Charter { chart, diags: &mut self.diags, annotations: &mut self.annotations };
    charter.chart(loaded);
    hooks.chart(&mut charter);

    let mut resolver = Resolver::new(
      &self.config,
      chart,
      &mut self.sigs,
      &mut self.diags,
      &mut self.resolutions,
      &mut self.annotations,
      &mut self.fragments,
      &mut self.finder_cache,
    );
    hooks.pre_resolve(&mut resolver);
    resolver.resolve_since(checkpoint);
    hooks.resolve(&mut resolver);

    let mut distiller = Distiller::new(chart, &self.sigs, &mut self.diags, &mut self.finder_cache);
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let fragment = &self.fragments[fragment_id];
      let mut vir = distiller.distill_fragment(fragment);
      hooks.distill(fragment_id, &mut vir);
      let mut vir =
        normalize(chart, &self.sigs, distiller.diags, distiller.finder_cache, fragment, &vir);
      analyze(chart, distiller.diags, fragment.tir.span, &mut vir);
      self.vir.push_to(fragment_id, vir);
    }

    for def in self.chart.defs.values() {
      for member in &def.named_members {
        if member.span != Span::NONE
          && !matches!(member.vis, VisId::Pub)
          && !self.annotations.references.contains_key(&member.span)
        {
          self.diags.warn(Diag::UnusedItem { span: member.span });
        }
      }
    }

    self.diags.bail()?;

    Ok(())
  }

  pub fn nets_from(
    &mut self,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
    checkpoint: &Checkpoint,
  ) {
    let guide = &Guide::build(table);

    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let fragment = &self.fragments[fragment_id];
      let vir = &self.vir[fragment_id];
      let template = emit(self.debug, &self.chart, fragment, vir, &mut self.specs, table, guide);
      self.templates.push_to(fragment_id, template);
    }

    let mut specializer = Specializer {
      chart: &self.chart,
      resolutions: &self.resolutions,
      fragments: &self.fragments,
      specs: &mut self.specs,
      vir: &self.vir,
      guide,
      table,
    };
    specializer.specialize_since(checkpoint);

    if let Some(main) = self.resolutions.main
      && main >= checkpoint.fragments
    {
      self.insert_main_net(table, nets, main, &Translator::default());
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      let spec = self.specs.specs[spec_id].as_ref().unwrap();
      match &spec.kind {
        SpecKind::Fragment(fragment_id) => {
          self.templates[*fragment_id].instantiate(table, guide, nets, &self.specs, spec);
        }
        SpecKind::Synthetic(item) => {
          synthesize(
            nets,
            self.debug,
            &self.files,
            &self.chart,
            &self.specs,
            spec,
            table,
            guide,
            item,
          );
        }
      }
    }

    if cfg!(debug_assertions) {
      FlatNet::assert_valid(nets, table);
    }
  }

  pub fn entrypoint_name(&mut self, table: &mut Table, entrypoint: FragmentId) -> NameId {
    let spec_id = self.specs.lookup[entrypoint][&Vec::new()];
    let spec = self.specs.specs[spec_id].as_ref().unwrap();
    table.add_name(spec.name.clone())
  }

  pub fn insert_main_net(
    &mut self,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
    entrypoint: FragmentId,
    translator: &Translator<'_>,
  ) {
    let entrypoint = self.entrypoint_name(table, entrypoint);
    let main = table.add_path_name("iv:main");
    let mut net = main_net(self.debug, entrypoint, &Guide::build(table));
    translator.translate(table, &mut net);
    nets.insert(main, net);
  }
}

pub trait Hooks {
  fn chart(&mut self, _charter: &mut Charter<'_>) {}
  fn pre_resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
}

impl Hooks for () {}

guide!(pub Guide {
  ref_: PathId = "vi:ref",
  tuple: PathId = "vi:tuple",
  interface: PathId = "vi:interface",
  eraser: PathId = "vi:eraser",
  dup: PathId = "vi:dup",
  enum_: PathId = "vi:enum",
  enum_variant: PathId = "vi:enum:variant",
  enum_match: PathId = "vi:enum:match",
  ext: PathId = "vi:ext",
  n32: PathId = "vi:n32",
  n32_add: NameId = "vi:n32:add",
  bool: PathId = "vi:bool",
  bool_if: PathId = "vi:bool:if",
  f32: PathId = "vi:f32",
  f64: PathId = "vi:f64",
  f64_from_bits: NameId = "vi:f64:from_bits",
  dbg: PathId = "vi:dbg",
  fn_: PathId = "vi:fn",
  graft: PathId = "vi:graft",
  graft_lazy: PathId = "vi:graft:lazy",
  black_box: PathId = "iv:black_box",

  n32_and: NameId = "vi:n32:and",
  n32_or: NameId = "vi:n32:or",
  n32_xor: NameId = "vi:n32:xor",
  n32_eq: NameId = "vi:n32:eq",
  n32_le: NameId = "vi:n32:le",
  n32_lt: NameId = "vi:n32:lt",

  bool_and: NameId = "vi:bool:and",
  bool_or: NameId = "vi:bool:or",
  bool_xor: NameId = "vi:bool:xor",
  bool_eq: NameId = "vi:bool:eq",
  bool_le: NameId = "vi:bool:le",
  bool_lt: NameId = "vi:bool:lt",
  bool_not: NameId = "vi:bool:not",
  bool_to_n32: NameId = "vi:bool:to_n32",

  main: NameId = "iv:main",

  io_split: NameId = "vi:io:split",
  io_merge: NameId = "vi:io:merge",

  error: PathId = "vi:error",

  closure: PathId = "vi:closure",
  closure_fork: PathId = "vi:closure:fork",
  closure_drop: PathId = "vi:closure:drop",

  synthetic_tuple: PathId = "vi:synthetic:Tuple",
  synthetic_object: PathId = "vi:synthetic:Object",
  synthetic_struct: PathId = "vi:synthetic:Struct",
  synthetic_enum: PathId = "vi:synthetic:Enum",
  synthetic_if_const: PathId = "vi:synthetic:IfConst",
  synthetic_opaque: PathId = "vi:synthetic:Opaque",

  synthetic: PathId = "vi:synthetic",
  synthetic_composite_deconstruct: PathId = "vi:synthetic:composite_deconstruct",
  synthetic_composite_reconstruct: PathId = "vi:synthetic:composite_reconstruct",
  synthetic_identity: PathId = "vi:synthetic:identity",
  synthetic_enum_variant_names: PathId = "vi:synthetic:enum_variant_names",
  synthetic_enum_deconstruct: PathId = "vi:synthetic:enum_deconstruct",
  synthetic_enum_reconstruct: PathId = "vi:synthetic:enum_reconstruct",
  synthetic_const_alias: PathId = "vi:synthetic:const_alias",
  synthetic_call_fn: PathId = "vi:synthetic:call_fn",
  synthetic_frame: PathId = "vi:synthetic:frame",
  synthetic_debug_state: PathId = "vi:synthetic:debug_state",
  synthetic_n32: PathId = "vi:synthetic:n32",
  synthetic_string: PathId = "vi:synthetic:string",
});

impl fmt::Debug for Guide {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Guide").finish_non_exhaustive()
  }
}
