use std::{collections::HashMap, fmt, mem::take};

use hedera::{
  guide,
  name::{FromTable, Name, NameId, Table},
  net::FlatNet,
};
use ivy::ast::Tree;
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
    tir::ClosureId,
    vir::{InterfaceKind, Vir},
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
    hooks: impl Hooks,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
  ) -> Result<(), ErrorGuaranteed> {
    let checkpoint = self.checkpoint();
    self._compile(hooks, &checkpoint, table, nets)
  }

  fn _compile(
    &mut self,
    hooks: impl Hooks,
    checkpoint: &Checkpoint,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
  ) -> Result<(), ErrorGuaranteed> {
    self._check(hooks, checkpoint)?;
    self.nets_from(checkpoint, table, nets);
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
    checkpoint: &Checkpoint,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
  ) {
    let guide = &Guide::build(table);

    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let fragment = &self.fragments[fragment_id];
      let vir = &self.vir[fragment_id];
      let template =
        emit(self.debug, &self.chart, &self.sigs, fragment, vir, &mut self.specs, guide);
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
      self.insert_main_net(table, nets, main);
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      let spec = self.specs.specs[spec_id].as_ref().unwrap();
      match &spec.kind {
        SpecKind::Fragment(fragment_id) => {
          self.templates[*fragment_id].instantiate(&mut nets, &self.specs, spec);
        }
        SpecKind::Synthetic(item) => {
          synthesize(
            &mut nets,
            self.debug,
            &self.files,
            &self.chart,
            &self.sigs,
            &self.specs,
            guide,
            spec,
            item,
          );
        }
      }
    }
  }

  pub fn entrypoint_name(&mut self, entrypoint: FragmentId, table: &mut Table) -> NameId {
    let path = &self.fragments[entrypoint].path;
    let vir = &self.vir[entrypoint];
    let func = vir.closures[ClosureId(0)];
    let InterfaceKind::Fn { call, .. } = vir.interfaces[func].kind else { unreachable!() };
    format!("::{}:s{}", &path[1..], call.0)
  }

  pub fn insert_main_net(
    &mut self,
    entrypoint: FragmentId,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
  ) {
    let global = self.entrypoint_name(entrypoint, table);
    let main = table.add_path("::");
    let main = table.add_name(main.to_name());
    nets.insert(main, main_net(self.debug, Tree::Global(global)));
  }
}

pub trait Hooks {
  fn chart(&mut self, _charter: &mut Charter<'_>) {}
  fn pre_resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
}

impl Hooks for () {}

guide!(pub(crate) Guide {
  ref_: "vi:ref",
  tuple: "vi:tuple",
  eraser: "vi:eraser",
  dup: "vi:dup",
  enum_: "vi:enum",
  n32: "vi:n32",
  n32_add: "vi:n32:add",
  f32: "vi:f32",
  f64: "vi:f64",

  fn_: "vi:fn",
  fn_fork: "vi:fn:fork",
  fn_drop: "vi:fn:drop",

  error: "vi:error",

  synthetic_tuple: "vi:synthetic:Tuple",
  synthetic_object: "vi:synthetic:Object",
  synthetic_struct: "vi:synthetic:Struct",
  synthetic_enum: "vi:synthetic:Enum",
  synthetic_if_const: "vi:synthetic:IfConst",
  synthetic_opaque : "vi:synthetic:Opaque",

  synthetic_composite_deconstruct: "vi:synthetic:composite_deconstruct",
  synthetic_composite_reconstruct: "vi:synthetic:composite_reconstruct",
  synthetic_identity: "vi:synthetic:identity",
  synthetic_enum_variant_names: "vi:synthetic:enum_variant_names",
  synthetic_enum_match: "vi:synthetic:enum_match",
  synthetic_enum_reconstruct: "vi:synthetic:enum_reconstruct",
  synthetic_const_alias: "vi:synthetic:const_alias",
  synthetic_fn_from_call: "vi:synthetic:fn_from_call",
  synthetic_call_from_fn: "vi:synthetic:call_from_fn",
  synthetic_frame: "vi:synthetic:frame",
  synthetic_debug_state: "vi:synthetic:debug_state",
  synthetic_n32: "vi:synthetic:n32",
  synthetic_string: "vi:synthetic:string",
});

impl fmt::Debug for Guide {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("Guide").finish_non_exhaustive()
  }
}
