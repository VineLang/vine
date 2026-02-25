use std::{collections::HashMap, mem::take};

use ivy::ast::{Nets, Tree};
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

  pub fn compile(&mut self, hooks: impl Hooks) -> Result<Nets, ErrorGuaranteed> {
    let checkpoint = self.checkpoint();
    self._compile(hooks, &checkpoint)
  }

  fn _compile(
    &mut self,
    hooks: impl Hooks,
    checkpoint: &Checkpoint,
  ) -> Result<Nets, ErrorGuaranteed> {
    self._check(hooks, checkpoint)?;
    Ok(self.nets_from(checkpoint))
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

  pub fn nets_from(&mut self, checkpoint: &Checkpoint) -> Nets {
    let mut nets = Nets::default();

    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let fragment = &self.fragments[fragment_id];
      let vir = &self.vir[fragment_id];
      let template = emit(self.debug, &self.chart, &self.sigs, fragment, vir, &mut self.specs);
      self.templates.push_to(fragment_id, template);
    }

    let mut specializer = Specializer {
      chart: &self.chart,
      resolutions: &self.resolutions,
      fragments: &self.fragments,
      specs: &mut self.specs,
      vir: &self.vir,
    };
    specializer.specialize_since(checkpoint);

    if let Some(main) = self.resolutions.main
      && main >= checkpoint.fragments
    {
      self.insert_main_net(&mut nets, main);
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
            spec,
            item,
          );
        }
      }
    }
    nets
  }

  pub fn entrypoint_name(&mut self, fragment_id: FragmentId) -> String {
    let path = &self.fragments[fragment_id].path;
    let vir = &self.vir[fragment_id];
    let func = vir.closures[ClosureId(0)];
    let InterfaceKind::Fn { call, .. } = vir.interfaces[func].kind else { unreachable!() };
    format!("::{}:s{}", &path[1..], call.0)
  }

  pub fn insert_main_net(&mut self, nets: &mut Nets, main: FragmentId) {
    let global = self.entrypoint_name(main);
    nets.insert("::".into(), main_net(self.debug, Tree::Global(global)));
  }
}

pub trait Hooks {
  fn chart(&mut self, _charter: &mut Charter<'_>) {}
  fn pre_resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
}

impl Hooks for () {}
