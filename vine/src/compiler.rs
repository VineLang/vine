use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::IdxVec;

use crate::{
  components::{
    analyzer::analyze, charter::Charter, distiller::Distiller, emitter::emit, loader::Loader,
    normalizer::normalize, resolver::Resolver, specializer::Specializer, synthesizer::synthesize,
  },
  features::{
    cfg::{Config, ConfigValue},
    debug::debug_main,
  },
  structures::{
    chart::Chart,
    checkpoint::Checkpoint,
    core::Core,
    diag::Diag,
    resolutions::{Fragment, FragmentId, Resolutions},
    signatures::Signatures,
    specializations::{SpecKind, Specializations},
    template::Template,
    tir::ClosureId,
    vir::{InterfaceKind, Vir},
  },
};

pub struct Compiler {
  pub core: &'static Core,
  pub config: Config,
  pub loader: Loader,
  pub chart: Chart,
  pub sigs: Signatures,
  pub resolutions: Resolutions,
  pub specs: Specializations,
  pub fragments: IdxVec<FragmentId, Fragment>,
  pub vir: IdxVec<FragmentId, Vir>,
  pub templates: IdxVec<FragmentId, Template>,
}

impl Compiler {
  pub fn new(core: &'static Core, mut config: Config) -> Self {
    config.insert(core.ident("debug"), ConfigValue::Bool(core.debug));
    Compiler {
      core,
      config,
      loader: Loader::new(core),
      chart: Chart::default(),
      sigs: Signatures::default(),
      resolutions: Resolutions::default(),
      specs: Specializations::default(),
      fragments: IdxVec::new(),
      vir: IdxVec::new(),
      templates: IdxVec::new(),
    }
  }

  pub fn compile(&mut self, hooks: impl Hooks) -> Result<Nets, Vec<Diag>> {
    let checkpoint = self.checkpoint();
    self._compile(hooks, &checkpoint).inspect_err(|_| {
      self.revert(&checkpoint);
    })
  }

  fn _compile(
    &mut self,
    mut hooks: impl Hooks,
    checkpoint: &Checkpoint,
  ) -> Result<Nets, Vec<Diag>> {
    let core = self.core;
    let root = self.loader.finish();
    core.bail()?;

    let chart = &mut self.chart;

    let mut charter = Charter { core, chart, config: &self.config };
    charter.chart_root(root);
    hooks.chart(&mut charter);

    let mut resolver =
      Resolver::new(core, chart, &mut self.sigs, &mut self.resolutions, &mut self.fragments);
    resolver.resolve_since(checkpoint);
    hooks.resolve(&mut resolver);

    let mut distiller = Distiller::new(core, chart, &self.sigs);
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let fragment = &self.fragments[fragment_id];
      let mut vir = distiller.distill_fragment(fragment);
      hooks.distill(fragment_id, &mut vir);
      let mut vir = normalize(core, chart, &self.sigs, fragment, &vir);
      analyze(self.core, fragment.tir.span, &mut vir);
      let template = emit(core, chart, fragment, &vir, &mut self.specs);
      self.vir.push_to(fragment_id, vir);
      self.templates.push_to(fragment_id, template);
    }

    let mut nets = Nets::default();

    let mut specializer = Specializer {
      core,
      chart,
      resolutions: &self.resolutions,
      fragments: &self.fragments,
      specs: &mut self.specs,
      vir: &self.vir,
      nets: &mut nets,
    };
    specializer.specialize_since(checkpoint);

    core.bail()?;

    if let Some(main) = self.resolutions.main {
      let path = &self.fragments[main].path;
      let vir = &self.vir[main];
      let func = vir.closures[ClosureId(0)];
      let InterfaceKind::Fn { call, .. } = vir.interfaces[func].kind else { unreachable!() };
      let global = format!("{path}:s{}", call.0);
      nets.insert(
        "::".into(),
        if self.core.debug {
          debug_main(Tree::Global(global))
        } else {
          Net::new(Tree::Global(global))
        },
      );
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      let spec = self.specs.specs[spec_id].as_ref().unwrap();
      match &spec.kind {
        SpecKind::Fragment(fragment_id) => {
          self.templates[*fragment_id].instantiate(&mut nets, &self.specs, spec);
        }
        SpecKind::Synthetic(item) => {
          synthesize(&mut nets, self.core, &self.chart, &self.specs, spec, item);
        }
      }
    }

    Ok(nets)
  }
}

pub trait Hooks {
  fn chart(&mut self, _charter: &mut Charter<'_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
}

impl Hooks for () {}
