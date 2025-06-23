use ivy::ast::Nets;
use vine_util::idx::IdxVec;

use crate::{
  components::{
    analyzer::analyze, charter::Charter, distiller::Distiller, emitter::Emitter, loader::Loader,
    normalizer::normalize, resolver::Resolver, specializer::Specializer,
  },
  structures::{
    chart::Chart,
    checkpoint::Checkpoint,
    core::Core,
    diag::Diag,
    resolutions::{Fragment, FragmentId, Resolutions},
    signatures::Signatures,
    specializations::Specializations,
    vir::Vir,
  },
};

pub struct Compiler<'core> {
  pub core: &'core Core<'core>,
  pub loader: Loader<'core>,
  pub chart: Chart<'core>,
  pub sigs: Signatures<'core>,
  pub resolutions: Resolutions,
  pub specs: Specializations,
  pub fragments: IdxVec<FragmentId, Fragment<'core>>,
  pub vir: IdxVec<FragmentId, Vir<'core>>,
}

impl<'core> Compiler<'core> {
  pub fn new(core: &'core Core<'core>) -> Self {
    Compiler {
      core,
      loader: Loader::new(core),
      chart: Chart::default(),
      sigs: Signatures::default(),
      resolutions: Resolutions::default(),
      specs: Specializations::default(),
      fragments: IdxVec::new(),
      vir: IdxVec::new(),
    }
  }

  pub fn compile(&mut self, hooks: impl Hooks<'core>) -> Result<Nets, Vec<Diag<'core>>> {
    let checkpoint = self.checkpoint();
    self._compile(hooks, &checkpoint).inspect_err(|_| {
      self.revert(&checkpoint);
    })
  }

  fn _compile(
    &mut self,
    mut hooks: impl Hooks<'core>,
    checkpoint: &Checkpoint,
  ) -> Result<Nets, Vec<Diag<'core>>> {
    let core = self.core;
    let root = self.loader.finish();
    core.bail()?;

    let chart = &mut self.chart;

    let mut charter = Charter { core, chart };
    charter.chart_root(root);
    hooks.chart(&mut charter);

    let mut resolver =
      Resolver::new(core, chart, &mut self.sigs, &mut self.resolutions, &mut self.fragments);
    resolver.resolve_since(checkpoint);
    hooks.resolve(&mut resolver);

    let mut distiller = Distiller::new(core, chart, &self.sigs);
    for fragment_id in self.fragments.keys_from(checkpoint.fragments) {
      let tir = &self.fragments[fragment_id].tir;
      let mut vir = distiller.distill_tir(tir);
      hooks.distill(fragment_id, &mut vir);
      let mut vir = normalize(&vir);
      analyze(self.core, tir.span, &mut vir);
      assert_eq!(self.vir.next_index(), fragment_id);
      self.vir.push(vir);
    }

    let mut specializer = Specializer {
      chart,
      resolutions: &self.resolutions,
      fragments: &self.fragments,
      specs: &mut self.specs,
      vir: &self.vir,
    };
    specializer.specialize_since(checkpoint);

    let mut emitter = Emitter::new(chart, &self.specs, &self.fragments, &self.vir);

    if let Some(main) = self.resolutions.main {
      emitter.emit_main(main);
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      emitter.emit_spec(spec_id);
    }

    hooks.emit(&mut emitter);

    core.bail()?;

    Ok(emitter.nets)
  }
}

pub trait Hooks<'core> {
  fn chart(&mut self, _charter: &mut Charter<'core, '_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'core, '_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
  fn emit(&mut self, _emitter: &mut Emitter<'core, '_>) {}
}

impl<'core> Hooks<'core> for () {}
