use ivy::ast::Nets;
use vine_util::idx::IdxVec;

use crate::{
  analyzer::analyze,
  chart::{checkpoint::ChartCheckpoint, Chart, ConcreteConstId, ConcreteFnId},
  charter::Charter,
  checker::Checker,
  core::Core,
  distiller::Distiller,
  emitter::Emitter,
  loader::Loader,
  normalizer::normalize,
  resolver::Resolver,
  signatures::Signatures,
  specializer::{SpecId, Specializations, Specializer},
  vir::VIR,
};

pub struct Compiler<'core> {
  core: &'core Core<'core>,
  pub loader: Loader<'core>,
  pub chart: Chart<'core>,
  pub sigs: Signatures<'core>,
  specs: Specializations<'core>,
  const_vir: IdxVec<ConcreteConstId, VIR>,
  fn_vir: IdxVec<ConcreteFnId, VIR>,
}

impl<'core> Compiler<'core> {
  pub fn new(core: &'core Core<'core>) -> Self {
    Compiler {
      core,
      loader: Loader::new(core),
      chart: Chart::default(),
      sigs: Signatures::default(),
      specs: Specializations::default(),
      const_vir: IdxVec::new(),
      fn_vir: IdxVec::new(),
    }
  }

  pub fn compile(&mut self, hooks: impl Hooks<'core>) -> Result<Nets, String> {
    let checkpoint = self.checkpoint();
    self._compile(hooks, &checkpoint).inspect_err(|_| {
      self.revert(&checkpoint);
    })
  }

  fn _compile(
    &mut self,
    mut hooks: impl Hooks<'core>,
    checkpoint: &CompilerCheckpoint,
  ) -> Result<Nets, String> {
    let core = self.core;
    let root = self.loader.finish();
    core.bail()?;

    let chart = &mut self.chart;

    let mut charter = Charter { core, chart };
    charter.chart_root(root);
    hooks.chart(&mut charter);

    let mut resolver = Resolver { core, chart };
    resolver.resolve_since(&checkpoint.chart);
    hooks.resolve(&mut resolver);

    let mut checker = Checker::new(core, chart, &mut self.sigs);
    checker.check_since(&checkpoint.chart);
    hooks.check(&mut checker);

    core.bail()?;

    let mut specializer = Specializer { chart, specs: &mut self.specs };
    specializer.specialize_since(&checkpoint.chart);
    hooks.specialize(&mut specializer);

    let mut distiller = Distiller::new(chart);
    let mut emitter = Emitter::new(chart, &self.specs);
    for const_id in chart.concrete_consts.keys_from(checkpoint.chart.concrete_consts) {
      let const_def = &chart.concrete_consts[const_id];
      let vir = distiller.distill_const_def(const_def);
      let mut vir = normalize(&vir);
      analyze(&mut vir);
      assert_eq!(self.const_vir.next_index(), const_id);
      self.const_vir.push(vir);
    }
    for fn_id in chart.concrete_fns.keys_from(checkpoint.chart.concrete_fns) {
      let fn_def = &chart.concrete_fns[fn_id];
      let vir = distiller.distill_fn_def(fn_def);
      let mut vir = normalize(&vir);
      analyze(&mut vir);
      assert_eq!(self.fn_vir.next_index(), fn_id);
      self.fn_vir.push(vir);
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      emitter.emit_spec(spec_id, &self.const_vir, &self.fn_vir);
    }

    hooks.emit(&mut distiller, &mut emitter);

    Ok(emitter.nets)
  }

  fn checkpoint(&self) -> CompilerCheckpoint {
    CompilerCheckpoint { chart: self.chart.checkpoint(), specs: self.specs.specs.next_index() }
  }

  fn revert(&mut self, checkpoint: &CompilerCheckpoint) {
    self.chart.revert(&checkpoint.chart);
    self.sigs.revert(&checkpoint.chart);
    self.specs.revert(&checkpoint.chart, checkpoint.specs);
    self.const_vir.truncate(checkpoint.chart.concrete_consts.0);
    self.fn_vir.truncate(checkpoint.chart.concrete_fns.0);
  }
}

pub trait Hooks<'core> {
  fn chart(&mut self, _charter: &mut Charter<'core, '_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'core, '_>) {}
  fn check(&mut self, _checker: &mut Checker<'core, '_>) {}
  fn specialize(&mut self, _specializer: &mut Specializer<'core, '_>) {}
  fn emit(&mut self, _distiller: &mut Distiller<'core, '_>, _emitter: &mut Emitter<'core, '_>) {}
}

impl<'core> Hooks<'core> for () {}

#[derive(Debug, Default)]
pub struct CompilerCheckpoint {
  pub chart: ChartCheckpoint,
  pub specs: SpecId,
}
