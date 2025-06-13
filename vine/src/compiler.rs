use ivy::ast::Nets;
use vine_util::idx::IdxVec;

use crate::{
  components::{
    analyzer::analyze,
    charter::Charter,
    distiller::Distiller,
    emitter::Emitter,
    loader::Loader,
    normalizer::normalize,
    resolver::{Resolutions, Resolver},
    specializer::{Specializations, Specializer},
  },
  structures::{
    chart::{Chart, ConcreteConstId, ConcreteFnId},
    checkpoint::Checkpoint,
    core::Core,
    diag::Diag,
    signatures::Signatures,
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
  pub const_vir: IdxVec<ConcreteConstId, Vir>,
  pub fn_vir: IdxVec<ConcreteFnId, Vir>,
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
      const_vir: IdxVec::new(),
      fn_vir: IdxVec::new(),
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

    let mut resolver = Resolver::new(core, chart, &mut self.sigs, &mut self.resolutions);
    resolver.resolve_since(checkpoint);
    hooks.resolve(&mut resolver);

    let mut specializer =
      Specializer { chart, resolutions: &self.resolutions, specs: &mut self.specs };
    specializer.specialize_since(checkpoint);
    hooks.specialize(&mut specializer);

    let mut distiller = Distiller::new(core, chart);
    let mut emitter = Emitter::new(chart, &self.specs);
    for const_id in chart.concrete_consts.keys_from(checkpoint.concrete_consts) {
      let tir = &self.resolutions.consts[const_id];
      let vir = distiller.distill_tir(tir);
      let mut vir = normalize(&vir);
      analyze(self.core, chart.concrete_consts[const_id].span, &mut vir);
      assert_eq!(self.const_vir.next_index(), const_id);
      self.const_vir.push(vir);
    }
    for fn_id in chart.concrete_fns.keys_from(checkpoint.concrete_fns) {
      let tir = &self.resolutions.fns[fn_id];
      let vir = distiller.distill_tir(tir);
      let mut vir = normalize(&vir);
      analyze(self.core, chart.concrete_fns[fn_id].span, &mut vir);
      assert_eq!(self.fn_vir.next_index(), fn_id);
      self.fn_vir.push(vir);
    }

    if let Some(main) = self.resolutions.main {
      emitter.emit_main(main);
    }

    for spec_id in self.specs.specs.keys_from(checkpoint.specs) {
      emitter.emit_spec(spec_id, &self.const_vir, &self.fn_vir);
    }

    hooks.emit(&mut distiller, &mut emitter);

    core.bail()?;

    Ok(emitter.nets)
  }
}

pub trait Hooks<'core> {
  fn chart(&mut self, _charter: &mut Charter<'core, '_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'core, '_>) {}
  fn specialize(&mut self, _specializer: &mut Specializer<'core, '_>) {}
  fn emit(&mut self, _distiller: &mut Distiller<'core, '_>, _emitter: &mut Emitter<'core, '_>) {}
}

impl<'core> Hooks<'core> for () {}
