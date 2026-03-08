use std::{collections::HashMap, fmt, mem::take};

use hedera::{
  guide,
  name::{FromTable, NameId, Table},
  net::FlatNet,
  translate::{
    Rule, Translator,
    common::{chain_binary, map_name, replace_name, replace_nilary, replace_path},
  },
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

    let vi = Guide::build(table);
    let ivm = IvmGuide::build(table);

    let translator = Translator::new((
      replace_name([vi.ref_, vi.fn_, vi.tuple, vi.dbg, vi.interface], ivm.x),
      replace_name([vi.dup], ivm.y),
      chain_binary([ivm.x, ivm.y]),
      replace_nilary([ivm.x, ivm.y, vi.eraser], ivm.eraser),
      replace_path([vi.global], ivm.global),
      replace_path([vi.n32], ivm.n32),
      map_name([vi.bool_if], move |_, name| {
        ivm.branch.with_children([name.children[1], name.children[0]])
      }),
      Rule([vi.enum_variant], move |node, _, net| {
        let tag = net.make(ivm.n32.with_data(node.name.data), []);
        net.add(ivm.x, node.pri, [tag, node.aux[0]]);
      }),
      Rule([vi.enum_match], move |mut node, _, net| {
        let enum_ = node.pri;
        let [ctx] = *node.aux else { unreachable!() };
        node.name.children.remove(0); // remove first child which specifies the enum
        let branches = node.name.children;
        let [tag, content] = net.wires();
        net.add(ivm.x, enum_, [tag, content]);
        let ctx = net.make(ivm.x, [content, ctx]);
        net.add(ivm.branch.with_children(branches), enum_, [ctx]);
      }),
    ));

    translator.translate_all(table, nets);

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
      self.insert_main_net(main, table, nets);
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
  }

  pub fn entrypoint_name(&mut self, entrypoint: FragmentId, table: &mut Table) -> NameId {
    let spec_id = self.specs.lookup[entrypoint][&Vec::new()];
    let spec = self.specs.specs[spec_id].as_ref().unwrap();
    table.add_name(spec.name.clone())
  }

  pub fn insert_main_net(
    &mut self,
    entrypoint: FragmentId,
    table: &mut Table,
    nets: &mut HashMap<NameId, FlatNet>,
  ) {
    let global = self.entrypoint_name(entrypoint, table);
    let main = table.add_path("vi:main");
    let main = table.add_name(main.to_name());
    nets.insert(main, main_net(self.debug, global, &Guide::build(table)));
  }
}

pub trait Hooks {
  fn chart(&mut self, _charter: &mut Charter<'_>) {}
  fn pre_resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn resolve(&mut self, _resolver: &mut Resolver<'_>) {}
  fn distill(&mut self, _fragment_id: FragmentId, _vir: &mut Vir) {}
}

impl Hooks for () {}

guide!(pub IvmGuide {
  x: "ivm:x",
  y: "ivm:y",
  global: "ivm:global",
  eraser: "ivm:eraser",
  n32: "ivm:n32",
  branch: "ivm:branch",
});

guide!(pub Guide {
  ref_: "vi:ref",
  tuple: "vi:tuple",
  interface: "vi:interface",
  eraser: "vi:eraser",
  dup: "vi:dup",
  enum_: "vi:enum",
  enum_variant: "vi:enum:variant",
  enum_match: "vi:enum:match",
  n32: "vi:n32",
  n32_add: "vi:n32:add",
  bool: "vi:bool",
  bool_not: "vi:bool:not",
  bool_and: "vi:bool:and",
  bool_if: "vi:bool:if",
  f32: "vi:f32",
  f64: "vi:f64",
  dbg: "vi:dbg",
  fn_: "vi:fn",
  global: "vi:global",

  io_split: "vi:io:split",
  io_merge: "vi:io:merge",

  error: "vi:error",

  closure: "vi:closure",
  closure_fork: "vi:closure:fork",
  closure_drop: "vi:closure:drop",

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
  synthetic_enum_deconstruct: "vi:synthetic:enum_deconstruct",
  synthetic_enum_reconstruct: "vi:synthetic:enum_reconstruct",
  synthetic_const_alias: "vi:synthetic:const_alias",
  synthetic_call_fn: "vi:synthetic:call_fn",
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
