use std::{
  collections::HashMap,
  iter,
  mem::{replace, take},
};

use hedera::{
  name::{NameId, Table},
  net::{FlatNet, Wire},
};
use vine_util::idx::{Counter, IdxVec, RangeExt};

use crate::{
  compiler::Guide,
  components::loader::FileId,
  features::enum_::enum_name,
  structures::{
    ast::{Ident, Span},
    chart::{
      Chart, ConcreteConstId, ConstId, EnumId, FnId, OpaqueTypeId, StructId, TraitConstId,
      TraitFnId, VariantId,
    },
    diag::FileInfo,
    resolutions::{ConstRelId, FnRelId, Rels},
    specializations::{Spec, Specializations},
    tir::TirImpl,
    vir::StageId,
  },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticImpl {
  Tuple(usize),
  Object(Ident, usize),
  Struct(StructId),
  Enum(EnumId),
  IfConst(ConcreteConstId),
  Opaque(OpaqueTypeId),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticItem {
  CompositeDeconstruct(usize),
  CompositeReconstruct(usize),
  Identity,
  EnumVariantNames(EnumId),
  EnumDeconstruct(EnumId),
  EnumReconstruct(EnumId),
  ConstAlias(ConcreteConstId),
  CallFn(FnId, usize, usize),
  Frame(String, Span),
  DebugState,
  N32(u32),
  String(String),
}

struct Synthesizer<'a> {
  nets: &'a mut HashMap<NameId, FlatNet>,
  files: &'a IdxVec<FileId, FileInfo>,
  chart: &'a Chart,
  specs: &'a Specializations,
  spec: &'a Spec,
  table: &'a mut Table,
  guide: &'a Guide,
  net: FlatNet,
  stages: Counter<StageId>,
  debug: bool,
}

pub fn synthesize(
  nets: &mut HashMap<NameId, FlatNet>,
  debug: bool,
  files: &IdxVec<FileId, FileInfo>,
  chart: &Chart,
  specs: &Specializations,
  spec: &Spec,
  table: &mut Table,
  guide: &Guide,
  item: &SyntheticItem,
) {
  Synthesizer {
    nets,
    debug,
    files,
    chart,
    specs,
    table,
    guide,
    spec,
    net: FlatNet::default(),
    stages: Counter::default(),
  }
  .synthesize(item);
}

impl Synthesizer<'_> {
  fn list<T>(
    &mut self,
    items: impl IntoIterator<Item = T, IntoIter: DoubleEndedIterator>,
    mut f: impl FnMut(&mut Self, T) -> Wire,
  ) -> Wire {
    let mut len = 0u32;
    let buf = self.net.wire();
    let mut cur = buf;
    for item in items {
      len += 1;
      let item = f(self, item);
      let next = self.net.wire();
      self.net.add(self.guide.tuple, cur, [item, next]);
      cur = next;
    }
    let end = cur;
    let len = self.net.make(self.guide.n32.with_data(len), []);
    self.net.make(self.guide.tuple, [len, buf, end])
  }

  fn string(&mut self, str: &str) -> Wire {
    self.list(str.chars(), |self_, char| self_.net.make(self_.guide.n32.with_data(char as u32), []))
  }

  fn synthesize(&mut self, item: &SyntheticItem) {
    self.stage(|self_| match item.clone() {
      SyntheticItem::CompositeDeconstruct(len) => self_.synthesize_composite_deconstruct(len),
      SyntheticItem::CompositeReconstruct(len) => self_.synthesize_composite_reconstruct(len),
      SyntheticItem::Identity => self_.synthesize_identity(),
      SyntheticItem::EnumVariantNames(enum_id) => self_.synthesize_enum_variant_names(enum_id),
      SyntheticItem::EnumDeconstruct(enum_id) => self_.synthesize_enum_deconstruct(enum_id),
      SyntheticItem::EnumReconstruct(enum_id) => self_.synthesize_enum_reconstruct(enum_id),
      SyntheticItem::ConstAlias(_) => self_.synthesize_const_alias(),
      SyntheticItem::CallFn(_, _, params) => self_.synthesize_call_fn(params),
      SyntheticItem::Frame(path, span) => self_.synthesize_frame(path, span),
      SyntheticItem::DebugState => self_.synthesize_debug_state(),
      SyntheticItem::N32(n32) => self_.synthesize_n32(n32),
      SyntheticItem::String(string) => self_.synthesize_string(string),
    });
  }

  fn fn_(&mut self, params: impl IntoIterator<Item = Wire>, ret: Wire) {
    let root = self.net.make(self.guide.fn_, params.into_iter().chain([ret]));
    self.const_(root)
  }

  fn const_(&mut self, value: Wire) {
    if self.debug {
      let dbg = self.net.wire();
      let borrow = self.net.make(self.guide.ref_, [dbg, dbg]);
      let root = self.net.make(self.guide.dbg, [borrow, value]);
      self.net.free.push(root);
    } else {
      self.net.free.push(value);
    }
  }

  fn synthesize_composite_deconstruct(&mut self, len: usize) {
    let [composite, ret, init, rest] = self.net.wires();
    let rest_fields = self.net.wires.chunk(len - 1);
    self.fn_([composite], ret);
    self.net.add(self.guide.tuple, composite, iter::once(init).chain(rest_fields.iter()));
    self.net.add(self.guide.tuple, ret, [init, rest]);
    self.net.add(self.guide.tuple, rest, rest_fields.iter());
  }

  fn synthesize_composite_reconstruct(&mut self, len: usize) {
    let [composite, init, rest] = self.net.wires();
    let rest_fields = self.net.wires.chunk(len - 1);
    self.fn_([init, rest], composite);
    self.net.add(self.guide.tuple, rest, rest_fields.iter());
    self.net.add(self.guide.tuple, composite, iter::once(init).chain(rest_fields.iter()));
  }

  fn synthesize_identity(&mut self) {
    let x = self.net.wire();
    self.fn_([x], x);
  }

  fn stage(&mut self, f: impl FnOnce(&mut Self)) -> NameId {
    let old_net = take(&mut self.net);
    let id = self.stages.next();
    let name = self.table.add_name(self.spec.name.clone().with_data(id.0));
    f(self);
    let net = replace(&mut self.net, old_net);
    self.nets.insert(name, net);
    name
  }

  fn synthesize_enum_variant_names(&mut self, enum_id: EnumId) {
    let names =
      self.list(self.chart.enums[enum_id].variants.values().map(|x| &*x.name.0), Self::string);
    self.const_(names)
  }

  fn synthesize_enum_deconstruct(&mut self, enum_id: EnumId) {
    let [enum_, variant] = self.net.wires();
    self.fn_([enum_], variant);
    self.match_enum(enum_id, enum_, variant, |self_, variant_id, content, ctx| {
      let variant = self_.create_variant(variant_id, content);
      self_.net.link(ctx, variant);
    });
  }

  fn synthesize_enum_reconstruct(&mut self, enum_id: EnumId) {
    let [enum_, variant] = self.net.wires();
    self.fn_([enum_], variant);
    self.match_variant(enum_id, enum_, variant, |self_, variant_id, content, ctx| {
      let variant = self_.enum_(enum_id, variant_id, content);
      self_.net.link(ctx, variant);
    });
  }

  fn synthesize_const_alias(&mut self) {
    let const_ = self.const_rel(ConstRelId(0));
    self.const_(const_);
  }

  fn synthesize_call_fn(&mut self, params: usize) {
    let [recv, tuple, ret] = self.net.wires();
    let params = self.net.wires.chunk(params);
    let mut root = self.net.make(self.guide.fn_, [recv, tuple, ret]);
    self.net.add(self.guide.tuple, recv, []);
    self.net.add(self.guide.tuple, tuple, params.iter());
    let mut call = self.net.make(self.guide.fn_, params.iter().chain([ret]));
    if self.debug {
      let dbg = self.net.wire();
      root = self.net.make(self.guide.dbg, [dbg, root]);
      call = self.net.make(self.guide.dbg, [dbg, call]);
    }
    self.net.free.push(root);
    let fn_ = self.fn_rel(FnRelId(0));
    self.net.link(fn_, call);
  }

  fn synthesize_frame(&mut self, path: String, span: Span) {
    let path = self.list(path[1..].split("::").collect::<Vec<_>>(), Self::string);
    let pos = self.files[span.file].get_pos(span.start);
    let file = self.string(pos.file);
    let line = self.net.make(self.guide.n32.with_data(pos.line as u32 + 1), []);
    let col = self.net.make(self.guide.n32.with_data(pos.col as u32 + 1), []);
    let frame = self.net.make(self.guide.tuple, [col, file, line, path]);
    let continuation = self.net.make(self.guide.n32.with_data(1u32), []);
    let entry = self.net.make(self.guide.tuple, [continuation, frame]);
    self.net.free.push(entry);
  }

  fn synthesize_debug_state(&mut self) {
    if self.debug {
      let ref_ = self.net.wire();
      let option = self.enum_(self.chart.builtins.option.unwrap(), VariantId(0), ref_);
      let fn_ = self.net.make(self.guide.fn_, [option]);
      let root = self.net.make(self.guide.dbg, [ref_, fn_]);
      self.net.free.push(root);
    } else {
      let nil = self.net.make(self.guide.tuple, []);
      let option = self.enum_(self.chart.builtins.option.unwrap(), VariantId(1), nil);
      let fn_ = self.net.make(self.guide.fn_, [option]);
      self.net.free.push(fn_);
    }
  }

  fn synthesize_n32(&mut self, n32: u32) {
    let n32 = self.net.make(self.guide.n32.with_data(n32), []);
    self.const_(n32);
  }

  fn synthesize_string(&mut self, string: String) {
    let string = self.string(&string);
    self.const_(string);
  }

  fn enum_(&mut self, enum_id: EnumId, variant_id: VariantId, content: Wire) -> Wire {
    let enum_name = enum_name(self.table, self.guide, self.chart, enum_id);
    let name = self.guide.enum_variant.with_children([enum_name]).with_data(variant_id.0);
    self.net.make(name, [content])
  }

  fn match_enum(
    &mut self,
    enum_id: EnumId,
    enum_: Wire,
    ctx: Wire,
    mut arm: impl FnMut(&mut Self, VariantId, Wire, Wire),
  ) {
    let enum_name = enum_name(self.table, self.guide, self.chart, enum_id);
    let enum_def = &self.chart.enums[enum_id];
    let stages = enum_def
      .variants
      .keys()
      .map(|i| {
        self.stage(|self_| {
          let [content, ctx] = self_.net.wires();
          let free = self_.net.make(self_.guide.interface, [content, ctx]);
          self_.net.free.push(free);
          arm(self_, i, content, ctx)
        })
      })
      .collect::<Vec<_>>();
    let name = self.guide.enum_match.with_children(iter::once(enum_name).chain(stages));
    self.net.add(name, enum_, [ctx]);
  }

  fn create_variant(&mut self, variant_id: VariantId, data: Wire) -> Wire {
    let variant_enum = self.chart.builtins.variant.unwrap();
    let mut cur = self.enum_(variant_enum, VariantId(0), data);
    for _ in 0..variant_id.0 {
      cur = self.enum_(variant_enum, VariantId(1), cur);
    }
    cur
  }

  fn match_variant(
    &mut self,
    enum_id: EnumId,
    variant: Wire,
    ctx: Wire,
    mut arm: impl FnMut(&mut Self, VariantId, Wire, Wire),
  ) {
    let mut iter = self.chart.enums[enum_id].variants.keys();
    self._match_variant(&mut iter, variant, ctx, &mut arm);
  }

  fn _match_variant(
    &mut self,
    variants: &mut impl Iterator<Item = VariantId>,
    variant: Wire,
    ctx: Wire,
    arm: &mut impl FnMut(&mut Self, VariantId, Wire, Wire),
  ) {
    if let Some(i) = variants.next() {
      self.match_enum(
        self.chart.builtins.variant.unwrap(),
        variant,
        ctx,
        |self_, v, content, ctx| {
          if v.0 == 0 {
            arm(self_, i, content, ctx)
          } else {
            self_._match_variant(variants, content, ctx, arm);
          }
        },
      );
    } else {
      self.net.link(variant, ctx);
    }
  }

  fn fn_rel(&mut self, id: FnRelId) -> Wire {
    match self.spec.rels.fns[id] {
      Ok((spec_id, stage_id)) => {
        let spec = self.specs.specs[spec_id].as_ref().unwrap();
        let name = self.table.add_name(spec.name.clone().with_data(stage_id.0));
        self.net.make(self.guide.global.with_children([name]), [])
      }
      Err(_) => self.net.make(self.guide.eraser, []),
    }
  }

  fn const_rel(&mut self, id: ConstRelId) -> Wire {
    match self.spec.rels.consts[id] {
      Ok(spec_id) => {
        let spec = self.specs.specs[spec_id].as_ref().unwrap();
        let name = self.table.add_name(spec.name.clone());
        self.net.make(self.guide.global.with_children([name]), [])
      }
      Err(_) => self.net.make(self.guide.eraser, []),
    }
  }
}

impl SyntheticImpl {
  pub fn fn_(&self, _: &Chart, fn_id: TraitFnId) -> SyntheticItem {
    match self {
      SyntheticImpl::Tuple(len) | SyntheticImpl::Object(_, len) => match fn_id {
        TraitFnId(0) => SyntheticItem::CompositeDeconstruct(*len),
        TraitFnId(1) => SyntheticItem::CompositeReconstruct(*len),
        _ => unreachable!(),
      },
      SyntheticImpl::Struct(_) => match fn_id {
        TraitFnId(0) | TraitFnId(1) => SyntheticItem::Identity,
        _ => unreachable!(),
      },
      SyntheticImpl::Enum(enum_id) => match fn_id {
        TraitFnId(0) => SyntheticItem::EnumDeconstruct(*enum_id),
        TraitFnId(1) => SyntheticItem::EnumReconstruct(*enum_id),
        _ => unreachable!(),
      },
      SyntheticImpl::Opaque(_) | SyntheticImpl::IfConst(_) => unreachable!(),
    }
  }

  pub fn const_(&self, chart: &Chart, const_id: TraitConstId) -> SyntheticItem {
    match self {
      SyntheticImpl::Opaque(opaque_ty_id) => match const_id {
        TraitConstId(0) => SyntheticItem::String(chart.opaque_types[*opaque_ty_id].name.0.clone()),
        _ => unreachable!(),
      },
      SyntheticImpl::Tuple(_) => unreachable!(),
      SyntheticImpl::Object(key, _) => match const_id {
        TraitConstId(0) => SyntheticItem::String(key.0.clone()),
        _ => unreachable!(),
      },
      SyntheticImpl::Struct(struct_id) => match const_id {
        TraitConstId(0) => SyntheticItem::String(chart.structs[*struct_id].name.0.clone()),
        _ => unreachable!(),
      },
      SyntheticImpl::Enum(enum_id) => match const_id {
        TraitConstId(0) => SyntheticItem::String(chart.enums[*enum_id].name.0.clone()),
        TraitConstId(1) => SyntheticItem::EnumVariantNames(*enum_id),
        _ => unreachable!(),
      },
      SyntheticImpl::IfConst(id) => match const_id {
        TraitConstId(0) => SyntheticItem::ConstAlias(*id),
        _ => unreachable!(),
      },
    }
  }
}

impl SyntheticItem {
  pub fn rels(&self) -> Rels {
    match self {
      SyntheticItem::CompositeDeconstruct(_)
      | SyntheticItem::CompositeReconstruct(_)
      | SyntheticItem::Identity
      | SyntheticItem::EnumVariantNames(_)
      | SyntheticItem::EnumDeconstruct(_)
      | SyntheticItem::EnumReconstruct(_)
      | SyntheticItem::Frame(..)
      | SyntheticItem::DebugState
      | SyntheticItem::N32(_)
      | SyntheticItem::String(_) => Rels::default(),
      SyntheticItem::CallFn(fn_id, impls, _) => Rels {
        consts: IdxVec::new(),
        fns: IdxVec::from([(*fn_id, (0..*impls).map(TirImpl::Param).collect())]),
      },
      SyntheticItem::ConstAlias(id) => {
        Rels { consts: IdxVec::from([(ConstId::Concrete(*id), Vec::new())]), fns: IdxVec::new() }
      }
    }
  }
}
