use std::mem::take;

use hedera::net::{FlatNet, FlatNode, Wire};
use ivy::ast::{Net, Nets};
use vine_util::idx::{Counter, IdxVec, RangeExt};

use crate::{
  compiler::Guide,
  components::loader::FileId,
  structures::{
    ast::{Ident, Span},
    chart::{
      Chart, ConcreteConstId, ConstId, EnumId, FnId, OpaqueTypeId, StructId, TraitConstId,
      TraitFnId, TraitId, VariantId,
    },
    diag::FileInfo,
    resolutions::{ConstRelId, FnRel, FnRelId, Rels},
    signatures::Signatures,
    specializations::{Spec, Specializations},
    template::global_name,
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
  EnumMatch(EnumId),
  EnumReconstruct(EnumId),
  ConstAlias(ConcreteConstId),
  FnFromCall(usize),
  CallFromFn(usize),
  Frame(String, Span),
  DebugState,
  N32(u32),
  String(String),
}

struct Synthesizer<'a> {
  nets: &'a mut Nets,
  files: &'a IdxVec<FileId, FileInfo>,
  chart: &'a Chart,
  sigs: &'a Signatures,
  specs: &'a Specializations,
  spec: &'a Spec,
  guide: &'a Guide,
  net: FlatNet,
  stages: Counter<StageId>,
  debug: bool,
}

pub fn synthesize(
  nets: &mut Nets,
  debug: bool,
  files: &IdxVec<FileId, FileInfo>,
  chart: &Chart,
  sigs: &Signatures,
  specs: &Specializations,
  spec: &Spec,
  guide: &Guide,
  item: &SyntheticItem,
) {
  Synthesizer {
    nets,
    debug,
    files,
    chart,
    sigs,
    specs,
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
      self.net.push(FlatNode(self.guide.tuple, [cur, item, next]));
      cur = next;
    }
    let end = cur;
    let len = self.net.insert(self.guide.n32.with_data(len), []);
    self.net.insert(self.guide.tuple, [len, buf, end])
  }

  fn string(&mut self, str: &str) -> Wire {
    self
      .list(str.chars(), |self_, char| self_.net.insert(self_.guide.n32.with_data(char as u32), []))
  }

  fn synthesize(&mut self, item: &SyntheticItem) {
    let stage = self.new_stage();
    let net = match item.clone() {
      SyntheticItem::CompositeDeconstruct(len) => self.synthesize_composite_deconstruct(len),
      SyntheticItem::CompositeReconstruct(len) => self.synthesize_composite_reconstruct(len),
      SyntheticItem::Identity => self.synthesize_identity(),
      SyntheticItem::EnumVariantNames(enum_id) => self.synthesize_enum_variant_names(enum_id),
      SyntheticItem::EnumMatch(enum_id) => self.synthesize_enum_match(enum_id),
      SyntheticItem::EnumReconstruct(enum_id) => self.synthesize_enum_reconstruct(enum_id),
      SyntheticItem::ConstAlias(_) => self.synthesize_const_alias(),
      SyntheticItem::FnFromCall(params) => self.synthesize_fn_from_call(params),
      SyntheticItem::CallFromFn(params) => self.synthesize_call_from_fn(params),
      SyntheticItem::Frame(path, span) => self.synthesize_frame(path, span),
      SyntheticItem::DebugState => self.synthesize_debug_state(),
      SyntheticItem::N32(n32) => self.synthesize_n32(n32),
      SyntheticItem::String(string) => self.synthesize_string(string),
    };
    self.nets.insert(stage, net);
  }

  fn do_fn(&mut self, params: impl IntoIterator<Item = Wire>, ret: Wire) {
    todo!()
  }

  fn synthesize_composite_deconstruct(&mut self, len: usize) {
    let [composite, ret, init, rest] = self.net.wires();
    let rest_fields = self.net.wires.chunk(len - 1);
    self.do_fn([composite], ret);
    self.net.add(self.guide.tuple, [composite, init].into_iter().chain(rest_fields.iter()));
    self.net.add(self.guide.tuple, [ret, init, rest]);
    self.net.add(self.guide.tuple, [rest].into_iter().chain(rest_fields.iter()));
  }

  fn synthesize_composite_reconstruct(&mut self, len: usize) {
    let [composite, init, rest] = self.net.wires();
    let rest_fields = self.net.wires.chunk(len - 1);
    self.do_fn([init, rest], composite);
    self.net.add(self.guide.tuple, [rest].into_iter().chain(rest_fields.iter()));
    self.net.add(self.guide.tuple, [composite, init].into_iter().chain(rest_fields.iter()));
  }

  fn synthesize_identity(&mut self) {
    let x = self.net.wire();
    self.do_fn([x], x);
  }

  fn new_stage(&mut self) -> String {
    let id = self.stages.next();
    global_name(self.spec, id)
  }

  fn synthesize_enum_variant_names(&mut self, enum_id: EnumId) {
    let names =
      self.list(self.chart.enums[enum_id].variants.values().map(|x| &*x.name.0), Self::string);
    self.const_(names)
  }

  fn synthesize_enum_match(&mut self, enum_id: EnumId) {
    let fn_ = self.fn_rel(FnRelId(0));
    let f = self.net.wire();
    let t = self.net.wire();
    let dbg = self.net.wire();
    Net::new(Tree::n_ary(
      "fn",
      [
        Tree::n_ary("dbg", self.debug.then_some(dbg.0).into_iter().chain([Tree::Erase])),
        self.match_enum(
          enum_id,
          |self_, variant_id| {
            let has_data = !self_.sigs.enums[enum_id].inner.variant_is_nil[variant_id];
            let data = self_.net.wire();
            let f = self_.net.wire();
            let t = self_.net.wire();
            let dbg = self_.net.wire();
            Net {
              root: Tree::n_ary(
                "enum",
                has_data.then_some(data.0).into_iter().chain([Tree::n_ary(
                  "x",
                  self_.debug.then_some(dbg.0).into_iter().chain([f, t]),
                )]),
              ),
              pairs: Vec::from([(
                fn_.clone(),
                Tree::n_ary(
                  "fn",
                  [
                    Tree::n_ary("dbg", self_.debug.then_some(dbg.1).into_iter().chain([f])),
                    self_.variant(variant_id, has_data.then_some(data.1)),
                    t,
                  ],
                ),
              )]),
            }
          },
          Tree::n_ary("x", self.debug.then_some(dbg.1).into_iter().chain([f, t])),
        ),
        f,
        t,
      ],
    ))
  }

  fn synthesize_enum_reconstruct(&mut self, enum_id: EnumId) {
    let mut cur_net = Net::new(Tree::Erase);
    for (variant_id, is_nil) in self.sigs.enums[enum_id].inner.variant_is_nil.iter().rev() {
      let has_data = !*is_nil;
      let mut prev_net = Some(cur_net);
      let out = self.net.wire();
      let match_ = self.match_enum(
        self.chart.builtins.variant.unwrap(),
        |self_, v| {
          let data = self_.net.wire();
          if v == 0 {
            Net::new(Tree::n_ary(
              "enum",
              [
                if has_data { data.0 } else { Tree::Erase },
                self_.enum_(enum_id, variant_id, |_| has_data.then_some(data.1)),
              ],
            ))
          } else {
            prev_net.take().unwrap()
          }
        },
        out.0,
      );
      if variant_id.0 == 0 {
        cur_net = Net::new(Tree::n_ary("fn", [self.fn_receiver(), match_, out.1]));
      } else {
        cur_net = Net::new(Tree::n_ary("enum", [match_, out.1]));
      }
    }
    cur_net
  }

  fn synthesize_const_alias(&self) {
    Net::new(self.const_rel(ConstRelId(0)))
  }

  fn synthesize_fn_from_call(&mut self, params: usize) {
    let (fn_, call) = self._fn_call(params);
    Net { root: fn_, pairs: vec![(self.fn_rel(FnRelId(0)), call)] }
  }

  fn synthesize_call_from_fn(&mut self, params: usize) {
    let (fn_, call) = self._fn_call(params);
    Net { root: call, pairs: vec![(self.fn_rel(FnRelId(0)), fn_)] }
  }

  fn _fn_call(&mut self, params: usize) -> (Wire, Wire) {
    let dbg = if self.debug {
      let wire = self.net.wire();
      (Some(wire.0).into_iter(), Some(wire.1).into_iter())
    } else {
      (None.into_iter(), None.into_iter())
    };
    let recv = self.net.wire();
    let params = (0..params).map(|_| self.net.wire()).collect::<(Vec<_>, Vec<_>)>();
    let ret = self.net.wire();
    (
      Tree::n_ary(
        "fn",
        [Tree::n_ary("dbg", dbg.0.chain([recv.0]))].into_iter().chain(params.0).chain([ret.0]),
      ),
      Tree::n_ary(
        "fn",
        [
          Tree::n_ary("dbg", dbg.1.chain([Tree::Erase])),
          recv.1,
          Tree::n_ary("tup", params.1),
          ret.1,
        ],
      ),
    )
  }

  fn synthesize_frame(&mut self, path: String, span: Span) {
    let path = self.list(path[1..].split("::").collect::<Vec<_>>(), Self::string);
    let pos = self.files[span.file].get_pos(span.start);
    let file = self.string(pos.file);
    let line = Tree::N32(pos.line as u32 + 1);
    let col = Tree::N32(pos.col as u32 + 1);
    Net::new(Tree::n_ary("tup", [Tree::N32(1), Tree::n_ary("tup", [col, file, line, path])]))
  }

  fn synthesize_debug_state(&mut self) {
    if self.debug {
      let x = self.net.wire();
      let y = self.net.wire();
      Net::new(Tree::n_ary(
        "fn",
        [
          Tree::n_ary("dbg", [x, Tree::Erase]),
          Tree::n_ary("enum", [Tree::n_ary("enum", [x, y]), Tree::Erase, y]),
        ],
      ))
    } else {
      let x = self.net.wire();
      Net::new(Tree::n_ary("fn", [Tree::Erase, Tree::n_ary("enum", [Tree::Erase, x, x])]))
    }
  }

  fn synthesize_n32(&mut self, n32: u32) {
    Net::new(Tree::N32(n32))
  }

  fn synthesize_string(&mut self, string: String) {
    Net::new(self.string(&string))
  }

  fn enum_(
    &mut self,
    enum_id: EnumId,
    variant_id: VariantId,
    content: impl FnOnce(&mut Self) -> Option<Wire>,
  ) -> Wire {
    let enum_def = &self.chart.enums[enum_id];
    let wire = self.net.wire();
    let mut variant = Tree::n_ary("enum", content(self).into_iter().chain([wire.0]));
    Tree::n_ary(
      "enum",
      (0..enum_def.variants.len())
        .map(|i| if variant_id.0 == i { take(&mut variant) } else { Tree::Erase })
        .chain([wire.1]),
    )
  }

  fn match_enum(
    &mut self,
    enum_id: EnumId,
    mut arm: impl FnMut(&mut Self, VariantId),
    ctx: Wire,
  ) -> Wire {
    let enum_def = &self.chart.enums[enum_id];
    Tree::n_ary(
      "enum",
      enum_def
        .variants
        .keys()
        .map(|i| {
          let stage = self.new_stage();
          let net = arm(self, i);
          self.nets.insert(stage.clone(), net);
          Tree::Global(stage)
        })
        .chain([ctx]),
    )
  }

  fn variant(&mut self, variant_id: VariantId, data: Option<Wire>) -> Wire {
    let variant_enum = self.chart.builtins.variant.unwrap();
    let mut cur = self.enum_(variant_enum, VariantId(0), |_| Some(data.unwrap_or(Tree::Erase)));
    for _ in 0..variant_id.0 {
      cur = self.enum_(variant_enum, VariantId(1), |_| Some(cur));
    }
    cur
  }

  fn fn_rel(&self, id: FnRelId) -> Wire {
    match self.spec.rels.fns[id] {
      Ok((spec_id, stage_id)) => {
        Tree::Global(global_name(self.specs.specs[spec_id].as_ref().unwrap(), stage_id))
      }
      Err(_) => Tree::Erase,
    }
  }

  fn const_rel(&self, id: ConstRelId) -> Wire {
    match self.spec.rels.consts[id] {
      Ok(spec_id) => {
        Tree::Global(global_name(self.specs.specs[spec_id].as_ref().unwrap(), StageId(0)))
      }
      Err(_) => Tree::Erase,
    }
  }

  fn fn_receiver(&mut self) -> Wire {
    if self.debug {
      let w = self.net.wire();
      Tree::Comb(
        "dbg".into(),
        Box::new(Tree::Comb("ref".into(), Box::new(w), Box::new(w))),
        Box::new(Tree::Erase),
      )
    } else {
      Tree::Erase
    }
  }

  fn const_(&mut self, value: Wire) {
    if self.debug {
      let w = self.net.wire();
      Net::new(Tree::n_ary("dbg", [Tree::n_ary("ref", [w, w]), value]))
    } else {
      Net::new(value)
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
        TraitFnId(0) => SyntheticItem::EnumMatch(*enum_id),
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
      | SyntheticItem::EnumReconstruct(_)
      | SyntheticItem::Frame(..)
      | SyntheticItem::DebugState
      | SyntheticItem::N32(_)
      | SyntheticItem::String(_) => Rels::default(),
      SyntheticItem::EnumMatch(_) => {
        Rels { consts: IdxVec::new(), fns: IdxVec::from([FnRel::Impl(TirImpl::Param(0), 1)]) }
      }
      SyntheticItem::CallFromFn(params) => {
        Rels { consts: IdxVec::new(), fns: IdxVec::from([FnRel::Impl(TirImpl::Param(0), *params)]) }
      }
      SyntheticItem::FnFromCall(_) => Rels {
        consts: IdxVec::new(),
        fns: IdxVec::from([FnRel::Item(
          FnId::Abstract(TraitId(usize::MAX), TraitFnId(0)),
          vec![TirImpl::Param(0)],
        )]),
      },
      SyntheticItem::ConstAlias(id) => {
        Rels { consts: IdxVec::from([(ConstId::Concrete(*id), Vec::new())]), fns: IdxVec::new() }
      }
    }
  }
}
