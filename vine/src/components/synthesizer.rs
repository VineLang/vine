use std::mem::take;

use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::{Counter, IdxVec};

use crate::structures::{
  ast::Ident,
  chart::{Chart, EnumId, StructId, TraitConstId, TraitFnId, VariantId},
  resolutions::{FnRel, FnRelId, Rels},
  specializations::{Spec, Specializations},
  template::global_name,
  tir::TirImpl,
  vir::StageId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticImpl<'core> {
  Tuple(usize),
  Object(Ident<'core>, usize),
  Struct(StructId),
  Enum(EnumId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticItem<'core> {
  CompositeDeconstruct(usize),
  CompositeReconstruct(usize),
  Ident(Ident<'core>),
  Identity,
  EnumVariantNames(EnumId),
  EnumMatch(EnumId),
  EnumReconstruct(EnumId),
  FnFromCall(usize),
  CallFromFn(usize),
}

struct Synthesizer<'core, 'a> {
  nets: &'a mut Nets,
  chart: &'a Chart<'core>,
  specs: &'a Specializations<'core>,
  spec: &'a Spec<'core>,
  var_id: Counter<usize>,
  stages: Counter<StageId>,
}

pub fn synthesize<'core>(
  nets: &mut Nets,
  chart: &Chart<'core>,
  specs: &Specializations<'core>,
  spec: &Spec<'core>,
  item: SyntheticItem<'core>,
) {
  Synthesizer { nets, chart, specs, spec, var_id: Counter::default(), stages: Counter::default() }
    .synthesize(item);
}

impl<'core> Synthesizer<'core, '_> {
  fn new_wire(&mut self) -> (Tree, Tree) {
    let var = format!("s{}", self.var_id.next());
    (Tree::Var(var.clone()), Tree::Var(var))
  }

  fn list<T>(
    &mut self,
    items: impl IntoIterator<Item = T, IntoIter: DoubleEndedIterator>,
    mut f: impl FnMut(&mut Self, T) -> Tree,
  ) -> Tree {
    let end = self.new_wire();
    let mut len = 0;
    let buf = Tree::n_ary(
      "tup",
      items
        .into_iter()
        .map(|t| {
          len += 1;
          f(self, t)
        })
        .chain([end.0]),
    );
    Tree::n_ary("tup", [Tree::N32(len), buf, end.1])
  }

  fn string(&mut self, str: &str) -> Tree {
    self.list(str.chars(), |_, char| Tree::N32(char as u32))
  }

  fn synthesize(&mut self, item: SyntheticItem<'core>) {
    let stage = self.new_stage();
    let net = match item {
      SyntheticItem::CompositeDeconstruct(len) => self.synthesize_composite_deconstruct(len),
      SyntheticItem::CompositeReconstruct(len) => self.synthesize_composite_reconstruct(len),
      SyntheticItem::Ident(ident) => self.synthesize_ident(ident),
      SyntheticItem::Identity => self.synthesize_identity(),
      SyntheticItem::EnumVariantNames(enum_id) => self.synthesize_enum_variant_names(enum_id),
      SyntheticItem::EnumMatch(enum_id) => self.synthesize_enum_match(enum_id),
      SyntheticItem::EnumReconstruct(enum_id) => self.synthesize_enum_reconstruct(enum_id),
      SyntheticItem::FnFromCall(params) => self.synthesize_fn_from_call(params),
      SyntheticItem::CallFromFn(params) => self.synthesize_call_from_fn(params),
    };
    self.nets.insert(stage, net);
  }

  fn synthesize_composite_deconstruct(&mut self, len: usize) -> Net {
    let x = self.new_wire();
    Net::new(if len == 1 {
      Tree::n_ary("fn", [Tree::Erase, x.0, Tree::n_ary("tup", [x.1, Tree::Erase])])
    } else {
      Tree::n_ary("fn", [Tree::Erase, x.0, x.1])
    })
  }

  fn synthesize_composite_reconstruct(&mut self, len: usize) -> Net {
    let x = self.new_wire();
    let y = self.new_wire();
    Net::new(if len == 1 {
      Tree::n_ary("fn", [Tree::Erase, x.0, Tree::Erase, x.1])
    } else {
      Tree::n_ary("fn", [Tree::Erase, x.0, y.0, Tree::n_ary("tup", [x.1, y.1])])
    })
  }

  fn synthesize_ident(&mut self, ident: Ident<'core>) -> Net {
    Net::new(self.string(ident.0 .0))
  }

  fn synthesize_identity(&mut self) -> Net {
    let x = self.new_wire();
    Net::new(Tree::n_ary("fn", [Tree::Erase, x.0, x.1]))
  }

  fn new_stage(&mut self) -> String {
    let id = self.stages.next();
    global_name(self.spec, id)
  }

  fn synthesize_enum_variant_names(&mut self, enum_id: EnumId) -> Net {
    Net::new(
      self.list(self.chart.enums[enum_id].variants.values().map(|x| x.name.0 .0), Self::string),
    )
  }

  fn synthesize_enum_match(&mut self, enum_id: EnumId) -> Net {
    let fn_ = self.fn_rel(FnRelId(0));
    let f = self.new_wire();
    let t = self.new_wire();
    Net::new(Tree::n_ary(
      "fn",
      [
        Tree::Erase,
        self.match_enum(
          enum_id,
          |self_, variant_id| {
            let has_data = self_.chart.enums[enum_id].variants[variant_id].data.is_some();
            let data = self_.new_wire();
            let f = self_.new_wire();
            let t = self_.new_wire();
            Net {
              root: Tree::n_ary(
                "enum",
                has_data.then_some(data.0).into_iter().chain([Tree::n_ary("x", [f.0, t.0])]),
              ),
              pairs: Vec::from([(
                fn_.clone(),
                Tree::n_ary(
                  "fn",
                  [f.1, self_.variant(variant_id, has_data.then_some(data.1)), t.1],
                ),
              )]),
            }
          },
          Tree::n_ary("x", [f.0, t.0]),
        ),
        f.1,
        t.1,
      ],
    ))
  }

  fn synthesize_enum_reconstruct(&mut self, enum_id: EnumId) -> Net {
    let mut cur_net = Net::new(Tree::Erase);
    for (variant_id, variant) in self.chart.enums[enum_id].variants.iter().rev() {
      let has_data = variant.data.is_some();
      let mut prev_net = Some(cur_net);
      let out = self.new_wire();
      let match_ = self.match_enum(
        enum_id,
        |self_, v| {
          let data = self_.new_wire();
          if v.0 == 0 {
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
        cur_net = Net::new(Tree::n_ary("fn", [Tree::Erase, match_, out.1]));
      } else {
        cur_net = Net::new(Tree::n_ary("enum", [match_, out.1]));
      }
    }
    cur_net
  }

  fn synthesize_fn_from_call(&mut self, params: usize) -> Net {
    let recv = self.new_wire();
    let params = (0..params).map(|_| self.new_wire()).collect::<(Vec<_>, Vec<_>)>();
    let ret = self.new_wire();
    Net {
      root: Tree::n_ary("fn", [recv.0].into_iter().chain(params.0).chain([ret.0])),
      pairs: vec![(
        self.fn_rel(FnRelId(0)),
        Tree::n_ary("fn", [Tree::Erase, recv.1, Tree::n_ary("tup", params.1), ret.1]),
      )],
    }
  }

  fn synthesize_call_from_fn(&mut self, params: usize) -> Net {
    let recv = self.new_wire();
    let params = (0..params).map(|_| self.new_wire()).collect::<(Vec<_>, Vec<_>)>();
    let ret = self.new_wire();
    Net {
      root: Tree::n_ary("fn", [Tree::Erase, recv.1, Tree::n_ary("tup", params.1), ret.1]),
      pairs: vec![(
        self.fn_rel(FnRelId(0)),
        Tree::n_ary("fn", [recv.0].into_iter().chain(params.0).chain([ret.0])),
      )],
    }
  }

  fn enum_(
    &mut self,
    enum_id: EnumId,
    variant_id: VariantId,
    content: impl FnOnce(&mut Self) -> Option<Tree>,
  ) -> Tree {
    let enum_def = &self.chart.enums[enum_id];
    let wire = self.new_wire();
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
    mut arm: impl FnMut(&mut Self, VariantId) -> Net,
    ctx: Tree,
  ) -> Tree {
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

  fn variant(&mut self, variant_id: VariantId, data: Option<Tree>) -> Tree {
    let variant_enum = self.chart.builtins.variant.unwrap();
    let mut cur = self.enum_(variant_enum, VariantId(0), |_| Some(data.unwrap_or(Tree::Erase)));
    for _ in 0..variant_id.0 {
      cur = self.enum_(variant_enum, VariantId(1), |_| Some(cur));
    }
    cur
  }

  fn fn_rel(&self, id: FnRelId) -> Tree {
    match self.spec.rels.fns[id] {
      Ok((spec_id, stage_id)) => {
        Tree::Global(global_name(self.specs.specs[spec_id].as_ref().unwrap(), stage_id))
      }
      Err(_) => Tree::Erase,
    }
  }
}

impl<'core> SyntheticImpl<'core> {
  pub fn fn_(self, _: &Chart<'core>, fn_id: TraitFnId) -> SyntheticItem<'core> {
    match self {
      SyntheticImpl::Tuple(len) | SyntheticImpl::Object(_, len) => match fn_id {
        TraitFnId(0) => SyntheticItem::CompositeDeconstruct(len),
        TraitFnId(1) => SyntheticItem::CompositeReconstruct(len),
        _ => unreachable!(),
      },
      SyntheticImpl::Struct(_) => match fn_id {
        TraitFnId(0) | TraitFnId(1) => SyntheticItem::Identity,
        _ => unreachable!(),
      },
      SyntheticImpl::Enum(enum_id) => match fn_id {
        TraitFnId(0) => SyntheticItem::EnumMatch(enum_id),
        TraitFnId(1) => SyntheticItem::EnumReconstruct(enum_id),
        _ => unreachable!(),
      },
    }
  }

  pub fn const_(self, chart: &Chart<'core>, const_id: TraitConstId) -> SyntheticItem<'core> {
    match self {
      SyntheticImpl::Tuple(_) => unreachable!(),
      SyntheticImpl::Object(key, _) => match const_id {
        TraitConstId(0) => SyntheticItem::Ident(key),
        _ => unreachable!(),
      },
      SyntheticImpl::Struct(struct_id) => match const_id {
        TraitConstId(0) => SyntheticItem::Ident(chart.structs[struct_id].name),
        _ => unreachable!(),
      },
      SyntheticImpl::Enum(enum_id) => match const_id {
        TraitConstId(0) => SyntheticItem::Ident(chart.enums[enum_id].name),
        TraitConstId(1) => SyntheticItem::EnumVariantNames(enum_id),
        _ => unreachable!(),
      },
    }
  }
}

impl<'core> SyntheticItem<'core> {
  pub fn rels(self) -> Rels<'core> {
    match self {
      SyntheticItem::CompositeDeconstruct(_)
      | SyntheticItem::CompositeReconstruct(_)
      | SyntheticItem::Ident(_)
      | SyntheticItem::Identity
      | SyntheticItem::EnumVariantNames(_)
      | SyntheticItem::EnumReconstruct(_) => Rels::default(),
      SyntheticItem::EnumMatch(_) => {
        Rels { consts: IdxVec::new(), fns: IdxVec::from([FnRel::Impl(TirImpl::Param(0), 1)]) }
      }
      SyntheticItem::CallFromFn(params) => {
        Rels { consts: IdxVec::new(), fns: IdxVec::from([FnRel::Impl(TirImpl::Param(0), params)]) }
      }
      SyntheticItem::FnFromCall(_) => Rels {
        consts: IdxVec::new(),
        fns: IdxVec::from([FnRel::Item(
          FnId::Abstract(TraitId(usize::MAX), TraitFnId(0)),
          vec![TirImpl::Param(0)],
        )]),
      },
    }
  }
}
