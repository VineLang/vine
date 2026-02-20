use std::{collections::HashMap, convert::Infallible, iter};

use ivy::{
  name::{Name, NameId, Table},
  net::{FlatDecode, FlatEncode, FlatNet, FlatNode, Wire},
};
use vine_util::idx::IdxVec;

use crate::{
  compiler::Guide,
  structures::{
    resolutions::{ConstRelId, FnRelId},
    specializations::{Spec, Specializations},
    vir::StageId,
  },
};

#[derive(Debug)]
pub struct Template {
  pub stages: IdxVec<StageId, Option<FlatNet<TemplateNode>>>,
}

impl Template {
  pub fn instantiate(
    &self,
    table: &mut Table,
    guide: &Guide,
    nets: &mut HashMap<NameId, FlatNet>,
    specs: &Specializations,
    spec: &Spec,
  ) {
    for (stage_id, stage) in self.stages.iter() {
      if let Some(stage) = stage {
        let name = table.add_name(spec.name.clone().with_payload(stage_id.0));
        nets.insert(name, stage.clone().encode(&mut EncodeCtx { table, guide, specs, spec }));
      }
    }
  }
}

#[derive(Debug, Clone)]
pub enum TemplateNode {
  Raw(FlatNode),
  Fn(FnRelId, Wire),
  Const(ConstRelId, Wire),
  Stage(StageId, Wire),
  If(StageId, StageId, Wire, Wire),
  Match(NameId, Vec<StageId>, Wire, Wire),
}

impl From<FlatNode> for TemplateNode {
  fn from(node: FlatNode) -> Self {
    TemplateNode::Raw(node)
  }
}

struct EncodeCtx<'a> {
  table: &'a mut Table,
  guide: &'a Guide,
  specs: &'a Specializations,
  spec: &'a Spec,
}

impl FlatEncode<EncodeCtx<'_>> for TemplateNode {
  fn encode(node: Self, ctx: &mut EncodeCtx<'_>) -> FlatNode {
    match node {
      TemplateNode::Raw(node) => node,
      TemplateNode::Fn(fn_rel_id, wire) => {
        let (spec_id, stage_id) = ctx.spec.rels.fns[fn_rel_id].unwrap();
        FlatNode(ctx.graft(ctx.specs.spec(spec_id), stage_id), wire, [])
      }
      TemplateNode::Const(const_rel_id, wire) => {
        let spec_id = ctx.spec.rels.consts[const_rel_id].unwrap();
        FlatNode(ctx.graft(ctx.specs.spec(spec_id), StageId(0)), wire, [])
      }
      TemplateNode::Stage(stage_id, wire) => FlatNode(ctx.graft(ctx.spec, stage_id), wire, []),
      TemplateNode::If(then, else_, bool, out) => {
        let then = ctx._graft(ctx.spec, then);
        let else_ = ctx._graft(ctx.spec, else_);
        FlatNode(ctx.guide.bool_if.with_children([then, else_]), bool, [out])
      }
      TemplateNode::Match(enum_name, stages, enum_, out) => {
        let name = ctx.guide.enum_match.with_children(
          iter::once(enum_name).chain(stages.into_iter().map(|s| ctx._graft(ctx.spec, s))),
        );
        FlatNode(name, enum_, [out])
      }
    }
  }
}

impl EncodeCtx<'_> {
  fn graft(&mut self, spec: &Spec, stage_id: StageId) -> Name {
    let name = self._graft(spec, stage_id);
    self.guide.graft.with_children([name])
  }

  fn _graft(&mut self, spec: &Spec, stage_id: StageId) -> NameId {
    self.table.add_name(spec.name.clone().with_payload(stage_id.0))
  }
}

impl FlatDecode<()> for TemplateNode {
  type Error = Infallible;
  fn decode(node: FlatNode, _: &mut ()) -> Result<Self, Self::Error> {
    Ok(TemplateNode::Raw(node))
  }
}
