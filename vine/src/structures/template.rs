use std::{convert::Infallible, fmt::Write};

use hedera::{
  name::{Name, Table},
  net::{FlatDecode, FlatEncode, FlatNet, FlatNode, Wire},
};
use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::IdxVec;

use crate::structures::{
  chart::VariantId,
  resolutions::{ConstRelId, FnRelId},
  specializations::{Spec, Specializations},
  vir::StageId,
};

#[derive(Debug)]
pub struct Template {
  pub stages: IdxVec<StageId, Option<FlatNet<TemplateNode>>>,
}

impl Template {
  pub fn instantiate(&self, nets: &mut Nets, specs: &Specializations, spec: &Spec) {
    for (stage_id, stage) in self.stages.iter() {
      if let Some(stage) = stage {
        nets.insert(global_name(spec, stage_id), stage.instantiate(specs, spec));
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
}

impl From<FlatNode> for TemplateNode {
  fn from(node: FlatNode) -> Self {
    TemplateNode::Raw(node)
  }
}

struct EncodeCtx<'a> {
  table: &'a mut Table,
  specs: &'a Specializations,
  spec: &'a Spec,
}

impl FlatEncode<EncodeCtx<'_>> for TemplateNode {
  fn encode(node: Self, EncodeCtx { table, specs, spec }: &mut EncodeCtx<'_>) -> FlatNode {
    match node {
      TemplateNode::Raw(node) => node,
      TemplateNode::Fn(fn_rel_id, wire) => {
        let (spec_id, stage_id) = spec.rels.fns[fn_rel_id].unwrap();
        FlatNode { name: global_name(table, specs.spec(spec_id), stage_id), ports: vec![wire] }
      }
      TemplateNode::Const(const_rel_id, wire) => {
        let spec_id = spec.rels.consts[const_rel_id].unwrap();
        FlatNode { name: global_name(table, specs.spec(spec_id), StageId(0)), ports: vec![wire] }
      }
      TemplateNode::Stage(stage_id, wire) => {
        FlatNode { name: global_name(table, spec, stage_id), ports: vec![wire] }
      }
    }
  }
}

impl FlatDecode<()> for TemplateNode {
  type Error = Infallible;
  fn decode(node: FlatNode, _: &mut ()) -> Result<Self, Self::Error> {
    Ok(TemplateNode::Raw(node))
  }
}

pub fn global_name(table: &mut Table, spec: &Spec, stage_id: StageId) -> Name {
  let mut str = if spec.path.starts_with("#") {
    format!("::{}", &spec.path[1..])
  } else {
    spec.path.to_owned()
  };
  if !spec.singular {
    write!(str, ":{}", spec.index).unwrap();
  }
  if stage_id.0 != 0 {
    write!(str, ":s{}", stage_id.0).unwrap();
  }
  table.add_path(str).into()
}
