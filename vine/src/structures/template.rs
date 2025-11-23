use std::fmt::Write;

use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::IdxVec;

use crate::structures::{
  resolutions::{ConstRelId, FnRelId},
  specializations::{Spec, Specializations},
  vir::StageId,
};

#[derive(Debug)]
pub struct Template {
  pub stages: IdxVec<StageId, Option<TemplateStage>>,
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

#[derive(Debug)]
pub struct TemplateStage {
  pub net: Net,
  pub rels: TemplateStageRels,
}

#[derive(Debug, Default)]
pub struct TemplateStageRels {
  pub fns: Vec<(FnRelId, Tree)>,
  pub consts: Vec<(ConstRelId, Tree)>,
  pub stages: Vec<(StageId, Tree)>,
}

impl TemplateStage {
  pub fn instantiate(&self, specs: &Specializations, spec: &Spec) -> Net {
    let mut net = self.net.clone();
    for (fn_rel_id, tree) in &self.rels.fns {
      let (spec_id, stage_id) = spec.rels.fns[*fn_rel_id].unwrap();
      net.pairs.push((tree.clone(), Tree::Global(global_name(specs.spec(spec_id), stage_id))));
    }
    for (const_rel_id, tree) in &self.rels.consts {
      let spec_id = spec.rels.consts[*const_rel_id].unwrap();
      net.pairs.push((tree.clone(), Tree::Global(global_name(specs.spec(spec_id), StageId(0)))));
    }
    for (stage_id, tree) in &self.rels.stages {
      net.pairs.push((tree.clone(), Tree::Global(global_name(spec, *stage_id))));
    }
    net
  }
}

pub fn global_name(spec: &Spec, stage_id: StageId) -> String {
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
  str
}
