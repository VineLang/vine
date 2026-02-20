use std::collections::HashMap;

use ivy::{
  engine::{Rewrite, Rules, common::eta_reduce},
  guide,
  name::{NameId, PathId},
  translate::{Translate, Translator},
};
use vine_util::register::Register;

guide!(pub Guide {
  x: PathId = "ivm:x",
  y: PathId = "ivm:y",
  graft: PathId = "ivm:graft",
  eraser: PathId = "ivm:eraser",
  n32: PathId = "ivm:n32",
  f32: PathId = "ivm:f32",
  nil: PathId = "ivm:nil",
  branch: PathId = "ivm:branch",
  branch_: NameId = "ivm:branch",
  ext_merge: PathId = "ivm:ext:merge",
  ext_merge_swap: PathId = "ivm:ext:merge:swap",
  ext_split: PathId = "ivm:ext:split",
  pair: NameId = "ivm:pair",
  ref_: NameId = "ivm:ref",
  black_box: PathId = "iv:black_box",
});

pub fn variadic_ext_fn<'r>(guide: &'r Guide, path: PathId) -> impl Register<Translator<'r>> {
  Translate([path], move |mut node, _, net| {
    let ext_fn = node.name.children[0];
    while node.aux.len() < 2 {
      node.aux.push(net.make(guide.eraser, []));
    }
    let inputs = node.name.payload.as_u32().unwrap() as usize;
    let outputs = (node.aux.len() + 1) - inputs;
    let (path, goal_inputs, goal_outputs) =
      if outputs == 1 { (guide.ext_merge, 2, 1) } else { (guide.ext_split, 1, 2) };
    node.aux.reverse();
    for _ in goal_inputs..inputs {
      let next = net.wire();
      let pair = guide.ext_merge.with_children([guide.pair]);
      net.add(pair, node.pri, [node.aux.pop().unwrap(), next]);
      node.pri = next;
    }
    for _ in goal_outputs..outputs {
      let next = net.wire();
      let ref_ = guide.ext_split.with_children([guide.ref_]);
      net.add(ref_, node.pri, [node.aux.pop().unwrap(), next]);
      node.pri = next;
    }
    assert_eq!(node.aux.len(), 2);
    node.aux.reverse();
    node.name = path.with_children([ext_fn]);
    net.push(node);
  })
}

pub fn replace_ext_fns<'r>(
  guide: &'r Guide,
  replacements: impl IntoIterator<Item = (NameId, NameId)>,
) -> impl Register<Translator<'r>> {
  let replacements = HashMap::<_, _>::from_iter(replacements);
  Translate([guide.ext_split, guide.ext_merge, guide.ext_merge_swap], move |mut node, _, net| {
    let ext_fn = &mut node.name.children[0];
    if let Some(replacement) = replacements.get(ext_fn) {
      *ext_fn = *replacement;
    }
    net.push(node);
  })
}

pub fn construct_f64s<'r>(
  guide: &'r Guide,
  f64: PathId,
  f64_from_bits: NameId,
) -> impl Register<Translator<'r>> {
  Translate([f64], move |node, _, net| {
    let [] = *node.aux else { unreachable!() };
    let n = node.name.payload.as_u64().unwrap();
    let lo = n as u32;
    let hi = (n >> 32) as u32;
    let lo = net.make(guide.n32.with_payload(lo), []);
    let hi = net.make(guide.n32.with_payload(hi), []);
    net.add(guide.ext_merge.with_children([f64_from_bits]), lo, [hi, node.pri]);
  })
}

pub fn optimizations<'r>(guide: &'r Guide) -> impl Register<Rules<'r>> {
  (
    eta_reduce([guide.x, guide.y], [guide.eraser, guide.n32, guide.f32], []),
    Rewrite([guide.ext_merge], |engine, _, net, node| {
      if !engine.follow(node.port(0)).is_principal() {
        return false;
      }
      let mut name = engine.name(node).clone();
      name.path = guide.ext_merge_swap;
      let [lhs, rhs, out] = engine.remove_node_n(node);
      let [rhs_, lhs_, out_] = engine.insert_node_n(net, name);
      engine.link(lhs, lhs_);
      engine.link(rhs, rhs_);
      engine.link(out, out_);
      true
    }),
  )
}
