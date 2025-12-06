use std::{
  collections::{HashMap, hash_map::Entry},
  mem::replace,
};

use vine_util::{
  idx::{Counter, IdxVec, IntSet},
  new_idx,
};

use crate::structures::{
  ast::Ident,
  chart::{
    Binding, Chart, DefId, DefImplKind, DefTraitKind, DefValueKind, FnId, ImplId, TraitId, VisId,
  },
  signatures::Signatures,
  types::ImplType,
};

#[derive(Debug, Default, Clone)]
pub struct CandidateSets {
  pub(crate) by_def: HashMap<(DefId, Level), CandidateSetHandle>,
  pub(crate) sets: IdxVec<CandidateSetId, CandidateSet>,
  pub(crate) checkpoint: DefId,
  empty: CandidateSet,
}

new_idx!(pub CandidateSetId; n => ["C{n}"]);

#[derive(Debug, Default, Clone)]
pub enum CandidateSetHandle {
  #[default]
  Empty,
  Id(CandidateSetId),
  Set(CandidateSet),
}

use CandidateSetHandle::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Level {
  /// The candidates associated with the given def.
  Direct,
  /// The candidates associated with any child of the given def.
  Within,
  /// The candidates associated with any child of any ancestor of the given def.
  WithinAncestors,
}

#[derive(Default, Clone, Debug)]
pub struct CandidateSet {
  pub methods: HashMap<Ident, HashMap<FnId, VisSet>>,
  pub impls: HashMap<(TraitId, /* basic */ bool), HashMap<ImplId, VisSet>>,
}

/// A union of several visibilities.
#[derive(Clone, Debug)]
pub enum VisSet {
  Pub,
  Defs(IntSet<DefId>),
}

impl Default for VisSet {
  fn default() -> Self {
    VisSet::Defs(IntSet::default())
  }
}

impl VisSet {
  pub fn insert(&mut self, vis: VisId) {
    match (self, vis) {
      (VisSet::Pub, _) => {}
      (set, VisId::Pub) => {
        *set = VisSet::Pub;
      }
      (VisSet::Defs(defs), VisId::Def(def)) => {
        defs.insert(def);
      }
    }
  }
}

impl CandidateSets {
  pub fn build_since(&mut self, chart: &Chart, sigs: &Signatures) {
    CandidateSetBuilder {
      sets: self,
      chart,
      sigs,
      component_stack: Default::default(),
      children_stack: Default::default(),
      nodes: Default::default(),
      index: Default::default(),
    }
    .visit_all();
  }

  pub fn get_within(&self, def: DefId) -> &CandidateSet {
    self.get(&self.by_def[&(def, Level::Within)])
  }

  pub fn get_within_ancestors(&self, def: DefId) -> &CandidateSet {
    self.get(&self.by_def[&(def, Level::WithinAncestors)])
  }

  fn get<'a>(&'a self, handle: &'a CandidateSetHandle) -> &'a CandidateSet {
    match handle {
      Empty => &self.empty,
      Id(id) => &self.sets[*id],
      Set(set) => set,
    }
  }

  fn get_owned(&self, handle: CandidateSetHandle) -> CandidateSet {
    match handle {
      Empty => CandidateSet::default(),
      Id(id) => self.sets[id].clone(),
      Set(set) => set,
    }
  }

  fn get_mut<'a>(&self, handle: &'a mut CandidateSetHandle) -> &'a mut CandidateSet {
    *handle = CandidateSetHandle::Set(self.get_owned(replace(handle, CandidateSetHandle::Empty)));
    let CandidateSetHandle::Set(set) = handle else { unreachable!() };
    set
  }

  fn insert(&mut self, handle: CandidateSetHandle) -> CandidateSetHandle {
    if let Set(set) = handle { Id(self.sets.push(set)) } else { handle }
  }

  fn merge(&self, chart: &Chart, a: &mut CandidateSetHandle, vis: VisId, b: &CandidateSetHandle) {
    if matches!(b, CandidateSetHandle::Empty) {
      return;
    }

    if matches!((&a, vis), (Empty, VisId::Pub)) {
      *a = b.clone();
      return;
    }

    if let (Id(a), Id(b)) = (&a, b)
      && a == b
    {
      return;
    }

    let a = self.get_mut(a);
    let b = self.get(b);

    for (name, b_methods) in &b.methods {
      let a_methods = a.methods.entry(name.clone()).or_default();
      for (&fn_id, b_vis) in b_methods {
        a_methods.entry(fn_id).or_default().merge_vis_sets(chart, vis, b_vis);
      }
    }

    for (&trait_id, b_impls) in &b.impls {
      let a_impls = a.impls.entry(trait_id).or_default();
      for (&impl_id, b_vis) in b_impls {
        a_impls.entry(impl_id).or_default().merge_vis_sets(chart, vis, b_vis);
      }
    }
  }
}

pub struct CandidateSetBuilder<'a> {
  sets: &'a mut CandidateSets,
  chart: &'a Chart,
  sigs: &'a Signatures,
  component_stack: Vec<Node>,
  children_stack: Vec<(VisId, Node)>,
  nodes: HashMap<Node, NodeState>,
  index: Counter<usize>,
}

struct NodeState {
  index: usize,
  on_component_stack: bool,
}

type Node = (DefId, Level);

impl<'a> CandidateSetBuilder<'a> {
  fn visit_all(&mut self) {
    for def_id in self.chart.defs.keys_from(self.sets.checkpoint) {
      self.visit((def_id, Level::WithinAncestors));
    }
    self.sets.checkpoint = self.chart.defs.next_index();
  }

  // Tarjan's strongly connected components algorithm
  fn visit(&mut self, node: Node) -> Option<usize> {
    if node.0 < self.sets.checkpoint {
      return None;
    }
    match self.nodes.entry(node) {
      Entry::Occupied(e) => {
        let state = e.get();
        state.on_component_stack.then_some(state.index)
      }
      Entry::Vacant(e) => {
        let index = self.index.next();
        e.insert(NodeState { index, on_component_stack: true });

        let component_stack_height = self.component_stack.len();
        let children_stack_height = self.children_stack.len();
        self.component_stack.push(node);

        let mut low_link = index;
        self.for_each_child(node, |self_, vis, child| {
          if let Some(low_link_) = self_.visit(child) {
            low_link = low_link.min(low_link_);
          } else {
            self_.children_stack.push((vis, child));
          }
        });

        if low_link == index {
          // The strongly connected component
          let component = &self.component_stack[component_stack_height..];
          // The children of the strongly connected component
          let children = &self.children_stack[children_stack_height..];

          let mut set = Empty;
          for &(vis, child) in children {
            self.sets.merge(self.chart, &mut set, vis, &self.sets.by_def[&child]);
          }
          for node in component {
            self.add_direct_candidate(&mut set, *node);
            self.nodes.get_mut(node).unwrap().on_component_stack = false;
          }

          let set = self.sets.insert(set);
          for node in component {
            self.sets.by_def.insert(*node, set.clone());
          }

          self.component_stack.truncate(component_stack_height);
          self.children_stack.truncate(children_stack_height);
          None
        } else {
          Some(low_link)
        }
      }
    }
  }

  fn for_each_child(&mut self, (def_id, level): Node, mut f: impl FnMut(&mut Self, VisId, Node)) {
    let def = &self.chart.defs[def_id];
    match level {
      Level::Direct => {
        if let Some(Binding { vis, kind: DefTraitKind::Trait(trait_id), .. }) = def.trait_kind {
          f(self, vis, (self.chart.traits[trait_id].def, Level::Within))
        }
        if let Some(Binding { vis, kind: DefImplKind::Impl(impl_id), .. }) = def.impl_kind
          && let ImplType::Trait(trait_id, _) = &self.sigs.impls[impl_id].inner.ty
        {
          f(self, vis, (self.chart.traits[*trait_id].def, Level::Within))
        }
        for binding in &def.implicit_members {
          if let Some(member) = self.sigs.get_member(binding.kind) {
            f(self, binding.vis, (member, Level::Direct))
          }
        }
      }
      Level::Within => {
        f(self, VisId::Pub, (def_id, Level::Direct));
        for binding in &def.named_members {
          if let Some(member) = self.sigs.get_member(binding.kind) {
            f(self, binding.vis, (member, Level::Direct))
          }
        }
      }
      Level::WithinAncestors => {
        f(self, VisId::Pub, (def_id, Level::Within));
        if let Some(parent) = def.parent {
          f(self, VisId::Pub, (parent, Level::WithinAncestors))
        } else if let Some(prelude) = self.chart.builtins.prelude {
          f(self, VisId::Pub, (prelude, Level::Within))
        }
      }
    }
  }

  fn add_direct_candidate(&self, set: &mut CandidateSetHandle, (def_id, level): Node) {
    if level != Level::Direct {
      return;
    }

    let def = &self.chart.defs[def_id];

    if let Some(Binding { vis, kind: DefValueKind::Fn(fn_id), .. }) = def.value_kind
      && self.chart.fn_is_method(fn_id)
    {
      let set = self.sets.get_mut(set);
      set.methods.entry(def.name.clone()).or_default().entry(fn_id).or_default().insert(vis);
    }

    if let Some(Binding { vis, kind: DefImplKind::Impl(impl_id), .. }) = def.impl_kind
      && let ImplType::Trait(trait_id, _) = &self.sigs.impls[impl_id].inner.ty
    {
      let impl_ = &self.chart.impls[impl_id];
      if !impl_.manual {
        let set = self.sets.get_mut(set);
        let key = (*trait_id, impl_.basic);
        set.impls.entry(key).or_default().entry(impl_id).or_default().insert(vis);
      }
    }
  }
}

impl VisSet {
  fn merge_vis_sets(&mut self, chart: &Chart, vis: VisId, b: &VisSet) {
    match (self, vis, b) {
      (VisSet::Pub, _, _) => {}
      (a, vis, VisSet::Pub) => {
        a.insert(vis);
      }
      (VisSet::Defs(a), VisId::Pub, VisSet::Defs(b)) => {
        for &def in b {
          a.insert(def);
        }
      }
      (VisSet::Defs(a), VisId::Def(x), VisSet::Defs(b)) => {
        for &y in b {
          if chart.visible(VisId::Def(y), x) {
            a.insert(x);
          } else if chart.visible(VisId::Def(x), y) {
            a.insert(y);
          }
        }
      }
    }
  }
}
