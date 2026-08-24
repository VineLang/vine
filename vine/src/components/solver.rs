use std::{
  fmt,
  mem::take,
  ops::{Add, Mul},
};

use crate::{
  components::finder::Finder,
  structures::{
    chart::{FnId, ImplId},
    diag::ErrorGuaranteed,
    tir::TirImpl,
    types::{
      ImplType, Type, UnifyResult,
      repo::{Delta, TypesCommit, TypesPatch, TypesRepo},
    },
  },
};

pub struct Solver<'a> {
  finder: Finder<'a>,
}

impl<'a> Solver<'a> {
  fn impl_candidates(&mut self, query: &ImplType, parent: &mut TypesCommit) -> Vec<ImplCandidate> {
    for candidate in self.finder.find_impl_candidates(types, query, false).unwrap() {
      let generics = self.chart.impls[candidate].generics;
      let type_params = (0..self.sigs.type_params[generics].params.len())
        .map(|_| types.new_var(self.span))
        .collect::<Vec<_>>();
      let ty = types.import(&self.sigs.impls[candidate], Some(&type_params)).ty;
      if types.unify_impl_type(&ty, query).is_success() {
        for result in self.find_impl_params(types, generics, type_params)? {
          found.push(TypeCtx { types: result.types, inner: TirImpl::Def(candidate, result.inner) });
        }
      }
    }

    todo!()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cardinality {
  Zero,
  One,
  Many,
  Error(ErrorGuaranteed),
  Unknown,
}

impl Add for Cardinality {
  type Output = Cardinality;

  fn add(self, rhs: Self) -> Self::Output {
    use Cardinality::*;
    match (self, rhs) {
      (Error(e), _) | (_, Error(e)) => Error(e),
      (Many, _) | (_, Many) => Many,
      (Unknown, _) | (_, Unknown) => Unknown,
      (One, One) => Many,
      (One, Zero) | (Zero, One) => One,
      (Zero, Zero) => Zero,
    }
  }
}

impl Mul for Cardinality {
  type Output = Cardinality;

  fn mul(self, rhs: Self) -> Self::Output {
    use Cardinality::*;
    match (self, rhs) {
      (Zero, _) | (_, Zero) => Zero,
      (Unknown, _) | (_, Unknown) => Unknown,
      (Error(e), _) | (_, Error(e)) => Error(e),
      (Many, _) | (_, Many) => Many,
      (One, One) => One,
    }
  }
}

pub trait Goal: fmt::Debug {
  type Result: fmt::Debug;
  fn cardinality(&self) -> Cardinality;
  fn patch(&mut self) -> Option<&mut TypesPatch>;
  fn finish(&mut self) -> Self::Result;
  fn abandon(&mut self, repo: &mut TypesRepo);
  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta);
  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit);
}

pub trait LazyGoal: Goal {
  type Query: fmt::Debug;
  fn start(init: &mut Self::Query, solver: &mut Solver, parent: &mut TypesCommit) -> Self;
}

#[derive(Debug)]
pub enum Lazy<G: LazyGoal> {
  Pending(G::Query),
  Active(G),
}

impl<G: LazyGoal> Goal for Lazy<G> {
  type Result = G::Result;

  fn cardinality(&self) -> Cardinality {
    match self {
      Lazy::Pending(_) => Cardinality::Unknown,
      Lazy::Active(goal) => goal.cardinality(),
    }
  }

  fn patch(&mut self) -> Option<&mut TypesPatch> {
    match self {
      Lazy::Pending(_) => None,
      Lazy::Active(goal) => goal.patch(),
    }
  }

  fn finish(&mut self) -> Self::Result {
    match self {
      Lazy::Pending(_) => unreachable!(),
      Lazy::Active(goal) => goal.finish(),
    }
  }

  fn abandon(&mut self, repo: &mut TypesRepo) {
    match self {
      Lazy::Pending(_) => {}
      Lazy::Active(goal) => goal.abandon(repo),
    }
  }

  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta) {
    match self {
      Lazy::Pending(_) => {}
      Lazy::Active(goal) => goal.rebase(solver, parent, parent_delta),
    }
  }

  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    match self {
      Lazy::Pending(query) => {
        *self = Lazy::Active(G::start(query, solver, parent));
      }
      Lazy::Active(goal) => {
        goal.deepen(solver, parent);
      }
    }
  }
}

#[derive(Debug)]
pub struct Conjunction<G: Goal> {
  patch: TypesPatch,
  children: Vec<Child<G>>,
  cardinality: Cardinality,
}

#[derive(Debug)]
enum Child<G: Goal> {
  Active(G),
  Finished(G::Result),
}

impl<G: Goal> Conjunction<G> {
  pub fn new(
    unification: UnifyResult,
    patch: TypesPatch,
    children: impl IntoIterator<Item = G>,
  ) -> Self {
    Conjunction {
      cardinality: match unification {
        UnifyResult::Success => Cardinality::Unknown,
        UnifyResult::Failure => return Self::zero(),
        UnifyResult::Indeterminate(err) => Cardinality::Error(err),
      },
      patch,
      children: children.into_iter().map(Child::Active).collect(),
    }
  }

  pub fn zero() -> Self {
    Conjunction {
      patch: TypesPatch::default(),
      children: Vec::new(),
      cardinality: Cardinality::Zero,
    }
  }

  fn update(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    if self.cardinality == Cardinality::Zero {
      return;
    }
    let mut cardinality = Cardinality::One;
    let mut child_patch = None;
    for (i, child) in self.children.iter_mut().enumerate() {
      if let Child::Active(goal) = child {
        let child_cardinality = goal.cardinality();
        cardinality = cardinality * child_cardinality;
        if goal.patch().is_some_and(|p| !p.is_empty()) {
          child_patch = child_patch.or(Some(i));
        } else if child_cardinality == Cardinality::One {
          *child = Child::Finished(goal.finish());
        }
      }
    }
    self.cardinality = cardinality;
    if self.cardinality == Cardinality::Zero {
      return self.abandon(parent.repo);
    }
    if let Some(i) = child_patch {
      let Child::Active(goal) = &mut self.children[i] else { unreachable!() };
      let delta = parent.child(&mut self.patch).squash(goal.patch().unwrap());
      self.rebase_children(solver, parent, &delta, Some(i));
    }
  }

  fn rebase_children(
    &mut self,
    solver: &mut Solver,
    parent: &mut TypesCommit,
    delta: &Delta,
    except: Option<usize>,
  ) {
    if self.cardinality == Cardinality::Zero {
      return;
    }
    let mut commit = parent.child(&mut self.patch);
    for (i, child) in self.children.iter_mut().enumerate() {
      if except != Some(i)
        && let Child::Active(goal) = child
      {
        goal.rebase(solver, &mut commit, delta);
      }
    }
    self.update(solver, parent);
  }
}

impl<G: Goal> Goal for Conjunction<G> {
  type Result = Vec<G::Result>;

  fn cardinality(&self) -> Cardinality {
    self.cardinality
  }

  fn patch(&mut self) -> Option<&mut TypesPatch> {
    Some(&mut self.patch)
  }

  fn finish(&mut self) -> Self::Result {
    assert_eq!(self.cardinality, Cardinality::One);
    take(&mut self.children)
      .into_iter()
      .map(|child| {
        let Child::Finished(value) = child else { unreachable!() };
        value
      })
      .collect()
  }

  fn abandon(&mut self, repo: &mut TypesRepo) {
    self.cardinality = Cardinality::Zero;
    repo.abandon(take(&mut self.patch));
    for child in take(&mut self.children) {
      if let Child::Active(mut goal) = child {
        goal.abandon(repo);
      }
    }
  }

  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta) {
    if self.cardinality == Cardinality::Zero {
      return;
    }
    let mut commit = parent.child(&mut self.patch);
    let Some(delta) = commit.rebase(parent_delta) else {
      return self.abandon(parent.repo);
    };
    self.rebase_children(solver, parent, &delta, None);
  }

  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    if self.cardinality == Cardinality::Zero {
      return;
    }
    let mut commit = parent.child(&mut self.patch);
    for child in &mut self.children {
      if let Child::Active(goal) = child {
        goal.deepen(solver, &mut commit);
      }
    }
    self.update(solver, parent);
  }
}

#[derive(Debug)]
pub struct Disjunction<G> {
  cardinality: Cardinality,
  children: Vec<G>,
}

impl<G> Disjunction<G> {
  pub fn new(goals: Vec<G>) -> Self {
    Disjunction { cardinality: Cardinality::Unknown, children: goals }
  }
}

impl<G: Goal> Disjunction<G> {
  fn update(&mut self, repo: &mut TypesRepo) {
    let mut cardinality = Cardinality::Zero;
    self.children.retain_mut(|child| {
      let child_cardinality = child.cardinality();
      cardinality = cardinality + child_cardinality;
      if child_cardinality == Cardinality::Zero {
        child.abandon(repo);
        false
      } else {
        true
      }
    });
    self.cardinality = cardinality;
  }
}

impl<G: Goal> Goal for Disjunction<G> {
  type Result = G::Result;

  fn cardinality(&self) -> Cardinality {
    self.cardinality
  }

  fn patch(&mut self) -> Option<&mut TypesPatch> {
    if let [child] = &mut *self.children { child.patch() } else { None }
  }

  fn finish(&mut self) -> Self::Result {
    assert_eq!(self.cardinality, Cardinality::One);
    assert_eq!(self.children.len(), 1);
    self.children[0].finish()
  }

  fn abandon(&mut self, repo: &mut TypesRepo) {
    self.cardinality = Cardinality::Zero;
    for mut child in take(&mut self.children) {
      child.abandon(repo);
    }
  }

  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta) {
    for child in &mut self.children {
      child.rebase(solver, parent, parent_delta);
    }
    self.update(parent.repo);
  }

  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    for child in &mut self.children {
      child.deepen(solver, parent);
    }
    self.update(parent.repo);
  }
}

#[derive(Debug)]
pub enum ImplGoal {
  Pending { query: ImplType, inherent: Vec<Type>, depth: usize },
  Active(Disjunction<ImplCandidate>),
}

impl Goal for ImplGoal {
  type Result = TirImpl;

  fn cardinality(&self) -> Cardinality {
    match self {
      ImplGoal::Pending { .. } => Cardinality::Many,
      ImplGoal::Active(goal) => goal.cardinality(),
    }
  }

  fn patch(&mut self) -> Option<&mut TypesPatch> {
    match self {
      ImplGoal::Pending { .. } => None,
      ImplGoal::Active(goal) => goal.patch(),
    }
  }

  fn finish(&mut self) -> Self::Result {
    match self {
      ImplGoal::Pending { .. } => unreachable!(),
      ImplGoal::Active(goal) => goal.finish(),
    }
  }

  fn abandon(&mut self, repo: &mut TypesRepo) {
    match self {
      ImplGoal::Pending { .. } => {}
      ImplGoal::Active(goal) => goal.abandon(repo),
    }
  }

  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta) {
    match self {
      ImplGoal::Pending { depth, .. } => {
        if *depth >= 1 {
          self.try_activate(solver, parent);
        }
      }
      ImplGoal::Active(goal) => goal.rebase(solver, parent, parent_delta),
    }
    todo!()
  }

  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    match self {
      ImplGoal::Pending { depth, .. } => {
        *depth += 1;
        if *depth == 1 {
          self.try_activate(solver, parent);
        }
      }
      ImplGoal::Active(goal) => goal.deepen(solver, parent),
    }
  }
}

impl ImplGoal {
  fn try_activate(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    let ImplGoal::Pending { query, inherent, depth } = self else { unreachable!() };
    if !inherent.iter().all(|ty| parent.state(*ty).is_known()) {
      return;
    }
    let mut goal = Disjunction::new(solver.impl_candidates(query, parent));
    for _ in 1..*depth {
      goal.deepen(solver, parent);
    }
    *self = ImplGoal::Active(goal);
  }
}

#[derive(Debug)]
pub struct ImplCandidate {
  kind: Option<ImplCandidateKind>,
  goal: Conjunction<ImplGoal>,
}

impl ImplCandidate {
  pub fn new(
    kind: ImplCandidateKind,
    unification: UnifyResult,
    patch: TypesPatch,
    children: impl IntoIterator<Item = ImplGoal>,
  ) -> Self {
    ImplCandidate { kind: Some(kind), goal: Conjunction::new(unification, patch, children) }
  }
}

impl Goal for ImplCandidate {
  type Result = TirImpl;

  fn cardinality(&self) -> Cardinality {
    self.goal.cardinality()
  }

  fn patch(&mut self) -> Option<&mut TypesPatch> {
    self.goal.patch()
  }

  fn finish(&mut self) -> Self::Result {
    self.kind.take().unwrap().construct(self.goal.finish())
  }

  fn abandon(&mut self, repo: &mut TypesRepo) {
    self.goal.abandon(repo);
  }

  fn rebase(&mut self, solver: &mut Solver, parent: &mut TypesCommit, parent_delta: &Delta) {
    self.goal.rebase(solver, parent, parent_delta);
  }

  fn deepen(&mut self, solver: &mut Solver, parent: &mut TypesCommit) {
    self.goal.deepen(solver, parent);
  }
}

#[derive(Debug, Clone)]
pub enum ImplCandidateKind {
  Fixed(TirImpl),
  Def(ImplId),
  Fn(FnId, usize),
}

impl ImplCandidateKind {
  fn construct(self, subimpls: Vec<TirImpl>) -> TirImpl {
    match self {
      ImplCandidateKind::Fixed(impl_) => impl_,
      ImplCandidateKind::Def(impl_id) => TirImpl::Def(impl_id, subimpls),
      ImplCandidateKind::Fn(fn_id, param_count) => TirImpl::Fn(fn_id, subimpls, param_count),
    }
  }
}
