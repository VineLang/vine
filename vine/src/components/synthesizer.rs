use ivy::ast::{Net, Nets, Tree};
use vine_util::idx::Counter;

use crate::structures::{
  ast::Ident,
  chart::{Chart, StructId, TraitConstId, TraitFnId},
  resolutions::Rels,
  specializations::Spec,
  template::global_name,
  vir::StageId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticImpl<'core> {
  Tuple(usize),
  Object(Ident<'core>, usize),
  Struct(StructId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntheticItem<'core> {
  CompositeDeconstruct(usize),
  CompositeReconstruct(usize),
  Ident(Ident<'core>),
  Identity,
}

struct Synthesizer<'core, 'a> {
  nets: &'a mut Nets,
  #[allow(unused)]
  chart: &'a Chart<'core>,
  spec: &'a Spec<'core>,
  var_id: Counter<usize>,
  stages: Counter<StageId>,
}

pub fn synthesize<'core>(
  nets: &mut Nets,
  chart: &Chart<'core>,
  spec: &Spec<'core>,
  item: SyntheticItem<'core>,
) {
  Synthesizer { nets, chart, spec, var_id: Counter::default(), stages: Counter::default() }
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
    }
  }
}

impl<'core> SyntheticItem<'core> {
  pub fn rels(self) -> Rels<'core> {
    match self {
      SyntheticItem::CompositeDeconstruct(_)
      | SyntheticItem::CompositeReconstruct(_)
      | SyntheticItem::Ident(_)
      | SyntheticItem::Identity => Rels::default(),
    }
  }
}
