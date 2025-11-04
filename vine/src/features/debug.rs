use std::{collections::hash_map::Entry, mem::replace};

use ivy::ast::{Net, Tree};

use crate::{
  components::{emitter::Emitter, synthesizer::SyntheticItem},
  structures::{
    ast::Span,
    specializations::{Spec, SpecKind, SpecRels},
  },
};

impl Emitter<'_> {
  pub(crate) fn tap_debug(&mut self) -> Tree {
    let w = self.new_wire();
    let debug = self.debug_state.as_mut().unwrap();
    Tree::Comb("ref".into(), Box::new(replace(&mut debug.0, w.0)), Box::new(w.1))
  }

  pub(crate) fn tap_debug_call(&mut self, span: Span) -> Tree {
    if self.fragment.frameless {
      return self.tap_debug();
    }
    let len = self.specs.synthetic.len();
    let index = match self
      .specs
      .synthetic
      .entry((SyntheticItem::Frame(self.fragment.path.clone(), span), vec![]))
    {
      Entry::Occupied(e) => self.specs.specs[*e.get()].as_ref().unwrap().index,
      Entry::Vacant(entry) => {
        let (item, _) = entry.key().clone();
        let spec_id = self.specs.specs.push(None);
        entry.insert(spec_id);
        self.specs.specs[spec_id] = Some(Spec {
          path: ":synthetic".into(),
          index: len,
          singular: false,
          rels: SpecRels::default(),
          kind: SpecKind::Synthetic(item),
        });
        len
      }
    };
    let w = self.new_wire();
    let io = self.new_wire();
    let len = self.new_wire();
    let buf = self.new_wire();
    let end = self.new_wire();
    let debug = self.debug_state.as_mut().unwrap();
    let val = replace(&mut debug.0, w.0);
    self.pairs.push((
      val,
      Tree::n_ary(
        "tup",
        [
          io.0,
          Tree::ExtFn("n32_add".into(), false, Box::new(Tree::N32(1)), Box::new(len.0)),
          buf.0,
          end.0,
        ],
      ),
    ));
    let new_value = Tree::n_ary(
      "tup",
      [
        io.1,
        len.1,
        Tree::n_ary("tup", [Tree::Global(format!(":synthetic:{index}")), buf.1]),
        end.1,
      ],
    );
    let io = self.new_wire();
    let len = self.new_wire();
    let buf = self.new_wire();
    let end = self.new_wire();
    let new_space = Tree::n_ary(
      "tup",
      [
        io.0,
        Tree::ExtFn("n32_sub".into(), false, Box::new(Tree::N32(1)), Box::new(len.0)),
        Tree::n_ary("tup", [Tree::Erase, buf.0]),
        end.0,
      ],
    );
    self.pairs.push((w.1, Tree::n_ary("tup", [io.1, len.1, buf.1, end.1])));
    Tree::Comb("ref".into(), Box::new(new_value), Box::new(new_space))
  }

  pub fn with_debug(&mut self, inner: Tree) -> Tree {
    if self.debug {
      let i = self.new_wire();
      let o = self.new_wire();
      self.debug_state = Some((i.0, o.0));
      Tree::Comb(
        "dbg".into(),
        Box::new(Tree::Comb("ref".into(), Box::new(i.1), Box::new(o.1))),
        Box::new(inner),
      )
    } else {
      inner
    }
  }
}

pub(crate) fn debug_main(main: Tree) -> Net {
  Net {
    root: Tree::n_ary("dup", [Tree::Var("io0".into()), Tree::Var("io1".into())]),
    pairs: vec![(
      main,
      Tree::n_ary(
        "fn",
        [
          Tree::n_ary(
            "ref",
            [
              Tree::n_ary(
                "tup",
                [
                  Tree::Var("io0".into()),
                  Tree::N32(0),
                  Tree::Var("x".into()),
                  Tree::Var("x".into()),
                ],
              ),
              Tree::Erase,
            ],
          ),
          Tree::Var("io1".into()),
        ],
      ),
    )],
  }
}
