use std::collections::{hash_map::Entry, HashMap};

use ivm::{
  addr::Addr,
  ext::ExtTy,
  port::{Port, PortRef, Tag},
  wire::Wire,
  IVM,
};

use crate::{ast::Tree, serialize::Labels};

pub struct Reader<'a, 'ivm> {
  ivm: &'a IVM<'ivm>,
  labels: &'a Labels<'a>,
  vars: HashMap<Addr, usize>,
  next_var: usize,
}

impl<'a, 'ivm> Reader<'a, 'ivm> {
  pub fn new(ivm: &'a IVM<'ivm>, labels: &'a Labels<'a>) -> Self {
    Reader { ivm, labels, vars: HashMap::new(), next_var: 0 }
  }

  pub fn read_port(&mut self, p: &Port<'ivm>) -> Tree {
    let p = self.ivm.follow_ref(p);
    match p.tag() {
      Tag::Wire => {
        let n = match self.vars.entry(p.addr()) {
          Entry::Occupied(e) => e.remove(),
          Entry::Vacant(e) => {
            let n = self.next_var;
            self.next_var += 1;
            e.insert(n);
            n
          }
        };
        Tree::Var(format!("n{n}"))
      }
      Tag::Global => Tree::Global(unsafe { p.as_global() }.name.clone()),
      Tag::Erase => Tree::Erase,
      Tag::ExtVal => {
        let val = unsafe { p.as_ext_val() };
        match val.ty() {
          ExtTy::u32 => Tree::U32(val.as_u32()),
          ExtTy::f32 => Tree::F32(val.as_f32()),
          ExtTy::IO => Tree::Var("#io".into()),
        }
      }
      Tag::Comb => {
        let label = p.label();
        let (p1, p2) = unsafe { p.aux_ref() };
        Tree::Comb(self.labels.from_id(label).to_owned(), self.read_wire(&p1), self.read_wire(&p2))
      }
      Tag::ExtFn => {
        let f = unsafe { p.as_ext_fn() };
        let (p1, p2) = unsafe { p.aux_ref() };
        Tree::ExtFn(f, self.read_wire(&p1), self.read_wire(&p2))
      }
      Tag::Branch => {
        let (p1, p2) = unsafe { p.aux_ref() };
        let p1 = PortRef::new_wire(&p1);
        let p1 = self.ivm.follow_ref(&p1);
        assert_eq!(p1.tag(), Tag::Branch);
        let (p11, p12) = unsafe { p1.aux_ref() };
        Tree::Branch(self.read_wire(&p11), self.read_wire(&p12), self.read_wire(&p2))
      }
    }
  }

  pub fn read_wire(&mut self, w: &Wire<'ivm>) -> Box<Tree> {
    Box::new(self.read_port(&PortRef::new_wire(w)))
  }
}
