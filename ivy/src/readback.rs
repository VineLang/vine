use std::collections::{hash_map::Entry, HashMap};

use ivm::{
  addr::Addr,
  ext::ExtTy,
  port::{Port, Tag},
  wire::Wire,
  IVM,
};

use crate::ast::{Net, Tree};

pub fn readback<'ivm>(ivm: &mut IVM<'ivm>, root: Port<'ivm>) -> Net {
  let mut reader = Reader { ivm, vars: HashMap::new(), next_var: 0 };
  let root = reader.read_port(root);
  let mut pairs = Vec::new();
  while let Some((a, b)) = reader.ivm.inert.pop() {
    let a = reader.read_port(a);
    let b = reader.read_port(b);
    pairs.push((a, Tree::BlackBox(Box::new(b))));
  }
  Net { root, pairs }
}

struct Reader<'a, 'ivm> {
  ivm: &'a mut IVM<'ivm>,
  vars: HashMap<Addr, usize>,
  next_var: usize,
}

impl<'a, 'ivm> Reader<'a, 'ivm> {
  fn read_port(&mut self, p: Port<'ivm>) -> Tree {
    let p = self.ivm.follow(p);
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
          ExtTy::IO => unimplemented!(),
        }
      }
      Tag::Comb => {
        let label = p.label();
        let (p1, p2) = unsafe { p.aux() };
        Tree::Comb(format!("l{label}"), self.read_wire(p1), self.read_wire(p2))
      }
      Tag::ExtFn => {
        let f = unsafe { p.as_ext_fn() };
        let (p1, p2) = unsafe { p.aux() };
        Tree::ExtFn(f, self.read_wire(p1), self.read_wire(p2))
      }
      Tag::Branch => {
        let (p1, p2) = unsafe { p.aux() };
        let p1 = self.ivm.follow(Port::new_wire(p1));
        assert_eq!(p1.tag(), Tag::Branch);
        let (p11, p12) = unsafe { p1.aux() };
        Tree::Branch(self.read_wire(p11), self.read_wire(p12), self.read_wire(p2))
      }
    }
  }

  fn read_wire(&mut self, w: Wire<'ivm>) -> Box<Tree> {
    Box::new(self.read_port(Port::new_wire(w)))
  }
}
