use std::collections::{hash_map::Entry, HashMap};

use ivm::{
  addr::Addr,
  ext::ExtTy,
  port::{Port, PortRef, Tag},
  wire::Wire,
  IVM,
};
use vine_util::idx::Counter;

use crate::{ast::Tree, host::Host};

impl<'ivm> Host<'ivm> {
  pub fn read(&self, ivm: &IVM<'ivm>, port: &Port<'ivm>) -> Tree {
    Reader::default().read(self, ivm, port)
  }
}

#[derive(Default)]
pub struct Reader {
  pub(crate) vars: HashMap<Addr, usize>,
  var_ids: Counter<usize>,
}

impl Reader {
  pub fn read<'ivm>(&mut self, host: &Host<'ivm>, ivm: &IVM<'ivm>, port: &Port<'ivm>) -> Tree {
    ActiveReader { host, ivm, reader: self }.read_port(port)
  }
}

struct ActiveReader<'ctx, 'ivm> {
  host: &'ctx Host<'ivm>,
  ivm: &'ctx IVM<'ivm>,
  reader: &'ctx mut Reader,
}

impl<'ctx, 'ivm> ActiveReader<'ctx, 'ivm> {
  fn read_port(&mut self, p: &Port<'ivm>) -> Tree {
    let p = self.ivm.follow_ref(p);
    match p.tag() {
      Tag::Wire => {
        let id = match self.reader.vars.entry(p.addr()) {
          Entry::Occupied(e) => e.remove(),
          Entry::Vacant(e) => {
            let id = self.reader.var_ids.next();
            e.insert(id);
            id
          }
        };
        Tree::Var(format!("n{id}"))
      }
      Tag::Global => Tree::Global(unsafe { p.as_global() }.name.clone()),
      Tag::Erase => Tree::Erase,
      Tag::ExtVal => {
        let val = unsafe { p.as_ext_val() };
        match val.ty() {
          ExtTy::N32 => Tree::N32(val.as_n32()),
          ExtTy::F32 => Tree::F32(val.as_f32()),
          ExtTy::IO => Tree::Var("#io".into()),
        }
      }
      Tag::Comb => {
        let label = p.label();
        let (p1, p2) = unsafe { p.aux_ref() };
        Tree::Comb(
          self.host.label_from_u16(label).to_owned(),
          self.read_wire(&p1),
          self.read_wire(&p2),
        )
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

  fn read_wire(&mut self, w: &Wire<'ivm>) -> Box<Tree> {
    Box::new(self.read_port(&PortRef::new_wire(w)))
  }
}
