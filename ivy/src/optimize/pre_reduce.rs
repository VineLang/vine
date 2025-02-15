use ivm::{
  heap::Heap,
  port::{Port, Tag},
  IVM,
};

use crate::{
  ast::{Nets, Tree},
  host::{readback::Reader, Host},
};

pub fn pre_reduce(nets: &mut Nets) {
  let heap = &Heap::new();
  let mut host = &mut Host::default();
  host.insert_nets(nets);
  dbg!(&host.get("::std::IO::print::3"));

  let ivm = &mut IVM::new(heap);
  for (name, net) in nets.iter_mut() {
    dbg!(&name);
    // if name != "::std::IO::println" {
    //   continue;
    // }

    let mut reader = Reader::default();
    let r = ivm.new_wire();
    ivm.instantiate(Port::new_wire(r.0), host.get(name).unwrap());
    net.root = reader.read(host, ivm, &Port::new_wire(r.1));
    net.pairs.clear();

    let mut i = 0;
    while let Some((a, b)) = ivm.active.pop() {
      i += 1;
      if i > 10000 {
        break;
      }
      let a = ivm.follow(a);
      let b = ivm.follow(b);
      if a.tag() == Tag::Wire && !reader.vars.contains_key(&a.addr()) {
        ivm.link_wire(unsafe { a.as_wire() }, b);
      } else if b.tag() == Tag::Wire && !reader.vars.contains_key(&b.addr()) {
        ivm.link_wire(unsafe { b.as_wire() }, a);
      } else if a.tag() == Tag::Wire || b.tag() == Tag::Wire {
        let a = reader.read(host, ivm, &a);
        let b = reader.read(host, ivm, &b);
        net.pairs.push((a, b));
      } else {
        ivm.interact(a, b);
      }
    }

    net.pairs.push((Tree::Erase, Tree::Erase));

    while let Some((a, b)) = ivm.active.pop() {
      let a = reader.read(host, ivm, &a);
      let b = reader.read(host, ivm, &b);
      net.pairs.push((a, b));
    }
  }
}
