use ivm::{heap::Heap, IVM};

use crate::{ast::Nets, readback::readback, serialize::SerializeOptions};

pub fn pre_reduce(nets: &mut Nets) {
  let mut globals = Vec::new();
  let globals = nets.serialize(&mut globals, SerializeOptions { black_box: true });

  let heap = Heap::new();
  let mut ivm = IVM::new(&heap);
  for global in globals {
    let root = ivm.boot_free(global);
    ivm.normalize();
    let net = readback(&mut ivm, root);
    *nets.get_mut(&global.name).unwrap() = net;
  }
}
