use std::{
  collections::HashMap,
  mem::replace,
  sync::{Arc, Mutex},
};

use ivm::{
  host::ext::{ExtFn, ExtTyBoxed, HostTable},
  runtime::{
    Runtime,
    ext::Boxed,
    port::{Port, Tag},
    wire::Wire,
  },
};
use vine_util::register::Register;

use crate::{
  backend::backend,
  components::{finder::Finder, specializer::Specializer},
  structures::{
    ast::Span,
    chart::GenericsId,
    types::{ImplType, Type},
  },
  tools::repl::CONFIG,
};

use super::Repl;

impl<'ctx, 'ivm, 'ext, 'comp> Repl<'ctx, 'ivm, 'ext, 'comp> {
  pub fn show(&mut self, ty: Type, port: &mut Port<'ivm>) -> String {
    let (new_port, string) = self._show(ty, replace(port, Port::ERASE));
    *port = new_port;
    string.unwrap_or_else(|| format!("<{}>", self.types.show(&self.compiler.chart, ty)))
  }

  fn _show(&mut self, ty: Type, port: Port<'ivm>) -> (Port<'ivm>, Option<String>) {
    let (Some(show), Some(repl_show)) =
      (self.compiler.chart.builtins.show, self.compiler.chart.builtins.repl_show)
    else {
      return (port, None);
    };

    let checkpoint = self.compiler.checkpoint();

    let mut finder = Finder::new(
      &self.compiler.chart,
      &self.compiler.sigs,
      &mut self.compiler.diags,
      &mut self.compiler.finder_cache,
      self.repl_mod,
      GenericsId::NONE,
      Span::NONE,
    );
    let impl_ty = ImplType::Trait(show, vec![ty]);
    let Ok(impl_) = finder.try_find_impl(&mut self.types, &impl_ty, false) else {
      return (port, None);
    };

    let mut specializer = Specializer {
      chart: &self.compiler.chart,
      resolutions: &self.compiler.resolutions,
      specs: &mut self.compiler.specs,
      fragments: &self.compiler.fragments,
      vir: &self.compiler.vir,
      guide: &self.guide,
      table: self.table,
    };
    let Ok((spec_id, stage_id)) =
      specializer.instantiate_fn_rel(None, &Vec::new(), &(repl_show, vec![impl_]))
    else {
      return (port, None);
    };

    {
      let mut nets = HashMap::new();
      self.compiler.nets_from(self.table, &mut nets, &checkpoint);
      backend(self.table, &CONFIG, &mut nets);
      self.program.build(self.host, self.table, &nets);
    }
    let name =
      self.compiler.specs.specs[spec_id].as_ref().unwrap().name.clone().with_payload(stage_id.0);
    let graft = self.program.graft(self.table.add_name(name)).unwrap();

    let w = self.rt.new_wire();
    let root = w.0;

    fn make_node<'ivm>(ivm: &mut Runtime<'ivm, '_>, wire: Wire<'ivm>) -> (Wire<'ivm>, Wire<'ivm>) {
      let node = unsafe { ivm.new_node(Tag::Comb, 0) };
      ivm.link_wire(wire, node.0);
      (node.1, node.2)
    }

    let (recv, cur) = make_node(self.rt, w.1);
    self.rt.link_wire(recv, Port::ERASE);
    let (slot_, cur) = make_node(self.rt, cur);
    let (ref_, nil) = make_node(self.rt, cur);
    let (val_in, val_out) = make_node(self.rt, ref_);
    self.rt.link_wire(val_in, port);
    self.rt.link_wire(root, Port::new_graft(graft));
    self.rt.link_wire(nil, Port::ERASE);

    let slot = Slot(Arc::new(Mutex::new(None)));
    let slot_ext_ty = self.host.get_ext_ty().unwrap();
    self.rt.link_wire(slot_, Port::new_ext_val(slot_ext_ty.wrap_static(Boxed::new(slot.clone()))));

    self.rt.normalize();

    let string = slot.0.lock().unwrap().take();

    (Port::new_wire(val_out), string)
  }
}

#[derive(Clone)]
struct Slot(Arc<Mutex<Option<String>>>);

impl<'ivm> ExtTyBoxed<'ivm> for Slot {
  type With<'x> = Slot;
}

pub fn extrinsics<'ivm, 'r>() -> impl Register<HostTable<'ivm, 'r>> {
  ExtFn("vi:repl:show", |(slot, str): (Slot, String)| {
    *slot.0.lock().unwrap() = Some(str);
  })
}
