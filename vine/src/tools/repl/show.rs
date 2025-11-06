use std::mem::replace;

use ivm::{
  port::{Port, Tag},
  wire::Wire,
  IVM,
};
use ivy::{ast::Tree, host::Host};

use crate::{
  components::{finder::Finder, specializer::Specializer},
  structures::{
    ast::Span,
    chart::GenericsId,
    template::global_name,
    types::{ImplType, Type},
  },
};

use super::Repl;

impl<'ctx, 'ivm, 'ext, 'comp> Repl<'ctx, 'ivm, 'ext, 'comp> {
  pub fn show(&mut self, ty: Type, port: &mut Port<'ivm>) -> String {
    let (new_port, string) = self._show(ty, replace(port, Port::ERASE));
    *port = new_port;
    string.unwrap_or_else(|| format!("<{}>", self.types.show(&self.compiler.chart, ty)))
  }

  fn _show(&mut self, ty: Type, port: Port<'ivm>) -> (Port<'ivm>, Option<String>) {
    let (Some(show), Some(show_to_string)) =
      (self.compiler.chart.builtins.show, self.compiler.chart.builtins.show_to_string)
    else {
      return (port, None);
    };

    let checkpoint = self.compiler.checkpoint();

    let mut finder = Finder::new(
      &self.compiler.chart,
      &self.compiler.sigs,
      &mut self.compiler.diags,
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
    };
    let impl_ = specializer.instantiate(None, &Vec::new(), &impl_);
    let Ok((spec_id, stage_id)) = specializer.instantiate_fn_id(show_to_string, vec![impl_]) else {
      return (port, None);
    };

    let nets = self.compiler.nets_from(&checkpoint);
    self.host.insert_nets(&nets);
    let global = global_name(self.compiler.specs.specs[spec_id].as_ref().unwrap(), stage_id);

    let label_fn = Host::label_to_u16("fn", &mut self.host.comb_labels);
    let label_ref = Host::label_to_u16("ref", &mut self.host.comb_labels);

    let w = self.ivm.new_wire();
    let root = w.0;

    fn make_node<'ivm>(
      ivm: &mut IVM<'ivm, '_>,
      label: u16,
      wire: Wire<'ivm>,
    ) -> (Wire<'ivm>, Wire<'ivm>) {
      let node = unsafe { ivm.new_node(Tag::Comb, label) };
      ivm.link_wire(wire, node.0);
      (node.1, node.2)
    }

    let (recv, cur) = make_node(self.ivm, label_fn, w.1);
    self.ivm.link_wire(recv, Port::ERASE);
    let (ref_, string) = make_node(self.ivm, label_fn, cur);
    let (val_in, val_out) = make_node(self.ivm, label_ref, ref_);
    self.ivm.link_wire(val_in, port);
    self.ivm.link_wire(root, Port::new_global(self.host.get(&global).unwrap()));
    self.ivm.normalize();
    let string = Port::new_wire(string);
    let string_tree = self.host.reader(self.ivm).read_port(&string);
    self.ivm.link(string, Port::ERASE);
    self.ivm.normalize();

    (Port::new_wire(val_out), self.read_string(&string_tree))
  }

  fn read_string(&mut self, tree: &Tree) -> Option<String> {
    let mut str = String::new();
    let Tree::Comb(l, len, tree) = tree else { None? };
    if l != "tup" {
      None?
    }
    let Tree::N32(len) = **len else { None? };
    let Tree::Comb(l, buf, _) = &**tree else { None? };
    if l != "tup" {
      None?
    };
    let mut cur = &**buf;
    for _ in 0..len {
      let Tree::Comb(l, char, next) = cur else { None? };
      if l != "tup" {
        None?
      }
      let Tree::N32(char) = **char else { None? };
      let Ok(char) = char::try_from(char) else { None? };
      str.push(char);
      cur = &**next;
    }
    Some(str)
  }
}
