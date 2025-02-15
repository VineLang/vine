use crate::{
  ivm::IVM,
  port::{Port, PortRef, Tag},
  wire::Wire,
};

macro_rules! sym {
  ($a: pat, $b: pat) => {
    ($a, $b) | ($b, $a)
  };
  ($a: pat) => {
    ($a, $a)
  };
}

impl<'ivm, 'ext> IVM<'ivm, 'ext> {
  /// Link two ports.
  pub fn link(&mut self, a: Port<'ivm>, b: Port<'ivm>) {
    use Tag::*;
    match (a.tag(), b.tag()) {
      (Wire, _) => self.link_wire(unsafe { a.as_wire() }, b),
      (_, Wire) => self.link_wire(unsafe { b.as_wire() }, a),
      sym!(Global | Erase) | sym!(ExtVal | Erase) => self.stats.erase += 1,
      sym!(Comb) | sym!(ExtFn) if a.label() == b.label() => self.active_fast.push((a, b)),
      sym!(Global, _) | sym!(Comb | ExtFn | Branch) => self.active_slow.push((a, b)),
      sym!(Erase, _) | sym!(ExtVal, _) => self.active_fast.push((a, b)),
    }
  }

  /// Link a wire and a port.
  pub fn link_wire(&mut self, a: Wire<'ivm>, b: Port<'ivm>) {
    let b = self.follow(b);
    if let Some(s) = a.swap_target(Some(unsafe { b.clone() })) {
      self.free_wire(a);
      self.link(s, b);
    }
  }

  /// Link two_wires.
  #[inline(always)]
  pub fn link_wire_wire(&mut self, a: Wire<'ivm>, b: Wire<'ivm>) {
    self.link_wire(a, Port::new_wire(b));
  }

  /// Follows as many `Wire`s with active targets as currently possible.
  #[inline]
  pub fn follow(&mut self, mut p: Port<'ivm>) -> Port<'ivm> {
    while p.tag() == Tag::Wire {
      if let Some(q) = unsafe { p.clone().as_wire() }.load_target() {
        self.free_wire(unsafe { p.as_wire() });
        p = q;
      } else {
        break;
      }
    }
    p
  }

  /// Non-destructively follows as many `Wire`s with active targets as currently
  /// possible.
  #[inline]
  pub fn follow_ref<'a>(&self, p: &'a Port<'ivm>) -> PortRef<'a, 'ivm> {
    let mut p = PortRef::from(p);
    while p.tag() == Tag::Wire {
      if let Some(q) = unsafe { p.clone().as_wire() }.load_target() {
        p = PortRef::from_port(q);
      } else {
        break;
      }
    }
    p
  }

  /// Execute an interaction between two principal ports.
  pub(crate) fn interact(&mut self, a: Port<'ivm>, b: Port<'ivm>) {
    use Tag::*;
    match ((a.tag(), a), (b.tag(), b)) {
      sym!((Wire, _), _) | sym!((Erase | ExtVal, _)) => unreachable!(),
      sym!((Global, c), (Comb, d)) if !unsafe { c.as_global() }.labels.has(d.label()) => {
        self.copy(c, d)
      }
      sym!((Global, c), (_, p)) => self.expand(c, p),
      ((Comb, a), (Comb, b)) | ((ExtFn, a), (ExtFn, b)) | ((Branch, a), (Branch, b))
        if a.label() == b.label() =>
      {
        self.annihilate(a, b)
      }
      sym!((Erase, n), (_, b)) | sym!((ExtVal, n), (Comb, b)) => self.copy(n, b),
      ((Comb | ExtFn | Branch, a), (Comb | ExtFn | Branch, b)) => self.commute(a, b),
      sym!((ExtFn, f), (ExtVal, v)) => self.call(f, v),
      sym!((Branch, b), (ExtVal, v)) => self.branch(b, v),
    }
  }

  // Note that all of the following functions are technically unsafe -- they
  // require the ports they are passed to have certain tags. They are not marked
  // as `unsafe` to reduce noise, and as such must be kept private.

  fn expand(&mut self, c: Port<'ivm>, p: Port<'ivm>) {
    self.stats.expand += 1;
    let global = unsafe { c.as_global() };
    self.execute(&global.instructions, p);
  }

  fn annihilate(&mut self, a: Port<'ivm>, b: Port<'ivm>) {
    self.stats.annihilate += 1;
    let (a1, a2) = unsafe { a.aux() };
    let (b1, b2) = unsafe { b.aux() };
    self.link_wire_wire(a1, b1);
    self.link_wire_wire(a2, b2);
  }

  fn copy(&mut self, n: Port<'ivm>, b: Port<'ivm>) {
    self.stats.copy += 1;
    let (x, y) = unsafe { b.aux() };
    self.link_wire(x, unsafe { n.clone() });
    self.link_wire(y, n);
  }

  fn commute(&mut self, a: Port<'ivm>, b: Port<'ivm>) {
    self.stats.commute += 1;
    let a_1 = unsafe { self.new_node(a.tag(), a.label()) };
    let a_2 = unsafe { self.new_node(a.tag(), a.label()) };
    let b_1 = unsafe { self.new_node(b.tag(), b.label()) };
    let b_2 = unsafe { self.new_node(b.tag(), b.label()) };

    let (a_0_1, a_0_2) = unsafe { a.aux() };
    let (b_0_1, b_0_2) = unsafe { b.aux() };

    self.link_wire_wire(a_1.1, b_1.1);
    self.link_wire_wire(a_1.2, b_2.1);
    self.link_wire_wire(a_2.1, b_1.2);
    self.link_wire_wire(a_2.2, b_2.2);

    self.link_wire(a_0_1, b_1.0);
    self.link_wire(a_0_2, b_2.0);
    self.link_wire(b_0_1, a_1.0);
    self.link_wire(b_0_2, a_2.0);
  }

  fn call(&mut self, f: Port<'ivm>, lhs: Port<'ivm>) {
    let ext_fn = unsafe { f.as_ext_fn() };
    let (rhs_wire, out) = unsafe { f.aux() };
    if let Some(rhs) = rhs_wire.load_target() {
      if rhs.tag() == Tag::ExtVal {
        self.stats.call += 1;
        self.free_wire(rhs_wire);
        let result = unsafe { self.extrinsics.call(ext_fn, lhs.as_ext_val(), rhs.as_ext_val()) };
        self.link_wire(out, Port::new_ext_val(result));
        return;
      }
    }
    let new_fn = unsafe { self.new_node(Tag::ExtFn, ext_fn.swap().bits()) };
    self.link_wire(rhs_wire, new_fn.0);
    self.link_wire(new_fn.1, lhs);
    self.link_wire_wire(new_fn.2, out);
  }

  fn branch(&mut self, b: Port<'ivm>, v: Port<'ivm>) {
    self.stats.branch += 1;
    let val = self.extrinsics.ext_val_as_n32(unsafe { v.as_ext_val() });
    let (a, o) = unsafe { b.aux() };
    let (b, z, p) = unsafe { self.new_node(Tag::Branch, 0) };
    self.link_wire(a, b);
    let (y, n) = if val == 0 { (z, p) } else { (p, z) };
    self.link_wire(n, Port::ERASE);
    self.link_wire_wire(o, y);
  }
}
