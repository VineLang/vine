use core::{fmt, slice};

use crate::runtime::{
  Runtime,
  port::{Port, Tag},
};

/// A net that can be grafted.
#[derive(Debug)]
pub struct Graft<'ivm> {
  /// The next unused register value; equivalently, the number of used
  /// registers.
  pub(crate) next_register: Register,
  /// The actual list of instructions; see [`Graft::push`] for the safety
  /// requirements of these instructions.
  pub(crate) instructions: Vec<Instruction<'ivm>>,
}

impl<'ivm> Graft<'ivm> {
  pub(crate) const fn new() -> Self {
    Self { next_register: Register::new(1), instructions: Vec::new() }
  }

  /// Returns a new, unused register.
  #[inline(always)]
  pub fn new_register(&mut self) -> Register {
    let register = self.next_register;
    self.next_register.byte_offset += REGISTER_SIZE;
    register
  }

  /// Appends an instruction to the list.
  ///
  /// ## Safety
  /// All registers used in the instruction must either be `Register::ROOT` or
  /// have been returned by `self.new_register()`.
  #[inline(always)]
  pub unsafe fn push(&mut self, instruction: Instruction<'ivm>) {
    self.instructions.push(instruction);
  }

  #[inline(always)]
  pub fn instructions(&self) -> &[Instruction<'ivm>] {
    &self.instructions
  }
}

/// An instruction to create a small sub-net.
///
/// It is not unsafe to construct an invalid instruction, but it is unsafe to
/// add it to a [`Graft`] (which is assumed to consist only of valid
/// instructions).
#[derive(Debug)]
pub enum Instruction<'ivm> {
  /// Create a nilary node.
  ///
  /// ## Validity
  /// Its port must truly be nilary (and thus safe to clone).
  Nilary(Port<'ivm>, Register),
  /// Create a binary node with a given tag and label.
  ///
  /// ## Validity
  /// Its tag must be that of a binary node, and its label must comply with the
  /// tag's requirements.
  Binary(Tag, u16, [Register; 3]),
}

impl<'ivm> Instruction<'ivm> {
  pub fn registers(&self) -> &[Register] {
    match self {
      Instruction::Nilary(.., register) => slice::from_ref(register),
      Instruction::Binary(.., registers) => registers,
    }
  }
}

/// A "register" of an [`Instruction`], used to identify pairs of ports to link.
///
/// Serves as an index into the `registers` field in an [`IVM`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register {
  /// The byte offset corresponding to the index; equivalent to `index *
  /// REGISTER_SIZE`.
  ///
  /// Stored as a `u32` to keep the size of an [`Instruction`] small.
  pub(crate) byte_offset: u32,
}

/// The size of the data referenced by a register.
const REGISTER_SIZE: u32 = 8;

impl Register {
  /// Register 0 is reserved for the root of a net.
  pub const ROOT: Register = Register::new(0);

  /// Creates a new register corresponding to the given index.
  #[inline(always)]
  const fn new(index: usize) -> Self {
    Register { byte_offset: index as u32 * REGISTER_SIZE }
  }

  /// Returns the index of this register.
  #[inline(always)]
  pub fn index(&self) -> usize {
    (self.byte_offset / REGISTER_SIZE) as usize
  }
}

impl fmt::Debug for Register {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Register({})", self.index())
  }
}

impl<'ivm> Runtime<'ivm, '_> {
  /// Links the given `port` to the given `register`.
  ///
  /// ## Safety
  /// `register.index() < self.registers.len()`
  #[inline]
  unsafe fn link_register(&mut self, register: Register, port: Port<'ivm>) {
    unsafe {
      debug_assert!(register.index() < self.registers.len());
      let register = &mut *(&mut *self.registers as *mut [_] as *mut Option<Port<'ivm>>)
        .byte_offset(register.byte_offset as isize);
      if let Some(got) = register.take() {
        self.link(port, got);
      } else {
        *register = Some(port);
      }
    }
  }

  /// Execute a [`Graft`], linking the net's root to `port`.
  pub fn graft(&mut self, graft: &Graft<'ivm>, port: Port<'ivm>) {
    let needed_registers = graft.next_register.index().max(1);
    if needed_registers > self.registers.len() {
      self.registers.resize_with(needed_registers, || None)
    }

    unsafe { self.link_register(Register::ROOT, port) };

    for i in &graft.instructions {
      unsafe {
        match *i {
          Instruction::Nilary(ref p, r) => self.link_register(r, p.clone()),
          Instruction::Binary(tag, label, [p0, p1, p2]) => {
            let node = self.new_node(tag, label);
            self.link_register(p0, node.0);
            self.link_register(p1, Port::new_wire(node.1));
            self.link_register(p2, Port::new_wire(node.2));
          }
        }
      }
    }

    // Using registers an odd number of times will cause severe logic errors.
    if cfg!(debug_assertions) {
      for r in &self.registers {
        assert!(r.is_none());
      }
    }
  }
}
