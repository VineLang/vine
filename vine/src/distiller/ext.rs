macro_rules! ext_fns {
  ($($name:ident),* $(,)?) => {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(u16)]

    #[allow(nonstandard_style)]
    pub enum ExtFnKind {
      $($name,)*
    }

    impl ExtFnKind {
      pub const ALL: &'static [Self] = &[$(Self::$name),*];
      #[allow(clippy::should_implement_trait)]
      pub fn from_str(str: &str) -> Option<Self> {
        match str {
          $(stringify!($name) => Some(Self::$name),)*
          _ => None,
        }
      }
    }


    impl From<ExtFnKind> for &'static str{
      fn from(f: ExtFnKind) -> &'static str {
        match f {
          $(ExtFnKind::$name => stringify!($name),)*
        }
      }
    }

    impl From<ExtFnKind> for String {
      fn from(f: ExtFnKind) -> String {
          <&'static str as From<ExtFnKind>>::from(f).to_string()
        }
    }
  }
}

ext_fns! {
  seq,

  add,
  sub,
  mul,
  div,
  rem,

  eq,
  ne,
  lt,
  le,

  n32_shl,
  n32_shr,
  n32_rotl,
  n32_rotr,

  n32_and,
  n32_or,
  n32_xor,

  n32_add_high,
  n32_mul_high,

  io_print_char,
  io_print_byte,
  io_flush,
  io_read_byte,
}
