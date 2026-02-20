pub trait Register<R>: Sized {
  fn register(self, registry: &mut R);
}

macro_rules! impl_tuple {
  ($($T:ident)* | $next:ident $($rest:ident)*) => {
    impl_tuple!($($T)* |);
    impl_tuple!($($T)* $next | $($rest)*);
  };
  ($($T:ident)* |) => {
    #[allow(warnings)]
    impl<R, $($T: Register<R>,)*> Register<R> for ($($T,)*) {
      fn register(self, registry: &mut R) {
        let ($($T,)*) = self;
        $($T.register(registry);)*
      }
    }
  };
}

impl_tuple!(|
  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P
  A1 B1 C1 D1 E1 F1 G1 H1 I1 J1 K1 L1 M1 N1 O1 P1
  A2 B2 C2 D2 E2 F2 G2 H2 I2 J2 K2 L2 M2 N2 O2 P2
  A3 B3 C3 D3 E3 F3 G3 H3 I3 J3 K3 L3 M3 N3 O3 P3
);
