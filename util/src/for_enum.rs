#[macro_export]
macro_rules! for_enum {
  (
    $(#[$_0:meta])*
    $_1:vis enum $enum:ident $($rest:tt)+
  ) => {
    $crate::for_enum!(_ ($) $enum $($rest)+);
  };
  (_ ($d:tt) $enum:ident $init:tt $($rest:tt)+) => {
    $crate::for_enum!(_ ($) $enum $($rest)+)
  };
  (_ ($d:tt) $enum:ident { $($variant:ident $(($($_1:tt)*))? $({$($_2:tt)*})? $(: $_3:ty)?),* $(,)? } ) => {
    #[allow(unused)]
    #[doc(hidden)]
    macro_rules! $enum {
      ($d macro:ident) => {
        $d macro!($($variant)*)
      }
    }
  };
  ($ident:ident in $enum:ident { $($body:tt)* }) => {
    $crate::for_enum!(_ _ ($) $ident $enum {$($body)*})
  };
  (_ _ ($d:tt) $ident:ident $enum:ident $body:tt) => {
    ({
      macro_rules! __for_enum {
        ($d($d $ident:tt)*) => { $body };
      }
      $enum!(__for_enum)
    })
  };
}

#[macro_export]
macro_rules! for_field {
  (
    $(#[$_0:meta])*
    $_1:vis struct $struct:ident $($rest:tt)+
  ) => {
    $crate::for_enum!(_ ($) $struct $($rest)+);
  };
  ($ident:ident in $struct:ident { $($body:tt)* }) => {
    $crate::for_enum!(_ _ ($) $ident $struct {$($body)*})
  };
}
