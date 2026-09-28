#[macro_export]
macro_rules! for_enum {
  (
    $(#[$meta:meta])*
    $vis:vis enum $enum:ident $($rest:tt)+
  ) => {
    $(#[$meta])*
    $vis enum $enum $($rest)*
    $crate::for_enum!(_ ($) $enum $($rest)+);
  };
  (_ ($d:tt) $enum:ident $init:tt $($rest:tt)+) => {
    $crate::for_enum!(_ ($) $enum $($rest)+)
  };
  (_ ($d:tt) $enum:ident { $($variant:ident $(($($_0:tt)*))? $({$($_1:tt)*})?),* $(,)? } ) => {
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
