#import "/lib.typ": *

= Primitive Types <primitive-types>

(Note: there is no implicit casting between numeric types. Values can be
converted to another primitive type using the #op[`as`] operator
(#expr[`45 as F32 + 1.0`]).)

== `N32` <n32>

#let nats-note = footnote[Natural numbers are non-negative integers. In other programming languages, they are often referred to as "unsigned integers". Seeing as positive integers do have a sign (namely, a positive sign), the only truly unsigned integer is zero.]

The type #ty[`N32`] describes natural numbers#nats-note, represented with 32 bits of precision.

#ty[`N32`] values can be written as literals in decimal (#expr[`46`]), hex (#expr[`0x2e`]), or
binary (#expr[`0b101110`]). Digits can be separated with underscores (#expr[`1_000_000`]).

#ty[`N32`]s support the usual arithmetic and bitwise operators (#expr[`4 * 11 + 2`],
#expr[`5 << 3 | 6`])

== `I32`

The type #ty[`I32`] describes integers, represented with 32 bits of precision.

#ty[`I32`] values can be written as literals in decimal (#expr[`+46`], #expr[`-46`]), hex (#expr[`+0x2e`],
#expr[`-0x2e`]), or binary (#expr[`+0b101110`], #expr[`-0b101110`]). The sign is required. Digits can
be separated with underscores (#expr[`+1_000_000`], #expr[`-1_000_000`]).

#ty[`I32`]s support the usual arithmetic and bitwise operators (#expr[`+4 * +12 + -2`],
#expr[`+5 << +3 | +6`])

== `F32` <f32>

The type #ty[`F32`] describes 32-bit floating-point numbers (following IEEE 754).

#ty[`F32`] values can be written as literals (#expr[`4.6e1`]). The decimal point is
required.

#ty[`F32`]s support the usual arithmetic operators (#expr[`3.6 * 12.3 + 1.72`]).

== `Char`

The type #ty[`Char`] describes Unicode scalar values. #ty[`Char`]s are primarily used
within #ty[`String`]s.

#ty[`Char`] values can be written as literals using single quotes (#expr[`'.'`]).

#ty[`Char`]s support adding an #ty[`N32`], resulting in another #ty[`Char`] (#expr[`'a' + 4`]), as
well as subtracting another #ty[`Char`], resulting in an #ty[`N32`] (#expr[`'G' - 'A'`]).

== `Bool`

The type #ty[`Bool`] describes booleans.

The two #ty[`Bool`] values can be written as literals (#expr[`true`], #expr[`false`]).

`Bool`s support the usual short-circuiting logical operators (#op[`&&`], #op[`||`], #op[`!`])
and non-short-circuiting ("bitwise") operators (#op[`&`], #op[`|`], #op[`^`]).

Expressions that evaluate to booleans are called @conditions[conditions].

== `IO`

#ty[`IO`] is a special primitive type used to interact with the outside world. Values
of this type cannot be explicitly constructed; instead, an #ty[`IO`] handle is passed
in to #fn[`main`] at the start of the program. See the section on @io[IO] for
more detail.

