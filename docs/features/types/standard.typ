#import "/lib.typ": *

= Standard Types <standard-types>

The standard library defines various commonly-used types.

(In the following sections, the time complexity of operations is described in
  #link("https://en.wikipedia.org/wiki/Big_O_notation")[Big O notation].)

== `String` <string>

The type #ty[`String`] describes Unicode strings,
  represented as a list of Unicode scalar values.

Strings can be written as literals using double quotes (#expr[`"Hello, world!"`]).

Expressions can be interpolated into string literals using braces
  (#expr[`"{10} + {36} = {10 + 36}"`]).

Strings can be concatenated with the #op[`++`] operator (`O(1)`).

== `List` <list>

The generic type #ty[`List[T]`] describes lists of values of type #ty[`T`].

Lists are optimized for fast concatenation and in-order iteration.
Accessing the first value in a list is `O(1)`,
  but accessing an arbitrary value is `O(n)`.
An array is a better choice if frequently accessing elements by index.

Lists can be written as literals using square brackets (#expr[`[1, 2, 3]`]).

Lists can be concatenated with the #op[`++`] operator (`O(1)`).

Lists can be used as a queue by
  using #fn[`.push_back`] to enqueue
  and #fn[`.pop_front`] to dequeue (both `O(1)`).

Lists can be used as a stack by
  using #fn[`.push_front`] to push
  and #fn[`.pop_front`] to pop (both `O(1)`).

== `Array`

The generic type #ty[`Array[T]`] describes arrays of values of type #ty[`T`].

Arrays are optimized for fast random access.
Accessing an arbitrary value in the array is `O(log(n))`.

Elements can be added and removed from either the front or the back with
  #fn[`.push_front`] / #fn[`.pop_front`] / #fn[`.push_back`] / #fn[`.pop_back`] (`O(log(n))`).

`Array`s can be converted to and from lists with
  #expr[`array as List`] and #expr[`list as Array`] (`O(n)`).

== `Map`

The generic type #ty[`Map[K, V]`] describes mappings from keys of type #ty[`K`] to values of type #ty[`V`].

Maps are sorted by their key,
  and can be efficiently iterated over using #fn[`.iter`] and #fn[`.into_iter`].

Inserting, accessing, and removing a value by key can be done with
  #fn[`.insert`] / #fn[`.get`] / #fn[`.remove`] (`O(log(n))`).

The minimum / maximum key-value pair can be removed with
  #fn[`.remove_min`] / #fn[`.remove_max`] (`O(log(n))`).

== `Set`

The generic type #ty[`Set[T]`] describes sets of elements of type #ty[`T`].

Sets are sorted by their key,
  and can be efficiently iterated over using #fn[`.iter`] and #fn[`.into_iter`].

Inserting, checking for, and removing a value can be done with
  #fn[`.insert`] / #fn[`.has`] / #fn[`.remove`] (`O(log(n))`).

The minimum / maximum value pair can be removed with
  #fn[`.remove_min`] / #fn[`.remove_max`] (`O(log(n))`).
