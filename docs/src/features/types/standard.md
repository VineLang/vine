# Standard

The standard library defines various commonly-used types.

(In the following sections, the time complexity of operations is described in
[Big O notation](https://en.wikipedia.org/wiki/Big_O_notation).)

## `String`

The type `String` describes Unicode strings, represented as a list of Unicode
scalar values.

Strings can be written as literals using double quotes (`"Hello, world!"`).

Strings can be concatenated with the `++` operator (`O(1)`).

## `List`

The generic type `List[T]` describes lists of values of type `T`.

Lists are optimized for fast concatenation and in-order iteration. Accessing the
first value in a list is `O(1)`, but accessing an arbitrary value is `O(n)`. An
array is a better choice if frequently accessing elements by index.

Lists can be written as literals using square brackets (`[1, 2, 3]`).

Lists can be concatenated with the `++` operator (`O(1)`).

Lists can be used as a queue by using `.push_back` to enqueue and `.pop_front`
to dequeue (both `O(1)`).

Lists can be used as a stack by using `.push_front` to push and `.pop_front` to
pop (both `O(1)`).

## `Array`

The generic type `Array[T]` describes arrays of values of type `T`.

Arrays are optimized for fast random access. Accessing an arbitrary value in the
array is `O(log(n))`.

Elements can be added and removed from either the front or the back with
`push_front`/`pop_front`/`push_back`/`pop_back` (`O(log(n))`).

`Array`s can be converted to and from lists with
`Array::to_list`/`Array::from_list` (`O(n)`).

## `Map`

The generic type `Map[K, V]` describes mappings from keys of type `K` to values
of type `V`.

Maps are sorted by their key, and can be efficiently iterated over using `.iter`
and `.into_iter`.

Inserting, accessing, and removing a value by key can be done with
`.insert`/`.get`/`.remove` (`O(log(n))`).

The maximum/minimum key-value pair can be removed with
`.remove_max`/`.remove_min` (`O(log(n))`).
