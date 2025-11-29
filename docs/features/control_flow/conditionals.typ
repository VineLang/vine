#import "/lib.typ": *

= Conditionals

== `if`

An #vi[`if`] branches on a @conditions[condition]
  and executes its block when the condition is #vi[`true`].

```vi
fn maybe_greet(&io: &IO, condition: Bool) {
  if condition {
    io.println("hello");
  }
}

maybe_greet(&io, true) // prints `hello`
maybe_greet(&io, false) // does nothing
```

An #vi[`if`] may be followed by an #vi[`else`]
  to execute when the condition is #vi[`false`].

```vi
fn pleasantry(&io: &IO, condition: Bool) {
  if condition {
    io.println("hello");
  } else {
    io.println("goodbye");
  }
}

pleasantry(&io, true) // prints `hello`
pleasantry(&io, false) // prints `goodbye`
```

An #vi[`if`]/#vi[`else`] can be used as an expression,
  and it will evaluate to the value of the executed block.

```vi
fn pleasantry(&io: &IO, condition: Bool) {
  let message = if condition { "hello" } else { "goodbye" };
  io.println(message);
}

pleasantry(&io, true) // prints `hello`
pleasantry(&io, false) // prints `goodbye`
```

Note that Vine does not have an #vi[`else if`] syntax;
  #vi[`when`] can be used instead.

== `when`

The #vi[`when`] expression checks multiple conditions,
  and evaluates the block corresponding to the first #vi[`true`] condition.

```vi
fn count(number: N32) -> String {
  when {
    number == 0 { "zero" }
    number == 1 { "one" }
    number <= 5 { "several" }
    _ { "many" }
  }
}

count(0) // "zero"
count(1) // "one"
count(3) // "several"
count(46) // "many"
```

== `match`

The #vi[`match`] expression checks a value against multiple @patterns[patterns]
  and evaluates the block corresponding to the first pattern that matches.
Pattern matching is often used with @enums[enums].

```vi
enum* Weekday {
  Monday(),
  Tuesday(),
  Wednesday(),
  Thursday(),
  Friday(),
}

fn mood(day: Weekday) -> String {
  match day {
    Weekday::Monday() { ":(" }
    Weekday::Friday() { ":)" }
    _ { ":|" }
  }
}
mood(Weekday::Friday()) // ":)"
mood(Weekday::Wednesday()) // ":|"
```

