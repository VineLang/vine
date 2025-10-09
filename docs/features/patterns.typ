#import "/lib.typ": *

= Patterns <patterns>

_Patterns_ appear in:

- #vi[`let`] bindings (#vi[`let pattern = ...`])
- #vi[`fn`] parameters (#vi[`fn foo(pattern: ...) { ... }`])
- #vi[`match`] arms (#vi[`match foo { pattern { ... } }`])
- @is[#op[`is`] expressions] (#vi[`foo is pattern`])

Patterns are used to declare variables, unwrap data, and match values.

== Complete Patterns

Variables are declared using _variable patterns_.

```vi
let x = 1; // here, `x` is a variable pattern
```

Variable patterns are often subpatterns of other patterns.

_Composite patterns_ unwrap @composite[composite types]:

```vi
// tuple pattern
let tuple = (1, 2);
let (x, y) = tuple; // here, `(x, y)` is a tuple pattern
                    // and `x` and `y` are variable subpatterns
x // 1
y // 2
```

```vi
// object pattern
let object = { a: 1, b: 2.0 };
let { a, b: float } = object;
a // 1
float // 2.0
```

_Struct_ patterns unwrap @structs[structs]:

```vi
struct Foo(N32);
let foo = Foo(123);

// struct pattern
let Foo(x) = foo;
x // 123
```

```vi
struct Point((N32, N32));
let p = Point((1, 2));

// struct pattern with tuple subpattern
let Point((x, y)) = p;
x // 123
y // "abc"

// the inner set of parentheses can also be omitted:
let Point(x, y) = p;
x // 123
y // "abc"
```

```vi
struct Place({ latitude: F32, longitude: F32 });
let magnetic_north_pole = Place({ latitude: 86.494, longitude: 162.867 });

// struct pattern with object subpattern
let Place({ latitude, longitude }) = magnetic_north_pole;
latitude // 86.494
longitude // 162.867
```

The `_` pattern discards the matched value.

```vi
let tuple = (1, 2);
let (x, _) = tuple; // discard the second field
x // 1
```

These are all _complete patterns_, because they match all values of their type.
Only complete patterns can be used in `let` bindings and `fn` parameters.
#todo[talk about let/else somewhere?]

Other kinds of complete patterns include:

- @references[reference/dereference patterns]
- @inverse[inverse patterns]

== Incomplete Patterns

_Enum patterns_ match values of an @enums[enum type].
They are _incomplete patterns_, because they do not match all values of their type;
  they only match one variant of the enum.
Any pattern with incomplete subpatterns is also incomplete.

Incomplete patterns can be used in `match` expressions to perform pattern matching.
For example:

```vi
fn display(score: Option[N32]) -> String {
  match score {
    Some(value) { "score: {value}" }
    None { "no score" }
  }
}

display(Some(123)) // "score: 123"
display(None) // "no score"
```

Incomplete patterns can also be used with the @is[`is` operator].
