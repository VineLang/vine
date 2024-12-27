# Methods

Currently, method syntax is just syntactic sugar for calling free-standing functions; e.g.
`foo.method(...)` is sugar for `method(&foo, ...)`. (Method names can be arbitrary paths, so
`foo.some::method(...)` is valid and sugar for `some::method(&foo, ...)`.) Eventually, this may be
replaced by type-directed dispatch (like in Rust).
