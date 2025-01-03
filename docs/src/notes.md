# Notes

---

### Vine Expressions, Patterns, Positions *[?]*

In the following blocks`<[value|space|place]-[pat|expr]>` indicate a value,
space, or place position for either for an expression or a pattern.

```
let <value-pat> = <value-expr>

<space-expr> = <value-expr>

<place-expr> += <value-expr>

&<place-expr> // given <place> : T, results in <value> : &T

<value-exp> is <value-pat>

if <value-exp> <block>

<place-exp> += <value-exp>

~<value-expr> // given <value> : T+, results in <space> : ~T-
~<space-expr> // given <space> : T-, results in <value> : ~T+
~<place-expr> // given <place> : T*, results in <place> : ~T*
```

---

#### Behavior of coercion

- to coerce place `{value; space}` for `<expr-value>`
  - copy `value` to `space` and `<expr-value>` result
- to coerce place `{value; space}` for `<expr-space>`
  - erase `value`
  - link `space` to `<expr-space>` result

---

#### Behavior of `let`

- `let <pat-value> : <T>;` uses an eraser as the initial value
  - it is equivalent to `let <pat-value> : <T> = ~_`;
  - `~_` evaluates to a generic eraser value (the inverse of a generic eraser
    space)

---

### Vine Interaction Theory *[?]*

#### Values & Spaces & Places

- a value `v` of type `T` is a *T-value*, written as `v : T+`
- a space `s` of type `T` is a *T-space*, written as `s : T-`
- a place `p` of type `T` is a *T-place*, written as `p : T*`
  - a place is a pairing of a value and a space,\
    `p : T* = (v : T+; s : T-)`

### Inverse

- the inverse of type `T` is the type `~T`
- the inverse of a `T`-value is an `~T`-space
  - `~( v : T+ ) = ~v : ~T-`
- the inverse of a `T`-space is an `~T`-value
  - `~( s : T- ) = ~s : ~T+`
- the inverse of a `T`-place is an `~T`-place
  - ```
      ~( p : T* )
    = ~( v : T+; s : T- )
    = ( ~( s : T- ); ~( v : T+ ) )
    = ( ~s : ~T+; ~v : ~T- )
    = ~p : ~T*
    ```

---

#### Wires

- a wire `w` connects a value `v` to a space `s`
  - ```
    v -- s
    ```
- a typed wire `w : T` connects a value `v : +T` to a space `s : -T`
  - ```
      v : +T -- s : -T
    = v -T- s
    ```

#### Arrows

- an arrow `a` has a space `-a` and a value `+a`
  - ```
      -a -> +a
    = >a>
    ```
- a typed arrow `a : T` has a space `-a : -T` and a value `+a : +T`
  - ```
      -a : -T -> +a : +T
    = -a -T> +a
    = T>a>
    ```
- an arrow connected to two wires is equivalent to a wire
  - ```
      v -- >a> -- s
    = v -- s
    ```
- *[show typed version?]*

#### Free Wires

- a free wire `f`
  - consists of
    - two arrows, `>i` and `>o`
    - a connecting wire, `+i -- -o`
    - two free ports, `-i` and `+o`
  - ```
      >i -- >o
    = -i -> +i -- -o -> +o
    = -i >> +o
    = >-f->
    ```
  - is is equivalent to an arrow `-i -> +o`
- *[show typed version?]*

#### Variables

- for variable `x : T`
  - sequence of free wires: `>-xf_n->`
    - `>xi_n> -- >xo_n>`
    - `-xi_n : -T`
    - `+xo_n : +T`
  - to declare `x`
    - create `xf_0`
  - to invoke `x` (for the `j`th time)
    - let `k = j + 1`
    - create free wire `xf_k`
    - result in place `(+xo_j; -xi_k)  // x_j`
  - to finalize `x` (after `n` invocations)
    - link `+xo_n` to `-xi_0`

---
