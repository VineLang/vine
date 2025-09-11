
[
  "="
  "#"
  "["
  "]"
  "("
  ")"
  "{"
  "}"
  (erase)
] @punctuation

[
  "@"
  "?"
  "$"
] @operator

(ext_fn (ident) @operator)

[(n32) (f32)] @constant.numeric

(global) @function

(comb (ident) @constructor)

