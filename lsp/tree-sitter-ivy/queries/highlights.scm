
[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
  ","
  ";"
  "$"
  "="
] @punctuation

(payload "#" @punctuation) @constant.numeric

((path) @function (#match? @function "^[^:]"))
((path) @string (#match? @string "^:"))

(free) @variable.builtin

(line_comment) @comment
(block_comment) @comment
