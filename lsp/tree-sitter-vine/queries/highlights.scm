
[
  "pub"
  "mod"
  "use"
  "as"
  "fn"
  "struct"
  "enum"
  "type"
  "inline_ivy!"
  "trait"
  "impl"
  "match"
  "let"
  "const"
  "in"
  "is"
  "return"
  "do"
  "loop"
  "while"
  "for"
  "break"
  "continue"
  "if"
  "when"
  "else"
  "true"
  "false"
] @keyword

[
  "."
  ","
  ";"
  ":"
  "::"
  "#"
  "["
  "]"
  "("
  ")"
  "{"
  "}"
  "_"
] @punctuation

[
  "&"
  "&&"
  "|"
  "||"
  "^"
  "+"
  "++"
  "-"
  "*"
  "**"
  "/"
  "%"
  "!"
  "?"
  "~"
  "="
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "->"
  "<-"
  "<<"
  ">>"
] @operator

(string ["\"" (string_content)] @string)
(char) @string
(num) @constant.numeric
(string_escape) @constant.character.escape

(line_comment) @comment
(block_comment) @comment

(chain_method (ident) @function)
(expr_path (path (ident) @function .) (exprs))
(expr_path (path (ident) @function . (generic_args) .) (exprs))

(path (ident) @namespace (ident))
(use_tree (ident) @namespace (use_tree))
(ty_path (path (ident) @type .))
(ty_path (path (ident) @type . (generic_args) .))
(ty_param (ident) @type)
(item_fn (ident) @function)
(stmt_let_fn (ident) @function)
