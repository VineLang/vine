
((ident) @keyword (#eq? @keyword "pub"))      "pub"      @keyword
((ident) @keyword (#eq? @keyword "mod"))      "mod"      @keyword
((ident) @keyword (#eq? @keyword "use"))      "use"      @keyword
((ident) @keyword (#eq? @keyword "as"))       "as"       @keyword
((ident) @keyword (#eq? @keyword "fn"))       "fn"       @keyword
((ident) @keyword (#eq? @keyword "struct"))   "struct"   @keyword
((ident) @keyword (#eq? @keyword "enum"))     "enum"     @keyword
((ident) @keyword (#eq? @keyword "type"))     "type"     @keyword
((ident) @keyword (#eq? @keyword "trait"))    "trait"    @keyword
((ident) @keyword (#eq? @keyword "impl"))     "impl"     @keyword
((ident) @keyword (#eq? @keyword "match"))    "match"    @keyword
((ident) @keyword (#eq? @keyword "let"))      "let"      @keyword
((ident) @keyword (#eq? @keyword "const"))    "const"    @keyword
((ident) @keyword (#eq? @keyword "in"))       "in"       @keyword
((ident) @keyword (#eq? @keyword "is"))       "is"       @keyword
((ident) @keyword (#eq? @keyword "return"))   "return"   @keyword
((ident) @keyword (#eq? @keyword "do"))       "do"       @keyword
((ident) @keyword (#eq? @keyword "loop"))     "loop"     @keyword
((ident) @keyword (#eq? @keyword "while"))    "while"    @keyword
((ident) @keyword (#eq? @keyword "for"))      "for"      @keyword
((ident) @keyword (#eq? @keyword "break"))    "break"    @keyword
((ident) @keyword (#eq? @keyword "continue")) "continue" @keyword
((ident) @keyword (#eq? @keyword "if"))       "if"       @keyword
((ident) @keyword (#eq? @keyword "when"))     "when"     @keyword
((ident) @keyword (#eq? @keyword "else"))     "else"     @keyword
((ident) @keyword (#eq? @keyword "true"))     "true"     @keyword
((ident) @keyword (#eq? @keyword "false"))    "false"    @keyword

"inline_ivy!" @keyword

[
  "."
  "..."
  ","
  ";"
  ":"
  "::"
  "#"
  "#["
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
  "=>"
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
  ".."
  "..="
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
