/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "ivy",

  extras: $ => [new RustRegex("[ \\t\\r\\n\\f]+"), $.line_comment, $.block_comment],

  supertypes: $ => [
    $._expr,
  ],

  rules: {
    source_file: $ => repeat($.net),

    line_comment: $ => seq("//", optional(new RustRegex("[^\\S\\n]+")), $.line_comment_content),
    line_comment_content: $ => new RustRegex(".*"),
    block_comment: $ =>
      choice(
        new RustRegex("/\\*{2,}/"),
        seq(
          new RustRegex("/\\*+"),
          optional($.block_comment_content),
          token.immediate(new RustRegex("\\*+/")),
        ),
      ),
    block_comment_content: $ =>
      repeat1(choice(
        new RustRegex("[^*/]+"),
        "//",
        new RustRegex("/+"),
        new RustRegex("\\*+"),
        alias($.inner_block_comment, $.block_comment),
      )),
    inner_block_comment: $ =>
      choice(
        token.immediate(new RustRegex("/+\\*{2,}/")),
        seq(
          token.immediate(new RustRegex("/+\\*+")),
          optional($.block_comment_content),
          new RustRegex("\\*+/"),
        ),
      ),

    net: $ => seq(optional($.name), $.stmts),

    stmts: $ => seq("{", repeat($.stmt), "}"),

    stmt: $ => seq($._expr, "=", $._expr, optional(";")),

    _expr: $ =>
      choice(
        $.node,
        $.free,
        $.wire,
        $.interpolation,
        $.subnet,
      ),

    wire: $ => new RustRegex("\\p{ID_Continue}+"),
    path: $ => new RustRegex("[:\\p{ID_Continue}]+"),
    payload: $ => seq("#", token.immediate(new RustRegex("\\w+"))),
    free: $ => new RustRegex("\\^\\w*"),

    name: $ => seq($.path, optional($.payload), optional($.names)),
    names: $ => seq("[", optional(seq($.name, repeat(seq(",", $.name)), optional(","))), "]"),
    node: $ => seq($.name, optional(seq("(", repeat($._expr), ")"))),
    subnet: $ => seq($._expr, $.stmts),

    interpolation: $ => seq("$", "{", $.vine, "}"),
    vine: $ => repeat1(choice($._braced, new RustRegex("[^{}]+"))),
    _braced: $ =>
      seq(
        new RustRegex("\\{"),
        repeat(choice($._braced, new RustRegex("[^{}]+"))),
        new RustRegex("\\}"),
      ),
  },
});
