/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "ivy",

  word: $ => $.ident,

  extras: $ => [new RustRegex("[ \\t\\r\\n\\f]+"), $.line_comment, $.block_comment],

  supertypes: $ => [
    $._tree,
  ],

  rules: {
    source_file: $ => repeat($.global_net),

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

    global_net: $ => seq(optional($.global), "{", $.net, "}"),
    net: $ => seq($._tree, repeat($.pair)),

    pair: $ => seq($._tree, "=", $._tree),

    _tree: $ =>
      choice(
        $.erase,
        $.comb,
        $.ext_fn,
        $.branch,
        $.n32,
        $.f32,
        $.var,
        $.global,
        $.black_box,
      ),

    erase: $ => "_",
    comb: $ => seq($.ident, "(", $._tree, $._tree, ")"),
    ext_fn: $ => seq("@", $.ident, optional("$"), "(", $._tree, $._tree, ")"),
    branch: $ => seq("?", optional("^"), "(", repeat($._tree), ")"),
    n32: $ => new RustRegex("\\d[\\d\\w]*"),
    f32: $ => new RustRegex("[+-][\\d\\w\\.\\+\\-]+"),
    var: $ => $.ident,
    global: $ => new RustRegex(":[:\\p{ID_Continue}]+"),
    black_box: $ => seq("#", "[", $._tree, "]"),

    ident: $ => new RustRegex("\\p{ID_Start}\\p{ID_Continue}*"),
  },
});
