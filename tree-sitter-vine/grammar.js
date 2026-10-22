
const generated = require("./gen.js")

module.exports = grammar({
  name: "vine",

  word: $ => $._bare_ident,

  extras: $ => [new RustRegex("[ \\t\\r\\n\\f]+"), $.line_comment, $.block_comment],

  // supertypes: $ => [
  //   $._expr,
  //   $._pat,
  //   $._ty,
  //   $._trait,
  //   $._impl,
  // ],

  reserved: {
    global: _ => generated.keywords,
  },

  rules: {
    source_file: $ => repeat(choice($.item, $._stmt)),

    _bare_ident: $ => /[\p{ID_Start}_]\p{ID_Continue}*/,

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

    ...generated.rules,
  },
});
