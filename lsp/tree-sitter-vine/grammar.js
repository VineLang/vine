/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const delimited = (open, sep, close, inner) =>
  seq(
    ...open ? [open] : [],
    optional(seq(
      inner,
      repeat(seq(...sep ? [sep] : [], inner)),
      ...sep ? [optional(sep)] : [],
    )),
    ...close ? [close] : [],
  );

const generics = (inherit, ty, impl) =>
  seq(
    "[",
    ...inherit ? [optional("...")] : [],
    delimited("", ",", "", ty),
    optional(delimited(";", ",", optional(";"), impl)),
    "]",
  );

const tuple = el =>
  seq(
    "(",
    optional(seq(
      el,
      ",",
      optional(seq(
        el,
        repeat(seq(",", el)),
        optional(","),
      )),
    )),
    ")",
  );

let i = 0;
const BP = {
  Min: i++,
  Assignment: i++,
  LogicalImplies: i++,
  LogicalOr: i++,
  LogicalAnd: i++,
  Is: i++,
  Comparison: i++,
  Range: i++,
  BitOr: i++,
  BitXor: i++,
  BitAnd: i++,
  BitShift: i++,
  Additive: i++,
  Multiplicative: i++,
  Exponential: i++,
  Annotation: i++,
  Prefix: i++,
  Max: i++,
};

/** @type {[number, "left" | "right", string][]} */
const BINARY_OP_TABLE = [
  [BP.BitOr, "left", "|"],
  [BP.BitXor, "left", "^"],
  [BP.BitAnd, "left", "&"],
  [BP.BitShift, "left", "<<"],
  [BP.BitShift, "left", ">>"],
  [BP.Additive, "left", "+"],
  [BP.Additive, "left", "-"],
  [BP.Additive, "left", "++"],
  [BP.Multiplicative, "left", "*"],
  [BP.Multiplicative, "left", "/"],
  [BP.Multiplicative, "left", "%"],
  [BP.Exponential, "right", "**"],
];

/** @type {[number, "left" | "right", string][]} */
const LOGICAL_OP_TABLE = [
  [BP.LogicalImplies, "right", "=>"],
  [BP.LogicalOr, "right", "||"],
  [BP.LogicalAnd, "right", "&&"],
];

module.exports = grammar({
  name: "vine",

  word: $ => $.ident,

  extras: $ => [new RustRegex("[ \\t\\r\\n\\f]+"), $.line_comment, $.block_comment],

  supertypes: $ => [
    $._expr,
    $._pat,
    $._ty,
    $._trait,
    $._impl,
  ],

  reserved: {
    global: _ => [
      "pub",
      "mod",
      "use",
      "as",
      "fn",
      "struct",
      "enum",
      "type",
      "trait",
      "impl",
      "match",
      "let",
      "const",
      "in",
      "is",
      "return",
      "do",
      "loop",
      "while",
      "for",
      "break",
      "continue",
      "if",
      "when",
      "else",
      "true",
      "false",
    ],
  },

  rules: {
    source_file: $ => repeat($._stmt),

    ident: $ => new RustRegex("\\p{ID_Start}\\p{ID_Continue}*|_\\p{ID_Continue}+"),
    num: $ => new RustRegex("\\d[\\d\\w]*(\\.\\d+([eE][+-]?\\d+)?)?"),

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

    _item: $ =>
      choice(
        $.item_fn,
        $.item_const,
        $.item_struct,
        $.item_enum,
        $.item_type,
        $.item_mod,
        $.item_trait,
        $.item_impl,
        $.item_use,
      ),

    item_fn: $ =>
      seq(
        repeat($.attr),
        optional($.vis),
        "fn",
        optional("."),
        $.ident,
        optional($.generic_params),
        $.pats,
        optional($.block_ty),
        choice(";", $.block),
      ),

    block_ty: $ => seq("->", $._ty),

    item_const: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "const",
        $.ident,
        optional($.generic_params),
        optional(seq(":", $._ty)),
        optional(seq("=", $._expr)),
        optional(";"),
      )),

    item_struct: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "struct",
        optional($.flex),
        $.ident,
        optional($.generic_params),
        "(",
        optional($.vis),
        $._ty,
        ")",
        optional(";"),
      )),

    item_enum: $ =>
      seq(
        repeat($.attr),
        optional($.vis),
        "enum",
        optional($.flex),
        $.ident,
        optional($.generic_params),
        delimited("{", ",", "}", $.enum_variant),
      ),
    enum_variant: $ => seq($.ident, optional(seq("(", $._ty, ")"))),

    item_type: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "type",
        $.ident,
        optional($.generic_params),
        optional(seq("=", $._ty)),
        optional(";"),
      )),

    item_mod: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "mod",
        $.ident,
        optional($.generic_params),
        choice(
          seq(optional(seq("=", $.string)), optional(";")),
          delimited("{", "", "}", $._item),
        ),
      )),

    item_trait: $ =>
      seq(
        repeat($.attr),
        optional($.vis),
        "trait",
        $.ident,
        optional($.generic_params),
        delimited("{", "", "}", $._item),
      ),

    item_impl: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "impl",
        $.ident,
        optional($.generic_params),
        ":",
        $._trait,
        choice(
          delimited("{", "", "}", $._item),
          seq(
            optional(seq("=", $._impl)),
            optional(";"),
          ),
        ),
      )),

    item_use: $ =>
      prec.right(seq(
        repeat($.attr),
        optional($.vis),
        "use",
        optional("::"),
        delimited(
          "",
          ",",
          "",
          choice(
            seq(optional("#"), $.use_tree),
            delimited("{", ",", "}", seq(optional("#"), $.use_tree)),
          ),
        ),
        optional(";"),
      )),

    use_tree: $ =>
      prec.right(seq(
        $.ident,
        optional(choice(
          seq("as", choice("_", $.ident)),
          seq("::", choice($.use_tree, delimited("{", ",", "}", $.use_tree))),
        )),
      )),

    vis: $ => seq("pub", optional(seq(".", $.ident))),

    attr: $ =>
      seq("#[", $.ident, optional(choice(seq("=", $.string), seq("(", $._expr, ")"))), "]"),

    generic_params: $ => generics(true, $.ty_param, $.impl_param),
    ty_param: $ => seq($.ident, optional($.flex)),
    impl_param: $ => seq(optional(seq($.ident, ":")), $._trait),

    generic_args: $ => generics(false, $._ty, $._impl),

    flex: $ => choice("+", "?", "*"),

    block: $ => delimited("{", "", "}", $._stmt),

    _stmt: $ =>
      choice(
        $.stmt_empty,
        $._item,
        prec(
          BP.Max,
          choice(
            $.expr_if,
            $.expr_when,
            $.expr_match,
            $.expr_loop,
            $.expr_do,
            $.expr_for,
            $.expr_while,
          ),
        ),
        $.stmt_expr,
        $.stmt_let,
        $.stmt_let_fn,
      ),

    stmt_empty: $ => ";",

    stmt_expr: $ => prec.right(seq($._expr, optional(";"))),

    stmt_let: $ =>
      prec.right(seq(
        "let",
        $._pat,
        optional(seq(
          "=",
          $._expr,
          optional(choice(
            seq("else", $.block),
            seq("else", "match", delimited("{", "", "}", $.match_arm)),
          )),
        )),
        optional(";"),
      )),

    stmt_let_fn: $ =>
      seq(
        "let",
        "fn",
        optional($.flex),
        $.ident,
        $.pats,
        optional($.block_ty),
        $.block,
      ),

    exprs: $ => delimited("(", ",", ")", $._expr),

    _expr: $ =>
      choice(
        "...",
        $.expr_hole,
        $.expr_paren,
        $.expr_path,
        $.expr_do,
        $.expr_assign,
        $.expr_match,
        $.expr_if,
        $.expr_when,
        $.expr_while,
        $.expr_loop,
        $.expr_for,
        $.expr_fn,
        $.expr_return,
        $.expr_break,
        $.expr_continue,
        $.expr_ref,
        $.expr_deref,
        $.expr_inverse,
        $.expr_place,
        $.expr_tuple,
        $.expr_object,
        $.expr_list,
        $.expr_sign,
        $.expr_bool,
        $.expr_not,
        $.expr_is,
        $.expr_cast,
        $.expr_range,
        $.expr_num,
        $.char,
        $.string,
        $.expr_inline_ivy,
        $.expr_chain,
        $.expr_binary_op,
        $.expr_logical_op,
        $.expr_comparison_op,
        $.expr_binary_op_assign,
      ),

    expr_num: $ => prec.right(seq($.num, optional(seq("[", $._ty, "]")))),

    expr_hole: $ => "_",

    expr_paren: $ => seq("(", $._expr, ")"),

    expr_path: $ => prec.right(seq($.path, optional($.exprs))),

    expr_do: $ => seq("do", optional($._label), optional($.block_ty), $.block),

    expr_assign: $ => prec.right(BP.Assignment, seq($._expr, optional("~"), "=", $._expr)),

    expr_match: $ => seq("match", $._expr, delimited("{", "", "}", $.match_arm)),
    match_arm: $ => seq($._pat, $.block),

    expr_if: $ => prec.right(seq("if", $._expr, $.block, optional(seq("else", $.block)))),

    expr_when: $ => seq("when", optional($._label), delimited("{", "", "}", $.when_arm)),
    when_arm: $ => seq($._expr, $.block),

    expr_while: $ =>
      prec.right(
        seq(
          "while",
          optional($._label),
          $._expr,
          optional($.block_ty),
          $.block,
          optional(seq("else", $.block)),
        ),
      ),

    expr_loop: $ => seq("loop", optional($._label), optional($.block_ty), $.block),

    expr_for: $ =>
      prec.right(
        seq(
          "for",
          optional($._label),
          $._pat,
          "in",
          $._expr,
          optional($.block_ty),
          $.block,
          optional(seq("else", $.block)),
        ),
      ),

    expr_fn: $ => seq("fn", optional($.flex), $.pats, optional($.block_ty), $.block),

    expr_return: $ => prec.right(seq("return", optional($._expr))),

    expr_break: $ => prec.right(seq("break", optional($._target), optional($._expr))),

    expr_continue: $ => prec.right(seq("continue", optional($._target))),

    expr_ref: $ => prec(BP.Prefix, seq("&", $._expr)),

    expr_deref: $ => prec(BP.Prefix, seq("*", $._expr)),

    expr_inverse: $ => prec(BP.Prefix, seq("~", $._expr)),

    expr_place: $ => seq("(", $._expr, ";", $._expr, ")"),

    expr_tuple: $ => tuple($._expr),

    expr_object: $ => delimited("{", ",", "}", $.expr_object_entry),
    expr_object_entry: $ => seq($.ident, optional(seq(":", $._expr))),

    expr_list: $ => delimited("[", ",", "]", $._expr),

    expr_sign: $ => prec(BP.Prefix, seq(choice("+", "-"), $._expr)),

    expr_bool: $ => choice("true", "false"),

    expr_not: $ => prec(BP.Prefix, seq("!", $._expr)),

    expr_is: $ => prec(BP.Is, seq($._expr, "is", $._pat)),

    expr_cast: $ => prec(BP.Annotation, seq($._expr, "as", $._ty)),

    expr_range: $ =>
      prec.left(BP.Range, seq(optional($._expr), choice("..", "..="), optional($._expr))),

    expr_inline_ivy: $ =>
      seq(
        "inline_ivy!",
        delimited("(", ",", ")", seq($.ident, choice("<-", "->"), $._expr)),
        optional($.block_ty),
        $.inline_ivy,
      ),

    inline_ivy: $ => new RustRegex("\\{[^\\{\\}]+\\}"),

    expr_chain: $ => prec(BP.Max, seq($._expr, $._chain)),

    expr_binary_op: $ =>
      choice(
        ...BINARY_OP_TABLE.map(([bp, assoc, op]) => prec[assoc](bp, seq($._expr, op, $._expr))),
      ),

    expr_binary_op_assign: $ =>
      choice(
        ...BINARY_OP_TABLE.map(([bp, _assoc, op]) =>
          prec(bp, seq($._expr, op, "=", prec.right(BP.Assignment, $._expr)))
        ),
      ),

    expr_logical_op: $ =>
      choice(
        ...LOGICAL_OP_TABLE.map(([bp, assoc, op]) => prec[assoc](bp, seq($._expr, op, $._expr))),
      ),

    expr_comparison_op: $ =>
      prec.left(
        BP.Comparison,
        seq(
          $._expr,
          repeat1(seq(
            choice("==", "!=", "<", ">", "<=", ">="),
            $._expr,
          )),
        ),
      ),

    _label: $ => seq(".", $.ident),
    _target: $ =>
      seq(
        ".",
        choice(
          "do",
          "loop",
          "while",
          "for",
          "when",
          $.ident,
        ),
      ),

    _chain: $ =>
      choice(
        $.chain_unwrap,
        $.chain_try,
        $.chain_field,
        $._chain_call,
        $.chain_method,
        $.chain_ref,
        $.chain_deref,
        $.chain_inverse,
        $.chain_as,
      ),

    chain_unwrap: $ => "!",
    chain_try: $ => "?",
    chain_field: $ => prec.right(seq(".", choice(new RustRegex("\\d+"), $.ident))),
    _chain_call: $ => $.exprs,
    chain_method: $ => seq(".", $.ident, optional($.generic_args), $.exprs),
    chain_ref: $ => seq(".", "&"),
    chain_deref: $ => seq(".", "*"),
    chain_inverse: $ => seq(".", "~"),
    chain_as: $ => seq(".", "as", "[", $._ty, "]"),

    string: $ =>
      seq(
        "\"",
        repeat(choice(
          $.string_content,
          $.string_escape,
          $.string_interpolation,
        )),
        "\"",
      ),
    string_content: $ => new RustRegex("[^\"\\{\\\\]+"),
    string_escape: $ =>
      token(seq("\\", new RustRegex("[\\\\\"'{ntr0]|x[0-7][0-9a-fA-F]|u\\{[0-9a-fA-F]{1,6}\\}"))),
    string_interpolation: $ => seq("{", $._expr, "}"),
    char: $ => seq("'", choice($.string_escape, new RustRegex("[^'\\\\]")), "'"),

    pats: $ => delimited("(", ",", ")", $._pat),

    _pat: $ =>
      choice(
        "...",
        $.pat_hole,
        $.pat_paren,
        $.pat_annotation,
        $.pat_path,
        $.pat_ref,
        $.pat_deref,
        $.pat_inverse,
        $.pat_tuple,
        $.pat_object,
      ),

    pat_hole: $ => "_",
    pat_paren: $ => seq("(", $._pat, ")"),
    pat_annotation: $ => prec(BP.Annotation, seq($._pat, ":", $._ty)),
    pat_path: $ => prec.right(seq($.path, optional($.pats))),
    pat_ref: $ => prec(BP.Prefix, seq("&", $._pat)),
    pat_deref: $ => prec(BP.Prefix, seq("*", $._pat)),
    pat_inverse: $ => prec(BP.Prefix, seq("~", $._pat)),
    pat_tuple: $ => tuple($._pat),
    pat_object: $ => delimited("{", ",", "}", $.pat_object_entry),
    pat_object_entry: $ =>
      seq(
        $.ident,
        optional(seq(
          ":",
          choice(
            $._pat,
            seq(":", $._ty),
          ),
        )),
      ),

    path: $ =>
      prec.right(
        seq(optional("::"), $.ident, repeat(seq("::", $.ident)), optional($.generic_args)),
      ),

    _ty: $ =>
      choice(
        "...",
        $.ty_hole,
        $.ty_never,
        $.ty_paren,
        $.ty_fn,
        $.ty_tuple,
        $.ty_object,
        $.ty_ref,
        $.ty_inverse,
        $.ty_path,
      ),

    ty_hole: $ => "_",
    ty_never: $ => "!",
    ty_paren: $ => seq("(", $._ty, ")"),
    ty_fn: $ => seq("fn", $.path),
    ty_tuple: $ => tuple($._ty),
    ty_object: $ => delimited("{", ",", "}", $.ty_object_entry),
    ty_object_entry: $ => seq($.ident, ":", $._ty),
    ty_ref: $ => seq("&", $._ty),
    ty_inverse: $ => seq("~", $._ty),
    ty_path: $ => $.path,

    _impl: $ =>
      choice(
        $.impl_hole,
        $.impl_paren,
        $.impl_fn,
        $.path,
      ),
    impl_hole: $ => "_",
    impl_paren: $ => seq("(", $.path, ")"),
    impl_fn: $ => seq("fn", $.path),

    _trait: $ =>
      choice(
        $.path,
        $.trait_fn,
      ),
    trait_fn: $ => seq("fn", $._ty, delimited("(", ",", ")", $._ty), optional($.block_ty)),
  },
});
