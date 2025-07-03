
(item_fn (block) @function.inside) @function.around
(stmt_let_fn (block) @function.inside) @function.around
(expr_fn (block) @function.inside) @function.around

(item_struct (_) @class.inside . ")") @class.around
(item_enum (enum_variant)+ @class.inside) @class.around
(item_trait "{" . (_)+ @class.inside . "}") @class.around
(item_impl "{" . (_)+ @class.inside . "}") @class.around
(item_type "=" . (_) @class.inside) @class.around

(pats ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
(exprs ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
(generic_params ((_) @parameter.inside . ","? @parameter.around) @parameter.around)
(generic_args ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(expr_tuple ((_) @entry.inside . ","? @entry.around) @entry.around)
(expr_list ((_) @entry.inside . ","? @entry.around) @entry.around)
(pat_tuple ((_) @entry.inside . ","? @entry.around) @entry.around)
(ty_tuple ((_) @entry.inside . ","? @entry.around) @entry.around)

(expr_object_entry (_) @entry.inside .) @entry.around
(pat_object_entry (_) @entry.inside .) @entry.around
(ty_object_entry (_) @entry.inside .) @entry.around

(line_comment (line_comment_content) @comment.inside) @comment.around
(block_comment (block_comment_content) @comment.inside) @comment.around
