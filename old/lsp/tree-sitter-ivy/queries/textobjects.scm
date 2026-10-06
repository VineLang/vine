
(net (stmts "{" . (_)+ @function.inside . "}")) @function.around

(line_comment (line_comment_content) @comment.inside) @comment.around
(block_comment (block_comment_content) @comment.inside) @comment.around
