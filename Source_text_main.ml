
open Source_text
open Source_text_lex
open Source_text_rewrite

let _ = if Array.length Sys.argv > 1 then ignore (parse Sys.argv.(1))
