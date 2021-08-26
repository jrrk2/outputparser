
open Source_text
open Source_text_lex
open Source_text_rewrite

let _ = if Array.length Sys.argv > 1 then let modlst,x,p,p' = rewrite_vhdl Sys.argv.(1) in List.iter print_endline modlst
