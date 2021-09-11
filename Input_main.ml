open Input
open Input_rewrite
open Input_rewrite_types

let il = "examples/count5_ref.ilang";;

let fd = open_out "junk.ilang";;
let p' = op fd il;;
let _ = close_out fd;;
