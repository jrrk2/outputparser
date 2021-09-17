open Input
open Input_rewrite
open Input_rewrite_types

let il = "examples/memory_ref.ilang";;

let op fd v =
    let p,p' = Input_rewrite.parse v in
    output_string fd (Input_dump.dump_ilst "\n" p');
    p,p'

let fd = open_out "junk.ilang";;
let p' = op fd il;;
let _ = close_out fd;;
