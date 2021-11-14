open Yosys_helper
open Printf

let (unhand:token list ref) = ref []

let dump = function
| oth -> unhand := oth; failwith "dump"
