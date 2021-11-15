open Sv2v
open Printf

let (unhand:token option ref) = ref None

let dump rtl = List.map (function
      | oth -> unhand := Some oth; failwith "dump40"
      ) (rtl)
