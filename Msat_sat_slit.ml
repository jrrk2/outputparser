(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2016 Guillaume Bury
*)

module String_lit = String_lit

include Msat.Make_pure_sat(struct
    module Formula = String_lit
    type proof = unit
  end)

