open CompilationUnit
open CompilationUnit_types

let rec dumptree = function
| CONS1(a) -> "CONS1 ("^dumptree a^")"
| CONS2(a,b) -> "CONS2 ("^dumptree a^", "^dumptree b^")"
| CONS3(a,b,c) -> "CONS3 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| CONS4(a,b,c,d) -> "CONS4 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| TUPLE2(a,b) -> "TUPLE2 ("^dumptree a^", "^dumptree b^")"
| TUPLE3(a,b,c) -> "TUPLE3 ("^dumptree a^", "^dumptree b^", "^dumptree c^")"
| TUPLE4(a,b,c,d) -> "TUPLE4 ("^dumptree a^", "^dumptree b^", "^dumptree c^", "^dumptree d^")"
| TUPLE5(a,b,c,d,e) -> "TUPLE5 ("^dumptree a^", "^dumptree b^", "^dumptree c^", "^dumptree d^", "^dumptree e^")"
| TUPLE6(a,b,c,d,e,f) -> "TUPLE6 ("^dumptree a^", "^dumptree b^", "^dumptree c^", "^dumptree d^", "^dumptree e^", "^dumptree f^")"
| TUPLE7(a,b,c,d,e,f,g) -> "TUPLE7 ("^dumptree a^", "^dumptree b^", "^dumptree c^", "^dumptree d^", "^dumptree e^", "^dumptree f^", "^dumptree g^")"

| PLAINID str -> "PLAINID \""^str^"\""
| INTEGERLITERAL num -> "INTEGERLITERAL \""^string_of_int num^"\""
| TLIST lst -> "TLIST ["^String.concat "; " (List.map dumptree lst)^"]"
| oth -> getstr oth

let failtree oth = print_endline "failtree:"; failwith (dumptree oth)

let loc = ref 0
let pcnv = function
| TLIST p -> p
| NULL -> []
| oth -> [oth]

let filt errlst _enums _externs _fbody _ftypes _globals _inits _inlines _structs _typedefs _unions = function
| oth -> errlst := oth :: !errlst
