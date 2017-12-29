
open Circuit
open Circuit_types
open Circuit_lex

type citm =
  | B of token*token list*token list
  | E of token
  | F of token*token list
  | G of token
  | I of (token*token)
  | L of (token*token list*token list)
  | S of token list
  | T of token
  | U of token list
	   
let verbose = ref true
let verbose2 = ref false

let rec findlst2 = function
| CONS1 a -> a :: []
| CONS2(a,b) -> b :: findlst2 a
| oth -> oth :: []

let rec findlst3 = function
| CONS1 a -> a :: []
| CONS3(a,COMMA,b) -> b :: findlst3 a
| oth -> oth :: []

let rec findlst4 = function
| CONS1 a -> a :: []
| CONS4(a,COMMA,b,c) -> TUPLE2(b,c) :: findlst4 a
| oth -> oth :: []

let rec tolst = function
| CONS1(a) -> tolst a
| CONS2(a,b) -> TLIST (List.rev_map tolst (b :: findlst2 a))
| CONS3(a,COMMA,b) -> TLIST (List.rev_map tolst (tolst b :: findlst3 a))
| CONS4(a,COMMA,b,c) -> TLIST (List.rev_map tolst (TUPLE2(b, c) :: findlst4 a))
| TUPLE2(a,b) -> TUPLE2(tolst a, tolst b)
| TUPLE3(a,b,c) -> TUPLE3(tolst a, tolst b, tolst c)
| TUPLE4(a,b,c,d) -> TUPLE4(tolst a, tolst b, tolst c, tolst d)
| TUPLE5(a,b,c,d,e) -> TUPLE5(tolst a, tolst b, tolst c, tolst d, tolst e)
| TUPLE6(a,b,c,d,e,f) -> TUPLE6(tolst a, tolst b, tolst c, tolst d, tolst e, tolst f)
| TUPLE7(a,b,c,d,e,f,g) -> TUPLE7(tolst a, tolst b, tolst c, tolst d, tolst e, tolst f, tolst g)
| oth -> oth

let rec search fn = function
| CONS1(a) -> search fn a
| CONS2(a,b) -> (search fn) a || (search fn) b
| CONS3(a,b,c) -> (search fn) a || (search fn) b || (search fn) c
| CONS4(a,b,c,d) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d
| TUPLE2(a,b) -> (search fn) a || (search fn) b
| TUPLE3(a,b,c) -> (search fn) a || (search fn) b || (search fn) c
| TUPLE4(a,b,c,d) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d
| TUPLE5(a,b,c,d,e) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e
| TUPLE6(a,b,c,d,e,f) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e || (search fn) f
| TUPLE7(a,b,c,d,e,f,g) -> (search fn) a || (search fn) b || (search fn) c || (search fn) d || (search fn) e || (search fn) f || (search fn) g
| TLIST lst -> List.fold_left (||) false (List.map (search fn) lst)
| oth -> fn oth

let getrslt parse arg =
   Printf.fprintf stderr "%s: " arg; flush stderr;
   parse arg

let dump parse chan main argv =
  let rslts = ref [] in
  prerr_endline "/*";
  for i = 1 to Array.length argv - 1 do rslts := getrslt parse argv.(i) :: !rslts; done;
  prerr_endline "*/";
  !rslts
