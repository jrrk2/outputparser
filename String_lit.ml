

type atom = bool * bool * string

type t = atom

(* Counters *)
let max_fresh = ref (-1)

let opaque : atom->t = fun (n,f,a) -> (n,f,a)

let transparent : t->atom = fun (n,f,a) -> (n,f,a)

let neg = fun (n,f,a:atom) -> (not n,f,a)

let abs = fun (n,f,a:t) -> (false,f,a)

let norm = fun (n,f,a:t) ->
  (false, f, a), if n then
    Msat.Solver_intf.Negated
  else
    Msat.Solver_intf.Same_sign

let sign = fun (n,f,a:t) -> n

let apply_sign b = fun (n,f,a:t) -> (b,f,a)

let set_sign b = fun (n,f,a:t) -> (b,f,a)

let hash : t->int = Hashtbl.hash

let equal (a:t) (b:t) = a=b
let compare (a:t) (b:t) = compare a b

let make (a:string) = (false,false,a)

let fresh () =
  incr max_fresh;
  (false,true,string_of_int (!max_fresh))

let pp fmt = fun (n,f,a:t) ->
  Format.fprintf fmt "%s%c%s" (string_of_bool n) (if f then 'f' else 'v') a
