type signal =
| PWR
| GND
| SCALAR of string
| INDEXED of string * int  [@@deriving yojson]

type atom' =
| Fresh of int
| Made of signal [@@deriving yojson]

type atom = bool * atom' [@@deriving yojson]

type t = atom [@@deriving yojson]

(* Counters *)
let max_fresh = ref (-1)

let opaque : atom->t = fun (n,a) -> (n,a)

let transparent : t->atom = fun (n,a) -> (n,a)

let neg = fun (n,a:atom) -> (not n,a)

let abs = fun (n,a:t) -> (false,a)

let norm = fun (n,a:t) ->
  (false, a), if n then
    Msat.Solver_intf.Negated
  else
    Msat.Solver_intf.Same_sign

let sign = fun (n,a:t) -> n

let apply_sign b = fun (n,a:t) -> (b,a)

let set_sign b = fun (n,a:t) -> (b,a)

let hash : t->int = Hashtbl.hash

let equal (a:t) (b:t) = a=b
let compare (a:t) (b:t) = compare a b

let make (a:signal) = (false,Made a)

let fresh () =
  incr max_fresh;
  (false,Fresh !max_fresh)

let string_of_signal = function
| PWR -> "PWR"
| GND -> "GND"
| SCALAR string -> string
| INDEXED (string, int) -> string ^ "[" ^ string_of_int int ^ "]"

let string_of_atom' = function
| Fresh int -> "f"^string_of_int int
| Made signal -> "v"^string_of_signal signal

let pp fmt = fun (n,a:t) ->
  Format.fprintf fmt "%s%s" (string_of_bool n) (string_of_atom' a)
