
exception Bad_atom
(** Exception raised if an atom cannot be created *)

type t = bytes
(** Atoms are represented as integers. [-i] begin the negation of [i].
    Additionally, since we nee dot be able to create fresh atoms, we
    use even integers for user-created atoms, and odd integers for the
    fresh atoms. *)

let max_lit = max_int

(* Counters *)
let max_fresh = ref (-1)

let to_int (a:t) = let h = Hashtbl.hash a in if Bytes.get a 0 == '~' then -h else h

let neg (a':t) = let a = a' in if Bytes.get a 0 == '~' then Bytes.set a 0 ' ' else Bytes.set a 0 '~'; a

let abs (a':t) = let a = a' in Bytes.set a 0 ' '; a

let norm (a:t) =
  abs a, if Bytes.get a 0 == '~' then
    Msat.Solver_intf.Negated
  else
    Msat.Solver_intf.Same_sign

let sign (a:t) = Bytes.get a 0 <> '~'

let apply_sign b (a:t) = if b then a else neg a

let set_sign b (a':t) = let a = a' in if b then Bytes.set a 0 ' ' else Bytes.set a 0 '~'; a

let hash (a:t) = Hashtbl.hash a
let equal (a:t) b = a=b
let compare (a:t) b = compare a b

let make (a:string) =  Bytes.of_string (" v"^a)

let fresh () =
  incr max_fresh;
  Bytes.of_string (" f"^string_of_int (!max_fresh))

(*
let iter: (t -> unit) -> unit = fun f ->
  for j = 1 to !max_index do
    f j
  done
*)

let pp fmt (a:t) =
  Format.fprintf fmt "%s" (String.of_bytes a)
