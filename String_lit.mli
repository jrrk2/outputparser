
(** {1 The module defining formulas} *)

(** SAT Formulas

    This modules implements formuals adequate for use in a pure SAT Solver.
    Atomic formuals are represented using strings, for clarity at the expense of efficiency
*)

include Msat.Solver_intf.FORMULA
(** This modules implements the requirements for implementing an SAT Solver. *)

type signal =
| PWR
| GND
| SCALAR of string
| INDEXED of string * int [@@deriving yojson]

type atom' =
| Fresh of int
| Made of signal [@@deriving yojson]

type atom = bool * atom' [@@deriving yojson]

val make : signal -> t
(** Make a proposition from a signal. *)

(*
val to_int : t -> int
*)

val fresh : unit -> t
(** Make a fresh atom *)

val compare : t -> t -> int
(** Compare atoms *)

val sign : t -> bool
(** Is the given atom positive ? *)

val apply_sign : bool -> t -> t
(** [apply_sign b] is the identity if [b] is [true], and the negation
    function if [b] is [false]. *)

val set_sign : bool -> t -> t
(** Return the atom with the sign set. *)

val norm : t -> t * Msat.Solver_intf.negated
(** normalise: split out abs(t) and sign(t) *)

val opaque : atom -> t
(** convert to opaque type *)

val transparent : t -> atom
(** convert to opaque type *)

val string_of_signal : signal -> string
(** dump signal as a string *)
