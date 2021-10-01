(*
MSAT is free software, using the Apache license, see file LICENSE
Copyright 2014 Guillaume Bury
Copyright 2014 Simon Cruanes
*)

(** Tseitin CNF conversion

    This modules implements Tseitin's Conjunctive Normal Form conversion, i.e.
    the ability to transform an arbitrary boolean formula into an equi-satisfiable
    CNF, that can then be fed to a SAT/SMT/McSat solver.
*)

(*
module type Arg = Tseitin_intf.Arg
(** The implementation of formulas required to implement Tseitin's CNF conversion. *)

module type S = Tseitin_intf.S
(** The exposed interface of Tseitin's CNF conversion. *)
*)

(*
module MakeCNF with type atom = String_lit.t
*)
(** This functor provides an implementation of Tseitin's CNF conversion. *)
(* exposed interface for debugging *)

module MakeCNF :
    sig
      type combinator = And | Or | Imp | Not
      type t = True | Lit of String_lit.atom | Comb of combinator * t list
(*
      val mypp : Format.formatter -> t -> unit
      val mypp_list : string -> Format.formatter -> t list -> unit
*)
      val make : combinator -> t list -> t
      val make_atom : String_lit.t -> t
      val f_true : t
      val f_false : t
      val flatten : combinator -> t list -> t list -> t list
      val opt_rev_map : ('a -> 'b option) -> 'b list -> 'a list -> 'b list
      val remove_true : t list -> t list
      val remove_false : t list -> t list
      val make_not : t -> t
      val make_and : t list -> t
      val make_or : t list -> t
      val make_imply : t -> t -> t
      val make_equiv : t -> t -> t
      val make_xor : t -> t -> t
      val ( %% ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
      val sform : t -> (t -> 'a) -> 'a
      val sform_list : t list -> t list -> (t list -> 'a) -> 'a
      val sform_list_not : t list -> t list -> (t list -> 'a) -> 'a
      val ( @@ ) : 'a list -> 'a list -> 'a list
      val mk_proxy : unit -> String_lit.atom
      val acc_or : (String_lit.atom * String_lit.atom list) list ref
      val acc_and : (String_lit.atom * String_lit.atom list) list ref
      val cnf : t -> String_lit.t list list
      val make_cnf : t -> String_lit.t list list
    end

