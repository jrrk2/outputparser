(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Output_types
open Output_parser

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      Output_parser.start Output_lexer.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  output
  
let tokens = List.map (function
    | TERMITM(ID nam, _) -> nam
    | TERMITM(oth, _) -> Ord.getstr oth
    | err -> failwith (Ord.getstr err))

let parse arg =
  let ch = open_in arg in
  let (u,g,t,n,s) = parse_output_ast_from_chan ch in
  close_in ch;
  let t' = tokens t in
  let ch' = open_out (Template.primary g^".mly") in
  Template.template ch' (t' (* @ u *) ) g;
  close_out ch'

let _ = if Array.length Sys.argv > 1 then parse Sys.argv.(1)
