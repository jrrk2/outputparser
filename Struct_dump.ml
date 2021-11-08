open Struct
open Printf

let unhand = ref None

let dump_map ast = function
        | TUPLE2 (SBox, TOK_ID memb) -> ast ^ memb
        | TUPLE2 (Svec, TOK_ID kind) -> ast ^ kind ^ " array"
        | TUPLE2 (SSpanned, TOK_ID nam) -> ast ^ nam
        | TUPLE2 (SOption, TLIST [TUPLE2 (SBox, TOK_ID memb)]) -> ast ^ memb ^ " option"
        | TOK_ID id -> ast ^ id
        | TUPLE2 (SOption, TLIST [TOK_ID memb]) -> ast ^ memb ^ " option"
        | oth -> unhand := Some oth; failwith "dump_itm_enum"

let map_star ast lst = String.concat " * " (List.map (dump_map ast) lst)

let tchk ast typlst memb =
  (if List.mem memb typlst then "'" else "")^ast^memb

let dump_itm fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| TUPLE2 (TOK_ID expr, TOK_ID memb) ->
  fprintf fd "    | SV_%s of %s\n" expr (tchk ast typlst memb)
| TUPLE2 (TOK_ID expr, SOption) ->
  fprintf fd "    | SV_%s of unit option\n" expr
| TUPLE2 (TOK_ID expr, TLIST []) ->
  fprintf fd "    | SV_%s\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2(LPAREN, TLIST [TOK_ID memb])) ->
  fprintf fd "    | SV_%s of %s\n" expr (tchk ast typlst memb)
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST [TUPLE2 (SSpanned, TOK_ID id)])) ->
  fprintf fd "    | SV_%s of %sSpanned * %s%s\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (SSpanned, TOK_ID id)]) ->
  fprintf fd "    | SV_%s of %sSpanned * %s%s\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TUPLE2 (LBRACE, TLIST lst)) ->
  fprintf fd "    | SV_%s of {\n" expr;
  List.iter (function
        | TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
        | TUPLE3 (COLON, TOK_ID index, TUPLE2 (SBox, TOK_ID expr')) ->
  fprintf fd "        %s: %s%s;\n" index ast expr';
        | TUPLE3 (COLON, TOK_ID memb, TOK_ID kind) ->
  fprintf fd "        %s: %s%s;\n" memb ast kind;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID slice])) ->
  fprintf fd "        %s: %s%s option;\n" memb ast slice;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (Svec, TOK_ID expr)) ->
  fprintf fd "        %s: %s%s array;\n" memb ast expr;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE2 (SBox, TOK_ID expr)])) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "        %s: %sSpanned * %s%s;\n" memb ast ast expr;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE2 (SSpanned, TOK_ID expr)])) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [])) ->
  fprintf fd "        %s: unit option;\n" memb;
        | oth -> unhand := Some oth; failwith "dump_itm_struct") lst;
  fprintf fd "    }\n"
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST lst)) ->
  fprintf fd "    | SV_%s of (%s)\n" expr (map_star ast lst)
| TUPLE2 (TOK_ID expr, TUPLE2 (SVec, TUPLE2 (TOK_ID arr, TOK_ID "a"))) ->
  fprintf fd "    | SV_%s of %s%s array\n" expr ast arr
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID expr, TOK_ID "a")) ->
  fprintf fd "    | SV_%s of %s%s\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TLIST lst)) ->
  fprintf fd "    | SV_%s of (%s) array\n" memb (map_star ast lst)
| TUPLE2 (TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "    | SV_%s of %s%s\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "    | SV_%s of %s%s array\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE2 (TOK_ID expr, TOK_ID "a"))) ->
  fprintf fd "    | SV_%s of %s%s\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE2 (TOK_ID anyNode, TOK_ID "a"))) ->
  fprintf fd "    | SV_%s of %s%s * %s%s\n" memb ast cell ast dyn
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID cell, TOK_ID usize)) ->
  fprintf fd "    | SV_%s of %s%s * %s%s\n" memb ast cell ast usize
| oth -> unhand := Some oth; failwith "dump_itm"

let dump_itm' fd arg typlst = function
| TUPLE4(typ, TOK_ID k, TLIST typ_p, TLIST lst) -> List.iter (dump_itm fd arg typlst) lst
| oth -> unhand := Some oth; failwith "dump_itm'"

let typ_dump arg typlst = function
| TUPLE2 (QUOTE, TOK_ID a) -> typlst := a :: !typlst; "'"^arg^a
| TOK_ID t -> typlst := t :: !typlst; "'"^arg^t
| oth -> unhand := Some oth; failwith "typ_dump"

let dump arg rtl =
  let fd = open_out (arg^"_types_new.ml") in
  output_string fd ("type "^arg^" = unit\n");
  output_string fd ("type "^arg^"Span = unit\n");
  output_string fd ("type "^arg^"Spanned = int\n");
  output_string fd ("type "^arg^"bool = bool\n");
  output_string fd ("type "^arg^"Name = string\n");
  output_string fd ("type "^arg^"Lit = string\n");
  output_string fd ("type "^arg^"Op = int\n");
  output_string fd ("type "^arg^"usize = int\n");
  output_string fd ("type "^arg^"dyn = int\n");
  output_string fd ("type "^arg^"PropSpec = int\n");
  output_string fd ("type "^arg^"NodeId = int\n");
  output_string fd ("type "^arg^"Cell = int\n");
  List.iter (function
      | TOK_COMMENT s ->
        fprintf fd "(* %s *)\n" s
      | TUPLE4(typ, TOK_ID k, TLIST typ_p, TLIST lst') as itm ->
        let lst = List.filter (function TOK_COMMENT _ -> false | _ -> true) lst' in
        let typlst = ref [] in
        fprintf fd "\n";
        fprintf fd "and %s%s%s =\n" (if typ_p <> [] then "("^String.concat ", " (List.map (typ_dump arg typlst) typ_p)^")" else "") arg k;
        dump_itm' fd arg !typlst itm;
        if lst = [] then output_string fd "unit\n";
      | oth -> unhand := Some oth; failwith "dump104"
      ) (List.sort compare rtl);
  close_out fd
