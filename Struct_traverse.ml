open Struct
open Printf

let unhand = ref None

let dump_map ast = function
        | TUPLE2 (SBox, TOK_ID memb) -> ast ^ memb
        | TUPLE3 (TLIST [], TUPLE2 (SBox, TOK_ID memb), TLIST []) -> ast ^ memb
        | TUPLE2 (Svec, TOK_ID kind) -> ast ^ kind
        | TUPLE2 (SSpanned, TOK_ID nam) -> ast ^ nam
        | TUPLE2 (SOption, TLIST [TUPLE2 (SBox, TOK_ID memb)]) -> ast ^ memb
        | TOK_ID id -> ast ^ id
        | TUPLE2 (SOption, TLIST [TOK_ID memb]) -> ast ^ memb
        | TUPLE2 (Svec, TUPLE2 (TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)])) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ kind
        | TUPLE3 (TLIST [], TUPLE2 (Svec, TUPLE2 (TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)])), TLIST []) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ kind
        | TUPLE3 (TLIST [], TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)]) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ kind
        | TUPLE3 (TLIST [], TOK_ID kind, TLIST []) -> ast ^ kind
        | TUPLE3 (TLIST [], TUPLE2 (SSpanned, TOK_ID kind), TLIST []) -> ast ^ kind
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE2 (SBox, TOK_ID memb), TLIST [])]), TLIST []) -> ast ^ memb
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID a)])]), TLIST []) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ memb
        | TUPLE3 (TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID q)]) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ memb
        | TUPLE3 (TOK_ID memb, QUOTE, TOK_ID a) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ memb
        | TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST []) -> (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ memb
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST [])]), TLIST []) ->
          (* "('" ^ ast ^ a ^ ")" ^ *) ast ^ expr
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [])]), TLIST []) ->
          ast ^ expr
        | oth -> unhand := Some oth; failwith "dump_map"

let map_star ast lst = String.concat ", " (List.mapi (fun ix itm -> dump_map ast itm ^ string_of_int (ix+1)) lst)

let dump_substruct fd ast = function
        | TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
        | TUPLE3 (COLON, TOK_ID index, TUPLE2 (SBox, TOK_ID expr')) ->
  fprintf fd "        %s;\n" index
        | TUPLE3 (COLON, TOK_ID memb, TOK_ID kind) ->
  fprintf fd "        %s;\n" memb
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID slice])) ->
  fprintf fd "        %s;\n" memb
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (Svec, TOK_ID expr)) ->
  fprintf fd "        %s;\n" memb
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE2 (SBox, TOK_ID expr)])) ->
  fprintf fd "        %s;\n" memb
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "        %s;\n" memb
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (Svec, TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]))) -> 
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SSpanned, TOK_ID expr), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [])]), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (Svec, TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)])), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE2 (SSpanned, TOK_ID expr), TLIST [])]), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TOK_ID expr, TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)])]), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "        %s;\n" memb
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST [])]), TLIST []) ->
  fprintf fd "        %s;\n" memb
        | oth -> unhand := Some oth; print_endline "unhandled_dump_substruct"

let dump_enum fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| TUPLE2 (TOK_ID expr, TOK_ID memb) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" expr ast memb
| TUPLE2 (TOK_ID expr, TLIST []) ->
  fprintf fd "    | SV_%s -> ()\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2(LPAREN, TLIST [TOK_ID memb])) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" expr ast memb
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST [TUPLE2 (SSpanned, TOK_ID id)])) ->
  fprintf fd "    | SV_%s (%sSpanned, %s%s) -> ()\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (SSpanned, TOK_ID id)]) ->
  fprintf fd "    | SV_%s (%sSpanned, %s%s) -> ()\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TUPLE2 (LBRACE, TLIST lst)) ->
  fprintf fd "    | SV_%s {\n" expr;
  List.iter (dump_substruct fd ast) lst;
  fprintf fd "    } -> ()\n"
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST lst)) ->
  fprintf fd "    | SV_%s (%s) -> ()\n" expr (map_star ast lst)
| TUPLE2 (TOK_ID expr, TUPLE2 (SVec, TUPLE2 (TOK_ID arr, TOK_ID a))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" expr ast arr
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID expr, TOK_ID a)) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TLIST lst)) ->
  fprintf fd "    | SV_%s ((%s) -> ()\n" memb (map_star ast lst)
| TUPLE2 (TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE2 (TOK_ID expr, TOK_ID "a"))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE2 (TOK_ID anyNode, TOK_ID "a"))) ->
  fprintf fd "    | SV_%s (%s%s, %s%s) -> ()\n" memb ast cell ast dyn
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s (%s%s, %s%s) -> ()\n" memb ast cell ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID expr; TOK_ID span])) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TOK_ID expr)) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a); TUPLE2 (SVec, TUPLE3 (TOK_ID arr, QUOTE, TOK_ID a'))])) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE5 (AMPERSAND, QUOTE, TOK_ID a, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a')]))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE5 (TOK_ID cell, SOption, TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID dyn, TUPLE3 (TOK_ID anynode, QUOTE, TOK_ID a'))) ->
  fprintf fd "    | SV_%s (%s%s) -> ()\n" memb ast cell
| oth -> unhand := Some oth; failwith "dump_enum"

let dump_struct fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| TUPLE2 (TOK_ID expr, TOK_ID memb) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, SOption) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TLIST []) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2(LPAREN, TLIST [TOK_ID memb])) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST [TUPLE2 (SSpanned, TOK_ID id)])) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (SSpanned, TOK_ID id)]) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2 (LBRACE, TLIST lst)) ->
  fprintf fd "        %s: {;\n" expr;
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST lst)) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2 (SVec, TUPLE2 (TOK_ID arr, TOK_ID a))) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID expr, TOK_ID a)) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TLIST lst)) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE2 (TOK_ID expr, TOK_ID "a"))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE2 (TOK_ID anyNode, TOK_ID "a"))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID expr; TOK_ID span])) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TOK_ID expr)) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a); TUPLE2 (SVec, TUPLE3 (TOK_ID arr, QUOTE, TOK_ID a'))])) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE5 (AMPERSAND, QUOTE, TOK_ID a, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a')]))) ->
  fprintf fd "        %s;\n" memb
| TUPLE2 (TOK_ID memb, TUPLE5 (TOK_ID cell, SOption, TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID dyn, TUPLE3 (TOK_ID anynode, QUOTE, TOK_ID a'))) ->
  fprintf fd "        %s;\n" memb
| oth -> unhand := Some oth; failwith "dump_struct"

let dump_itm fd arg typlst = function
| TUPLE4(Senum, TOK_ID k, TLIST typ_p, TLIST lst') ->
  let lst = List.filter (function TOK_COMMENT _ -> false | _ -> true) lst' in
  fprintf fd "\n";
  List.iter (dump_enum fd arg typlst) lst';
  fprintf fd "\n";
| TUPLE4(Sstruct, TOK_ID k, TLIST typ_p, TLIST lst') ->
  let lst = List.filter (function TOK_COMMENT _ -> false | _ -> true) lst' in
  if lst <> [] then
    begin
      fprintf fd " {\n";
    end;
  List.iter (dump_struct fd arg typlst) lst';
  if lst <> [] then
    fprintf fd "\n} -> ()\n\n"
  else
    output_string fd " _ -> ()\n"
| oth -> unhand := Some oth; failwith "dump_itm"

let typ_dump arg typlst = function
| TUPLE2 (QUOTE, TOK_ID a) -> typlst := a :: !typlst; "'"^arg^a
| TOK_ID t -> typlst := t :: !typlst; "'"^arg^t
| oth -> unhand := Some oth; failwith "typ_dump"

let dump arg rtl =
  let fd = open_out (arg^"_dump_new.ml") in
  output_string fd ("open Ast_types_old\n\n");
  output_string fd ("let dump = function _ -> ()\n");
  output_string fd ("let dumpchar = function _ -> ()\n");
  output_string fd ("let dumpSpan = function _ -> ()\n");
  output_string fd ("let dumpSpanned = function _ -> ()\n");
  output_string fd ("let dumpbool = function _ -> ()\n");
  output_string fd ("let dumpName = function _ -> ()\n");
  output_string fd ("let dumpKw = function _ -> ()\n");
  output_string fd ("let dumpusize = function _ -> ()\n");
  output_string fd ("let dumpdyn = function _ -> ()\n");
  output_string fd ("let dumpPropSpec = function _ -> ()\n");
  output_string fd ("let dumpNodeId = function _ -> ()\n");
  output_string fd ("let dumpCell = function _ -> ()\n");
  output_string fd ("let dumpAnyNode = function _ -> ()\n");
  List.iter (function
      | TOK_COMMENT s ->
        fprintf fd "(* %s *)\n" s
      | TUPLE4(typ, TOK_ID k, TLIST typ_p, TLIST lst') as itm ->
        let typlst = ref [] in
        fprintf fd "\n";
        fprintf fd "and (dump%s:%s%s%s -> unit) = function " k (if typ_p <> [] then "("^String.concat ", " (List.map (typ_dump arg typlst) typ_p)^")" else "") arg k;
        dump_itm fd arg !typlst itm;
      | oth -> unhand := Some oth; failwith "dump104"
      ) (rtl);
  close_out fd
