open Struct
open Printf

let unhand = ref None

let dump_map ast = function
        | TUPLE2 (SBox, TOK_ID memb) -> ast ^ memb
        | TUPLE3 (TLIST [], TUPLE2 (SBox, TOK_ID memb), TLIST []) -> ast ^ memb
        | TUPLE2 (Svec, TOK_ID kind) -> ast ^ kind ^ " array"
        | TUPLE2 (SSpanned, TOK_ID nam) -> ast ^ nam
        | TUPLE2 (SOption, TLIST [TUPLE2 (SBox, TOK_ID memb)]) -> ast ^ memb ^ " option"
        | TOK_ID id -> ast ^ id
        | TUPLE2 (SOption, TLIST [TOK_ID memb]) -> ast ^ memb ^ " option"
        | TUPLE2 (Svec, TUPLE2 (TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)])) -> "('" ^ ast ^ a ^ ")" ^ ast ^ kind ^ " array"
        | TUPLE3 (TLIST [], TUPLE2 (Svec, TUPLE2 (TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)])), TLIST []) -> "('" ^ ast ^ a ^ ")" ^ ast ^ kind ^ " array"
        | TUPLE3 (TLIST [], TOK_ID kind, TLIST [TUPLE2 (QUOTE, TOK_ID a)]) -> "('" ^ ast ^ a ^ ")" ^ ast ^ kind
        | TUPLE3 (TLIST [], TOK_ID kind, TLIST []) -> ast ^ kind
        | TUPLE3 (TLIST [], TUPLE2 (SSpanned, TOK_ID kind), TLIST []) -> ast ^ kind
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE2 (SBox, TOK_ID memb), TLIST [])]), TLIST []) -> ast ^ memb ^ " option"
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID a)])]), TLIST []) -> "('" ^ ast ^ a ^ ")" ^ ast ^ memb ^ " option"
        | TUPLE3 (TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID q)]) ->  "('" ^ ast ^ a ^ ")" ^ ast ^ memb
        | TUPLE3 (TOK_ID memb, QUOTE, TOK_ID a) -> "('" ^ ast ^ a ^ ")" ^ ast ^ memb
        | TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID memb, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST []) -> "('" ^ ast ^ a ^ ")" ^ ast ^ memb
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST [])]), TLIST []) ->
          "('" ^ ast ^ a ^ ")" ^ ast ^ expr ^ " option"
        | TUPLE3 (TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [])]), TLIST []) ->
          ast ^ expr ^ " option"
        | oth -> unhand := Some oth; failwith "dump_map"

let map_star ast lst = String.concat " * " (List.map (dump_map ast) lst)

let tchk ast typlst memb =
  (if List.mem memb typlst then "'" else "")^ast^memb

let dump_substruct fd ast = function
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
        | TUPLE3 (COLON, TOK_ID memb, TUPLE2 (Svec, TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]))) -> 
  fprintf fd "        %s: ('%s%s)%s%s array;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SSpanned, TOK_ID expr), TLIST []) ->
  fprintf fd "        %s: %sSpanned * %s%s;\n" memb ast ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [])]), TLIST []) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (Svec, TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)])), TLIST []) ->
  fprintf fd "        %s: ('%s%s)%s%s array;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE2 (SSpanned, TOK_ID expr), TLIST [])]), TLIST []) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TOK_ID expr, TLIST []) ->
  fprintf fd "        %s: %s%s;\n" memb ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST []) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)])]), TLIST []) ->
  fprintf fd "        %s: ('%s%s)%s%s option;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE2 (SBox, TOK_ID expr), TLIST [])]), TLIST []) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST []), TLIST []) ->
  fprintf fd "        %s: unit option;\n" memb;
        | TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr;
        | TUPLE5 (COLON, TOK_ID memb, TLIST [], TUPLE2 (SOption, TLIST [TUPLE3 (TLIST [], TUPLE3 (SBox, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a)]), TLIST [])]), TLIST []) ->
  fprintf fd "        %s:  ('%s%s)%s%s option;\n" memb ast a ast expr;
        | oth -> unhand := Some oth; failwith "dump_substruct"

let dump_enum fd ast typlst = function
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
  List.iter (dump_substruct fd ast) lst;
  fprintf fd "    }\n"
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST lst)) ->
  fprintf fd "    | SV_%s of (%s)\n" expr (map_star ast lst)
| TUPLE2 (TOK_ID expr, TUPLE2 (SVec, TUPLE2 (TOK_ID arr, TOK_ID a))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s array\n" expr ast a ast arr
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID expr, TOK_ID a)) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s\n" memb ast a ast expr
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
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s\n" memb ast a ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s array\n" memb ast a ast expr;
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s of %s%s * %s%s\n" memb ast cell ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID expr; TOK_ID span])) ->
  fprintf fd "    | SV_%s of %s%s option\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TOK_ID expr)) ->
  fprintf fd "    | SV_%s of %s%s option\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s option\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a); TUPLE2 (SVec, TUPLE3 (TOK_ID arr, QUOTE, TOK_ID a'))])) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s option\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s option\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "    | SV_%s of %s%s option\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE5 (AMPERSAND, QUOTE, TOK_ID a, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a')]))) ->
  fprintf fd "    | SV_%s of ('%s%s) %s%s option\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE5 (TOK_ID cell, SOption, TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID dyn, TUPLE3 (TOK_ID anynode, QUOTE, TOK_ID a'))) ->
  fprintf fd "    | SV_%s of ('%s%s)%s%s option\n" memb ast a ast cell
| oth -> unhand := Some oth; failwith "dump_enum"

let dump_struct fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| TUPLE2 (TOK_ID expr, TOK_ID memb) ->
  fprintf fd "        %s: %s;\n" expr (tchk ast typlst memb)
(*
| TUPLE2 (TOK_ID memb, TOK_ID expr) ->
  fprintf fd "        %s: %s%s;\n" memb ast expr;
*)
| TUPLE2 (TOK_ID expr, SOption) ->
  fprintf fd "        %s: unit option;\n" expr
| TUPLE2 (TOK_ID expr, TLIST []) ->
  fprintf fd "        %s;\n" expr
| TUPLE2 (TOK_ID expr, TUPLE2(LPAREN, TLIST [TOK_ID memb])) ->
  fprintf fd "        %s: %s;\n" expr (tchk ast typlst memb)
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST [TUPLE2 (SSpanned, TOK_ID id)])) ->
  fprintf fd "        %s: %sSpanned * %s%s;\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TLIST [TUPLE2 (SSpanned, TOK_ID id)]) ->
  fprintf fd "        %s: %sSpanned * %s%s;\n" expr ast ast id
| TUPLE2 (TOK_ID expr, TUPLE2 (LBRACE, TLIST lst)) ->
  fprintf fd "        %s: {;\n" expr;
| TUPLE2 (TOK_ID expr, TUPLE2 (LPAREN, TLIST lst)) ->
  fprintf fd "        %s: (%s);\n" expr (map_star ast lst)
| TUPLE2 (TOK_ID expr, TUPLE2 (SVec, TUPLE2 (TOK_ID arr, TOK_ID a))) ->
  fprintf fd "        %s: ('%s%s)%s%s array;\n" expr ast a ast arr
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID expr, TOK_ID a)) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TLIST lst)) ->
  fprintf fd "        %s: (%s) array;\n" memb (map_star ast lst)
| TUPLE2 (TOK_ID memb, TUPLE2 (SSpanned, TOK_ID expr)) ->
  fprintf fd "        %s: %s%s;\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "        %s: %s%s array;\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE2 (TOK_ID expr, TOK_ID "a"))) ->
  fprintf fd "        %s: %s%s;\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE2 (TOK_ID anyNode, TOK_ID "a"))) ->
  fprintf fd "        %s: %s%s * %s%s;\n" memb ast cell ast dyn
(*
| TUPLE2 (TOK_ID memb, TUPLE2 (TOK_ID cell, TOK_ID usize)) ->
  fprintf fd "        %s: %s%s * %s%s;\n" memb ast cell ast usize
*)
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SVec, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s: ('%s%s)%s%s array;\n" memb ast a ast expr;
| TUPLE2 (TOK_ID memb, TUPLE3 (TOK_ID cell, TOK_ID dyn, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s: %s%s * %s%s;\n" memb ast cell ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s: ('%s%s)%s%s;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TOK_ID expr; TOK_ID span])) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TOK_ID expr)) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr;
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a))) ->
  fprintf fd "        %s: ('%s%s)%s%s option;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TLIST [TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a); TUPLE2 (SVec, TUPLE3 (TOK_ID arr, QUOTE, TOK_ID a'))])) ->
  fprintf fd "        %s: ('%s%s)%s%s option;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SBox, TUPLE3 (TOK_ID expr, QUOTE, TOK_ID a)))) ->
  fprintf fd "        %s: ('%s%s)%s%s option;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE2 (SSpanned, TOK_ID expr))) ->
  fprintf fd "        %s: %s%s option;\n" memb ast expr
| TUPLE2 (TOK_ID memb, TUPLE2 (SOption, TUPLE5 (AMPERSAND, QUOTE, TOK_ID a, TOK_ID expr, TLIST [TUPLE2 (QUOTE, TOK_ID a')]))) ->
  fprintf fd "        %s: ('%s%s) %s%s option;\n" memb ast a ast expr
| TUPLE2 (TOK_ID memb, TUPLE5 (TOK_ID cell, SOption, TLIST [TUPLE3 (AMPERSAND, QUOTE, TOK_ID a)], TOK_ID dyn, TUPLE3 (TOK_ID anynode, QUOTE, TOK_ID a'))) ->
  fprintf fd "        %s: ('%s%s)%s%s option;\n" memb ast a ast cell
| oth -> unhand := Some oth; failwith "dump_struct"

let dump_itm fd arg typlst = function
| TUPLE4(Senum, TOK_ID k, TLIST typ_p, TLIST lst') ->
  let lst = List.filter (function TOK_COMMENT _ -> false | _ -> true) lst' in
  fprintf fd "\n";
  List.iter (dump_enum fd arg typlst) lst';
| TUPLE4(Sstruct, TOK_ID k, TLIST typ_p, TLIST lst') ->
  let lst = List.filter (function TOK_COMMENT _ -> false | _ -> true) lst' in
  if lst <> [] then
    begin
      fprintf fd " {\n";
    end;
  List.iter (dump_struct fd arg typlst) lst';
  if lst <> [] then
    fprintf fd "\n}\n"
  else
    output_string fd " unit\n"
| oth -> unhand := Some oth; failwith "dump_itm"

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
  output_string fd ("type "^arg^"AnyNode = unit\n");
  List.iter (function
      | TOK_COMMENT s ->
        fprintf fd "(* %s *)\n" s
      | TUPLE4(typ, TOK_ID k, TLIST typ_p, TLIST lst') as itm ->
        let typlst = ref [] in
        fprintf fd "\n";
        fprintf fd "and %s%s%s =" (if typ_p <> [] then "("^String.concat ", " (List.map (typ_dump arg typlst) typ_p)^")" else "") arg k;
        dump_itm fd arg !typlst itm;
      | oth -> unhand := Some oth; failwith "dump104"
      ) (rtl);
  close_out fd
