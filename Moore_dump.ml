open Moore
open Printf

let unhand = ref None

let dump_map ast = function
        | oth -> unhand := Some oth; failwith "dump_map"

let map_star ast lst = String.concat " * " (List.map (dump_map ast) lst)

let tchk ast typlst memb =
  (if List.mem memb typlst then "'" else "")^ast^memb

let dump_substruct fd ast = function
        | oth -> unhand := Some oth; failwith "dump_substruct"

let dump_enum fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| oth -> unhand := Some oth; failwith "dump_enum"

let dump_struct fd ast typlst = function
| TOK_COMMENT s ->
  fprintf fd "    (* %s *)\n" s
| oth -> unhand := Some oth; failwith "dump_struct"

let dump_itm fd arg typlst = function
| oth -> unhand := Some oth; failwith "dump_itm"

let typ_dump arg typlst = function
| oth -> unhand := Some oth; failwith "typ_dump"

let dump arg rtl =
  let fd = open_out (arg^"_types_new.ml") in
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
