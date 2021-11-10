open Moore
open Printf
open Ast_types_old

let unhand = ref None
let unhand_src  = ref None
let unhand_tim  = ref None
let unhand_itms  = ref None
let unhand_itm  = ref None
let unhand_mod  = ref None
let unhand_life  = ref None
let unhand_imp  = ref None
let unhand_parm  = ref None
let unhand_item  = ref None
let unhandlst = ref []

let cnv_life = function
| "Static" ->
  let rslt:astLifetime = SV_Static in rslt
| oth -> unhand := Some (TOK_STRING oth); failwith "cnv_life"

let cnv_dir = function
| "Input" ->
  let (rslt:astPortDir option) = Some SV_Input in rslt
| "Output" ->
  let (rslt:astPortDir option) = Some SV_Output in rslt
| oth -> unhand := Some (TOK_STRING oth); failwith "cnv_dir"
| oth -> None

let cnv_expr = function
(*
| TUPLE6 (TOK_ID "rhs", COLON, TOK_ID "Expr", TOK_INT _, TOK_ID "BinaryExpr", TLIST lst
| TUPLE3 (TOK_ID "op", COLON, TOK_ID "Add") -> ()
| TUPLE6 (TOK_ID "lhs", COLON, TOK_ID "Expr", TOK_INT _, TOK_ID "IdentExpr", TLIST [TUPLE2 (TOK_ID "q", TLIST [TOK_INT n])]) -> ()
| TUPLE6 (TOK_ID "rhs", COLON, TOK_ID "Expr", TOK_INT _, TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "BasedInteger", TLIST lst)]) -> ()
| TUPLE2 (TOK_ID "Some", TLIST lst) -> ()
| TUPLE2 (TOK_INT 8, TLIST [TOK_INT 15])]) -> ()
| TOK_ID "false" -> ()
| TOK_OTH 98 -> ()
| TUPLE2 (TOK_INT 1, TLIST [TOK_INT 17]) -> ()
| TUPLE6 (TOK_ID "lhs", COLON, TOK_ID "Expr", TOK_INT _, TOK_ID "IdentExpr", TLIST [TUPLE2 (TOK_ID "q", TLIST [TOK_INT n])]) -> ()
| TUPLE6 (TOK_ID "rhs", COLON, rhs) -> ()
| TUPLE3 (TOK_ID "op", COLON, TOK_ID "Identity")] -> ()
*)
| TUPLE4 (TOK_ID "Expr", TOK_INT _, TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "Number", TLIST [TUPLE2 (TOK_INT n, TLIST [TOK_INT _]); TOK_ID "None"])]) ->
  let (rslt:('a)astExpr) = SV_LiteralExpr (SV_Number(string_of_int n, None)) in rslt
| oth -> unhand := Some oth; failwith "cnv_expr"

let cnv_dims = function
| TUPLE2 (TOK_ID "Range", TLIST [lft; rght]) ->
  let (rslt:('a)astTypeDim) = SV_Range (cnv_expr lft, cnv_expr rght) in rslt
| oth -> unhand := Some oth; failwith "cnv_dims"

let cnv_stmt = function
  | TUPLE6 (TOK_ID "main_stmt", COLON, TOK_ID "Stmt",
            TOK_INT 35, TOK_ID "StmtData",
            TLIST
              [TUPLE3 (TOK_ID "label", COLON, TOK_ID "None");
               TUPLE3
                 (TOK_ID "kind", TOK_ID "BlockingAssignStmt",
                  TLIST lst)]) -> ()
| oth -> failwith "cnv_stmt"

let dump_port = function
| TOK_ID id -> failwith "dump_port"
| TUPLE4
     (TOK_ID "Port", TOK_INT _, TOK_ID "Named",
      TLIST
       [TUPLE4 (TOK_ID "dir", COLON, TOK_ID "Some", TLIST [TOK_ID dir]);
        TUPLE3 (TOK_ID "kind", COLON, TOK_ID "None");
        TUPLE6
         (TOK_ID "ty", COLON, TOK_ID "Type", TOK_INT _, TOK_ID "TypeData",
          TLIST
           [TUPLE4 (TOK_ID "kind", COLON, TOK_ID "TypeKind", TOK_INT _);
            TUPLE3 (TOK_ID "sign", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "dims", COLON, TLIST dims)]);
        TUPLE4 (TOK_ID "name", COLON, TOK_ID nam, TOK_INT _);
        TUPLE3 (TOK_ID "dims", COLON, TLIST []);
        TUPLE3 (TOK_ID "expr", COLON, TOK_ID "None")]) ->
  let ty:('a)astType = {kind = SV_LogicType; sign = SV_None; dims = Array.of_list (List.map cnv_dims dims)} in
  let rslt:('a)astPort = SV_Named {dir = cnv_dir dir; kind = None; ty = ty; name = (0,nam); dims = [||]; expr = None} in
  rslt
| oth -> unhand := Some oth; failwith "dump_port"

let dump_imp = function
| oth ->
  let rslt:('a)astImportDecl = {items=[||]} in rslt
| oth -> unhand_imp := Some oth; failwith "dump_imp"

let dump_parm = function
| oth ->
  let rslt:('a)astParamDecl = {local=false; kind=SV_Value [||]} in rslt
| oth -> unhand_parm := Some oth; failwith "dump_parm"

let dump_proc = function
| TUPLE4 (TOK_ID "Procedure", TOK_INT 52, TOK_ID "ProcedureData", TLIST lst) ->
  List.map (function
      | TUPLE3 (TOK_ID "kind", TOK_ID "IfStmt", TLIST lst) -> ()
      | TUPLE3 (TOK_ID "kind", COLON, TOK_ID "Always") -> ()
      | TUPLE6 (TOK_ID "stmt", COLON, TOK_ID "Stmt", TOK_INT _, TOK_ID "StmtData", TLIST lst) -> ()
      | TUPLE4 (TOK_ID "Stmt", TOK_INT _, TOK_ID "StmtData", TLIST lst) -> ()
      | TUPLE4 (TOK_ID "Stmt", TOK_INT _, TOK_ID "StmtData", TLIST lst) -> ()
| oth -> failwith "proc") lst

let dump_item = function
(*
| TUPLE4 (TOK_ID "Item", TOK_INT _, TOK_ID "Procedure", TLIST lst) ->
  List.map (dump_proc) lst
*)
| oth ->
  SV_Procedure
      {kind = SV_Always;
       stmt =
        {label = None;
         kind =
          SV_IfStmt
           {up = None; cond = SV_IdentExpr "rst";
            main_stmt =
             {label = None;
              kind =
               SV_BlockingAssignStmt
                {lhs = SV_IdentExpr "q";
                 rhs =
                  SV_LiteralExpr (SV_BasedInteger (None, false, 'b', "0"));
                 op = SV_Identity}};
            else_stmt =
             Some
              {label = None;
               kind =
                SV_BlockingAssignStmt
                 {lhs = SV_IdentExpr "q";
                  rhs =
                   SV_BinaryExpr
                    {op = Token_types_old.SV_Add;
                     lhs = SV_IdentExpr "q";
                     rhs =
                      SV_LiteralExpr
                       (SV_BasedInteger (None, false, 'b', "1"))};
                  op = SV_Identity}}}}}
| oth -> unhand_item := Some oth; failwith "dump_item"

let dump_itm' = function
|  TUPLE3 (TOK_ID "lifetime", COLON, TOK_ID life) ::
   TUPLE4 (TOK_ID "name", COLON, TOK_ID nam, TOK_INT _) ::
   TUPLE3 (TOK_ID "imports", COLON, TLIST implst) ::
   TUPLE3 (TOK_ID "params", COLON, TLIST parlst) ::
   TUPLE3 (TOK_ID "ports", COLON, TLIST portlst) ::
   TUPLE3 (TOK_ID "items", COLON, TLIST itmlst) :: [] ->
  {lifetime = cnv_life life; name = nam;
  imports = Array.of_list (List.map dump_imp implst);
  params = Array.of_list (List.map dump_parm parlst);
  ports = Array.of_list (List.map (fun itm ->
     let port:('a)astPort = dump_port itm in port) portlst);
  items = Array.of_list (List.map dump_item itmlst)}
| oth -> unhandlst := oth; failwith "dump_itm'"

let dump_mod = function
| TUPLE4 (TOK_ID "Module", _, TOK_ID "ModuleData", TLIST lst) -> SV_ModuleDecl (dump_itm' lst)
| oth -> unhand_mod := Some oth; failwith "dump_itm"

let dump_itm = function
| TUPLE4 (TOK_ID "Item", _, TOK_ID "ModuleDecl", TLIST lst) -> List.map (dump_mod) lst
| oth -> unhand_itm := Some oth; failwith "dump_itm"

let dump_tim = function
| TUPLE3 (TOK_ID "timeunits", _, TLIST [TUPLE3 (TOK_ID "unit", COLON, TOK_ID "None"); TUPLE3 (TOK_ID "prec", COLON, TOK_ID "None")]) ->
  let rslt:astTimeunit = { unit=None; prec=None; } in rslt
| oth -> unhand_tim := Some oth; failwith "dump_itm"

let dump_itms = function
| TUPLE3 (TOK_ID "items", COLON, TLIST lst) -> List.map (dump_itm) lst
| oth -> unhand_itms := Some oth; failwith "dump_itm"

let rec dump_src = function
| TUPLE4 (TOK_ID "SourceFile", _, TOK_ID "SourceFileData", TLIST (unitlst :: itmlst :: [])) ->
  let unit' = dump_tim unitlst in
  let itm' = List.flatten (dump_itms itmlst) in
   { files = [| { timeunits=unit'; items=Array.of_list (itm'); } |] }
| oth -> unhand_src := Some oth; failwith "dump_itm"

and dump_life = function
| TUPLE3 (TOK_ID "lifetime", COLON, TOK_ID life) -> life
| oth -> unhand_life := Some oth; failwith "dump_itm"

let dump rtl = List.map (function
      | TUPLE2(TOK_ID "Svlog", TLIST lst) ->
        List.map (dump_src) lst;
      | oth -> unhand := Some oth; failwith "dump40"
      ) (rtl)
