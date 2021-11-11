open Moore
open Printf
open Ast_types_old
open Token_types_old

let unhand = ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
let unhand= ref None
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

let cnv_op = function
| "Add" -> let rslt:tokenOp = SV_Add in rslt

let rec cnv_expr = function
| TUPLE3 (TOK_ID "Expr", TOK_ID "BinaryExpr", TLIST
        [TUPLE3 (TOK_ID "op", COLON, TOK_ID op);
         TUPLE3 (TOK_ID "lhs", COLON, lhs);
         TUPLE3 (TOK_ID "rhs", COLON, rhs)]) -> SV_BinaryExpr {op=cnv_op op; lhs=cnv_expr lhs; rhs=cnv_expr rhs}
| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "Number", TLIST [TUPLE2 (TOK_INT n, TLIST [TOK_INT _]); TOK_ID "None"])]) ->
  let (rslt:('a)astExpr) = SV_LiteralExpr (SV_Number(string_of_int n, None)) in rslt
| TUPLE3 (TOK_ID "Expr", TOK_ID "IdentExpr", TLIST [TUPLE2 (TOK_ID id, TLIST [TOK_INT _])]) -> SV_IdentExpr id
| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "BasedInteger", TLIST [TUPLE2 (TOK_ID "Some",
                      TLIST [TUPLE2 (TOK_INT n, TLIST [TOK_INT m])]);
                   TOK_ID truth; TOK_OTH p;
                   TUPLE2 (TOK_INT q, TLIST [TOK_INT r])])]) ->
  SV_LiteralExpr (SV_BasedInteger (None, bool_of_string truth, Char.chr p,""))
| oth -> unhand := Some oth; failwith "cnv_expr"

let cnv_dims = function
| TUPLE2 (TOK_ID "Range", TLIST [lft; rght]) ->
  let (rslt:('a)astTypeDim) = SV_Range (cnv_expr lft, cnv_expr rght) in rslt
| oth -> unhand := Some oth; failwith "cnv_dims"

let dump_port = function
| TOK_ID id -> failwith "dump_port"
| TUPLE3
     (TOK_ID "Port", TOK_ID "Named",
      TLIST
       [TUPLE3 (TOK_ID "dir", COLON, TUPLE2(TOK_ID "Some", TLIST [TOK_ID dir]));
        TUPLE3 (TOK_ID "kind", COLON, TOK_ID "None");
        TUPLE3
         (TOK_ID "ty", COLON, TUPLE3 (TOK_ID "Type", TOK_ID "TypeData",
          TLIST
           [TUPLE3 (TOK_ID "kind", COLON, TUPLE2(TOK_ID "TypeKind", TOK_ID ("RegType"|"ImplicitType")));
            TUPLE3 (TOK_ID "sign", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "dims", COLON, TLIST dims)]));
        TUPLE3 (TOK_ID "name", COLON, TUPLE2(TOK_ID nam, TLIST [TOK_INT _]));
        TUPLE3 (TOK_ID "dims", COLON, TLIST []);
        TUPLE3 (TOK_ID "expr", COLON, TOK_ID "None")]) ->
  let ty:('a)astType = {kind = SV_LogicType; sign = SV_None; dims = Array.of_list (List.map cnv_dims dims)} in
  let rslt:('a)astPort = SV_Named {dir = cnv_dir dir; kind = None; ty = ty; name = (0,nam); dims = [||]; expr = None} in
  rslt
| oth -> unhand := Some oth; failwith "dump_port"

let dump_imp = function
| oth ->
  let rslt:('a)astImportDecl = {items=[||]} in rslt
| oth -> unhand := Some oth; failwith "dump_imp"

let dump_parm = function
| oth ->
  let rslt:('a)astParamDecl = {local=false; kind=SV_Value [||]} in rslt
| oth -> unhand := Some oth; failwith "dump_parm"

let rec dump_expr = function
| TUPLE3 (TOK_ID "span", COLON, TUPLE5 (TOK_ID "Source", TOK_INT n, TOK_STRING src, TOK_INT n', TOK_INT n'')) -> ()
| oth -> unhand := Some oth; failwith "dump_expr"

let cnv_op = function
| TOK_ID "Identity" -> SV_Identity

let dummy_stmt = { label=None; kind=SV_NullStmt }
let (stmt'':(unit)astStmt ref) = ref dummy_stmt

let rec dump_stmt = function
| TUPLE3 (TOK_ID "Stmt", TOK_ID "StmtData", TLIST
           [TUPLE3 (TOK_ID "label", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "kind", COLON, stmt)]) -> { label=None; kind=dump_blk stmt }
| TUPLE3 (TOK_ID "kind", COLON, TOK_ID "Always") -> dummy_stmt
| TUPLE3 (TOK_ID "stmt", COLON, stmt) -> dump_stmt stmt
| oth -> unhand := Some oth; failwith "dump_stmt"

and dump_blk = function
| TUPLE2 (TOK_ID "BlockingAssignStmt", TLIST
        [TUPLE3 (TOK_ID "lhs", COLON, lhs);
         TUPLE3 (TOK_ID "rhs", COLON, rhs);
         TUPLE3 (TOK_ID "op", COLON, op)]) ->
  SV_BlockingAssignStmt {
        lhs=cnv_expr lhs;
        rhs=cnv_expr rhs;
        op=cnv_op op }
| TUPLE2 (TOK_ID "IfStmt", TLIST
          [TUPLE3 (TOK_ID "up", COLON, TOK_ID "None");
           TUPLE3 (TOK_ID "cond", COLON, cond);
           TUPLE3 (TOK_ID "main_stmt", COLON, main_stmt);
           TUPLE3 (TOK_ID "else_stmt", COLON, else_stmt)]) ->
  SV_IfStmt {up=None; cond=cnv_expr cond; main_stmt=dump_stmt main_stmt; else_stmt=match else_stmt with TUPLE2(TOK_ID "Some", TLIST [else_stmt']) -> Some (dump_stmt else_stmt') | _ -> None}
| TUPLE2 (TOK_ID "TimedStmt", TLIST [lft;rght]) as oth ->
  SV_TimedStmt (dump_ev lft, dump_stmt rght )
| oth -> unhand := Some oth; failwith "dump_blk"

and dump_ev = function
| TUPLE2 (TOK_ID "Event", TLIST lst) -> SV_Event (dump_ev_ctrl lst)
| oth -> unhand := Some oth; failwith "dump_ev"

and dump_ev_ctrl = function
| TUPLE2 (TOK_ID "EventControl", TLIST [TUPLE3 (TOK_ID "span", COLON, TUPLE5 _); TUPLE3 (TOK_ID "data", COLON, TUPLE2 (TOK_ID "Expr", TLIST (evlst :: [])))]) :: [] ->
  { span=(); data=SV_Expr (dump_ev_expr evlst) }
| oth -> unhandlst := oth; failwith "dump_ev_ctrl"

and cnv_edge = function
| TOK_ID "Posedge" -> SV_Posedge
| oth -> unhand := Some oth; failwith "cnv_edge"

and dump_ev_expr = function
| TUPLE2 (TOK_ID "Or", TLIST (TUPLE3 (TOK_ID "span", COLON, TUPLE5 _) :: TUPLE3 (TOK_ID "lhs", COLON, lhs) :: TUPLE3 (TOK_ID "rhs", COLON, rhs) :: [])) ->
  SV_Or  { span=(); lhs=dump_ev_expr lhs; rhs=dump_ev_expr rhs}
| TUPLE2 (TOK_ID "Edge", TLIST [
    TUPLE3 (TOK_ID "span", COLON, TUPLE5 _);
    TUPLE3 (TOK_ID "edge", COLON, edg);
    TUPLE3 (TOK_ID "value", COLON, expr')]) -> SV_Edge { span=(); edge=cnv_edge edg; value=cnv_expr expr' }
| oth -> unhand := Some oth; failwith "dump_ev_expr"

let dump_proc = function
| TUPLE3 (TOK_ID "Procedure", TOK_ID "ProcedureData", TLIST [
      TUPLE3 (TOK_ID "kind", COLON, TOK_ID "Always");
      TUPLE3 (TOK_ID "stmt", COLON, stmt)]) ->
    SV_Procedure
      {kind = SV_Always;
       stmt = dump_stmt stmt}
| oth -> unhand := Some oth; failwith "proc"

let dump_item = function
| TUPLE3 (TOK_ID "Item", TOK_ID "Procedure", TLIST [proc]) -> dump_proc proc
| oth -> unhand := Some oth; failwith "dump_item"

let dump_mod = function
| TUPLE3 (TOK_ID "Module", TOK_ID "ModuleData", TLIST (
      TUPLE3 (TOK_ID "lifetime", COLON, TOK_ID life) ::
      TUPLE3 (TOK_ID "name", COLON, TUPLE2(TOK_ID nam, TLIST [TOK_INT _])) ::
      TUPLE3 (TOK_ID "imports", COLON, TLIST implst) ::
      TUPLE3 (TOK_ID "params", COLON, TLIST parlst) ::
      TUPLE3 (TOK_ID "ports", COLON, TLIST portlst) ::
      TUPLE3 (TOK_ID "items", COLON, TLIST itmlst) :: [])) ->
  SV_ModuleDecl {lifetime = cnv_life life; name = nam;
  imports = Array.of_list (List.map dump_imp implst);
  params = Array.of_list (List.map dump_parm parlst);
  ports = Array.of_list (List.map (fun itm ->
     let port:('a)astPort = dump_port itm in port) portlst);
  items = Array.of_list (List.map dump_item itmlst)}
| oth -> unhand := Some oth; failwith "dump_mod"

let dump_itm = function
| TUPLE3 (TOK_ID "Item", TOK_ID "ModuleDecl", TLIST [itm]) -> dump_mod itm
| oth -> unhand := Some oth; failwith "dump_itm"

let dump_tim = function
| TUPLE3 (TOK_ID "timeunits", COLON, TUPLE2(TOK_ID "Timeunit", TLIST [TUPLE3 (TOK_ID "unit", COLON, TOK_ID "None"); TUPLE3 (TOK_ID "prec", COLON, TOK_ID "None")])) ->
  let rslt:astTimeunit = { unit=None; prec=None; } in rslt
| oth -> unhand := Some oth; failwith "dump_tim"

let rec dump_src = function
| TUPLE3 (TOK_ID "SourceFile", TOK_ID "SourceFileData", TLIST (unitlst :: TUPLE3 (TOK_ID "items", COLON, TLIST itmlst) :: [])) ->
   { files = [| { timeunits=dump_tim unitlst; items=Array.of_list (List.map (dump_itm) itmlst); } |] }
| oth -> unhand := Some oth; failwith "dump_src"

and dump_life = function
| TUPLE3 (TOK_ID "lifetime", COLON, TOK_ID life) -> life
| oth -> unhand := Some oth; failwith "dump_life"

let dump rtl = List.map (function
      | TUPLE2(TOK_ID "Svlog", TLIST lst) ->
        List.map (dump_src) lst;
      | oth -> unhand := Some oth; failwith "dump40"
      ) (rtl)
