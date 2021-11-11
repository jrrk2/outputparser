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
| TOK_ID "Static" ->
  let rslt:astLifetime = SV_Static in rslt
| oth -> unhand := Some oth; failwith "cnv_life"

let cnv_life_opt = function
| TOK_ID "None" -> None
| oth -> Some (cnv_life oth)

let cnv_dir = function
| "Input" ->
  let (rslt:astPortDir option) = Some SV_Input in rslt
| "Output" ->
  let (rslt:astPortDir option) = Some SV_Output in rslt
| oth -> unhand := Some (TOK_STRING oth); failwith "cnv_dir"
| oth -> None

let cnv_op = function
| TOK_ID "Add" -> let rslt:tokenOp = SV_Add in rslt
| TOK_ID "Sub" -> let rslt:tokenOp = SV_Sub in rslt
| TOK_ID "LogicAnd" -> SV_LogicAnd
| TOK_ID "LogicNot" -> SV_LogicNot
| oth -> unhand := Some oth; failwith "cnv_op"

let rec cnv_expr = function
| TUPLE3 (TOK_ID "Expr", TOK_ID "UnaryExpr", TLIST
       [TUPLE3 (TOK_ID "op", COLON, op);
        TUPLE3 (TOK_ID "expr", COLON, expr);
        TUPLE3 (TOK_ID "postfix", COLON, TOK_ID postfix)]) ->
  SV_UnaryExpr {op=cnv_op op; expr=cnv_expr expr; postfix=bool_of_string postfix}
| TUPLE3 (TOK_ID "Expr", TOK_ID "BinaryExpr", TLIST
        [TUPLE3 (TOK_ID "op", COLON, op);
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

let cnv_dim = function
| TUPLE2 (TOK_ID "Range", TLIST [lft; rght]) ->
  let (rslt:('a)astTypeDim) = SV_Range (cnv_expr lft, cnv_expr rght) in rslt
| oth -> unhand := Some oth; failwith "cnv_dim"

let cnv_dims dims = Array.of_list (List.map cnv_dim dims)

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
  let ty:('a)astType = {kind = SV_LogicType; sign = SV_None; dims = cnv_dims dims} in
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

let cnv_delay = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_delay"

let cnv_event = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_event"

let dummy_stmt = { label=None; kind=SV_NullStmt }
let (stmt'':(unit)astStmt ref) = ref dummy_stmt

let rec dump_stmt_kind = function
| TUPLE3 (TOK_ID "Stmt", TOK_ID "StmtData", TLIST
           [TUPLE3 (TOK_ID "label", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "kind", COLON, stmt)]) -> { label=None; kind=dump_stmt stmt }
| TUPLE3 (TOK_ID "kind", COLON, TOK_ID "Always") -> dummy_stmt
| TUPLE3 (TOK_ID "stmt", COLON, stmt) -> dump_stmt_kind stmt
| oth -> unhand := Some oth; failwith "dump_stmt_kind"

and opt_stmt = function
| TUPLE2(TOK_ID "Some", TLIST [else_stmt']) -> Some (dump_stmt_kind else_stmt')
| _ -> None

and cnv_prior_opt = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_prior_opt"

and cnv_case_kind = function
| TOK_ID "Normal" -> let mode:astCaseKind = SV_Normal in mode
| oth -> unhand := Some oth; failwith "cnv_case_kind"

and cnv_case_mode = function
| TOK_ID "Normal" -> let mode:astCaseMode = SV_Normal in mode
| oth -> unhand := Some oth; failwith "cnv_case_mode"

and cnv_case_item = function
| TUPLE2 (TOK_ID "Expr", TLIST [TLIST exprlst; stmtkind]) ->
  let itm:('a)astCaseItem = SV_Expr (Array.of_list (List.map cnv_expr exprlst), dump_stmt_kind stmtkind) in itm
| oth -> unhand := Some oth; failwith "cnv_case_item"

and dump_stmt = function
| TUPLE2 (TOK_ID "CaseStmt", TLIST
       [TUPLE3 (TOK_ID "up", COLON, up);
        TUPLE3 (TOK_ID "kind", COLON, kind);
        TUPLE3 (TOK_ID "expr", COLON, expr);
        TUPLE3 (TOK_ID "mode", COLON, mode);
        TUPLE3 (TOK_ID "items", COLON, TLIST itemlst)]) ->
  SV_CaseStmt {
    up=cnv_prior_opt up;
    kind=cnv_case_kind kind;
    expr=cnv_expr expr;
    mode=cnv_case_mode mode;
    items=Array.of_list (List.map cnv_case_item itemlst); }
| TUPLE2 (TOK_ID "NonblockingAssignStmt", TLIST
        [TUPLE3 (TOK_ID "lhs", COLON, lhs);
         TUPLE3 (TOK_ID "rhs", COLON, rhs);
         TUPLE3 (TOK_ID "delay", COLON, delay);
         TUPLE3 (TOK_ID "event", COLON, event)]) ->
  SV_NonblockingAssignStmt {
    lhs=cnv_expr lhs;
    rhs=cnv_expr rhs;
    delay=cnv_delay delay;
    event=cnv_event event; }
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
  SV_IfStmt {up=None;
             cond=cnv_expr cond;
             main_stmt=dump_stmt_kind main_stmt;
             else_stmt=opt_stmt else_stmt; }
| TUPLE2 (TOK_ID "TimedStmt", TLIST [lft;rght]) as oth ->
  SV_TimedStmt (dump_ev lft, dump_stmt_kind rght )
| TUPLE2 (TOK_ID "SequentialBlock", TLIST [TLIST lst]) -> SV_SequentialBlock (Array.of_list (List.map (dump_stmt_kind) lst))
| TOK_ID "NullStmt" -> SV_NullStmt
| oth -> unhand := Some oth; failwith "dump_stmt"

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
       stmt = dump_stmt_kind stmt}
| oth -> unhand := Some oth; failwith "dump_proc"

let cnv_sign = function
| TOK_ID "None" -> let rslt:astTypeSign = SV_None in rslt
| oth -> unhand := Some oth; failwith "cnv_sign"

let cnv_type_kind = function
| TUPLE3 (TOK_ID "TypeKind", TOK_ID "EnumType", TLIST [
      TUPLE3 (TOK_ID "Enum", TOK_ID "EnumData", TLIST 
                [TUPLE3 (TOK_ID "base_type", COLON, base_type);
                 TUPLE3 (TOK_ID "variants", COLON, TLIST vlst)])]) ->
      SV_EnumType {base_type=None;
       variants=[||]}
| oth -> unhand := Some oth; failwith "cnv_type_kind"

let cnv_ty = function
| TUPLE3 (TOK_ID "Type", TOK_ID "TypeData", TLIST
              [TUPLE3 (TOK_ID "kind", COLON, kind);
               TUPLE3 (TOK_ID "sign", COLON, sign);
               TUPLE3 (TOK_ID "dims", COLON, TLIST dims)]) ->
  { kind=cnv_type_kind kind;
    sign=cnv_sign sign;
    dims=cnv_dims dims; }

| oth -> unhand := Some oth; failwith "cnv_ty"

let cnv_span = function
| TUPLE5 (TOK_ID "Source", TOK_INT n, TOK_STRING pth, TOK_INT strt, TOK_INT stop) -> ()
| oth -> unhand := Some oth; failwith "cnv_span"

let cnv_init = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_init"

let cnv_names = function
| TUPLE3 (TOK_ID "VarDeclName", TOK_ID "VarDeclNameData", TLIST
       [TUPLE3 (TOK_ID "name", COLON, TUPLE2 (TOK_ID nam, TLIST [TOK_INT _]));
        TUPLE3 (TOK_ID "name_span", COLON, span);
        TUPLE3 (TOK_ID "dims", COLON, TLIST dims);
        TUPLE3 (TOK_ID "init", COLON, init)]) ->
  { name=nam;
    name_span=cnv_span span;
    dims=cnv_dims dims;
    init=cnv_init init;
  }
| oth -> unhand := Some oth; failwith "cnv_names"

let dump_decl = function
| TUPLE3 (TOK_ID "VarDecl", TOK_ID "VarDeclData", TLIST
           [TUPLE3 (TOK_ID "konst", COLON, TOK_ID konst);
            TUPLE3 (TOK_ID "var", COLON, TOK_ID var);
            TUPLE3 (TOK_ID "lifetime", COLON, life);
            TUPLE3 (TOK_ID "ty", COLON, ty);
            TUPLE3 (TOK_ID "names", COLON, TLIST namlst)]) ->
    SV_VarDecl
      {konst = bool_of_string konst;
       var = bool_of_string var;
       lifetime = cnv_life_opt life;
       ty = cnv_ty ty;
       names = Array.of_list (List.map cnv_names namlst);
      }
| oth -> unhand := Some oth; failwith "dump_decl"

let dump_item = function
| TUPLE3 (TOK_ID "Item", TOK_ID "Procedure", TLIST [proc]) -> dump_proc proc
| TUPLE3 (TOK_ID "Item", TOK_ID "VarDecl", TLIST [decl]) -> dump_decl decl
| oth -> unhand := Some oth; failwith "dump_item"

let dump_mod = function
| TUPLE3 (TOK_ID "Module", TOK_ID "ModuleData", TLIST (
      TUPLE3 (TOK_ID "lifetime", COLON, life) ::
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
