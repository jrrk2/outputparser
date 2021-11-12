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

(* example conversions for Sexplib interface *)

let print_source x' =
   let buf = Buffer.create 10000 in
   let formatter = Format.formatter_of_buffer buf in
   Sexplib.Sexp.pp_hum formatter x';
   Format.pp_print_flush formatter ();
   Buffer.contents buf

let cnv_range_mode = function
| TOK_ID "Absolute" -> SV_Absolute
| TOK_ID "RelativeUp" -> SV_RelativeUp
| oth -> unhand := Some oth; failwith "cnv_range_mode"

let cnv_life = function
| TOK_ID "Static" ->
  let rslt:astLifetime = SV_Static in rslt
| oth -> unhand := Some oth; failwith "cnv_life"

let cnv_life_opt = function
| TOK_ID "None" -> None
| oth -> Some (cnv_life oth)

let cnv_dir = function
| TOK_ID "Input" ->
  let (rslt:astPortDir) = SV_Input in rslt
| TOK_ID "Output" ->
  let (rslt:astPortDir) = SV_Output in rslt
| oth -> unhand := Some oth; failwith "cnv_dir"

let cnv_dir_opt = function
| TUPLE2(TOK_ID "Some", TLIST [dir]) -> Some (cnv_dir dir)
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_dir_opt"

let cnv_op = function
(* Assignment *)
| TOK_ID "Assign" -> let rslt:tokenOp = SV_Assign in rslt
(* Arith *)
| TOK_ID "Add" -> SV_Add
| TOK_ID "Sub" -> SV_Sub
| TOK_ID "Mul" -> SV_Mul
| TOK_ID "Div" -> SV_Div
| TOK_ID "Mod" -> SV_Mod
(* Equality *)
| TOK_ID "LogicEq" -> SV_LogicEq
| TOK_ID "LogicNeq" -> SV_LogicNeq
(* Relational *)
| TOK_ID "Lt" -> SV_Lt
| TOK_ID "Leq" -> SV_Leq
| TOK_ID "Geq" -> SV_Geq
| TOK_ID "Gt" -> SV_Gt
(* Logic *)
| TOK_ID "LogicAnd" -> SV_LogicAnd
| TOK_ID "LogicOr" -> SV_LogicOr
| TOK_ID "LogicNot" -> SV_LogicNot
| TOK_ID "LogicNand" -> SV_BitNand
(* Bitwise *)
| TOK_ID "BitAnd" -> SV_BitAnd
| TOK_ID "BitOr" -> SV_BitOr
| TOK_ID "BitXor" -> SV_BitXor
| TOK_ID "BitNot" -> SV_BitNot
| TOK_ID "BitNand" -> SV_BitNand
(* Shift *)
| TOK_ID "LogicShL" -> SV_LogicShL
| TOK_ID "LogicShR" -> SV_LogicShR
| TOK_ID "ArithShR" -> SV_ArithShR
(* Sequence *)
| oth -> unhand := Some oth; failwith "cnv_op"

let cnv_call_name_opt = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_call_name_opt"

let cnv_span = function
| TUPLE5 (Source, TOK_INT n, TOK_STRING pth, TOK_INT strt, TOK_INT stop) -> ()
| oth -> unhand := Some oth; failwith "cnv_span"

let rec cnv_expr = function
| TUPLE3 (TOK_ID "Expr", TOK_ID "UnaryExpr", TLIST
       [TUPLE3 (TOK_ID "op", COLON, op);
        TUPLE3 (TOK_ID "expr", COLON, expr);
        TUPLE3 (TOK_ID "postfix", COLON, TOK_ID postfix)]) ->
  SV_UnaryExpr {op=cnv_op op; expr=cnv_expr expr; postfix=bool_of_string postfix}

| TUPLE3 (TOK_ID "Expr", TOK_ID "BinaryExpr", TLIST
        [TUPLE3 (TOK_ID "op", COLON, op);
         TUPLE3 (TOK_ID "lhs", COLON, lhs);
         TUPLE3 (TOK_ID "rhs", COLON, rhs)]) ->
  SV_BinaryExpr {op=cnv_op op; lhs=cnv_expr lhs; rhs=cnv_expr rhs}

| TUPLE3 (TOK_ID "Expr", TOK_ID "TernaryExpr", TLIST
       [TUPLE3 (TOK_ID "cond", COLON, cond);
        TUPLE3 (TOK_ID "true_expr", COLON, true_expr);
        TUPLE3 (TOK_ID "false_expr", COLON, false_expr)]) ->
  SV_TernaryExpr {cond=cnv_expr cond; true_expr=cnv_expr true_expr; false_expr=cnv_expr false_expr}

| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "Number", TLIST [TUPLE2 (TOK_INT n, TLIST [TOK_INT _]); TOK_ID "None"])]) ->
  let (rslt:('a)astExpr) = SV_LiteralExpr (SV_Number(n, None)) in rslt
| TUPLE3 (TOK_ID "Expr", TOK_ID "IdentExpr", TLIST [TUPLE2 (TOK_ID id, TLIST [TOK_INT _])]) -> SV_IdentExpr id
| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "BasedInteger", TLIST [TUPLE2 (TOK_ID "Some",
                      TLIST [TUPLE2 (TOK_INT n, TLIST [TOK_INT m])]);
                   TOK_ID truth; TOK_OTH p;
                   TUPLE2 (q, TLIST [TOK_INT r])])]) ->
  SV_LiteralExpr (SV_BasedInteger (None, bool_of_string truth, Char.chr p,""))
| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TUPLE2 (TOK_ID "BasedInteger", TLIST
                                                                [TOK_ID "None";
                                                                 TOK_ID truth;
                                                                 TOK_OTH p;
                                                                 TUPLE2 (q, TLIST [TOK_INT r])])]) ->
  SV_LiteralExpr (SV_BasedInteger (None, bool_of_string truth, Char.chr p,""))

| TUPLE3 (TOK_ID "Expr", TOK_ID "SysIdentExpr", TLIST [TUPLE2 (TOK_ID sysid, TLIST [TOK_INT _])]) ->
  let (rslt:('a)astExpr) = SV_SysIdentExpr (sysid) in rslt
| TUPLE3 (TOK_ID "Expr", TOK_ID "LiteralExpr", TLIST [TOK_STRING str]) -> 
  let (rslt:('a)astExpr) = SV_LiteralExpr (SV_Str str) in rslt
| TUPLE3 (TOK_ID "Expr", TOK_ID "CallExpr", TLIST [expr1; TLIST args]) ->
  SV_CallExpr ( cnv_expr expr1, Array.of_list (List.map cnv_call_arg args) )
| TUPLE3 (TOK_ID "Expr", TOK_ID "ConcatExpr", TLIST
            [TUPLE3 (TOK_ID "repeat", COLON, repeat);
             TUPLE3 (TOK_ID "exprs", COLON, TLIST exprs)]) ->
  SV_ConcatExpr { repeat=cnv_repeat_opt repeat; exprs=Array.of_list (List.map cnv_expr exprs); }
| TUPLE3 (TOK_ID "Expr", TOK_ID "IndexExpr", TLIST
       [TUPLE3 (TOK_ID "indexee", COLON, indexee);
        TUPLE3 (TOK_ID "index", COLON, index)]) ->
  SV_IndexExpr { indexee=cnv_expr indexee; index=cnv_expr index; }
| TUPLE3 (TOK_ID "Expr", TOK_ID "RangeExpr", TLIST
       [TUPLE3 (TOK_ID "mode", COLON, mode);
        TUPLE3 (TOK_ID "lhs", COLON, lhs);
        TUPLE3 (TOK_ID "rhs", COLON, rhs)]) ->
  SV_RangeExpr { mode=cnv_range_mode mode; lhs=cnv_expr lhs; rhs=cnv_expr rhs }
| TUPLE3 (TOK_ID "Expr", TOK_ID "AssignExpr", TLIST
       [TUPLE3 (TOK_ID "op", COLON, op);
        TUPLE3 (TOK_ID "lhs", COLON, lhs);
        TUPLE3 (TOK_ID "rhs", COLON, rhs)]) ->
  SV_AssignExpr { op=cnv_asgn_op op; lhs=cnv_expr lhs; rhs=cnv_expr rhs }
| TUPLE3  (TOK_ID "Expr", TOK_ID "MemberExpr", TLIST
       [TUPLE3 (TOK_ID "expr", COLON, expr);
        TUPLE3 (TOK_ID "name", COLON, TUPLE2 (TOK_ID name, TLIST [TOK_INT _]))]) ->
  SV_MemberExpr { expr=cnv_expr expr; name=(0, name) }
| oth -> unhand := Some oth; failwith "cnv_expr"

and cnv_asgn_op = function
| TOK_ID "Identity" -> SV_Identity
| oth -> unhand := Some oth; failwith "cnv_asgn_op"

and cnv_repeat_opt = function
| TOK_ID "None" -> None
| TUPLE2 (TOK_ID "Some", TLIST [expr]) -> Some (cnv_expr expr)
| oth -> unhand := Some oth; failwith "cnv_repeat_op"

and cnv_call_arg = function
| TUPLE2 (TOK_ID "CallArg", TLIST
            [TUPLE3 (TOK_ID "span", COLON, span);
             TUPLE3 (TOK_ID "name_span", COLON, name_span);
             TUPLE3 (TOK_ID "name", COLON, name);
             TUPLE3 (TOK_ID "expr", COLON, expr)]) -> 
{ span=cnv_span span; name_span=cnv_span name_span; name=cnv_call_name_opt name; expr=cnv_expr_opt expr }

and cnv_expr_opt = function
| TUPLE2 (TOK_ID "Some", TLIST [expr]) -> Some (cnv_expr expr)
| oth -> unhand := Some oth; failwith "cnv_expr_opt"

let cnv_dim = function
| TUPLE2 (TOK_ID "Range", TLIST [lft; rght]) ->
  let (rslt:('a)astTypeDim) = SV_Range (cnv_expr lft, cnv_expr rght) in rslt
| oth -> unhand := Some oth; failwith "cnv_dim"

let cnv_dims dims = Array.of_list (List.map cnv_dim dims)

let cnv_sign = function
| TOK_ID "Signed" -> SV_Signed
| TOK_ID "None" -> SV_None
| oth -> unhand := Some oth; failwith "cnv_sign"

let dump_port = function
| TUPLE3
     (TOK_ID "Port", TOK_ID "Named", TLIST
       [TUPLE3 (TOK_ID "dir", COLON, dir);
        TUPLE3 (TOK_ID "kind", COLON, TOK_ID "None");
        TUPLE3  (TOK_ID "ty", COLON, TUPLE3 (TOK_ID "Type", TOK_ID "TypeData",
          TLIST
           [TUPLE3 (TOK_ID "kind", COLON, TUPLE2 (TOK_ID "TypeKind", TOK_ID ("RegType"|"ImplicitType")));
            TUPLE3 (TOK_ID "sign", COLON, sign);
            TUPLE3 (TOK_ID "dims", COLON, TLIST dims)]));
        TUPLE3 (TOK_ID "name", COLON, TUPLE2(TOK_ID nam, TLIST [TOK_INT _]));
        TUPLE3 (TOK_ID "dims", COLON, TLIST dims');
        TUPLE3 (TOK_ID "expr", COLON, TOK_ID "None")]) ->
  let ty:('a)astType = {kind = SV_LogicType; sign = cnv_sign sign; dims = cnv_dims dims} in
  let rslt:('a)astPort = SV_Named {dir = cnv_dir_opt dir; kind = None; ty = ty; name = (0,nam); dims = cnv_dims dims'; expr = None} in
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
| TUPLE2 (TOK_ID "Default", TLIST [stmtkind]) ->
  let itm:('a)astCaseItem = SV_Default (dump_stmt_kind stmtkind) in itm
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
| TUPLE2 (TOK_ID "ExprStmt", TLIST [expr]) -> SV_ExprStmt (cnv_expr expr)
| TUPLE2 (TOK_ID "ForStmt", TLIST [arg1;arg2;arg3;arg4]) -> SV_ForStmt (dump_stmt_kind arg1, cnv_expr arg2, cnv_expr arg3, dump_stmt_kind arg4)
| oth -> unhand := Some oth; failwith "dump_stmt"

and dump_ev = function
| TUPLE2 (TOK_ID "Event", TLIST lst) -> SV_Event (dump_ev_ctrl lst)
| oth -> unhand := Some oth; failwith "dump_ev"

and dump_ev_ctrl = function
| TUPLE2 (TOK_ID "EventControl", TLIST [TUPLE3 (TOK_ID "span", COLON, span); TUPLE3 (TOK_ID "data", COLON, TUPLE2 (TOK_ID "Expr", TLIST (evlst :: [])))]) :: [] ->
  { span=cnv_span span; data=SV_Expr (dump_ev_expr evlst) }
| TUPLE2 (TOK_ID "EventControl", TLIST [TUPLE3 (TOK_ID "span", COLON, span); TUPLE3 (TOK_ID "data", COLON, TOK_ID "Implicit")]) :: [] ->
  { span=cnv_span span; data=SV_Implicit }
| oth -> unhandlst := oth; failwith "dump_ev_ctrl"

and cnv_edge = function
| TOK_ID "Posedge" -> SV_Posedge
| TOK_ID "Negedge" -> SV_Negedge
| oth -> unhand := Some oth; failwith "cnv_edge"

and dump_ev_expr = function
| TUPLE2 (TOK_ID "Or", TLIST (TUPLE3 (TOK_ID "span", COLON, TUPLE5 _) :: TUPLE3 (TOK_ID "lhs", COLON, lhs) :: TUPLE3 (TOK_ID "rhs", COLON, rhs) :: [])) ->
  SV_Or  { span=(); lhs=dump_ev_expr lhs; rhs=dump_ev_expr rhs}
| TUPLE2 (TOK_ID "Edge", TLIST [
    TUPLE3 (TOK_ID "span", COLON, TUPLE5 _);
    TUPLE3 (TOK_ID "edge", COLON, edg);
    TUPLE3 (TOK_ID "value", COLON, expr')]) -> SV_Edge { span=(); edge=cnv_edge edg; value=cnv_expr expr' }
| oth -> unhand := Some oth; failwith "dump_ev_expr"

let cnv_proc_kind = function
| TOK_ID "Always" -> SV_Always
| TOK_ID "Initial" -> SV_Initial
| oth -> unhand := Some oth; failwith "cnv_proc_kind"

let dump_proc = function
| TUPLE3 (TOK_ID "Procedure", TOK_ID "ProcedureData", TLIST [
      TUPLE3 (TOK_ID "kind", COLON, kind);
      TUPLE3 (TOK_ID "stmt", COLON, stmt)]) ->
    SV_Procedure
      {kind = cnv_proc_kind kind;
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
| TUPLE2 (TOK_ID "TypeKind", TOK_ID "ImplicitType") -> SV_ImplicitType
| TUPLE2 (TOK_ID "TypeKind", TOK_ID "RegType") -> SV_RegType
| TUPLE2 (TOK_ID "TypeKind", TOK_ID "IntegerType") -> SV_IntegerType
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

let cnv_init = function
| TOK_ID "None" -> None
| TUPLE2 (TOK_ID "Some", TLIST [expr]) -> Some (cnv_expr expr)
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

let cnv_var_kind_opt = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_var_kind_opt"

let dump_portdecl = function
| TUPLE3 (TOK_ID "PortDecl", TOK_ID "PortDeclData", TLIST
           [TUPLE3 (TOK_ID "dir", COLON, dir);
            TUPLE3 (TOK_ID "kind", COLON, kind);
            TUPLE3 (TOK_ID "ty", COLON, ty);
            TUPLE3 (TOK_ID "names", COLON, TLIST namlst)]) -> 
let port:('a)astPortDecl = { dir=cnv_dir dir; kind=cnv_var_kind_opt kind; ty=cnv_ty ty; names=Array.of_list (List.map cnv_names namlst); }
in let itm:('a)astItem = SV_PortDecl port in
itm
| oth -> unhand := Some oth; failwith "dump_portdecl"

let dump_one_cont = function
| TLIST (TUPLE3 (TOK_ID "Expr", TOK_ID "IdentExpr", _) as lhs :: (TUPLE3 (TOK_ID "Expr", _, _) as rhs) :: []) ->
  cnv_expr lhs, cnv_expr rhs
| oth -> unhand := Some oth; failwith "dump_one_cont"

let dump_cont = function
| TUPLE3 (TOK_ID "ContAssign", TOK_ID "ContAssignData", TLIST
           [TUPLE3 (TOK_ID "strength", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "delay", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "delay_control", COLON, TOK_ID "None");
            TUPLE3 (TOK_ID "assignments", COLON, TLIST alst)]) ->
SV_ContAssign { strength=None; delay=None; delay_control=None; assignments=Array.of_list (List.map dump_one_cont alst); }
| oth -> unhand := Some oth; failwith "dump_cont"

let cnv_retty = function
| TOK_ID "None" -> None

let cnv_subr_kind = function
| TOK_ID "Task" -> SV_Task
| TOK_ID "Func" -> SV_Func

let cnv_subr_port = function
| oth -> unhand := Some oth; failwith "cnv_subr_port"

let cnv_subr_items = function
| TUPLE2 (TOK_ID "Stmt", TLIST [x]) -> SV_Stmt (dump_stmt_kind x)
| oth -> unhand := Some oth; failwith "cnv_subr_items"

let dump_subr = function
| TUPLE3 (TOK_ID "SubroutineDecl", TOK_ID "SubroutineDeclData", TLIST
           [TUPLE3 (TOK_ID "prototype", COLON, TUPLE3 (TOK_ID "SubroutinePrototype", TOK_ID "SubroutinePrototypeData", TLIST
                 [TUPLE3 (TOK_ID "kind", COLON, kind);
                  TUPLE3 (TOK_ID "lifetime", COLON, life);
                  TUPLE3 (TOK_ID "name", COLON, TUPLE2 (TOK_ID name, TLIST [TOK_INT _]));
                  TUPLE3 (TOK_ID "args", COLON, TLIST args);
                  TUPLE3 (TOK_ID "retty", COLON, retty)]));
            TUPLE3 (TOK_ID "items", COLON, TLIST itmlst)]) ->
let subr:('a)astSubroutineDecl = 
  { prototype = { kind=cnv_subr_kind kind; lifetime=cnv_life_opt life; name=name; args=Array.of_list (List.map cnv_subr_port args); retty=cnv_retty retty; };
  items=Array.of_list (List.map cnv_subr_items itmlst) } in
let itm:('a)astItem = SV_SubroutineDecl subr in
itm
| oth -> unhand := Some oth; failwith "dump_subr"

let cnv_param_name = function
| TUPLE2 (TOK_ID nam, TLIST [TOK_INT _]) -> nam
| oth -> unhand := Some oth; failwith "cnv_param_name"

let dump_param_decl = function
| TUPLE3 (TOK_ID "ParamValueDecl", TOK_ID "ParamValueDeclData", TLIST
             [TUPLE3 (TOK_ID "ty", COLON, ty);
              TUPLE3 (TOK_ID "name", COLON, name);
              TUPLE3 (TOK_ID "dims", COLON, TLIST dims);
              TUPLE3 (TOK_ID "expr", COLON, expr)]) ->
  { ty=cnv_ty ty; name=cnv_param_name name; dims=cnv_dims dims; expr=cnv_expr_opt expr; }
| oth -> unhand := Some oth; failwith "dump_param_decl"

let dump_param_kind = function
| TUPLE2 (TOK_ID "Value", TLIST [TLIST decls]) ->
  SV_Value (Array.of_list (List.map dump_param_decl decls))
| oth -> unhand := Some oth; failwith "dump_param_kind"

let dump_param = function
| TUPLE3 (TOK_ID "ParamDecl", TOK_ID "ParamDeclData", TLIST
           [TUPLE3 (TOK_ID "local", COLON, TOK_ID local);
            TUPLE3 (TOK_ID "kind", COLON, kind)]) ->
let param:('a)astParamDecl = { local=bool_of_string local; kind=dump_param_kind kind; } in
let itm:('a)astItem = SV_ParamDecl param in
itm
| oth -> unhand := Some oth; failwith "dump_param"

let cnv_net_type = function
| TOK_ID "Wire" -> SV_Wire
| oth -> unhand := Some oth; failwith "cnv_net_type"

let cnv_net_str_opt = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_net_type_opt"

let cnv_net_kind = function
| TOK_ID "None" -> let kind:astNetKind = SV_None in kind
| oth -> unhand := Some oth; failwith "cnv_net_kind_opt"

let cnv_net_names = function
| TUPLE3 (TOK_ID "VarDeclName", TOK_ID "VarDeclNameData", TLIST
       [TUPLE3 (TOK_ID "name", COLON, TUPLE2 (TOK_ID name, TLIST [TOK_INT _]));
        TUPLE3 (TOK_ID "name_span", COLON, name_span);
        TUPLE3 (TOK_ID "dims", COLON, TLIST dims);
        TUPLE3 (TOK_ID "init", COLON, init)]) ->
 { name=name; name_span=cnv_span name_span; dims=cnv_dims dims; init=cnv_init init }
| oth -> unhand := Some oth; failwith "cnv_net_names"

let dump_net = function
| TUPLE3 (TOK_ID "NetDecl", TOK_ID "NetDeclData", TLIST
           [TUPLE3 (TOK_ID "net_type", COLON, typ);
            TUPLE3 (TOK_ID "strength", COLON, str);
            TUPLE3 (TOK_ID "kind", COLON, kind);
            TUPLE3 (TOK_ID "ty", COLON, ty);
            TUPLE3 (TOK_ID "delay", COLON, dly);
            TUPLE3 (TOK_ID "names", COLON, TLIST nets)]) ->
SV_NetDecl { net_type=cnv_net_type typ;
             strength=cnv_net_str_opt str;
             kind=cnv_net_kind kind;
             ty=cnv_ty ty;
             delay=cnv_delay dly;
             names=Array.of_list (List.map cnv_net_names nets) }
| oth -> unhand := Some oth; failwith "dump_net"

let cnv_gen_label_opt = function
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_gen_label_opt"

let cnv_id_name = function
| TUPLE2 (TOK_ID nam, TLIST [TOK_INT _]) -> nam
| oth -> unhand := Some oth; failwith "cnv_id_name"

let cnv_id = function
| TUPLE2 (TOK_ID "Identifier", TLIST
            [TUPLE3 (TOK_ID "span", COLON, span);
             TUPLE3 (TOK_ID "name", COLON, name)]) ->
 let id:astIdentifier = { span=cnv_span span; name=cnv_id_name name } in
 id
| oth -> unhand := Some oth; failwith "cnv_id"

let cnv_id_opt = function
| TUPLE2 (TOK_ID "Some", TLIST [id]) -> Some (cnv_id id)

let cnv_typ_or_expr = function
| TUPLE2 (TOK_ID "Expr", TLIST [expr]) ->
  let expr:('asta)astTypeOrExpr = SV_Expr (cnv_expr expr) in
  expr
| oth -> unhand := Some oth; failwith "cnv_typ_or_expr"

let cnv_inst_params = function
| TUPLE2 (TOK_ID "ParamAssignment", TLIST
       [TUPLE3 (TOK_ID "span", COLON, span);
        TUPLE3 (TOK_ID "name", COLON, name);
        TUPLE3 (TOK_ID "expr", COLON, expr)]) ->
  let asgn:('asta)astParamAssignment = {span=cnv_span span; name=cnv_id_opt name; expr=cnv_typ_or_expr expr } in
  asgn
| oth -> unhand := Some oth; failwith "cnv_inst_params"

let dump_inst_conn = function
| TUPLE3 (TOK_ID "PortConn", TOK_ID "Named", TLIST
            [TUPLE2 (TOK_ID nam, TLIST [TOK_INT _]);
             TUPLE2 (TOK_ID "Connected", TLIST [expr])]) ->
  let conn:('a)astPortConn = SV_Named (nam, SV_Connected (cnv_expr expr)) in
  conn
| oth -> unhand := Some oth; failwith "dump_inst_conn"

let dump_inst_name = function
| TUPLE3 (TOK_ID "InstName", TOK_ID "InstNameData", TLIST
            [TUPLE3 (TOK_ID "name", COLON, TUPLE2 (TOK_ID name, TLIST [TOK_INT _]));
             TUPLE3 (TOK_ID "dims", COLON, TLIST dims);
             TUPLE3 (TOK_ID "conns", COLON, TLIST conns)]) ->
{ name=name; dims=cnv_dims dims; conns=Array.of_list (List.map dump_inst_conn conns) }

| oth -> unhand := Some oth; failwith "dump_inst_name"

let dump_inst = function
| TUPLE3 (TOK_ID "Inst", TOK_ID "InstData", TLIST
           [TUPLE3 (TOK_ID "target", COLON, TUPLE2 (TOK_ID target, TLIST [TOK_INT _]));
            TUPLE3 (TOK_ID "params", COLON, TLIST params);
            TUPLE3 (TOK_ID "names", COLON, TLIST names)]) ->
  SV_Inst {target=target; params=Array.of_list (List.map cnv_inst_params params); names=Array.of_list (List.map dump_inst_name names); }
| oth -> unhand := Some oth; failwith "dump_inst"

let rec dump_item = function
| TUPLE3 (TOK_ID "Item", TOK_ID "Procedure", TLIST [proc]) -> dump_proc proc
| TUPLE3 (TOK_ID "Item", TOK_ID "VarDecl", TLIST [decl]) -> dump_decl decl
| TUPLE3 (TOK_ID "Item", TOK_ID "PortDecl", TLIST [port]) -> dump_portdecl port
| TUPLE3 (TOK_ID "Item", TOK_ID "ContAssign", TLIST [cont]) -> dump_cont cont
| TUPLE3 (TOK_ID "Item", TOK_ID "SubroutineDecl", TLIST [subr]) -> dump_subr subr
| TUPLE3 (TOK_ID "Item", TOK_ID "ParamDecl", TLIST [param]) -> dump_param param
| TUPLE3 (TOK_ID "Item", TOK_ID "NetDecl", TLIST [net]) -> dump_net net
| TUPLE3 (TOK_ID "Item", TOK_ID "GenerateRegion", TLIST itms) -> gen_itms itms
| TUPLE3 (TOK_ID "Item", TOK_ID "GenerateIf", TLIST [itm]) -> dump_gen_if itm
| TUPLE3 (TOK_ID "Item", TOK_ID "Inst", TLIST [itm]) -> dump_inst itm
| oth -> unhand := Some oth; failwith "dump_item"

and gen_itms = function
| TUPLE5(Source, _,_,_,_) as x :: TLIST tl :: [] -> SV_GenerateRegion (cnv_span x, Array.of_list (List.map dump_item tl))
| TUPLE3 (TOK_ID "GenerateIf", TOK_ID "GenerateIfData", TLIST _) as x :: [] -> dump_gen_if x
| oth -> unhandlst := oth; failwith "gen_itms"

and cnv_gen_blk = function
| TUPLE3 (TOK_ID "GenerateBlock", TOK_ID "GenerateBlockData", TLIST
            [TUPLE3 (TOK_ID "label", COLON, label);
             TUPLE3 (TOK_ID "items", COLON, TLIST lst)]) ->
  let blk:('a)astGenerateBlock = { label=cnv_gen_label_opt label; items=Array.of_list (List.map dump_item lst) } in
  blk
| oth -> unhand := Some oth; failwith "cnv_gen_blk"

and cnv_gen_blk_opt = function
| TUPLE2 (TOK_ID "Some", TLIST [blk]) -> Some (cnv_gen_blk blk)
| TOK_ID "None" -> None
| oth -> unhand := Some oth; failwith "cnv_gen_blk_opt"

and dump_gen_if = function
| TUPLE3 (TOK_ID "GenerateIf", TOK_ID "GenerateIfData", TLIST
            [TUPLE3 (TOK_ID "cond", COLON, cond);
             TUPLE3 (TOK_ID "main_block", COLON, main_block);
             TUPLE3 (TOK_ID "else_block", COLON, else_block)]) ->
 SV_GenerateIf { cond=cnv_expr cond; main_block=cnv_gen_blk main_block; else_block=cnv_gen_blk_opt else_block }
| oth -> unhand := Some oth; failwith "dump_gen_if"

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
