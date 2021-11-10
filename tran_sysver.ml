open Source_text_rewrite_types
open Source_text_lex
open Source_text
open Printf
open Token_types_old
open Ast_types_old

let unhand = ref None
let dumps = ref ""

let rec obin w n =
  (if w > 1 then obin (w-1) (n lsr 1) else "")^string_of_int (n land 1)

let vlst = ref []

(*
let dot_extract lft rght =
  match Hashtbl.find_opt lft with
    | Some (Vsu _ | MaybePort(_, Vsu _, _)) ->
      let off,wid,typ = off_width_field lft rght in
      dbgdot := (lft,(rght,off,wid,typ)) :: !dbgdot;
      IdArrayedColon (Id lft, Intgr (off+wid-1), Intgr off)
    | Some oth ->
      dbgfield := Some oth;
      dbgdot := (lft,(rght,0,0,oth)) :: !dbgdot;
      Id (lft^"."^rght)
    | None ->
      dbgdot := (lft,(rght,0,0,Unsigned)) :: !dbgdot;
      print_endline (lft^" not found in types dump_sysver.ml:30");
      Id (lft^"."^rght)

let dot_extract3 lft field subfield =
  match Hashtbl.find_opt lft with
    | Some (Vsu _ | MaybePort(_, Vsu _, _)) ->
      let off,wid,typ = off_width_field lft field in
      let field_lst = match typ with
         | Vsu (Id id, field_lst) -> field_lst
         | oth -> dbgfld := Some oth; failwith "extract3" in
      let off',wid',typ' = search_field off subfield field_lst in
      dbgdot := (lft,(field,off',wid',typ')) :: !dbgdot;
      IdArrayedColon (Id lft, Intgr (off'+wid'-1), Intgr off')
    | Some oth -> dbgfield := Some oth; Id (lft^"."^field^"."^subfield)
    | None -> failwith (lft^" not found in types dump_sysver.ml:43")
*)

let basechar = function
| 2 -> 'b'
| 8 -> 'o'
| 10 -> 'd'
| 16 -> 'x'
| n -> failwith ("basechar: "^string_of_int n^"not supported")

let rec vexpr'' = function
| Tilde expr -> SV_UnaryExpr {op=SV_BitNot; expr=vexpr expr; postfix=false}
| Pling expr -> SV_UnaryExpr {op=SV_LogicNot; expr=vexpr expr; postfix=false}
| TildeAnd expr -> SV_UnaryExpr {op=SV_BitNand; expr=vexpr expr; postfix=false}
| Id s -> SV_IdentExpr (s)
| Expression x -> vexpr x
| Number (b,w,n,s) -> SV_LiteralExpr (SV_BasedInteger (None,false,basechar b,s))
| Intgr n -> SV_LiteralExpr (SV_Number(string_of_int n, None))
| Concat lst -> SV_ConcatExpr ({repeat=None; exprs=Array.of_list (List.map (vexpr ) lst)})
| Add (lhs, rhs) -> SV_BinaryExpr {op=SV_Add; lhs=vexpr lhs; rhs=vexpr rhs}
| Sub (lhs, rhs) -> SV_BinaryExpr {op=SV_Sub; lhs=vexpr lhs; rhs=vexpr rhs}
| Mult (lhs, rhs) -> SV_BinaryExpr {op=SV_Mul; lhs=vexpr lhs; rhs=vexpr rhs}
| StarStar (lhs, rhs) -> SV_BinaryExpr {op=SV_Pow; lhs=vexpr lhs; rhs=vexpr rhs}
| Div (lhs, rhs) -> SV_BinaryExpr {op=SV_Div; lhs=vexpr lhs; rhs=vexpr rhs}
| UMinus (rhs) -> SV_BinaryExpr {op=SV_Sub; lhs=vexpr (Intgr 0); rhs=vexpr rhs}
| LtEq (lhs, rhs) -> SV_BinaryExpr {op=SV_Leq; lhs=vexpr lhs; rhs=vexpr rhs}
| Equals (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicEq; lhs=vexpr lhs; rhs=vexpr rhs}
| NotEq (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicNeq; lhs=vexpr lhs; rhs=vexpr rhs}
| GtEq (lhs, rhs) -> SV_BinaryExpr {op=SV_Geq; lhs=vexpr lhs; rhs=vexpr rhs}
| Less (lhs, rhs) -> SV_BinaryExpr {op=SV_Lt; lhs=vexpr lhs; rhs=vexpr rhs}
| Greater (lhs, rhs) -> SV_BinaryExpr {op=SV_Gt; lhs=vexpr lhs; rhs=vexpr rhs}
| Or (lhs, rhs) -> SV_BinaryExpr {op=SV_BitOr; lhs=vexpr lhs; rhs=vexpr rhs}
| Or2 (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicOr; lhs=vexpr lhs; rhs=vexpr rhs}
| Xor (lhs, rhs) -> SV_BinaryExpr {op=SV_BitXor; lhs=vexpr lhs; rhs=vexpr rhs}
| And (lhs, rhs) -> SV_BinaryExpr {op=SV_BitAnd; lhs=vexpr lhs; rhs=vexpr rhs}
| And2 (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicAnd; lhs=vexpr lhs; rhs=vexpr rhs}
| Unsigned expr -> SV_CallExpr (SV_SysIdentExpr "$unsigned", [|{span=();name_span=();name=None;expr=Some (vexpr expr)}|])
| Signed expr -> SV_CallExpr (SV_SysIdentExpr "$signed", [|{span=();name_span=();name=None;expr=Some (vexpr expr)}|])
| Shiftl (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicShL; lhs=vexpr lhs; rhs=vexpr rhs}
| Shiftr (lhs, rhs) -> SV_BinaryExpr {op=SV_LogicShR; lhs=vexpr lhs; rhs=vexpr rhs}
| Shiftr3 (lhs, rhs) -> SV_BinaryExpr {op=SV_ArithShR; lhs=vexpr lhs; rhs=vexpr rhs}
(*
| CellPinItem2 (port, expr) -> SV_Named(port, SV_Connected(vexpr expr))
| CellPinItemImplied (port) -> SV_Named(port, SV_Auto)
| CellPinItemNC (port) -> SV_Named(port, SV_Unconnected)
| Deflt -> "()"
| Atom ".*" -> "" (* placeholder *)
*)
| Query (cond', ctrue', cfalse') -> SV_TernaryExpr {cond=vexpr cond'; true_expr=vexpr ctrue'; false_expr=vexpr cfalse'}
| RedOr expr -> SV_UnaryExpr {op=SV_BitOr; expr=vexpr expr; postfix=false}
| RedAnd expr -> SV_UnaryExpr {op=SV_BitAnd; expr=vexpr expr; postfix=false}
| RedXor expr -> SV_UnaryExpr {op=SV_BitXor; expr=vexpr expr; postfix=false}
| TildeOr expr -> SV_UnaryExpr {op=SV_BitNor; expr=vexpr expr; postfix=false}
| IdArrayed2 (id, ix) -> SV_IndexExpr {indexee=vexpr id; index=vexpr ix}
| IdArrayed3 (PackageBody (pkg, []) :: [], arr) -> SV_ScopeExpr (vexpr arr, pkg)
| IdArrayedColon (id, expr, expr') -> SV_RangeExpr {mode=SV_Absolute; lhs=vexpr expr; rhs=vexpr expr'}
| IdArrayedPlusColon (id, expr, expr') -> SV_DummyExpr
| IdArrayedHyphenColon (id, expr, expr') -> SV_DummyExpr
| FunRef (fn, arglst) -> SV_DummyExpr
| FunRef2 (fn, _, arglst) -> SV_DummyExpr
| AsgnPat lst -> SV_DummyExpr
| Repl (expr', [expr]) -> SV_DummyExpr
| InsideRange (first, last) -> SV_DummyExpr
| OpenRange lst -> SV_DummyExpr
| ExprOKL lst -> SV_DummyExpr
| PackageBody (pkg, id :: []) -> SV_ScopeExpr (vexpr id, pkg)
| Sys (sys_id, expr) -> SV_CallExpr (SV_SysIdentExpr sys_id, [|{span=();name_span=();name=None;expr=Some (vexpr expr)}|])
| Sys ("$bits", expr) -> SV_BitsExpr {name=(0, ""); arg=SV_Expr(vexpr expr)}
| Sys (sys_id, arglst) -> let args = Array.of_list(match arglst with
        | Itmlst lst -> List.map (fun itm -> let rslt:('a)astCallArg = {span=();name_span=();name=None;expr=Some (vexpr itm)} in rslt) lst
	| oth -> {span=();name_span=();name=None;expr=Some (vexpr oth)} :: []) in
  SV_CallExpr (SV_SysIdentExpr sys_id, args)
| Typ3 (id_t, [PackageBody (pkg, [])]) -> SV_ScopeExpr (SV_IdentExpr id_t, pkg)
| PackageBody (pkg, []) -> SV_ScopeExpr (SV_DummyExpr, pkg)
| Dot1(lft, Id rght) -> SV_MemberExpr {expr=vexpr lft; name=(0, rght)}
| PackageRef (pkg_id, id) -> SV_ScopeExpr (SV_IdentExpr pkg_id, id)
(*
| Atom kind -> kind (* placeholder *)
| Typ1 id_t -> id_t
| PatMember1 (Id id, expr) -> id ^ " == " ^ vexpr expr
| PatMemberDflt expr -> vexpr expr
| ValueRange (lft, rght) -> "["^vexpr lft^" .. "^vexpr rght^"]"
| String s -> s
| AtStar -> "*"
| Dot1 (FunRef2 _, _) -> "FunRef2.x" (* placeholder *)
| Itmlst _ -> "Itmlst ..." (* placeholder *)
| DeclAsgn (Id id, (AnyRange _ :: _ as dims)) -> "DeclAsgn ("^id^", ...)"
| InitPat _ -> failwith "InitPat";
*)
| oth -> unhand := Some oth; failwith "vexpr tran_sysver.ml:132"

and cexpr' = function
    | Id s -> s
    | Intgr n -> string_of_int n
    | Number (_,_,n,_) -> string_of_int n
    | Add (lhs, rhs) -> cexpr' lhs ^ "+" ^ cexpr' rhs
    | Sub (lhs, rhs) -> cexpr' lhs ^ "-" ^ cexpr' rhs
    | Mult (lhs, rhs) -> cexpr' lhs ^ "*" ^ cexpr' rhs
    | Div (lhs, rhs) -> cexpr' lhs ^ "/" ^ cexpr' rhs
    | Or (lhs, rhs) -> cexpr' lhs ^ " | " ^ cexpr' rhs
    | Or2 (lhs, rhs) -> cexpr' lhs ^ " || " ^ cexpr' rhs
    | Equals (lhs, rhs) -> cexpr' lhs ^ " == " ^ cexpr' rhs
    | Query (lhs, rhs, rhs') -> cexpr' lhs ^ " ? " ^ cexpr' rhs ^ " : " ^ cexpr' rhs
    | Greater (lhs, rhs) -> cexpr' lhs ^ " > " ^ cexpr' rhs
    | LtEq (lhs, rhs) -> cexpr' lhs ^ " <= " ^ cexpr' rhs
    | Less (lhs, rhs) -> cexpr' lhs ^ " < " ^ cexpr' rhs
    | Shiftl (lhs, rhs) -> cexpr' lhs ^ " << " ^ cexpr' rhs
    | Shiftr (lhs, rhs) -> cexpr' lhs ^ " >> " ^ cexpr' rhs
    | Tilde rhs -> "~"  ^ cexpr' rhs
    | Sys (sysnam, _) -> "1" (* placeholder *)
    | Dot1 (port, conn) -> (cexpr' port) ^ " => " ^ (cexpr' conn)
    | Expression x -> cexpr' x
    | StarStar (lhs, rhs) -> cexpr' lhs ^ "+" ^ cexpr' rhs
    | PackageBody (pkg, [Id id]) -> pkg^"::"^id
    | Concat lst -> "{" ^ String.concat ", " (List.map (cexpr') lst) ^ "}"
    | ExprOKL lst -> "{" ^ String.concat ", " (List.map (cexpr') lst) ^ "}"
    | Repl (expr, expr' :: []) -> "{{"^cexpr' expr^"} {"^cexpr' expr^"}}"
    | FunRef2 (fid, [PackageBody (pkg, [])], arglst) -> pkg^"::"^fid^"("^String.concat ", " (List.map (cexpr') arglst)^")"
    | ExprQuote1 (Atom typ, arg) -> "("^typ^")"^cexpr' arg
    | oth -> unhand := Some oth; failwith "cexpr"

and simplify = function
| Add (Intgr lhs, Intgr rhs) -> Intgr (lhs + rhs)
| Add (lhs, Intgr 0) -> lhs
| Add (Intgr 0, rhs) -> rhs
| Sub (Intgr lhs, Intgr rhs) -> Intgr (lhs - rhs)
| Sub (lhs, Intgr 0) -> lhs
| And (Intgr lhs, Intgr rhs) -> Intgr (lhs land rhs)
| Or (Intgr lhs, Intgr rhs) -> Intgr (lhs lor rhs)
| Xor (Intgr lhs, Intgr rhs) -> Intgr (lhs lxor rhs)
| Shiftl (Intgr lhs, Intgr rhs) -> Intgr (lhs lsl rhs)
| Add (lhs, ExprQuote1 (typ, rhs)) -> Add (simplify lhs, simplify rhs)
| Add (lhs, rhs) -> Add (simplify lhs, simplify rhs)
| Sub (lhs, rhs) -> Sub (simplify lhs, simplify rhs)
| And (lhs, rhs) -> And (simplify lhs, simplify rhs)
| Or (lhs, rhs) -> Or (simplify lhs, simplify rhs)
| Xor (lhs, rhs) -> Xor (simplify lhs, simplify rhs)
| Shiftl (lhs, rhs) -> Shiftl (simplify lhs, simplify rhs)
| Query (cond, lft, rght) -> Query(simplify cond, simplify lft, simplify rght)
| Expression (Intgr _ as x) -> x
| Expression (Shiftl (lhs, rhs)) -> Expression (simplify (Shiftl (simplify lhs, simplify rhs)))
| oth -> oth

and simplify'' x = 
  let rslt1 = ref (simplify (simplify (simplify (simplify (match x with Expression x -> x | _ -> x))))) in
  let rslt2 = ref (simplify (simplify (simplify (simplify !rslt1)))) in
  while !rslt1 <> !rslt2 do
    rslt1 := simplify (simplify (simplify (simplify !rslt2)));
    rslt2 := simplify (simplify (simplify (simplify !rslt1)));
  done;
  !rslt2

and vexpr x =
  let x' = match simplify'' x with
    | ExprQuote1 (typ, x) -> x
    | oth -> oth in
  vlst := (x,x') :: !vlst;  
  let s = vexpr'' x' in
  s

let asgn expr lhs =
  {strength=None; delay=None; delay_control=None; assignments=[| (vexpr lhs, (vexpr expr)) |]}

(*
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval x in
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         : type := logic [%s : %s]" nam (vexpr lft) (vexpr rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         : type := %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         : type := %s.%s" nam lft rght
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      sprintf "%24s         : integer := %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
*)

let cnv_dim = function
| AnyRange (lft, rght) -> 
  let (rslt:('a)astTypeDim) = SV_Range (vexpr lft, vexpr rght) in rslt
| InstRange (lft, rght) -> 
  let (rslt:('a)astTypeDim) = SV_Range (vexpr lft, vexpr rght) in rslt
| oth -> unhand := Some oth; failwith "cnv_dim"

let cnv_dims lst = Array.of_list (List.map (cnv_dim ) lst)

let map_typ = function
| Intgr _ -> {kind=SV_IntType; sign=SV_None; dims=[||]}
| Number (b, w, n, _) -> {kind=SV_LogicType; sign=SV_None; dims=cnv_dims [AnyRange(Intgr (w-1), Intgr 0)]}
| oth -> unhand := Some oth; failwith "parm_generic"

(*
  | PackageParam2 (id_t, nam, [], Id s) ->
      sprintf "%24s         => %s" id_t s
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      sprintf "%24s         => %s" nam id
  | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_generic inner | _ -> "") lst)
*)

let rec parm_generic = function
  | Param (nam, (Intgr _ | Number _ as x), []) ->
    let (rslt:('a)astParamDecl) = {local=false; kind=SV_Value [| { ty=map_typ x; name=nam; dims=[||]; expr=Some (vexpr x)} |] } in rslt
  | TypParam (nam, (Atom typ as x), []) ->
    let typ':('a)astParamTypeDecl = { name=nam; ty=Some (map_typ x); } in
    let (rslt:('a)astParamDecl) = {local=false; kind=SV_Type ([| typ' |]) } in rslt
(*
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      sprintf "%24s         => %s" nam id_t
  | TypParam (nam, Atom typ, AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %s[%s : %s]" nam typ (vexpr lft) (vexpr rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], expr :: []), []) ->
      sprintf "%24s         => %s" nam (vexpr expr)
  | Param (nam, String s, []) ->
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      sprintf "%24s         => %s.%s" nam (vexpr lft) (vexpr rght)
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _|Equals _|Query _) as x), []) ->
      let n = ceval x in
      sprintf "%24s         => %d" nam n
*)
  | oth -> unhand := Some oth; failwith "parm_generic"

(*
let rec parm_map = function
  | CellParamItem2 (nam, Intgr n) ->
      sprintf "%24s         => %d" nam n
  | CellParamItem2 (nam, Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
   | CellParamItem2 (nam, Typ1 s) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Typ3(id_t, PackageBody (pkg,[]) :: [])) ->
      sprintf "%24s         : type := %s" nam id_t
  | CellParamItem2 (nam, Typ5(Atom "logic", AnyRange(lft,rght) :: [])) ->
      sprintf "%24s         => logic[%s : %s]" nam (vexpr lft) (vexpr rght)
  | CellParamItem2 (nam, PackageBody (s, Id id :: _)) ->
      sprintf "%24s         => %s" nam s
  | CellParamItem2 (nam, Dot1 (Id lft, Id rght)) ->
      sprintf "%24s         => %s.%s" nam lft rght
  | CellParamItem2 (nam, ((Add _|Sub _|Mult _|Div _|Sys _) as x)) ->
      let n = ceval x in
      sprintf "%24s         => %d" nam n
  | CellParamItem3 (nam, Typ1(id_t)) ->
      sprintf "%24s         => %s" nam id_t
  | CellParamItem3 (nam, Typ3(id_t, PackageBody (pkg, []) :: [])) ->
      sprintf "%24s         => %s" nam id_t
  | PackageParam (lst, inner) -> String.concat ", " (List.map (function PkgImport (Itmlst [PkgImportItm (pkg, Atom "*")]) -> parm_map inner | _ -> "") lst)
  | PackageParam2 (grp_e, nam, [PackageBody (pkg, [])], PackageBody (pkg', [Id s])) ->
      sprintf "%24s         => %s" grp_e s
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], Number (_, _, n, _)) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (Number (_, _, n, ""))]) ->
      sprintf "%24s         => %d" nam n
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], AsgnPat [PatMemberDflt (PackageBody (pkg', [Id id]))]) ->
      sprintf "%24s         => %s" nam id
  | PackageParam2 (id_t, nam, [PackageBody (pkg, [])], ExprQuote1 (Typ3(id, PackageBody (pkg', []) :: []), expr)) ->
      sprintf "%24s         => %s" nam id
  | TypParam (nam, Atom typ, []) ->
      sprintf "%24s         => %s" nam typ
  | TypParam (nam, Id id_t, PackageBody (pkg, []) :: []) ->
      sprintf "%24s         => %s" nam id_t
  | Param (nam, Number (_, _, n, _), []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, String s, []) ->
      sprintf "%24s         => %s" nam s
  | Param (nam, Dot1(lft,rght), []) ->
      sprintf "%24s         => %s.%s" nam (vexpr lft) (vexpr rght)
  | Param (nam, PackageBody (pkg, [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, FunRef2 (fn, [PackageBody (pkg, [])], [Id id]), []) ->
      sprintf "%24s         => %s" nam id
  | Param (nam, Number (_, _, n, _), AnyRange (left, rght) :: []) ->
      sprintf "%24s         => %d" nam n
  | Param (nam, UMinus (Number (_, _, n, _)), []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, UMinus (Number (_, _, n, _)), AnyRange (lft, rght) :: []) ->
      sprintf "%24s         => %d" nam (-n)
  | Param (nam, ((Add _|Sub _|Mult _|Div _|StarStar _ |Sys _) as x), []) ->
      let n = ceval x in
      sprintf "%24s         => %d" nam n
  | oth -> unhand := Some oth; failwith "parm_map"

let parm_dump_template parm_lst = 
  if parm_lst <> [] then
  fprintf "    generic (\n%s\n    ); // 588\n" (String.concat ";\n" (List.map (parm_generic) parm_lst))

let decl_mem first last hi lo cnt mem =
    fprintf "logic [%s : %s] %s [%s : %s]; // 591\n" (vexpr last) (vexpr first) mem (vexpr hi) (vexpr lo)

let dump_enum front lft rght elst back =
        fprintf "%senum logic [%s:%s] {\n\t%s\n} %s // 326\n" front (vexpr lft) (vexpr rght) (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") elst)) back

let rec dump_unpack_typ prefix sep stem = function
  | (nam, (Vsu (id, lst))) -> let stem' = stem^"."^nam in String.concat sep (List.map (dump_unpack_typ prefix sep stem') lst)
  | (nam, ((Unsigned | Signed | Unsigned_vector _ | Signed_vector _ ) as typ')) ->
    prefix ^ " [" ^ string_of_int (csiz' typ') ^ ":0] \\" ^ stem ^ "." ^ nam ^ " "
  | oth -> dbgsu := Some oth; failwith "unpack_typ"

let rec dump_struct_flat memblst stem =
  let sep = ";\n" in
  let sulst = List.flatten (List.map (struct_union) memblst) in
  dbgsulst := (stem,sulst) :: !dbgsulst;
  let cat = String.concat sep (List.map (dump_unpack_typ "logic" sep stem) sulst) in
  fprintf "%s; // 758\n" cat

let rec dump_struct = function
  | SUMember (Typ6 (Atom ("logic"|"bit"|"byte"|"int"|"longint" as kind)), lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 350\n" (atom_width kind - 1) id
      | oth -> unhand := Some oth; failwith "SUMember5") lst
  | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: []), lst) -> List.iter (function
      | Id id -> fprintf "%s [%s:%s] %s; // 353\n" kind  (vexpr lft) (vexpr rght) id;
      | oth -> unhand := Some oth; failwith "SUMember2") lst
  | SUMember (Typ5 (Atom kind, AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), lst) -> List.iter (function
      | Id id -> fprintf "%s [%s:%s] [%s:%s] %s; // 356\n" kind (vexpr lft) (vexpr rght) (vexpr lft') (vexpr rght') id;
      | oth -> unhand := Some oth; failwith "SUMember2") lst
  | SUMember (Typ6 (Typ5 (Atom ("logic" as kind), [AnyRange (lft, rght)])), id_lst) -> List.iter (function
      | Id id -> fprintf "%s [%s:%s] %s; // 359\n" kind  (vexpr lft) (vexpr rght) id;
      | oth -> unhand := Some oth; failwith "SUMember7") id_lst
  | SUMember (Typ5 (TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%s:%s] %s; // 362\n" (vexpr lft) (vexpr rght) id;
      | oth -> unhand := Some oth; failwith "SUMember8") id_lst
  | SUMember (TypEnum6 (id_t, TypEnum3 [AnyRange (lft, rght)], eid_lst), id_lst) ->  List.iter (function
      | Id id -> fprintf "logic [%s:%s] %s; // 365\n" (vexpr lft) (vexpr rght) id;
      | oth -> unhand := Some oth; failwith "SUMember9") id_lst
  | SUMember (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct) lst
      | oth -> unhand := Some oth; failwith "SUMember10") id_lst
  | SUMember (Typ8 (Atom (("byte") as kind), Deflt), id_lst) -> List.iter (function
      | Id id -> fprintf "%s %s; // 371\n" kind id;
      | oth -> unhand := Some oth; failwith "SUMember11") id_lst
  | SUMember (Typ8 (Atom ("int"|"longint" as kind), Atom "unsigned"), id_lst) -> List.iter (function
      | Id id -> fprintf "%s %s; // 374\n" kind  id;
      | oth -> unhand := Some oth; failwith "SUMember12") id_lst
  | SUMember (Typ8 (Atom ("int"|"longint" as kind), Deflt), id_lst) -> List.iter (function
      | Id id -> fprintf "%s %s; // 377\n" kind  id;
      | oth -> unhand := Some oth; failwith "SUMember13") id_lst
  | SUMember (Typ9 (id_t, [AnyRange _ as dim1], Typ5 (Atom "logic", dims)), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (Unsigned_array (mapdims (dim1 :: dims))) - 1) id;
      | oth -> unhand := Some oth; failwith "SUMember14") id_lst
  | SUMember (Typ9 (old_id, (AnyRange _ as dim1 :: []),
                    Typ9 (old_id', (AnyRange (lft', rght') :: []), 
                          Typ5 (TypEnum3 (AnyRange _ :: _ as dims), e_lst))), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (Unsigned_array (mapdims (dim1 :: dims))) - 1) id;
      | oth -> unhand := Some oth; failwith "SUMember15") id_lst
  | SUMember (Typ6 (PackageRef (pkgid, idp)), id_lst) -> dump_struct (SUMember (find_pkg pkgid idp, id_lst))
  | SUMember (Typ3 (old_id, [Typ5 (Atom "logic", [AnyRange (lft, rght)])]), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%s:%s] %s; // 362\n" (vexpr lft) (vexpr rght) id;
      | oth -> unhand := Some oth; failwith "SUMember16") id_lst
  | SUMember (Typ3 (old_id, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)]), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct) lst
      | oth -> unhand := Some oth; failwith "SUMember17") id_lst
  | SUMember (Typ9 (old_id_t, [TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst)], Typ5 (TypEnum3 (AnyRange _ :: _ as dims'), e_lst')), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (Unsigned_array (mapdims (dims @ dims'))) - 1) id;
      | oth -> unhand := Some oth; failwith "SUMember15") id_lst
  | SUMember (Typ9 (old_id, [Typ5 (Atom "logic", [AnyRange (Intgr hi, Intgr lo)])], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (Vsua (hi, lo, List.flatten (List.map (struct_union) lst)))) id;
      | oth -> unhand := Some oth; failwith "SUMember16") id_lst
  | SUMember (Typ8 (Atom ("byte"|"logic" as kind), Atom ("signed"|"unsigned" as signage)), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (atom_cnv_signage (kind, signage))) id;
      | oth -> unhand := Some oth; failwith "SUMember17") id_lst
  | SUMember (Typ12 ([Atom ("reg" as kind)], Atom ("signed"|"unsigned" as signage), AnyRange(hi,lo) :: []), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' (atom_cnv_signage_array hi lo (kind, signage))) id;
      | oth -> unhand := Some oth; failwith "SUMember18") id_lst
  | SUMember (Typ3 (id_t, (AnyRange _ :: _ as dims)), id_lst) ->
    let typ' = match Hashtbl.find_opt id_t with
      | Some (Unsigned_array lst) -> (Unsigned_array (mapdims dims@lst))
      | None -> print_endline ("Missing: "^id_t); Unsigned_vector(Intgr 0, Intgr 0) in
    List.iter (function 
        | Id id -> fprintf "logic [%d:0] %s; // 430\n" (csiz' typ') id;
	| oth -> unhand := Some oth; failwith "SUMember_525") id_lst
  | SUMember (Typ9 (old_id, [AnyRange (lft,rght)], PackageRef (pkgid, new_id)), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%d:0] %s; // 527\n" (abs (ceval rght - ceval lft) + 1) id;
      | oth -> unhand := Some oth; failwith "SUMember_528") id_lst
  | SUMember (Typ3 (old_id, [PackageRef (pkgid, idp)]), id_lst) ->
    let x = match find_pkg pkgid idp with
      | (Typ5 _ | Typ8 _ | TypEnum6 _ as x) -> x
      | oth -> unhand := Some oth; failwith "SUMember_532" in
    dump_struct (SUMember (x, id_lst))
  | SUMember (Typ6 (TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)), id_lst) -> List.iter (function
      | Id id -> fprintf "logic [%s:%s] %s; // 535\n" (vexpr hi) (vexpr lo) id;
      | oth -> unhand := Some oth; failwith "SUMember_536") id_lst
  | SUMember (Typ6 (Typ8 (SUDecl (Atom "packed", lst), Deflt)), id_lst) -> List.iter (function
      | Id id -> List.iter (dump_struct) lst
      | oth -> unhand := Some oth; failwith "SUMember_539") id_lst
  | SUMember (Atom "logic", id_lst) -> List.iter (function
      | Id id -> fprintf "logic %s; // 541\n" id
      | oth -> unhand := Some oth; failwith "SUMember_542") id_lst
  | SUMember (Typ1 id_t, id_lst) -> List.iter (function
      | Id id -> fprintf "logic %s; // 544 %s FIXME\n" id id_t
      | oth -> unhand := Some oth; failwith "SUMember_542") id_lst
  | oth -> unhand := Some oth; failwith "dump_struct"

let dump_dep = function
| Pos (Id signal) -> sprintf "posedge %s" signal
| Neg (Id signal) -> sprintf "negedge %s" signal
| oth -> unhand := Some oth; failwith "dump_dep"
 
let rec dump_deps kind = function
| [] -> ()
| (Pos _ | Neg _) :: _ as lst -> fprintf "%s @(%s)\n" kind (String.concat " or " (List.map dump_dep lst))
| oth -> unhand_lst := oth; failwith "dump_deps"

let rec dump_deps_comb kind lst =
fprintf "%s @(%s)\n" kind (String.concat " or " (List.map (vexpr ) lst))

let dbgiff = ref None
let dbgdmpu = ref None
*)

let dbgasgn = ref None

let cnv_typ dims = function
| Atom "logic" -> { kind=SV_LogicType; sign=SV_None; dims=cnv_dims dims}
| Atom "integer" -> { kind=SV_IntegerType; sign=SV_None; dims=cnv_dims dims}
| PackageRef (pkg, nam) -> { kind=SV_ScopedType { ty= { kind=SV_NamedType(pkg); sign=SV_None; dims=cnv_dims dims}; member=false; name=(0,nam); }; sign=SV_None; dims=cnv_dims dims }

let cnv_logic dims dims' logic nam =
  SV_VarDecl ( { konst=false; var=true; lifetime=None; ty=cnv_typ dims logic; names=[|  |] } )

let rec datatype_template = function
  | Typ8 (Union (Atom ("packed"|"signed" as signage), lst), Deflt) -> SV_StructType {kind=SV_Union; packed=true; signing=SV_None; members=[||]}
  | oth -> unhand := Some oth; failwith "datatype_template"

let rec instance_template id_lst = function
  | Typ8 (Union _, Deflt) as x ->
    let u = datatype_template x in
    SV_VarDecl ({ konst=false; var=true; lifetime=None; ty={kind=u; dims=[||]; sign=SV_None}; names=[|  |] } )
(*
    | InstDecl (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [], inst_lst) -> List.iter (function
        | Id id -> fprintf "logic [%s:%s] %s; // 844\n" (vexpr hi) (vexpr lo) id
        | oth -> unhand := Some oth; failwith "InstDecl829") inst_lst;
    | InstDecl (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt), [], inst_lst) ->
        fprintf "struct packed { // 622\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" (String.concat ", " (List.map (vexpr ) inst_lst));
    | InstDecl (typ, params, lst) as x -> List.iter (function
        | InstNameParen1 (inst, pins) -> instance_dump_template typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst pins
        | InstNameParen2 (inst, InstRange(lft,rght) :: []) -> instance_dump_template typ (match params with Itmlst lst :: _ -> lst | _ -> []) inst []
        | Id id -> unhand := Some x; failwith "InstDecl"
        | oth -> unhand := Some oth; failwith "InstDecl829") lst;
    | Typ5 (Union (Atom "packed", lst), [Id "u"]) when false -> ()
    | Typ5 (Union (Atom ("packed"|"signed" as signage), lst), id_lst) -> List.iter (function
        | Id id ->
        fprintf "union packed { // 718\n";
        dbgunion := lst :: !dbgunion;
        List.iter (dump_union) lst;
        fprintf "} %s;\n" id) id_lst
    | Typ11 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), AnyRange (hi,lo) :: [], inst_lst) -> List.iter (function
        | Id stem -> dump_struct_single stem memblst;
        | oth -> failwith "dump_sysver.ml:916") inst_lst
    | Typ12 ([AnyRange (hi, lo)], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), inst_lst) -> List.iter (function
        | Id stem ->
          let sep = ";\n" in
          let sulst = List.flatten (List.map (struct_union) memblst) in
          dbgsulst := (stem,sulst) :: !dbgsulst;
          let cat = String.concat sep (List.map (dump_unpack_typ "logic" sep stem) sulst) in
          fprintf "%s; // 755\n" cat
        | oth -> failwith "dump_sysver.ml:745") inst_lst
    | Typ12 (AnyRange (hi, lo) :: [], Typ5 (TypEnum3 (AnyRange (hi', lo') :: []), e_lst), inst_lst) -> ()
    | Typ11 (TypEnum6 (old_id, TypEnum3 (AnyRange (lft, rght) :: []), elst), AnyRange (lft', rght') :: [], id_lst) ->
        List.iter (function
            | Id id as nam -> fprintf "logic [%d: 0] %s; // 930\n" (width nam - 1) id
            | oth -> failwith "dump_sysver.ml:931") id_lst
    | NetDecl (Atom "wire" :: [], wire_lst) -> List.iter (function
          | Id nam ->
	      fprintf "logic %s; // 610\n" nam
	  | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf "logic [%s:%s] %s; // 612\n" (vexpr hi) (vexpr lo) (vexpr nam)
          | InitSig (nam, expr) -> (function
	      | Id id -> fprintf "logic %s = %s; // 614\n" (vexpr nam) (vexpr expr)
              | ExprOKL lbls -> fprintf "logic %s : %s; // 615\n" (vexpr nam) (String.concat "; " (List.map (vexpr ) lbls))
	      | SysFuncCall ("$random", [Deflt]) -> fprintf "    signal %s : $random; // 616\n" (vexpr nam)
              | Query _ as x -> fprintf "logic %s : %s; // 617\n" (vexpr nam) (vexpr x)

	      | oth -> unhand := Some oth; failwith "initsig") expr
	  | oth -> unhand := Some oth; failwith "NetDecl'") wire_lst;
    | Itmlst (id_lst) -> List.iter (function
	  | Id nam -> fprintf "logic %s; // 544\n" nam
          | Seq(lbl, lst) -> List.iter (proc_dump_template modules) lst
	  | oth -> unhand := Some oth; failwith "DeclLogic647") id_lst;
    | DeclLogic (reg_lst) -> List.iter (function
	  | Id nam ->  fprintf "logic %s; // 848\n" nam
          | DeclAsgn (nam, AnyRange (hi, lo) :: []) ->
              fprintf "logic [%s : %s] %s; // 628\n" (vexpr hi) (vexpr lo) (vexpr nam)
          | VarDeclAsgn (nam, expr) ->
              fprintf "logic %s = %s\n" (vexpr nam) (vexpr expr)
	  | oth -> unhand := Some oth; failwith "DeclLogic651"
        ) reg_lst;
    | DeclLogic2 (wire_lst, (AnyRange (hi, lo) as x) :: []) -> List.iter (function
	  | Id nam -> fprintf "logic [%s : %s] %s; // 635\n" (vexpr hi) (vexpr lo) (nam)
	  | DeclAsgn (nam, (AnyRange _ :: _ as dims)) ->
              let wid = width nam in
              printf "mem siz = %d\n" wid;
              fprintf "logic [%d : 0] %s ; // 746\n" (wid - 1) (vexpr nam)
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: []) -> List.iter (function
	  | Id nam ->
	  fprintf "logic [%s : %s] [%s : %s] %s ; // 641\n" (vexpr hi) (vexpr lo) (vexpr hi') (vexpr lo') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclLogic2 (wire_lst, AnyRange (hi, lo) :: AnyRange (hi', lo') :: AnyRange (hi'', lo'') :: []) -> List.iter (function
	  | Id nam ->
	  fprintf "logic [%s : %s] [%s : %s] [%s : %s] %s ; // 645\n" (vexpr hi) (vexpr lo) (vexpr hi') (vexpr lo') (vexpr hi'') (vexpr lo'') nam
	  | oth -> unhand := Some oth; failwith "DeclWire") wire_lst
    | DeclReg (reg_lst, [], Deflt) -> List.iter (function
      | Id nam -> fprintf "logic %s; // 535\n" nam
      | oth -> unhand := Some oth; failwith "DeclReg547") reg_lst
    | DeclReg (reg_lst, [AnyRange(hi,lo)], Deflt) -> List.iter (function
      | Id nam -> fprintf "logic [%s:%s] %s; // 538\n" (vexpr hi) (vexpr lo) nam
      | DeclAsgn (Id nam, (AnyRange (first,last) :: [])) ->
        fprintf "logic [%s:%s] %s[%s:%s]; // 550\n" (vexpr hi) (vexpr lo) nam (vexpr first) (vexpr last)
      | VarDeclAsgn (Id nam, expr) -> fprintf "reg [%s:%s] %s = %s;\n" (vexpr hi) (vexpr lo) nam (vexpr expr)
      | oth -> unhand := Some oth; failwith "DeclReg550") reg_lst
    | DeclInt2 id_lst -> List.iter (function
	| Id itm -> fprintf "logic %s; // 672\n" itm
        | VarDeclAsgn (id, expr) -> fprintf "logic %s = %s\n" (vexpr id) (vexpr expr)
        | Intgr _ -> () (* probably a result of loop unrolling and substitution *)
        | oth -> unhand := Some oth; failwith "DeclInt2") id_lst
    | Typ2 (id_t, [Typ8 (SUDecl (Atom "packed", lst), Deflt)], inst_lst) ->
        fprintf "struct packed { // 888\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" (String.concat ", " (List.map (vexpr ) inst_lst));
    | Typ2 (nam, _, id_lst) ->
        List.iter (function
	     | Id _ as itm -> let s = vexpr itm in  fprintf "logic %s : %s; // 684\n" s nam
             | DeclAsgn (id, AnyRange(lft,rght) :: AnyRange(lft',rght') :: []) ->
                fprintf "logic %s; // 686\n" (vexpr id)
             | oth -> unhand := Some oth; failwith "Typ2") id_lst;
    | Typ3 (nam, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ4 (nam, pkg, rng, id_lst) -> List.iter (fun _ -> ()) id_lst
    | Typ9 (orig_id, id_lst, Typ5 (TypEnum3 (AnyRange(lft,rght) :: []), elst)) -> List.iter (function
          | Id id -> fprintf "%s %s;\n" orig_id id
          | oth -> unhand := Some oth; failwith "enum range") id_lst
    | Typ9 (orig_id, id_lst, Typ5 (Deflt, elst)) -> List.iter (function
          | Id id -> fprintf "%s %s;\n" orig_id id
            | oth -> unhand := Some oth; failwith "enum range") id_lst
    | Typ5 (SUDecl (Atom ("packed"|"signed" as signage), lst), inst_lst) -> List.iter (function
        Id id ->
        fprintf "typedef struct packed { // 926\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" id;
        | DeclAsgn (Id id, [AnyRange (lft, rght)]) ->
        fprintf "struct packed { // 930\n";
        List.iter (dump_struct) lst;
        fprintf "} %s[%s:%s]; // 932\n" id (vexpr lft) (vexpr rght);
        | DeclAsgn (Id id, (AnyRange _ :: _ as dims)) ->
        fprintf "struct packed { // 930\n";
        List.iter (dump_struct) lst;
        let dims' = String.concat " " (List.map (fun (hi,lo) -> sprintf "[%s:%s]" (vexpr hi) (vexpr lo)) (mapdims dims)) in
        fprintf "} %s %s;\n" dims' id
        | oth -> unhand := Some oth; failwith "dump_sysver:929") inst_lst
    | Typ5 (Typ5 (Atom "logic", AnyRange(hi,lo) :: []), inst_lst) -> List.iter (function
        | Id id -> fprintf "logic %s; // 619\n" id
                     | oth -> failwith "dump_sysver.ml:615") inst_lst
    | Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), su_lst), inst_lst) ->
        fprintf "struct packed { // 622\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" (String.concat ", " (List.map (vexpr ) inst_lst));
    | Typ9 (old_id, inst_lst, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), su_lst), Deflt)) ->
        fprintf "struct packed { // 917\n";
        List.iter (dump_struct) su_lst;
        fprintf "} %s;\n" (String.concat ", " (List.map (vexpr ) inst_lst));
    | Typ6 (SUDecl (Atom ("packed"|"signed" as signage), lst)) ->
        fprintf "typedef struct packed { // 630\n";
        List.iter (dump_struct) lst;
        fprintf "};\n"
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: AnyRange(lft'',rght'') :: [])) ->
        fprintf "logic %s; // 692\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: AnyRange(lft',rght') :: [])) ->
        fprintf "logic %s; // 694\n" nam
    | Typ7 (nam, Typ5 (Atom "logic", AnyRange(lft,rght) :: [])) ->
        fprintf "typedef logic [%s:%s] %s; // 696\n" (vexpr lft) (vexpr rght) nam
    | Typ7 (nam, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), lst), Deflt)) ->
        fprintf "typedef struct packed { // 646\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" nam
    | Typ7 (id_t, Typ8 (Union (Atom ("packed"|"signed" as signage), lst), Deflt)) ->
        fprintf "typedef union packed { // 718\n";
        dbgunion := lst :: !dbgunion;
        List.iter (dump_union) lst;
        fprintf "} %s;\n" id_t
    | Typ7 (id_t, Typ8 (Itmlst lst, Deflt)) ->
        fprintf "typedef struct { // 722\n";
        List.iter (dump_struct) lst;
        fprintf "} %s;\n" id_t
    | TypEnum4 (TypEnum3 (AnyRange(lft,rght) :: []), id_lst, id_lst') ->
        let kind = Unsigned_vector(lft,rght) in
        let f' itm = let s = vexpr itm in  s in
        let f'' = function Id nam -> fprintf "typedef enum {%s} %s; // 606\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (Deflt, id_lst, id_lst') ->
        let f' itm = let s = vexpr itm in  s in
        let f'' = function Id nam -> fprintf "    typedef enum {%s} %s; // 610\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum4 (TypEnum5 (Atom "logic"), id_lst, id_lst') ->
        let f' itm = let s = vexpr itm in  s in
        let f'' = function Id nam -> fprintf "    typedef enum {%s} %s; // 614\n" (String.concat ", " (List.map f' id_lst)) nam | _ -> () in
        List.iter f'' id_lst'
    | TypEnum6 (nam, TypEnum3 (AnyRange(lft,rght) :: []), id_lst) -> dump_enum "typedef // 670\n" lft rght id_lst (nam^"; ")
    | Typ5 (TypEnum6 (old_id, TypEnum3 (AnyRange(lft,rght) :: []), eid_lst), id_lst) ->
          List.iter (function
            | Id itm ->
(*
              dump_enum "// 671\n" lft rght eid_lst (itm^"; ")
*)
              fprintf "logic [%s:%s] %s; // 880\n" (vexpr lft) (vexpr rght) itm
	    | oth -> unhand := Some oth; failwith "TypEnum6") id_lst
    | TypEnum6 (nam, TypEnum5 (Atom "logic"), id_lst) -> 
        fprintf "typedef enum logic {\n\t%s\n} %s; // 660\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Deflt, id_lst) -> 
        fprintf "typedef enum logic {\n\t%s\n} %s; // 974\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | TypEnum6 (nam, Typ8 (Atom "int", Atom "unsigned"), id_lst) -> 
        fprintf "typedef enum int {\n\t%s\n} %s; // 634\n" (String.concat ",\n\t" (List.map (function
        | Id e -> e
        | EnumInit (e, expr) ->
	    let s = vexpr expr in
	    let s' = sprintf "%s = %s" e s in
	    s'
	| oth -> unhand := Some oth; failwith "TypEnum6") id_lst)) nam
    | Typ6 (Atom ("packed"|"signed" as signage)) -> () (* placeholder *)
    | Typ10 (id_t, AnyRange (lft, rght) :: [], id_t') -> () (* placeholder *)
    | Typ12 (AnyRange (hi, lo) :: [], Typ5 (TypEnum3 (AnyRange (hi', lo') :: []), e_lst), inst_lst) -> List.iter (function
        | Id id -> fprintf "logic %s; // 733\n" id
        | oth -> failwith "dump_sysver.ml:733") inst_lst
    | Typ12 ([AnyRange (hi, lo)], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), inst_lst) -> List.iter (function
(*
        | Id stem -> dump_struct_flat memblst stem;
*)
        | oth -> failwith "dump_sysver.ml:751") inst_lst
    | Typ11 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), AnyRange (hi, lo) :: [], inst_lst) -> List.iter (function
        | Id stem ->
        fprintf "struct packed { // 756\n";
        List.iter (dump_struct) memblst;
        fprintf "} [%s:%s] %s;\n" (vexpr hi) (vexpr lo) (String.concat ", " (List.map (vexpr ) inst_lst))
        | oth -> unhand := Some oth; failwith "dump_sysver.ml:759") inst_lst
    | Typ11 (TypEnum6 (old_id, TypEnum3 (AnyRange (lft, rght) :: []), elst), AnyRange (lft', rght') :: [], id_lst) ->
        List.iter (function
            | Id nam -> dump_enum "" lft rght elst (sprintf "%s [%s:%s]; // 768" nam (vexpr lft') (vexpr rght'))
            | oth -> unhand := Some oth; failwith "dump_sysver.ml:765") id_lst
    | Typ11 (Typ8 (SUDecl (Atom "packed", memblst), Deflt), (AnyRange _ :: _ as dims), inst_lst) ->
        fprintf "struct packed { // 1125\n";
        List.iter (dump_struct) memblst;
        let dims' = String.concat " " (List.map (fun (hi,lo) -> sprintf "[%s:%s]" (vexpr hi) (vexpr lo)) (mapdims dims)) in
        fprintf "} %s %s;\n" dims' (String.concat ", " (List.map (vexpr ) inst_lst))
    | Typ12 ([TypEnum6 (old_id, TypEnum3 [AnyRange (lft, rght)], e_lst)], Typ5 (TypEnum3 [AnyRange (lft', rght')], e_lst'), id_lst) ->
        List.iter (function
            | Id nam -> dump_enum "" lft rght e_lst (sprintf "%s [%s:%s]; // 768" nam (vexpr lft') (vexpr rght'))
            | oth -> unhand := Some oth; failwith "dump_sysver.ml:765") id_lst
    | Typ12 ([TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst)], Typ5 (TypEnum5 (Atom "logic"), e_lst'), inst_lst) -> List.iter (function
            | Id nam -> fprintf "logic %s; // 1147\n" nam
            | oth -> unhand := Some oth; failwith "dump_sysver.ml:1148") inst_lst
    | Typ12 ([Typ8 (SUDecl (Atom "packed", su_lst), Deflt)], Typ8 (SUDecl (Atom "packed", su_lst'), Deflt), inst_lst) -> List.iter (function
            | Id nam -> fprintf "logic %s; // 1151\n" nam
            | oth -> unhand := Some oth; failwith "dump_sysver.ml:1148") inst_lst
    | TypEnum4 _ -> ()
    | TypEnum6 _ -> ()
    | Typ2 (typ, _, typ_lst) -> ()
    | Typ3 _ -> ()
    | Typ4 _ -> ()
(*
    | Typ5 _ -> ()
*)
    | Typ6 _ -> ()
    | Typ7 _ -> ()
    | Typ9 _ -> ()
    | Typ11 (SUDecl (Atom ("packed"|"signed" as signage), su_lst), [AnyRange (hi, lo)], inst_lst) when false -> ()
    | Typ11 (SUDecl (Atom ("packed"|"signed" as signage), memblst), (AnyRange (hi,lo) :: []), inst_lst) -> List.iter (function
        | Id stem -> dump_struct_single stem memblst;
        | DeclAsgn (Id id, [VarDim (Id dim)]) ->  dump_struct_single id memblst;
        | oth -> failwith "dump_sysver.ml:916") inst_lst
    | Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [TypEnum6 (old_id, TypEnum3 [AnyRange (hi', lo')], e_lst')], inst_lst) -> fprintf "// 1197\n"
    | Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [AnyRange (hi', lo')], inst_lst) -> fprintf "// 1198\n"
    | Typ11 (Atom "logic", [AnyRange (lft, rght)], inst_lst) -> fprintf "// 1199\n"
    | Typ11 (Typ5 (TypEnum5 (Atom "logic"), e_lst), [TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst')], inst_lst) -> fprintf "// 1203\n"
    | Typ11 (Typ8 (SUDecl (Atom "packed", su_lst), Deflt), [Typ8 (SUDecl (Atom "packed", su_lst'), Deflt)], inst_lst) -> fprintf "// 1204\n"
*)
  | oth -> unhand := Some oth; failwith "instance_template"

let rec stmt_clause = function
      | If2 (condition, if_lst, else_lst) -> {label=None;kind=SV_IfStmt {up=None;
                                                        cond=vexpr condition;
                                                        main_stmt=stmt_clause if_lst;
                                                        else_stmt=Some (stmt_clause else_lst)}}
      | Itmlst lst -> {label=None;kind=SV_SequentialBlock (Array.of_list (List.map (fun itm -> stmt_clause itm) lst))}
      | BeginBlock lst -> {label=None;kind=SV_SequentialBlock (Array.of_list (List.map (fun itm -> stmt_clause itm) lst))}
      | Seq (id, lst) -> {label=None;kind=SV_SequentialBlock (Array.of_list (List.map (fun itm -> stmt_clause itm) lst))}
      | If1 (condition, if_lst) -> {label=None;kind=SV_IfStmt {up=None;
                                              cond=vexpr condition;
                                              main_stmt=stmt_clause if_lst;
                                              else_stmt=None}}
      | Blocking (Asgn1 (lhs, expr)) -> {label=None;kind=SV_BlockingAssignStmt {lhs=vexpr lhs; rhs=vexpr expr; op=SV_Identity}}
      | Blocking (FopAsgn (lhs, expr)) -> {label=None;kind=SV_BlockingAssignStmt {lhs=vexpr lhs; rhs=vexpr expr; op=SV_Identity}}

(*
      | DeclLogic lst -> ()
      | Blocking (FopAsgn (id, ExprQuote1 _)) -> fprintf "        %s = ExprQuote1; // 489\n" (vexpr id)
      | Blocking (FopAsgn (id, expr)) -> fprintf "        %s = %s; // 779\n" (vexpr id) (vexpr expr)
      | Blocking (FopAsgn1 (id, id', id'', expr)) -> fprintf "        %s = %s; // 780\n" (vexpr id) (vexpr expr)
      | Blocking (FopAsgnArrayMemSel (id, hi, lo, expr)) -> fprintf "        %s[%s : %s] = %s; // 781\n" (vexpr id) (vexpr hi) (vexpr lo) (vexpr expr)
      | Blocking (FopAsgnConcat (idlst, expr)) -> fprintf "        %s = %s; // 782\n" (String.concat ", " (List.map (vexpr ) idlst)) (vexpr expr)
      | Blocking (FopAsgnArraySel (id, ix, expr)) -> fprintf "        %s[%s] = %s; // 783\n" (vexpr id) (vexpr ix) (vexpr expr)
      | Blocking (FopAsgnArrayWid (id, hi, lo, expr)) -> fprintf "        %s[%s : %s] = %s; // 469\n" (vexpr id) (vexpr hi) (vexpr lo) (vexpr expr)
      | Blocking (FopAsgnArrayRange (id, hi, lo, expr)) -> fprintf "        %s[%s : %s] = %s; // 785\n" (vexpr id) (vexpr hi) (vexpr lo) (vexpr expr)
      | Blocking (FopAsgnArrayRange2 (id, ix, ix', expr)) -> fprintf "        %s[%s][%s] = %s; // 786\n" (vexpr id) (vexpr ix) (vexpr ix') (vexpr expr)
      | Blocking (FopAsgnArrayField (id,  ix, expr)) -> fprintf "        %s = %s; // 787\n" (vexpr (Dot1(id, ix))) (vexpr expr)
      | Blocking (FopAsgnArrayField2 (id, IdArrayedColon(Id ix, hi, lo), expr)) ->
          fprintf "        %s.%s[%s : %s] = %s; // 548\n" (vexpr id) ix (vexpr hi) (vexpr lo) (vexpr expr)
      | Blocking (FopAsgnArrayField3 (id, sel, sel', expr)) ->
          fprintf "        %s[%s].%s = %s; // 550\n" (vexpr id) (vexpr sel) (vexpr sel') (vexpr expr)
      | Blocking (FopAsgnArrayField4 (id, sel, id', sel', sel'', expr)) ->
          fprintf "        %s[%s].%s[%s] = %s; // 552\n" (vexpr id) (vexpr sel) (vexpr id') (vexpr sel') (vexpr expr)
      | Blocking (FopAsgnArrayField5 (id, sel, id', sel', expr)) ->
          fprintf "        %s[%s].%s[%s] = %s; // 554\n" (vexpr id) (vexpr sel) (vexpr id') (vexpr sel') (vexpr expr)
      | Blocking (FopAsgnArrayField6 (id, sel, sel', id', expr)) ->
          fprintf "        %s[%s][%s].%s = %s; // 556\n" (vexpr id) (vexpr sel) (vexpr sel') (vexpr id') (vexpr expr)
      | Blocking (FopAsgnArrayField7 (id, sel, sel', id', expr)) ->
          fprintf "        %s[%s][%s].%s = %s; // 558\n" (vexpr id) (vexpr sel) (vexpr sel') (vexpr id') (vexpr expr)
      | Blocking (FopAsgnArrayField8 (id, sel, id', id'', expr)) ->
          fprintf "        %s[%s][%s].%s = %s; // 560\n" (vexpr id) (vexpr sel) (vexpr id') (vexpr id'') (vexpr expr)
      | Blocking (FopAsgnArrayField9 (id, arr, id', id'', id3, expr)) ->
          fprintf "        %s.%s[%s][%s].%s = %s; // 562\n" (vexpr id) (vexpr arr) (vexpr id') (vexpr id'') (vexpr id3) (vexpr expr)

      | ForEach (ix, lst) -> astConstraintItem {span=(); data=SV_Foreach}

      | ForLoop (Asgn1 (Id ix, strt) :: [], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf "            for %s; // 579\n" ix;
          stmt_clause seq;
      | ForLoop (Asgn1 (Id ix, strt) :: [], LtEq (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, 1, _))), seq) ->
          fprintf "            for %s; // 582\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), Asgn1 (Id ix'', Add (Id ix''', Number (_, _, inc, _))), seq) ->
          fprintf "            for %s; // 585\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf "            for %s; // 588\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ7 (ix, Atom ("int"|"unsigned_int"))], GtEq (Id ix', limit), SideEffect (Id xi'', Atom "--"), seq) ->
          fprintf "            for %s; // 591\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ9 (ix, AnyRange(hi,lo) :: [], Atom ("logic"))], Less (Id ix', limit), SideEffect (Id xi'', Atom "++"), seq) ->
          fprintf "            for %s; // 594\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ9 (ix, [Atom "unsigned"; Atom "int"], strt)], Less (Id ix', stop), SideEffect (Id ix'', Atom "++"), seq) ->
          fprintf "            for %s; // 597\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ9 (ix, [Atom "int"], strt)], Less (Id ix', stop), Asgn1 (Id ix'', Add (Id ix3, inc)), seq) ->
          fprintf "            for %s; // 601\n" ix;
          stmt_clause seq;
      | ForLoop ([Typ9 (ix, [Atom "int"], strt)], Less (Id ix', stop), SideEffect (Id ix'', Atom "++"), seq) ->
          fprintf "            for %s; // 604\n" ix;
          stmt_clause seq;
      | Equate (id,AsgnPat exprlst) ->
        dbgeq := Some (id, exprlst);
      | Equate (id,expr) ->
        fprintf "            %s <= %s; // 586\n" (vexpr id) (vexpr expr);
      | EquateSlice (id,hi,lo,expr) -> fprintf "            %s[%s : %s] <= %s; // 587\n" (vexpr id) (vexpr hi) (vexpr lo) (vexpr expr);
      | EquateSelect (id,ix,expr) -> fprintf "            %s[%s] <= %s; // 588\n" (vexpr id) (vexpr ix) (vexpr expr);
      | EquateSelect2 (id,ix,expr) -> fprintf "            %s[%s] <= %s; // 589\n" (vexpr id) (vexpr ix) (vexpr expr);
      | EquateArrayField (id,id',ix,ix',expr) -> fprintf "            %s.%s[%s][%s] <= %s; // 590\n" (vexpr id) (vexpr id') (vexpr ix) (vexpr ix') (vexpr expr);
      | CaseStart (CaseStart1 (sel), lst) ->
        fprintf "case (%s)\n" (vexpr sel);
        List.iter (case_clause) lst;
        fprintf "endcase; // 825\n";
      | CaseStartInside (sel, lst) ->
        fprintf "case (%s) inside\n" (vexpr sel);
        List.iter (case_clause) lst;
        fprintf "endcase; // 829\n";
      | CaseStartUniq (CaseStart1 (sel), lst) ->
        fprintf "unique case (%s)\n" (vexpr sel);
        List.iter (case_clause) lst;
        fprintf "endcase; // 833\n";
      | CaseStartUniqInside (sel, lst) ->
        fprintf "unique case (%s) inside\n" (vexpr sel);
        List.iter (case_clause) lst;
        fprintf "endcase; // 841\n";
      | Blocking (SideEffect (id, Atom "++")) -> fprintf "            %s <= %s+1; // 842\n" (vexpr id) (vexpr id)
      | Blocking (SideEffect (id, Atom "--")) -> fprintf "            %s <= %s-1; // 843\n" (vexpr id) (vexpr id)
      | DeclData _ -> ()
      | Blocking (BreakSemi) -> () (* placeholder *)
      | BreakSemi -> () (* placeholder *)
      | Assert -> ()
      | Atom ";" -> ()
      | TaskRef (tid, lst) -> () (* placeholder *)
      | TaskRef2 (tid, (IdArrayedColon (Id id, hi, lo) as expr)) -> fprintf "// %s %s // 666\n" tid (vexpr expr)
      | TFBody (decls, lst) -> List.iter (stmt_clause) lst
      | SysTaskCall (tid, args) -> fprintf "%s(...);\n" tid
      | EquateField (id, field, expr) ->  fprintf "            %s.%s <= %s; // 851\n" (vexpr id) (vexpr field) (vexpr expr)
      | DeclInt2 _ -> ()
      | DeclLogic2 _ -> fprintf "// 491\n"
      | Return expr -> fprintf "return %s;\n" (vexpr expr)
      | CaseStmt _ as x -> case_clause x
      | SysTaskRef (Atom ("$fwrite" as task), Id f :: tl) -> fprintf "%s(%s, ...)" task f
      | SysTaskRef (Atom ("$warning" as task), String s :: []) -> fprintf "%s(\"%s\")" task s
*)
      | oth -> unhand := Some oth; failwith "stmt_clause"

(*
and case_clause = function
        | CaseStmt (lbls, body) ->
            fprintf "\t";
            if lbls <> [] then List.iter (function
               | Id lbl -> fprintf "%s" lbl
               | Intgr _ as lbl -> fprintf "%s" (vexpr lbl)
               | Number _ as lbl -> fprintf "%s" (vexpr lbl)
               | Atom "default" -> fprintf "default"
               | PackageBody (pkg, Id lbl :: []) -> fprintf "                %s::%s       =>  " pkg lbl
	       | OpenRange lst -> fprintf "%s" (String.concat ", " (List.map (function
		   | Id id -> id
		   | ValueRange (lft, rght) -> " [" ^ vexpr lft ^ " : " ^ vexpr rght ^ " ] "
		   | oth -> unhand := Some oth; failwith "open range") lst))
		   | ValueRange(lft, rght) -> fprintf " [%s.%s] " (vexpr lft) (vexpr rght)
	       | ExprOKL lbls -> List.iter (function
		   | Number _ as lbl -> fprintf "                case %s:  " (vexpr lbl)
		   | oth -> unhand := Some oth; failwith "case_label'") lbls
	       | oth -> unhand := Some oth; failwith "case_label") lbls
                else fprintf "default";
	    fprintf " : begin\n\t";
            List.iter (stmt_clause) body;
	    fprintf "end\n";
        | Id lbl -> fprintf "                %s: ; " lbl
        | Number _ as lbl -> fprintf "                %s:  " (vexpr lbl)
        | Atom "default" -> fprintf "                default: "
	| Atom ":" -> ()
	| Atom ";" -> fprintf "                ; "
        | PackageBody (pkg, Id id :: []) -> fprintf "                case %s::%s: " pkg id
        | Itmlst lst -> List.iter (case_clause) lst
        | (Seq _ | Blocking _ | If1 _ |If2 _ | ForLoop _ | CaseStartUniq _ ) as x -> stmt_clause x
        | Return expr -> fprintf "return %s;\n" (vexpr expr)
	| oth -> unhand := Some oth; failwith "case_item"

let unhand_conn = ref None

let dump_conn = function
  | CellPinItem2 (pin, ExprOKL (InitPair _ :: _)) as x when false -> unhand := Some x; failwith "dump_conn'"
  | CellPinItem2 (pin, ExprOKL (InitPair (Typ8 (SUDecl (Atom "packed", sulst), Deflt), InitPat patlst) :: _)) ->
     "." ^ pin ^ " ( { " ^ String.concat ", " (List.map (function
      | (SUMember (arg1, arg2), PatMember1 (Id rhs, expr)) -> print_endline rhs; rhs
      | oth -> unhand_conn := Some oth; failwith "dump_conn'") (List.combine sulst patlst))^" } )"
  | (Id _ | CellPinItem2 _ | CellPinItemImplied _ | CellPinItemNC _) as x -> vexpr x
  | oth -> unhand := Some oth; failwith "dump_conn"

*)

let param_template = function
(*
    | ParamDecl (Atom "localparam", [ParamAsgn1 (nam, expr)]) -> fprintf "    localparam %s = %s; // 740\n" nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ5 (TypEnum3 [AnyRange (lft, rght)], e_lst)), param_lst) -> List.iter (function
          | ParamAsgn1 (nam, expr) ->
              let wid = width expr in
              ()
	  | oth -> unhand := Some oth; failwith "localparam_int") param_lst;
    | ParamDecl (LocalParamTyp (Typ1 id), ParamAsgn1 (nam, init) :: []) ->
    (match init with
	   | InitPat lst ->
               fprintf "localparam %s %s = // 935\n" id nam;
               List.iter (function
                   | AsgnPat lst -> List.iter (fun itm -> fprintf "    parameter %s = %s; // 940\n" nam (vexpr itm)) lst
		   | PatMember1 (Id id, AsgnPat lst) -> List.iter (function
                      | PatMemberDflt expr -> fprintf "    parameter %s = %s; // 941\n" nam (vexpr expr)
                      | AsgnPat (PatMemberDflt expr :: []) -> fprintf "    parameter %s = %s; // 941\n" nam (vexpr expr)
		      | oth -> unhand := Some oth; failwith "ParamDecl'''") lst
		   | PatMember1 (Id id, (Id _ | Number _ | ExprOKL _ as x)) -> fprintf "    %s: %s; // 942\n" id (vexpr x)
                   | (Number _ | Id _ as x) -> fprintf "    parameter %s = %s; // 941\n" nam (vexpr x)
	           | oth -> unhand := Some oth; failwith "ParamDecl''") lst
           | (Number _ | Query _ | Expression _  as x) -> fprintf "    parameter %s = %s; // 943\n" nam (vexpr x)
           | oth -> unhand := Some oth; failwith "ParamDecl'")
    | ParamDecl (LocalParamTyp (Typ3 (id, PackageBody (pkg, []) :: [])), ParamAsgn1 (nam, cexpr) :: []) -> ()
    | ParamDecl (LocalParamTyp (Typ3 (id_t, AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, InitPat lst) :: []) -> List.iter (function
        | AsgnPat lst -> List.iter (fun itm -> fprintf "    parameter %s = %s; // 947\n" nam (vexpr itm)) lst
	| oth -> unhand := Some oth; failwith "ParamDecl") lst
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), [ParamAsgn2 (nam, [AnyRange (lft', rght')], InitPat lst)]) ->
        fprintf "%s; // 773\n" nam
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: [])), [ParamAsgn1 (nam, expr)]) ->
        fprintf "%s = %s; // 775\n" nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ5 (Atom "logic", AnyRange (lft, rght) :: [])), ParamAsgn1 (nam, expr) :: []) ->
        fprintf "localparam logic [%s:%s] %s = %s; // 777\n" (vexpr lft) (vexpr rght) nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ6 (Atom ("bit"|"logic"))), lst) -> List.iter (function
	        | ParamAsgn1 (id, expr) -> ()
                | oth -> unhand := Some oth; failwith "param_asgn") lst
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint"), Deflt)), [ParamAsgn1 (nam , expr)]) ->
        fprintf "%s = %s; // 782\n" nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom "int", Atom "unsigned")), [ParamAsgn1 (nam , expr)]) ->
        fprintf "localparam unsigned %s = %s; // 915\n" nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ8 (Atom ("int"|"integer"|"longint" as kind), Atom kind')), [ParamAsgn1 (nam , expr)]) ->
        fprintf "localparam %s %s %s = %s; // 915\n" kind' kind nam (vexpr expr)
    | ParamDecl (LocalParamTyp (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), sulst), Deflt)), id_lst) -> 
        fprintf "localparamtyp SUDecl ...; // 737\n"
*)      
    | InstNameParen2 (slave, [InstRange (hi, lo)]) -> {span=(); name=Some {span=(); name=slave}; expr=SV_Expr (SV_DummyExpr)}
    | oth -> unhand := Some oth; failwith "param_template_972"

let inst_map = function
| Id id -> {name=id; dims=[||]; conns=[||]}
| InstNameParen2 (id, (InstRange _ :: _ as x)) -> {name=id; dims=cnv_dims x; conns=[||]}
| oth -> unhand := Some oth; failwith "inst_map_975"

let submodule_inst_template (typ:rw) params lst =
  match typ with 
    | Id typ -> SV_Inst ( {target=typ; params=Array.of_list (List.map (param_template) params); names=Array.of_list (List.map (inst_map) lst)} )
    | oth -> failwith "submodule_inst_template"

let subroutine_port = function
| Id id as x ->  {
        dir=None;
        var=true;
        ty=cnv_typ [] x;
        name=Some {name=id; dims=[||]; expr=None};
}
| oth -> unhand := Some oth; failwith "subroutine_port"

let subroutine_proto nam typ = function
| FunGuts (ports, lst) ->
  {
        kind=SV_Func;
        lifetime=None;
        name=nam;
        args=Array.of_list (List.map (subroutine_port) ports);
        retty=Some (cnv_typ [] typ);
      }
| oth -> unhand := Some oth; failwith "subroutine_proto"

let subroutine_items = function
| FunGuts (ports, lst) -> Array.of_list (List.map (fun itm -> SV_Stmt (stmt_clause itm)) lst)
| oth -> unhand := Some oth; failwith "subroutine_items"

(*
let dump_struct_single nam memblst =
  let sulst = List.flatten (List.map (struct_union) memblst) in
  let wid = csiz' (Vsu (Id nam, sulst)) in
  fprintf "logic [%d : 0] %s; // 607\n" (wid-1) nam

let rec dump_union = function
  | SUMember (Typ5 (Atom "logic", [AnyRange (hi, lo)]), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval hi - ceval lo + 1 in
          fprintf "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_713") id_lst
  | SUMember (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), id_lst) -> List.iter (function
        | Id id ->
          let sulst = List.flatten (List.map (struct_union) memblst) in
          let wid = csiz' (Vsu (Id id, sulst)) in
          fprintf "logic [%d : 0] %s; // 663\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_719") id_lst
  | SUMember (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval hi - ceval lo + 1 in
          fprintf "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_724") id_lst
  | SUMember (Typ8 (Atom kind, Deflt), id_lst) -> List.iter (function
        | Id id ->
          let wid = atom_width kind in
          fprintf "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_729") id_lst
  | SUMember (TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst), id_lst) -> List.iter (function
        | Id id ->
          let wid = ceval hi - ceval lo + 1 in
          fprintf "logic [%d : 0] %s; // 657\n" (wid-1) id
        | oth -> dbgdmpu := Some oth; failwith "dump_union_724") id_lst
  | SUMember (Typ6 (PackageRef (pkgid, idp)), id_lst) -> dump_union (SUMember (find_pkg pkgid idp, id_lst))
  | oth -> dbgdmpu := Some oth; failwith "dump_union_726"
(*
  (dump_vdir dir^" ["^string_of_int (wid-1)^" : 0] "^nam)
*)

let dbgcomb = ref None
*)

let rec proc_dump_template modules = function
(*
    | AlwaysComb2 Assert -> SV_NullStmt
*)
    | oth -> unhand := Some oth; failwith "proc_dump_template"

let rec item_template modules = function
    | AlwaysFF (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) -> SV_Procedure {kind=SV_AlwaysFf; stmt=stmt_clause sent_lst}
    | AlwaysLegacy (At (EventOr (Pos clk :: _ as dep_lst)), sent_lst) -> SV_Procedure {kind=SV_Always; stmt=stmt_clause sent_lst}
    | AlwaysLegacy (At (EventOr dep_lst), sent_lst) -> SV_Procedure {kind=SV_Always; stmt=stmt_clause sent_lst}
    | AlwaysLegacy (AtStar, sent_lst) -> SV_Procedure {kind=SV_Always; stmt=stmt_clause sent_lst}
    | AlwaysComb2 (sent_lst) -> SV_Procedure {kind=SV_AlwaysComb; stmt=stmt_clause sent_lst}
    | AlwaysLatch ( sent_lst ) -> SV_Procedure {kind=SV_AlwaysLatch; stmt=stmt_clause sent_lst}
    | CaseStart (Id id, (CaseItm (BeginBlock [] :: Unknown ("$error",_) :: Deflt :: []) :: [])) -> SV_GenerateCase ()
  (* elaboration case *)
    | ContAsgn lst -> SV_ContAssign ( {strength=None; delay=None; delay_control=None; assignments=Array.of_list (List.map (function
      | Asgn1 (Id lhs, ExprQuote1 _) -> failwith ("ExprQuote1: "^lhs)
      | Asgn1 (lhs, expr) -> dbgasgn := Some (lhs,expr); (vexpr lhs, (vexpr expr))
      | oth -> unhand := Some oth; failwith "assign_dump_template") lst)} )
    | Iff (cond, if') -> SV_GenerateIf ( {cond=vexpr cond; main_block={label=None;items=[|item_template modules if'|]}; else_block=None} )
    | InstDecl (typ, params, (Id _ :: _ as id_lst)) -> instance_template id_lst typ
    | Itmlst lst -> SV_GenerateRegion ((), Array.of_list (List.map (item_template modules) lst))
    | InstDecl (typ, params, ((InstNameParen1 _ | InstNameParen2 _) :: _ as lst)) -> submodule_inst_template typ params lst
    | DeclLogic (wire_lst) -> cnv_logic [] [] (Atom "logic") (List.map (function
	  | Id nam -> {name=nam; name_span=(); dims=cnv_dims []; init=None}
	  | DeclAsgn (Id nam, dims') -> {name=nam; name_span=(); dims=cnv_dims dims'; init=None}
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst)
    | DeclInt2 (wire_lst) -> cnv_logic [] [] (Atom "integer") (List.map (function
	  | Id nam -> {name=nam; name_span=(); dims=cnv_dims []; init=None}
	  | DeclAsgn (Id nam, dims') -> {name=nam; name_span=(); dims=cnv_dims dims'; init=None}
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst)
    | DeclLogic2 (wire_lst, dims) -> cnv_logic dims [] (Atom "logic") (List.map (function
	  | Id nam -> {name=nam; name_span=(); dims=cnv_dims []; init=None}
	  | DeclAsgn (Id nam, dims') -> {name=nam; name_span=(); dims=cnv_dims dims'; init=None}
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst)
    | Typ2 (id_t, [PackageRef _ as pkg], wire_lst) -> cnv_logic [] [] pkg (List.map (function
	  | Id nam -> {name=nam; name_span=(); dims=cnv_dims []; init=None}
	  | DeclAsgn (Id nam, dims') -> {name=nam; name_span=(); dims=cnv_dims dims'; init=None}
	  | oth -> unhand := Some oth; failwith "DeclLogic2") wire_lst)
    | FunDecl (fn, typ, guts) -> SV_SubroutineDecl {prototype=subroutine_proto fn typ guts; items=subroutine_items guts}
    | AutoFunDecl (fn, typ, guts) -> SV_SubroutineDecl {prototype=subroutine_proto fn typ guts; items=subroutine_items guts}
(*
    | PkgImport (Itmlst lst) -> List.iter (proc_dump_template modules) lst
    | PkgImportItm (pkg, Atom "*") -> ()
    | DeclData (Typ5 (Atom "logic", AnyRange (lft, rght) :: AnyRange (lft', rght') :: []), Deflt, VarDeclAsgn (mem, ExprOKL lst) :: []) -> () (* placeholder *)
    | AssertProperty -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Deflt) -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Atom "signed") -> ()
    | DeclInt2 _ -> ()
    | NetDecl _ -> ()
    | DeclLogic2 _ -> ()
    | LoopGen1 _ -> () (* placeholder *)
    | CondGen1 _ -> () (* placeholder *)
    | GenItem _ -> () (* placeholder *)
    | ParamDecl _ -> ()
    | Initial _ -> fprintf "// initial is not implemented\n"
    | Final _ -> fprintf "// final is not implemented\n"
    | Itmlst (Id _ :: _) -> ()
    | PkgImport _ -> ()
    | DeclData _ -> ()
    | AssertProperty -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Deflt) -> ()
    | Port (dir, nam, [AnyRange (hi, lo)], Atom "signed") -> ()
    | CaseStmt _ -> ()
    | ContAsgn _ -> ()
    | LoopGen1 _ -> ()
    | CondGen1 _ -> ()
    | GenItem _ -> ()
    | Genvar _ -> ()
    | AlwaysComb2 _ -> ()
    | AlwaysFF _ -> ()
    | AlwaysLatch _ -> ()
    | AlwaysLegacy _ -> ()
    | Initial _ -> ()
    | Final _ -> ()
    | DeclReg _ -> ()
    | DeclLogic _ -> ()
    | PackageParam _ -> ()
    | Dot3 (Id bus, Id dir, Id inst) -> fprintf "// %s.%s.%s // 1163\n" bus dir inst
    | DeclModPort lst -> List.iter (function
          | ModPortItm (itm_id, dirlst) -> List.iter (function
              | PortDir ((In|Out as dir), Id id) -> fprintf "%s" id
              | Id id -> fprintf "%s" id
              | oth -> unhand := Some oth; failwith "del_mod_port_1204") dirlst
          | oth -> unhand := Some oth; failwith "decl_mod_port_1205") lst
    | DotBus (Id bus, Id dir, Id inst, [AnyRange (hi, lo)]) -> fprintf "// %s.%s.%s[%s:%s] // 1175\n" bus dir inst (vexpr hi) (vexpr lo)
    | Deflt -> fprintf "// Deflt // 1183\n"
*)
    | oth -> unhand := Some oth; failwith "item_template"

let signcnv = function
        | (Deflt,AnyRange(hi,lo)) -> Unsigned_vector(hi,lo)
	| (Deflt,Deflt) -> Unsigned
        | (Atom "signed",Deflt) -> Signed
        | (Atom "signed",AnyRange(hi,lo)) -> Signed_vector(hi,lo)
	| (Deflt,oth) -> failwith "signcnv'"
	| oth, _ -> unhand := Some oth; failwith "signcnv"

let dump_vdir = function
  | In -> let (rslt:astPortDir) = SV_Input in Some rslt
  | Out -> Some SV_Output
  | Inout -> Some SV_Inout
  | Deflt -> None
  | oth -> unhand := Some oth; failwith "dump_vdir"

let dump_array_port hi lo dir = function
  | Id id ->
    let rslt:('a)astPort = SV_Named {dir=dump_vdir dir; kind=None; ty=cnv_typ [] (Atom "logic"); name=(0, id); dims=cnv_dims [AnyRange(hi,lo)]; expr=None} in
    rslt
  | oth -> unhand := Some oth; failwith "dump_array_port"

let struct_union = function
  | oth -> unhand := Some oth; failwith "struct_union"

let dump_port_single nam dir typ =
  let rslt:('a)astPort = SV_Named {dir=dump_vdir dir; kind=None; ty=cnv_typ [] (Atom "logic"); name=(0, nam); dims=[||]; expr=None} in
  rslt

let dump_struct_port_single nam dir memblst =
  let sulst = List.flatten (List.map (struct_union) memblst) in
  let rslt:('a)astPort = SV_Named {dir=dump_vdir dir; kind=None; ty=cnv_typ [] (Atom "logic"); name=(0, nam); dims=[||]; expr=None} in
  rslt

let dump_struct_port_array nam hi lo dir memblst =
  let sulst = List.flatten (List.map (struct_union) memblst) in
  let rslt:('a)astPort = SV_Named {dir=dump_vdir dir; kind=None; ty=cnv_typ [] (Atom "logic"); name=(0, nam); dims=cnv_dims [AnyRange(hi,lo)]; expr=None} in
  rslt

let dump_port_array_dims nam dir dims =
  let rslt:('a)astPort = SV_Named {dir=dump_vdir dir; kind=None; ty=cnv_typ [] (Atom "logic"); name=(0, nam); dims=cnv_dims dims; expr=None} in
  rslt

let rec dump_port = function
    | Port ((In|Out|Inout|Deflt) as dir, nam, [], sgn) -> dump_port_single nam dir (signcnv (sgn, Deflt))
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, [], sgn) -> dump_port_single nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port hi lo dir (Id nam)
    | Port (PortDir ((In|Out|Inout|Deflt) as dir, Atom ("wire"|"reg"|"logic")), nam, AnyRange (hi, lo) :: [], sgn) -> dump_array_port hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: [], sgn) -> dump_array_port hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, AnyRange (hi, lo) :: AnyRange(hi', lo') :: AnyRange(hi'', lo'') :: [], sgn) -> dump_array_port hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ6 (Atom primtyp) :: [], sgn) -> dump_port_single nam dir Unsigned
    | Port (Deflt, nam, Typ2 (typ_t, PackageRef (pkg, idp) :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port hi lo Inout (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_t, [], []) :: [], sgn) ->
        dump_port_single nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, idp)) :: [], []) :: [], sgn) ->
        dump_port_single nam dir (signcnv (sgn, Deflt))
    | Port ((In|Out|Inout) as dir, nam, Typ2 (typ_e, (PackageRef (pkg, idp)) :: [], []) :: AnyRange (hi, lo) :: [], sgn) ->
        dump_array_port hi lo dir (Id nam)
    | DotBus (bus, dir, member, AnyRange(hi,lo) :: []) ->
        dump_array_port hi lo Inout bus
    | Port ((In|Out|Inout) as dir, stem, [Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), [])], Deflt) ->
        dump_struct_port_single stem dir memblst
    | Port ((In|Out|Inout) as dir, stem, [Typ12 ([], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), [])], Deflt) ->
        dump_struct_port_single stem dir memblst
    | Port ((In|Out|Inout) as dir, stem, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt)], Deflt) ->
        dump_struct_port_single stem dir memblst
    | Port ((In|Out|Inout) as dir, nam, Typ5 (Typ5 (Atom "logic", AnyRange(hi,lo) :: []), []) :: [], Deflt) ->
        dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, stem, [Typ5 (Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), []); AnyRange (Intgr 1, Intgr 0)], Deflt) ->
        dump_struct_port_single stem dir memblst
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (TypEnum6 (id_t, TypEnum3 [AnyRange (hi, lo)], e_lst), [])], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Out|Inout) as dir, nam, [Typ5 (TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst), [])], Deflt) ->
        dump_port_single nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ5 (Atom "logic", [AnyRange (hi, lo)])], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [TypEnum6 (old_id, TypEnum5 (Atom "logic"), e_lst)], Deflt) ->
      dump_port_single nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([], Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt), []); AnyRange (hi, lo)],  Deflt) ->
      dump_struct_port_array nam hi lo dir memblst
    | Port ((In|Inout|Out) as dir, nam, [Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst)], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt); AnyRange (hi, lo)], Deflt) ->
      dump_struct_port_array nam hi lo dir memblst
    | Port ((In|Inout|Out) as dir, nam, Typ12 ([], Typ5 (TypEnum3 (AnyRange _ :: _ as dim), e_lst), []) :: (AnyRange _ :: _ as dims), Deflt) ->
      dump_port_array_dims nam dir (dim@dims)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (id_t, [Typ5 (Atom "logic", [AnyRange (hi,lo)] )], [])], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (id_t, [TypEnum6 (old_id, TypEnum3 [AnyRange (hi,lo)], e_lst)], [])], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_t, [Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt)], [])], Deflt) ->
      dump_struct_port_single nam dir memblst
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst)], Typ5 (TypEnum3 (AnyRange _ :: _ as dims'), e_lst'), [])], Deflt) ->
      dump_port_array_dims nam dir (dims@dims')
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [PackageRef (pkg, entry)], [])], Deflt) ->
        dump_port_single nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, (Typ2 (old_id, [PackageRef (pkg, entry)], []) :: AnyRange(hi,lo) :: []), Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id,[TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], [])], Deflt) ->
      dump_array_port (Intgr (Source_text_simplify.clog2 (List.length e_lst))) (Intgr 0) dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], Typ5 (TypEnum5 (Atom "logic"), e_lst'), [])], Deflt) ->
      dump_array_port (Intgr (Source_text_simplify.clog2 (List.length e_lst))) (Intgr 0) dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [TypEnum6 (id_t, TypEnum5 (Atom "logic"), e_lst)], [])], Deflt) ->
      dump_port_single nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [Typ8 (SUDecl (Atom "packed", su_lst), Deflt)], []); AnyRange (hi, lo)], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ2 (old_id, [TypEnum6 (id_t, TypEnum3 [AnyRange (hi, lo)], e_lst)], []); AnyRange (lft, rght)], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ12 ([TypEnum6 (old_id, TypEnum3 [AnyRange (hi, lo)], e_lst)], Typ5 (TypEnum3 [AnyRange (lft, rght)], e_lst'), []); AnyRange (lft', rght')], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, (TypEnum6 (id_t, TypEnum3 (AnyRange _ :: _ as dims), e_lst) :: (AnyRange _ :: _ as dims')), Deflt) ->
      dump_port_array_dims nam dir (dims@dims')
    | Port (Deflt, nam, Typ8 (SUDecl (Atom ("packed"|"signed" as signage), memblst), Deflt) :: AnyRange (hi,lo) :: [], Deflt) ->
      dump_array_port hi lo Inout (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Typ8 (SUDecl (Atom "packed", lst), Deflt), [], []); AnyRange (hi, lo)], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Typ5 (TypEnum3 [AnyRange (hi, lo)], e_lst), [], []); AnyRange (hi', lo')], Deflt) ->
      dump_array_port hi lo dir (Id nam)
    | Port ((In|Inout|Out) as dir, nam, [Atom "logic"], Deflt) ->
        dump_port_single nam dir Unsigned
    | Port ((In|Inout|Out) as dir, nam, [Typ11 (Atom "logic", [], []); AnyRange (hi, lo)], Deflt) -> 
      dump_array_port hi lo dir (Id nam)
(*
    | Dot3 (bus, dir, member) -> "dot3 port"
*)
    | oth -> unhand := Some oth; failwith "dump_component"

let dump_template modules = function
  | Modul(nam, parm_lst, port_lst, body_lst) as x ->
  SV_ModuleDecl {lifetime=SV_Static;
                 name=nam;
                 imports=[||]; 
                 params=Array.of_list (List.map (parm_generic) parm_lst);
                 ports=Array.of_list (List.map (dump_port) port_lst);
                 items=Array.of_list (List.map (item_template modules) body_lst);
                 }
  | PackageBody (pkg, body_lst) as x ->
  SV_PackageDecl {lifetime=SV_Static;
                 name=pkg;
                 items=Array.of_list (List.map (item_template modules) body_lst);
                 }
  | IntfDecl(id, params, ports, decl) as x ->
  let parmlst = match params with Itmlst lst -> lst | oth -> [oth] in
  let portlst = match ports with Itmlst lst -> lst | oth -> [oth] in
  let itms = match decl with Itmlst lst -> lst | oth -> [oth] in
  SV_InterfaceDecl {lifetime=SV_Static;
                 name=id;
                 params=Array.of_list (List.map (parm_generic) parmlst);
                 ports=Array.of_list (List.map (dump_port) portlst);
                 items=Array.of_list (List.map (item_template modules) itms);
                 }
  | oth -> unhand := Some oth; failwith "This template only handles modules/packages"
