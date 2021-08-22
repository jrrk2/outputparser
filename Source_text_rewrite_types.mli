
type rw =
  | Add of rw * rw
  | AlwaysComb of rw list
  | AlwaysComb2 of rw
  | AlwaysFF of rw * rw
  | AlwaysLatch of rw
  | AlwaysLegacy of rw * rw
  | And of rw * rw
  | And2 of rw * rw
  | And3 of rw * rw
  | AnyRange of rw * rw
  | Asgn1 of rw * rw
  | AsgnPat of rw list
(*
  | Asgnlst of rw list
*)
  | Assert (* of rw * rw *)
  | AssertProperty (* of rw * rw *)
  | At of rw
  | AtStar
  | Atom of string
(*
  | Auto of rw list * rw list
*)
  | AutoFunDecl of string * rw * rw
  | BeginBlock of rw list
  | Bitlst of rw list
(*
  | Blocking of rw * rw
*)
  | BreakSemi
(*
  | Caret of rw
*)
  | CaseItm of rw list
  | CaseStart of rw * rw list
  | CaseStart1 of rw
  | CaseStart2 of rw * rw list
  | CaseStartUniq of rw * rw list
  | CaseStartUniq2 of rw * rw list
  | CaseStmt of rw * rw list
  | Cast of rw * rw
  | CellParamItem1 of string * string
  | CellParamItem2 of string * rw
  | CellParamItem3 of string * rw
  | CellPinItem1 of string * string
  | CellPinItem2 of string * rw
  | CellPinItemImplied of string
  | CellPinItemNC of string
(*
  | ColonColon of string * string
*)
  | Concat of rw list
  | CondGen1 of rw * rw * rw
  | ContAsgn of rw list
(*
  | Dec of rw
*)
  | DeclAsgn of string * rw list
  | DeclData of rw * rw list
(*
  | DeclGenvar of string list
  | DeclInt of string list
*)
  | DeclInt2 of rw list
(*
  | DeclIntf of string * rw list * rw list * rw list
  | DeclIntf1 of string *rw list
  | DeclIntf2 of string *rw list
*)
  | DeclLogic of rw list
  | DeclLogic2 of rw list * rw list
  | DeclModPort of rw list
  | DeclReg of rw list * string list * rw list list
  | DeclReg2 of rw list * rw list
(*
  | DeclTask of string * rw list * rw * rw
  | DeclWire of rw list * rw list
*)
  | Deflt
(*  
  | DepLst of string list
  | Dim of rw * rw
*)
  | Div of rw * rw
(*
  | Dot of string * rw
*)
  | Dot1 of rw * rw
(*
  | Dot2 of string * rw list * string
*)
  | Dot3 of string * string * string
(*
  | Dot4 of rw list * string
  | Dot5 of string * rw list
*)
  | DotBus of string * string * string * rw list
  | Edge of rw * rw
  | ElabTask of rw
  | Elist of rw list
  | ElseStmt of rw
  | EnumInit of string * rw
  | Equals of rw * rw
  | Equals3 of rw * rw
  | EqualsQuery of rw * rw
  | Equate of string * rw
  | EquateArrayField of string * string * rw * rw * rw
  | EquateField of string * string * rw
  | EquateSelect of string * rw * rw
  | EquateSelect2 of rw * rw * rw
  | EquateSlice of rw * rw * rw * rw
  | EventOr of rw * rw
  | ExprOKL of rw list
  | ExprQuote1 of rw * rw
  | Expression of rw
(*
  | Field of rw * rw
*)
  | Final of rw list
  | FopAsgn of string * rw
  | FopAsgn1 of string * string * string * rw
  | FopAsgnArrayField of string * string * rw
  | FopAsgnArrayField2 of string * rw * rw
  | FopAsgnArrayField3 of string * rw * string * rw
  | FopAsgnArrayField4 of string * rw * string * rw * rw * rw
  | FopAsgnArrayField5 of string * rw * string * rw * rw
  | FopAsgnArrayField6 of string * string * rw * rw * rw
  | FopAsgnArrayField7 of string * rw * rw * string * rw
  | FopAsgnArrayMemSel of string * rw * rw * rw
  | FopAsgnArrayRange of string * rw * rw * rw
  | FopAsgnArrayRange2 of rw * rw * rw * rw
  | FopAsgnArraySel of string * rw * rw
  | FopAsgnArrayWid of string * rw * rw * rw
  | FopAsgnConcat of rw list * rw
  | ForEach of string * rw list
  | ForLoop of rw list * rw * rw * rw
  | FunDecl of string * rw * rw
  | FunGuts of rw list * rw list
  | FunRef of string * rw list
  | FunRef2 of string * rw list * rw list
  | GenBlock of rw list
  | GenItem of string * rw list
  | Generate of rw
  | Greater of rw * rw
  | GtEq of rw * rw
(*
  | Hash of string * rw list * rw list
*)
  | HyphenGt of rw * rw
  | Id of string
  | IdArrayed1 of rw * rw * rw
  | IdArrayed2 of rw * rw
  | IdArrayed3 of rw list * rw
  | IdArrayedColon of rw * rw * rw
  | IdArrayedHyphenColon of rw * rw * rw
  | IdArrayedPlusColon of rw * rw * rw
  | If1 of rw * rw
  | If2 of rw * rw * rw
(*
  | Ifelse of rw * rw * rw
*)
  | Iff of rw * rw
  | Import of rw list
  | In
  | Inc of rw
  | InitPair of string * rw
  | InitPat of rw list
  | InitSig of string * rw
  | Initial of rw list
  | Inout
  | InsideCase of rw * rw
  | InsideRange of rw * rw
  | InstDecl1 of string * rw list * rw list
  | InstDecl2 of string * rw list
  | InstNameParen1 of string * rw list
  | InstNameParen2 of string * rw list
  | InstRange of rw * rw
  | IntfDecl of string * rw * rw * rw
  | Intgr of int
  | ItemAsgn of rw
  | Itmlst of rw list
(*
  | Latch of rw list
*)
  | Less of rw * rw
(*
  | LocalP of rw list * rw list
*)
  | LocalParamTyp of rw
  | Logic of rw list * rw list
  | LoopGen1 of string * string * rw * rw * rw * rw list
  | LtEq of rw * rw
  | LtGt of rw * rw
(*
  | Mem1 of string * rw list
  | Mem3 of rw * rw * rw * rw
*)
  | Mod of rw * rw
  | ModPortItm of string * rw list
  | Modul of string * rw list * rw list * rw list
  | Mult of rw * rw
  | Nand of rw * rw
  | Neg of string
  | NetDecl of rw * rw list
  | NonBlocking of rw * rw
  | Nor of rw * rw
  | NotEq of rw * rw
  | NotEq3 of rw * rw
  | NotEqQuery of rw * rw
  | Number of int * int * int * string
  | OpenRange of rw * rw
  | Or of rw * rw
  | Or2 of rw * rw
  | Out
  | Package of string * rw list
  | Param of string * rw
  | ParamAsgn1 of string * rw
  | ParamAsgn2 of string * rw list * rw
  | ParamDecl of rw * rw list
  | ParamPort of rw list
(*
  | Parenth of string * rw list
  | PartSel of rw * rw * rw
*)
  | PatMember1 of rw * rw
  | PatMemberDflt of rw
  | PkgImport of rw
  | PkgImportItm of string * rw
  | Pling of rw
  | Port of rw * string * rw list * rw list
  | PortDir of rw * rw
  | PortFront of rw * rw
  | PortItem of rw * rw
  | PortItemFront of rw * rw
  | PortsStar of rw list
  | Pos of string
  | PropertySpec (* of rw * rw *)
  | Query of rw * rw * rw
(*
  | QuoteLbrace of string * rw list * rw list
  | RedAnd of rw
  | RedOr of rw
*)
  | Repl of rw * rw list
  | Return of rw
  | SUMember of rw * rw list
(*
  | Sel of rw * rw
  | Sentry of rw * rw
*)
  | Seq of string * rw list
  | Shiftl of rw * rw
  | Shiftr of rw * rw
  | Shiftr3 of rw * rw
  | SideEffect of rw * rw
  | Signed of rw
(*
  | Slice of string * rw * rw
  | Slice2 of string * rw * rw
*)
  | StarStar of rw * rw
  | Stmt1 of rw
  | String of string
(*
  | Struct of string * rw list
*)
  | Sub of rw * rw
  | Sys of string * rw
  | SysFuncCall of string * rw list
  | SysTaskCall of string * rw list
  | TaskBody of rw list * rw list
  | TaskRef of string * rw list
  | Tilde of rw
  | TildeAnd of rw
  | TildeOr of rw
  | Typ1 of string
  | Typ2 of string * rw list * rw list
  | Typ3 of string * rw list
  | Typ4 of string * rw list * rw list * rw list
  | Typ5 of rw * rw list
  | Typ6 of rw
  | Typ7 of string * rw
  | Typ8 of rw * rw
  | Typ9 of string * rw list * rw
  | Typ10 of string * rw list * string
(*
  | TypEnum of string * rw list * rw list
  | TypEnum2 of rw list * rw list * rw list
*)
  | TypEnum3 of rw list
  | TypEnum4 of rw * rw list * rw list
  | TypEnum5 of rw
  | TypEnum6 of string * rw * rw list
  | TypParam of string * rw * rw list
  | UMinus of rw
  | UPlus of rw
  | Union of rw * rw list
  | Unknown of string
  | Unsigned of rw
  | VNum of string
  | ValueRange of rw * rw
  | VarDeclAsgn of string * rw
  | VarDim of rw
  | While of rw * rw list
  | WireExpr of string * rw
  | Xnor of rw * rw
  | Xor of rw * rw
