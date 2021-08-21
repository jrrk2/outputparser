
type rw =
  | Unknown of string
  | In
  | Out
  | Inout
  | Itmlst of rw list
  | Expression of rw
  | Port of rw * string * rw list * rw list
  | Modul of string * rw list * rw list * rw list
  | Pos of string
  | Neg of string
  | Edge of rw * rw
  | Id of string
  | Intgr of int
  | Number of int * int * int * string
  | Sel of rw * rw
  | NonBlocking of rw * rw
  | Query of rw * rw * rw
  | RedAnd of rw
  | RedOr of rw
  | UMinus of rw
  | UPlus of rw
  | Inc of rw
  | Dec of rw
  | Pling of rw
  | Tilde of rw
  | TildeAnd of rw
  | TildeOr of rw
  | Caret of rw
  | Bits of rw
  | Typ of string * rw list * rw list
  | Typ1 of string
  | Typ2 of string * rw list * rw list
  | Typ3 of string * rw list
  | Typ4 of string * rw list * rw list * rw list
  | Typ5 of rw * rw list
  | Typ6 of rw
  | Typ7 of string * rw
  | Typ8 of rw * rw
  | Struct of string * rw list
  | TypEnum of string * rw list * rw list
  | TypEnum2 of rw list * rw list * rw list
  | TypEnum3 of rw list
  | TypEnum4 of rw * rw list * rw list
  | TypEnum5 of rw
  | TypEnum6 of string * rw * rw list
  | Comma of rw * rw * rw
  | Clog2 of rw
  | Equals of rw * rw
  | Equals3 of rw * rw
  | EqualsQuery of rw * rw
  | NotEq of rw * rw
  | NotEq3 of rw * rw
  | NotEqQuery of rw * rw
  | HyphenGt of rw * rw
  | LtGt of rw * rw
  | LtEq of rw * rw
  | GtEq of rw * rw
  | Less of rw * rw
  | Greater of rw * rw
  | And of rw * rw
  | Nand of rw * rw
  | And2 of rw * rw
  | And3 of rw * rw
  | Or of rw * rw
  | Nor of rw * rw
  | Or2 of rw * rw
  | Xor of rw * rw
  | Xnor of rw * rw
  | Shiftl of rw * rw
  | Shiftr of rw * rw
  | Shiftr3 of rw * rw
  | Add of rw * rw
  | Sub of rw * rw
  | Mult of rw * rw
  | Div of rw * rw
  | Mod of rw * rw
  | StarStar of rw * rw
  | Ifelse of rw * rw * rw
  | Iff of rw * rw
  | If1 of rw * rw
  | If2 of rw * rw * rw
  | ForLoop of rw list * rw * rw * rw
  | CaseStart of rw * rw list
  | CaseStartUniq of rw * rw list
  | CaseStartUniq2 of rw * rw list
  | CaseStart1 of rw
  | CaseStmt of rw * rw list
  | CaseItm of rw list
  | AlwaysComb of rw list
  | AlwaysComb2 of rw
  | AlwaysFF of rw * rw
  | AlwaysLatch of rw
  | AlwaysLegacy of rw * rw
  | Sentry of rw * rw
  | Latch of rw list
  | Blocking of rw * rw
  | Asgnlst of rw list
  | DeclReg of rw list * string list * rw list list
  | DeclReg2 of rw list * rw list
  | DeclInt of string list
  | DeclInt2 of rw list
  | DeclGenvar of string list
  | DeclAsgn of string * rw list
  | DeclData of rw * rw list
  | Dim of rw * rw
  | BeginBlock of rw list
  | Bitlst of rw list
  | Dot of string * rw
  | Dot0 of rw * rw
  | Dot1 of rw * rw
  | Dot2 of string * rw list * string
  | Dot3 of string * string * string
  | Dot4 of rw list * string
  | Dot5 of string * rw list
  | DotBus of string * string * string * rw list
  | Unsigned of rw
  | Signed of rw
  | Concat of rw list
  | NetDecl of rw * rw list
  | DeclWire of rw list * rw list
  | WireExpr of string * rw
  | DeclIntf1 of string *rw list
  | DeclIntf2 of string *rw list
  | Hash of string * rw list * rw list
  | DeclIntf of string * rw list * rw list * rw list
  | DeclModPort of rw list
  | IntfDecl of string * rw * rw
  | Repl of rw * rw list
  | Slice of string * rw * rw
  | Slice2 of string * rw * rw
  | Field of rw * rw
  | Parenth of string * rw list
  | Logic of rw list * rw list
  | Param of string * rw
  | ParamAsgn1 of string * rw
  | ParamAsgn2 of string * rw list * rw
  | ParamDecl of rw * rw list
  | ParamPort of rw list
  | LocalP of rw list * rw list
  | LocalParamTyp of rw
  | DeclLogic of rw list
  | DeclLogic2 of rw list * rw list
  | DeclTask of string * rw list * rw * rw
  | TaskRef of string * rw list
  | TaskBody of rw list * rw list
  | Mem1 of string * rw list
  | Mem3 of rw * rw * rw * rw
  | PartSel of rw * rw * rw
  | GenBlock of rw list
  | GenItem of string * rw list
  | Cast of rw * rw
  | Package of string * rw list
  | DepLst of string list
  | InsideCase of rw * rw
  | InsideRange of rw * rw
  | OpenRange of rw * rw
  | ValueRange of rw * rw
  | AnyRange of rw * rw
  | TypParam of string * rw * rw list
  | Auto of rw list * rw list
  | QuoteLbrace of string * rw list * rw list
  | Import of rw list
  | InitPair of string * rw
  | InitPat of rw list
  | InitSig of string * rw
  | AutoInc of string * rw
  | PreInc of string
  | ColonColon of string * string
  | Deflt
  | ExprOKL of rw list
  | VarDeclAsgn of string * rw
  | ContAsgn of rw list
  | Sys of string * rw
  | SysTaskCall of string * rw list
  | SysFuncCall of string * rw list
  | Asgn1 of rw * rw
  | Elist of rw list
  | String of string
  | Atom of string
  | AutoFunDecl of string * rw * rw
  | FunRef of string * rw list
  | FunRef2 of string * rw list * rw list
  | FunDecl of string * rw * rw
  | FunGuts of rw list * rw list
  | PortDir of rw * rw
  | PortItem of rw * rw
  | PortFront of rw * rw
  | PortsStar of rw list
  | PrimTyp of rw * rw
  | ItemAsgn of rw
  | At of rw
  | AtStar
  | EventOr of rw * rw
  | Seq of string * rw list
  | Equate of string * rw
  | EquateField of string * string * rw
  | EquateArrayField of string * string * rw * rw * rw
  | EquateSelect of string * rw * rw
  | EquateSelect2 of rw * rw * rw
  | EquateSlice of rw * rw * rw * rw
  | ExprQuote1 of rw * rw
  | ExprQuoteUnsigned of rw
  | IdArrayedColon of rw * rw * rw
  | IdArrayedPlusColon of rw * rw * rw
  | IdArrayed1 of rw * rw * rw
  | IdArrayed2 of rw * rw
  | IdArrayed3 of rw list * rw
  | VNum of string
  | Stmt1 of rw
  | FopAsgn of string * rw
  | FopAsgnArraySel of string * rw * rw
  | FopAsgnArrayMemSel of string * rw * rw * rw
  | FopAsgnArrayRange of string * rw * rw * rw
  | FopAsgnArrayRange2 of rw * rw * rw * rw
  | FopAsgnArrayWid of string * rw * rw * rw
  | FopAsgnArrayField of string * string * rw
  | FopAsgnArrayField2 of string * rw * rw
  | FopAsgnArrayField3 of string * rw * string * rw
  | LoopGen1 of string * string * rw * rw * rw * rw list
  | CondGen1 of rw * rw * rw
  | InstDecl1 of string * rw list * rw list
  | InstDecl2 of string * rw list
  | InstRange of rw * rw
  | InstNameParen1 of string * rw list
  | InstNameParen2 of string * rw list
  | CellPinItem1 of string * string
  | CellPinItem2 of string * rw
  | CellPinItemNC of string
  | CellPinItemImplied of string
  | CellParamItem1 of string * string
  | CellParamItem2 of string * rw
  | CellParamItem3 of string * rw
  | Initial of rw list
  | Final of rw list
  | Assert (* of rw * rw *)
  | AssertProperty (* of rw * rw *)
  | PropertySpec (* of rw * rw *)
  | Return of rw
  | ElabTask of rw
  | ElseStmt of rw
  | Generate of rw
  | PkgImport of rw
  | PkgImportItm of string * rw
  | AsgnPat of rw list
  | PatMember1 of rw * rw
  | PatMemberDflt of rw
  | EnumInit of string * rw
  | AndEq of rw * rw
  | PlusEq of rw * rw
  | MinusEq of rw * rw
  | BreakSemi
  | PortItemFront of rw * rw
  | FopAsgnConcat of rw list * rw