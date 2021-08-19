
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
  | Inc of rw
  | Dec of rw
  | Pling of rw
  | Tilde of rw
  | TildeAnd of rw
  | TildeOr of rw
  | Caret of rw
  | Bits of rw
  | Typ of string * rw list * rw list
  | Typ2 of string * rw list * rw list
  | Typ3 of string * rw list
  | Typ4 of string * rw list * rw list * rw list
  | Typ5 of rw * rw list
  | Typ6 of rw
  | Struct of string * rw list
  | TypEnum of string * rw list * rw list
  | TypEnum2 of rw list * rw list * rw list
  | TypEnum3 of rw list
  | TypEnum4 of rw * rw list * rw list
  | TypEnum5 of rw
  | Comma of rw * rw * rw
  | Clog2 of rw
  | Equals of rw * rw
  | NotEq of rw * rw
  | LtEq of rw * rw
  | GtEq of rw * rw
  | Less of rw * rw
  | Greater of rw * rw
  | And of rw * rw
  | And2 of rw * rw
  | Or of rw * rw
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
  | StarStar of rw * rw
  | Ifelse of rw * rw * rw
  | Iff of rw * rw
  | ForLoop of rw list * rw * rw * rw
  | CaseStmt of rw * rw list
  | CaseItm of rw list
  | AlwaysComb of rw list
  | AlwaysFF of rw * rw
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
  | Dot2 of string * rw list * string
  | Dot3 of string * string * string
  | Dot4 of rw list * string
  | Dot5 of string * rw list
  | DotBus of string * string * string * rw list
  | Unsigned of rw
  | Signed of rw
  | Concat of rw list
  | DeclWire of rw list * rw list
  | WireExpr of string * rw
  | DeclIntf1 of string *rw list
  | DeclIntf2 of string *rw list
  | Hash of string * rw list * rw list
  | DeclIntf of string * rw list * rw list * rw list
  | DeclModPort of rw list
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
  | LocalP of rw list * rw list
  | DeclLogic of rw list
  | DeclLogic2 of rw list * rw list
  | DeclTask of string * rw list * rw * rw
  | Mem1 of string * rw list
  | Mem3 of rw * rw * rw * rw
  | PartSel of rw * rw * rw
  | GenBlock of rw list
  | Cast of rw * rw
  | Package of string * rw list
  | DepLst of string list
  | InsideCase of rw * rw
  | InsideRange of rw * rw
  | TypParam of string * rw * rw list
  | Auto of rw list * rw list
  | QuoteLbrace of string * rw list * rw list
  | Import of rw list
  | InitPair of string * rw
  | AutoInc of string * rw
  | PreInc of string
  | ColonColon of string * string
  | Deflt
  | ExprOKL of rw list
  | InitPat of rw list
  | VarDeclAsgn of string * rw
  | ContAsgn of rw list
  | Sys of string * rw
  | SysTaskCall of string * rw list
  | Asgn1 of rw * rw
  | Elist of rw list
  | String of string
  | Atom of string
  | AutoFunDecl of string * rw * rw
  | FunDecl of string * rw * rw
  | FunGuts of rw list * rw list
  | PortItem of rw * rw
  | PortFront of rw * rw
  | PrimTyp of rw * rw