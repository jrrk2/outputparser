
type mem_opts = {off:int list; siz:int list; wid:int; tot:int}

type rw =
  | Active of vtyp * rw * rw
  | Add of rw * rw
  | AlwaysComb of rw list
  | AlwaysComb2 of rw
  | AlwaysFF of rw * rw
  | AlwaysLatch of rw
  | AlwaysLegacy of rw * rw
  | AlwaysSync
  | And of rw * rw
  | And2 of rw * rw
  | And3 of rw * rw
  | AnyRange of rw * rw
  | Asgn1 of rw * rw
  | AsgnPat of rw list
  | Assert (* of rw * rw *)
  | AssertProperty (* of rw * rw *)
  | At of rw
  | AtStar
  | Atom of string
  | AutoFunDecl of string * rw * rw
  | BeginBlock of rw list
  | Bitlst of rw list
  | BlockItem of rw
  | Blocking of rw
  | BreakSemi
  | CaretTilde of rw
  | CaseItm of rw list
  | CaseStart of rw * rw list
  | CaseStart1 of rw
  | CaseStartInside of rw * rw list
  | CaseStartUniq of rw * rw list
  | CaseStartUniqInside of rw * rw list
  | CaseStmt of rw list * rw list
  | Cast of rw * rw
  | CellParamItem2 of string * rw
  | CellParamItem3 of string * rw
  | CellPinItem2 of string * rw
  | CellPinItemImplied of string
  | CellPinItemNC of string
  | Concat of rw list
  | CondGen1 of rw * rw * rw
  | ContAsgn of rw list
  | DeclAsgn of rw * rw list
  | DeclData of rw * rw * rw list
  | DeclInt2 of rw list
  | DeclLogic of rw list
  | DeclLogic2 of rw list * rw list
  | DeclModPort of rw list
  | DeclReg of rw list * rw list * rw
  | Deflt
  | Div of rw * rw
  | Dot1 of rw * rw
  | Dot3 of rw * rw * rw
  | DotBus of rw * rw * rw * rw list
  | Edge of rw * rw
  | ElabTask of rw
  | ElseStmt of rw
  | EnumInit of string * rw
  | Equals of rw * rw
  | Equals3 of rw * rw
  | EqualsQuery of rw * rw
  | Equate of rw * rw
  | EquateArrayField of rw * rw * rw * rw * rw
  | EquateConcat of rw list * rw
  | EquateField of rw * rw * rw
  | EquateSelect of rw * rw * rw
  | EquateSelect2 of rw * rw * rw
  | EquateSlice of rw * rw * rw * rw
  | EquateSlicePlus of rw * rw * rw * rw
  | EventOr of rw list
  | ExprOKL of rw list
  | ExprQuote1 of rw * rw
  | Expression of rw
  | Final of rw list
  | Float of float
  | FopAsgn of rw * rw
  | FopAsgn1 of rw * rw * rw * rw
  | FopAsgnArrayField of rw * rw * rw
  | FopAsgnArrayField2 of rw * rw * rw
  | FopAsgnArrayField3 of rw * rw * rw * rw
  | FopAsgnArrayField4 of rw * rw * rw * rw * rw * rw
  | FopAsgnArrayField5 of rw * rw * rw * rw * rw
  | FopAsgnArrayField6 of rw * rw * rw * rw * rw
  | FopAsgnArrayField7 of rw * rw * rw * rw * rw
  | FopAsgnArrayField8 of rw * rw * rw * rw * rw
  | FopAsgnArrayField9 of rw * rw * rw * rw * rw * rw
  | FopAsgnArrayMemSel of rw * rw * rw * rw
  | FopAsgnArrayRange of rw * rw * rw * rw
  | FopAsgnArrayRange2 of rw * rw * rw * rw
  | FopAsgnArraySel of rw * rw * rw
  | FopAsgnArrayWid of rw * rw * rw * rw
  | FopAsgnConcat of rw list * rw
  | ForEach of rw * rw list
  | ForLoop of rw list * rw * rw * rw
  | FunDecl of string * rw * rw
  | FunGuts of rw list * rw list
  | FunRef of string * rw list
  | FunRef2 of string * rw list * rw list
  | GenBlock of rw list
  | GenItem of string * rw list
  | Genvar of rw list
  | Greater of rw * rw
  | GtEq of rw * rw
  | Hash of rw
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
  | Iff of rw * rw
  | Import of rw list
  | In
  | Inc of rw
  | InitPair of rw * rw
  | InitPat of rw list
  | InitSig of rw * rw
  | Initial of rw list
  | Inout
  | InsideCase of rw * rw
  | InsideRange of rw * rw
  | InstArrayDecl of rw * rw list * rw * rw list * rw list
  | InstDecl of rw * rw list * rw list
  | InstNameParen1 of string * rw list
  | InstNameParen2 of string * rw list
  | InstRange of rw * rw
  | IntfDecl of string * rw * rw * rw
  | Intgr of int
  | ItemAsgn of rw
  | Itmlst of rw list
  | Less of rw * rw
  | LocalParamTyp of rw
  | Logic of rw list * rw list
  | LoopGen1 of rw * string * rw * rw * rw * rw list
  | LtEq of rw * rw
  | LtGt of rw * rw
  | Mod of rw * rw
  | ModPortItm of string * rw list
  | Modul of string * rw list * rw list * rw list
  | Mult of rw * rw
  | Nand of rw * rw
  | Neg of rw
  | NetDecl of rw list * rw list
  | NonBlocking of rw * rw
  | Nor of rw * rw
  | NotEq of rw * rw
  | NotEq3 of rw * rw
  | NotEqQuery of rw * rw
  | Number of int * int * int * string
  | OpenRange of rw list
  | Or of rw * rw
  | Or2 of rw * rw
  | Out
  | PackageBody of string * rw list
  | PackageParam of rw list * rw
  | PackageParam2 of string * string * rw list * rw
  | PackageRef of string * rw
  | Param of string * rw * rw list
  | ParamAsgn1 of string * rw
  | ParamAsgn2 of string * rw list * rw
  | ParamDecl of rw * rw list
  | ParamPort of rw list
  | PatMember1 of rw * rw
  | PatMemberDflt of rw
  | PkgImport of rw
  | PkgImportItm of string * rw
  | Pling of rw
  | Port of rw * string * rw list * rw
  | PortDir of rw * rw
  | PortFront of rw * rw
  | PortItem of rw * rw
  | PortItemFront of rw * rw
  | PortItemFront2 of rw * rw * rw list
  | PortsStar of rw list
  | Pos of rw
  | PropertySpec (* of rw * rw *)
  | Query of rw * rw * rw
  | RedAnd of rw
  | RedOr of rw
  | RedXor of rw
  | Repl of rw * rw list
  | Return of rw
  | SUDecl of rw * rw list
  | SUMember of rw * rw list
  | Seq of string * rw list
  | Shiftl of rw * rw
  | Shiftr of rw * rw
  | Shiftr3 of rw * rw
  | SideEffect of rw * rw
  | Signed of rw
  | Split of rw * rw
  | StarStar of rw * rw
  | String of string
  | Sub of rw * rw
  | Sys of string * rw
  | SysFuncCall of string * rw list
  | SysTaskCall of string * rw list
  | SysTaskRef of rw * rw list
  | TFBody of rw list * rw list
  | TF_port_decl of rw * rw list * rw list
  | TF_variable of rw * rw * rw * rw
  | TaskDecl of string * rw * rw * rw
  | TaskRef of string * rw list
  | TaskRef2 of string * rw
  | Tilde of rw
  | TildeAnd of rw
  | TildeOr of rw
  | Typ1 of string
  | Typ10 of string * rw list * string
  | Typ11 of rw * rw list * rw list
  | Typ12 of rw list * rw * rw list
  | Typ2 of string * rw list * rw list
  | Typ3 of string * rw list
  | Typ4 of string * rw list * rw list * rw list
  | Typ5 of rw * rw list
  | Typ6 of rw
  | Typ7 of string * rw
  | Typ8 of rw * rw
  | Typ9 of string * rw list * rw
  | TypEnum3 of rw list
  | TypEnum4 of rw * rw list * rw list
  | TypEnum5 of rw
  | TypEnum6 of string * rw * rw list
  | TypParam of string * rw * rw list
  | UMinus of rw
  | UPlus of rw
  | Unimplemented of string * rw list
  | Union of rw * rw list
  | Unknown of string * rw list
  | Unsigned of rw
  | VNum of string
  | ValueRange of rw * rw
  | VarDeclAsgn of rw * rw
  | VarDim of rw
  | While of rw * rw list
  | WireExpr of rw * rw
  | Xnor of rw * rw
  | Xor of rw * rw

and vtyp =
  | Vint of int
  | Vpkg of string * string
  | Unsigned
  | Unsigned_vector of rw * rw
  | Signed
  | Signed_vector of rw * rw
  | Vsigtyp
  | Vdot
  | Vstr of string
  | Vtyp of string
  | Vfun of string
  | Venum of string
  | Vintf of rw
  | MaybePort of int * vtyp * rw
  | Vemember of string * string * vtyp * int
  | Task of rw * rw * rw
  | Vmem of mem_opts
  | InstArray of rw * rw * rw
  | Vlong of int64
  | Vreal of float
  | Vlocal of int * rw
  | Vsu of rw * (string * vtyp) list
  | Vsua of int * int * (string * vtyp) list

module E = Msat_sat_slit.String_lit (* expressions *)
module F = Msat_tseitin.MakeCNF

type ind = {
  wires:(E.signal, F.t option) Hashtbl.t;
  inffop:(E.signal, unit) Hashtbl.t;
  stash:(E.signal, string * string * Input_rewrite_types.ilang list) Hashtbl.t;
  wid:(string, int) Hashtbl.t;
}

type dead = 
| Undecidable
| Always_false
| Always_true
