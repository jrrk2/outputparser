open Ast_types_old
open Printf

let dump = function _ -> failwith "dump1"
let dumpchar = function _ -> failwith "dump2"
let dumpSpan = function _ -> failwith "dump3"
let dumpSpanned = function _ -> failwith "dump4"
let dumpbool = function _ -> failwith "dump5"
let dumpName = function _ -> failwith "dump6"
let dumpKw = function _ -> failwith "dump7"
let dumpusize = function _ -> failwith "dump8"
let dumpdyn = function _ -> failwith "dump9"
let dumpPropSpec = function _ -> failwith "dump10"
let dumpNodeId = function _ -> failwith "dump11"
let dumpCell = function _ -> failwith "dump12"
let dumpAnyNode = function _ -> failwith "dump13"
let dumpOp = Token_dump_old.dumpOp
let dumpLit = Token_dump_old.dumpLit

(* // Copyright (c) 2016-2021 Fabian Schuiki *)
(* //! An abstract syntax tree for SystemVerilog. *)
(* /// An AST node. *)
(* // Compare and hash nodes by reference for use in the query system. *)
(* /// Basic attributes of an AST node. *)
(* /// *)
(* /// If this trait is present on `Node<T>`, then `Node<T>` will automatically *)
(* /// implement the full `AnyNode` trait. *)
(* /// Common details of an AST node. *)
(* /// Format a node in indefinite form. *)
(* /// Format a node in definite form. *)
(* /// A node which allows iterating over each child node. *)
(* /// A node which can pass itself as `AnyNode` to a callback. *)
(* /// Common denominator across all AST nodes. *)

let dump_array f delim x = String.concat delim (Array.to_list (Array.map f x))

let rec (dumpNode:('asta, 'astT)astNode -> unit) = function  {
    (* /// Unique ID assigned to the node. *)
        id;
    (* /// Full span the node covers in the input. *)
        span;
    (* /// Parent node. *)
        parent;
    (* /// Lexical order of the node. *)
        order;
    (* /// Per-node data. *)
        data;

} -> failwith "dump14"

(* // The following are needed due to the `Cell`s in `Node`. It is safe to share *)
(* // nodes between threads if we never change `parent` and `order` afterwards. *)
(* // We only set these cells once immediately after constructing an AST, and never *)
(* // again after. *)
(* /// Automatically implement `AnyNode` for `Node<T>` if enough information is *)
(* /// present. *)
(* /// Automatically implement `AnyNodeData` for `Node<T>` if the inner node *)
(* /// implements it. *)
(* /// Allow any node to be printed in diagnostics in a human-friendly form. *)
(* /// Allow any node to be debug-formatted. *)
(* // Compare and hash nodes by reference for use in the query system. *)
(* /// A node that accepts `Visitor`s. *)
(* /// A node that walks a `Visitor` over itself. *)
(* // Deprecated names. *)
(* /// All things being compiled. *)

and (dumpRoot:('asta)astRoot -> string) = function  {
        files;

} -> dump_array (dumpSourceFile) "\n\n" files

(* /// An entire source file. *)

and (dumpSourceFile:('asta)astSourceFile -> string) = function  {
        timeunits;
        items;

} -> (dump_array dumpItem ";\n" items)

(* /// An item that may appear in a hierarchical scope. *)
(* /// *)
(* /// This includes the following scopes: *)
(* /// - file root *)
(* /// - modules *)
(* /// - interfaces *)
(* /// - packages *)
(* /// - classes *)
(* /// - generates *)

and (dumpItem:('asta)astItem -> string) = function 
    | SV_Dummy -> ""
    | SV_ModuleDecl (astModule1) -> dumpModule (astModule1)
    | SV_InterfaceDecl (astInterface1) -> failwith "dumpItem1"
    | SV_PackageDecl (astPackage1) -> dumpPackage astPackage1
    | SV_ClassDecl (astClassDecl1) -> failwith "dumpItem3"
    | SV_ProgramDecl -> failwith "dumpItem4"
    | SV_ImportDecl (astImportDecl1) -> failwith "dumpItem5"
    | SV_DpiDecl (astDpiDecl1) -> failwith "dumpItem6"
    | SV_ParamDecl (astParamDecl1) -> (dumpParamDecl(astParamDecl1))
    | SV_ModportDecl (astModport1) -> failwith "dumpItem8"
    | SV_Typedef (astTypedef1) -> failwith "dumpItem9"
    | SV_PortDecl (astPortDecl1) -> failwith "dumpItem10"
    | SV_Procedure (astProcedure1) -> dumpProcedure (astProcedure1)
    | SV_SubroutineDecl (astSubroutineDecl1) -> dumpSubroutineDecl (astSubroutineDecl1)
    | SV_ContAssign (astContAssign1) -> (dumpContAssign (astContAssign1))
    | SV_GenvarDecl (astGenvarDecl1) -> failwith "dumpItem14"
    | SV_GenerateRegion (astSpan1, astItem2) -> dump_array dumpItem ";\n" (astItem2)
    | SV_GenerateFor (astGenerateFor1) -> failwith "dumpItem16"
    | SV_GenerateIf (astGenerateIf1) -> dumpGenerateIf (astGenerateIf1)
    | SV_GenerateCase (astGenerateCase1) -> failwith "dumpItem18"
    | SV_Assertion (astAssertion1) -> failwith "dumpItem19"
    | SV_NetDecl (astNetDecl1) -> dumpNetDecl astNetDecl1
    | SV_VarDecl (astVarDecl1) -> dumpVarDecl astVarDecl1
    | SV_Inst (astInst1) -> dumpInst astInst1
    | SV_Timeunit (astTimeunit1) -> failwith "dumpItem23"

(* /// A module. *)

and (dumpModule:('asta)astModule -> string) = function  {
        lifetime;
    (* // default static *)
        name;
        imports;
        params;
        ports;
        items;

} -> 
"module " ^ name ^ "(\n\t" ^ dump_array dumpPort ",\n\t" ports ^ ");\n\n" ^ dump_array dumpItem ";\n" items ^ "endmodule\n"

(* /// An interface. *)

and (dumpInterface:('asta)astInterface -> unit) = function  {
        lifetime;
    (* // default static *)
        name;
        params;
        ports;
        items;

} -> failwith "dump43"

(* /// A package. *)

and (dumpPackage:('asta)astPackage -> string) = function  {
        lifetime;
        name;
        items;

} -> ""

(* /// Lifetime specifier for variables, tasks, and functions. Defaults to static. *)

and (dumpLifetime:astLifetime -> unit) = function 
    | SV_Static -> failwith "dump45"
    | SV_Automatic -> failwith "dump46"

(* /// A time unit specification. *)
(* /// *)
(* /// ```text *)
(* /// "timeunit" time_literal ["/" time_literal] ";" *)
(* /// "timeprecision" time_literal ";" *)
(* /// ``` *)

and (dumpTimeunit:astTimeunit -> unit) = function  {
        unit;
        prec;

} -> failwith "dump47"

(* /// A type. *)

and (dumpType:('asta)astType -> unit) = function  {
        kind;
        sign;
        dims;

} -> failwith "dump48"

(* /// A type without sign and packed dimensions. *)

and (dumpTypeKind:('asta)astTypeKind -> unit) = function 
    | SV_ImplicitType -> failwith "dump49"
    | SV_VoidType -> failwith "dump50"
    | SV_NamedType (astName1) -> failwith "dump51"
    | SV_StringType -> failwith "dump52"
    | SV_ChandleType -> failwith "dump53"
    | SV_VirtIntfType (astName1) -> failwith "dump54"
    | SV_EventType -> failwith "dump55"
    | SV_MailboxType -> failwith "dump56"
    | SV_ImplicitSignedType -> failwith "dump57"
    | SV_ImplicitUnsignedType -> failwith "dump58"
    (* // Scoping *)
    | SV_ScopedType {
        ty;
        member;
        name;
    } -> failwith "dump59"
    (* // Forward declarations *)
    | SV_ForwardType {
        kind;
    } -> failwith "dump60"
    (* // Integer Vector Types *)
    | SV_BitType -> failwith "dump61"
    | SV_LogicType -> failwith "dump62"
    | SV_RegType -> failwith "dump63"
    (* // Integer Atom Types *)
    | SV_ByteType -> failwith "dump64"
    | SV_ShortIntType -> failwith "dump65"
    | SV_IntType -> failwith "dump66"
    | SV_IntegerType -> failwith "dump67"
    | SV_LongIntType -> failwith "dump68"
    | SV_TimeType -> failwith "dump69"
    (* // Non-integer Types *)
    | SV_ShortRealType -> failwith "dump70"
    | SV_RealType -> failwith "dump71"
    | SV_RealtimeType -> failwith "dump72"
    (* // Enumerations *)
    | SV_EnumType (astEnum1) -> failwith "dump73"
    | SV_StructType (astStruct1) -> failwith "dump74"
    (* // Specialization *)
    | SV_SpecializedType (astType1, astParamAssignment2) -> failwith "dump75"
    (* /// Type reference, such as `type(x)` or `type(int)`. *)
    | SV_TypeRef (astTypeOrExpr1) -> failwith "dump76"


and (dumpTypeSign:astTypeSign -> unit) = function 
    | SV_None -> failwith "dump77"
    | SV_Signed -> failwith "dump78"
    | SV_Unsigned -> failwith "dump79"


and (dumpTypeDim:('asta)astTypeDim -> unit) = function 
    | SV_Expr (astExpr1) -> failwith "dump80"
    | SV_Range (astExpr1, astExpr2) -> failwith "dump81"
    | SV_Queue (astExpr1) -> failwith "dump82"
    | SV_Unsized -> failwith "dump83"
    | SV_Associative (astType1) -> failwith "dump84"

(* /// An enum definition. *)
(* /// *)
(* /// For example `enum { FOO = 42 }`. *)

and (dumpEnum:('asta)astEnum -> unit) = function  {
        base_type;
        variants;

} -> failwith "dump85"

(* /// A single entry in an enum. *)
(* /// *)
(* /// For example the `FOO = 42` in `enum { FOO = 42 }`. *)

and (dumpEnumName:('asta)astEnumName -> unit) = function  {
        name;
        range;
        value;

} -> failwith "dump86"

(* /// A struct definition. *)
(* /// *)
(* /// For example `struct packed { byte foo; }`. *)

and (dumpStruct:('asta)astStruct -> unit) = function  {
        kind;
        packed;
        signing;
        members;

} -> failwith "dump87"


and (dumpStructKind:astStructKind -> unit) = function 
    (* /// A `struct`. *)
    | SV_Struct -> failwith "dump88"
    (* /// A `union`. *)
    | SV_Union -> failwith "dump89"
    (* /// A `union tagged`. *)
    | SV_TaggedUnion -> failwith "dump90"

(* /// A struct field. *)
(* /// *)
(* /// For example the `byte foo;` in `struct packed { byte foo; }`. *)

and (dumpStructMember:('asta)astStructMember -> unit) = function  {
        rand_qualifier;
        ty;
        names;

} -> failwith "dump91"

(* /// A module or interface port as declared in the port list. *)

and (dumpPort:('asta)astPort -> string) = function 
    | SV_Intf {
        modport;
        name=(span,nam);
        dims;
        expr;
    } -> nam
    | SV_Explicit {
        dir;
        name=(span,nam);
        expr;
    } -> nam
    | SV_Named {
        dir;
        kind;
        ty;
        name=(span,nam);
        dims;
        expr;
    } -> (match dir with Some dir' -> dumpPortDir dir' | None -> "") ^ " " ^ nam
    | SV_Implicit (astExpr1) -> ""

(* /// A port declaration in an item body. *)

and (dumpPortDecl:('asta)astPortDecl -> unit) = function  {
        dir;
        kind;
        ty;
        names;

} -> failwith "dump96"

(* /// Whether a declaration is a variable or a net. *)

and (dumpVarKind:astVarKind -> unit) = function 
    (* /// A variable declaration. *)
    | SV_Var -> failwith "dump97"
    (* /// A net declaration. *)
    | SV_Net {
        ty;
    (* /// The net type, such as `wire` or `trireg`. *)
        kind;
    (* /// Additional `vectored` or `scalared` specifier. *)
    } -> failwith "dump98"


and (dumpPortDir:astPortDir -> string) = function 
    | SV_Input -> "input"
    | SV_Output -> "output"
    | SV_Inout -> "inout"
    | SV_Ref -> "ref"


and (dumpNetType:astNetType -> string) = function 
    | SV_Supply0 -> ""
    | SV_Supply1 -> ""
    | SV_Tri -> ""
    | SV_TriAnd -> ""
    | SV_TriOr -> ""
    | SV_TriReg -> ""
    | SV_Tri0 -> ""
    | SV_Tri1 -> ""
    | SV_Uwire -> ""
    | SV_Wire -> "wire"
    | SV_WireAnd -> ""
    | SV_WireOr -> ""

(* /// A procedure such as `always*`, `initial`, or `final`. *)

and (dumpProcedure:('asta)astProcedure -> string) = function  {
        kind;
        stmt;

} -> (dumpProcedureKind kind)


and (dumpProcedureKind:astProcedureKind -> string) = function 
    | SV_Initial -> "initial"
    | SV_Always -> "always"
    | SV_AlwaysComb -> "always_comb"
    | SV_AlwaysLatch -> "always_latch"
    | SV_AlwaysFf -> "always_ff"
    | SV_Final -> "final"

(* /// A statement. *)

and (dumpStmt:('asta)astStmt -> string) = function  {
        label;
        kind;

} -> dumpStmtKind kind

(* /// The different kinds of statement. *)

and (dumpStmtKind:('asta)astStmtKind -> string) = function 
    | SV_NullStmt -> failwith "dumpStmtKind1"
    | SV_SequentialBlock (astStmt1) -> dump_array dumpStmt ";\n" astStmt1
    | SV_ParallelBlock (astStmt1, astJoinKind2) -> failwith "dumpStmtKind3"
    | SV_IfStmt {
        up;
        cond;
        main_stmt;
        else_stmt;
    } -> sprintf "if (%s) begin %s end%s" (dumpExpr cond) (dumpStmt main_stmt) (match else_stmt with
        | Some else_stmt' -> " else begin " ^ dumpStmt else_stmt' ^ " end "
        | None -> "")
    | SV_BlockingAssignStmt {
        lhs;
        rhs;
        op;
    } -> sprintf "%s = %s;\n" (dumpExpr lhs) (dumpExpr rhs)
    | SV_NonblockingAssignStmt {
        lhs;
        rhs;
        delay;
    } -> sprintf "%s <= %s;\n" (dumpExpr lhs) (dumpExpr rhs)
    | SV_TimedStmt (astTimingControl1, astStmt2) -> failwith "dumpStmtKind7"
    | SV_CaseStmt {
        up;
        kind;
        expr;
        mode;
        items;
    } -> failwith "dumpStmtKind8"
    | SV_ForeverStmt (astStmt1) -> failwith "dumpStmtKind9"
    | SV_RepeatStmt (astExpr1, astStmt2) -> failwith "dumpStmtKind10"
    | SV_WhileStmt (astExpr1, astStmt2) -> failwith "dumpStmtKind11"
    | SV_DoStmt (astStmt1, astExpr2) -> failwith "dumpStmtKind12"
    | SV_ForStmt (astStmt1, astExpr2, astExpr3, astStmt4) -> failwith "dumpStmtKind13"
    | SV_ForeachStmt (astExpr1, astForeachIndex2, astStmt3) -> failwith "dumpStmtKind14"
    | SV_ExprStmt (astExpr1) -> dumpExpr astExpr1
    | SV_VarDeclStmt (astVarDecl1) -> failwith "dumpStmtKind16"
    | SV_GenvarDeclStmt (astGenvarDecl1) -> failwith "dumpStmtKind17"
    | SV_ContinueStmt -> failwith "dumpStmtKind18"
    | SV_BreakStmt -> failwith "dumpStmtKind19"
    | SV_ReturnStmt (astExpr1) -> failwith "dumpStmtKind20"
    | SV_ImportStmt (astImportDecl1) -> failwith "dumpStmtKind21"
    | SV_AssertionStmt (astAssertion1) -> failwith "dumpStmtKind22"
    | SV_WaitExprStmt (astExpr1, astStmt2) -> failwith "dumpStmtKind23"
    | SV_WaitForkStmt -> failwith "dumpStmtKind24"
    | SV_DisableForkStmt -> failwith "dumpStmtKind25"
    | SV_DisableStmt (astName1) -> failwith "dumpStmtKind26"


and (dumpJoinKind:astJoinKind -> unit) = function 
    | SV_All -> failwith "dump149"
    | SV_Any -> failwith "dump150"
    | SV_None -> failwith "dump151"


and (dumpUniquePriority:astUniquePriority -> unit) = function 
    | SV_Unique -> failwith "dump152"
    | SV_Unique0 -> failwith "dump153"
    | SV_Priority -> failwith "dump154"


and (dumpCaseKind:astCaseKind -> unit) = function 
    | SV_Normal -> failwith "dump155"
    | SV_DontCareZ -> failwith "dump156"
    | SV_DontCareXZ -> failwith "dump157"


and (dumpCaseMode:astCaseMode -> unit) = function 
    | SV_Normal -> failwith "dump158"
    | SV_Inside -> failwith "dump159"
    | SV_Pattern -> failwith "dump160"


and (dumpCaseItem:('asta)astCaseItem -> unit) = function 
    | SV_Default (astStmt1) -> failwith "dump161"
    | SV_Expr (astExpr1, astStmt2) -> failwith "dump162"


and (dumpDelayControl:('asta)astDelayControl -> unit) = function  {
        span;
        expr;

} -> failwith "dump163"


and (dumpEventControl:('asta)astEventControl -> unit) = function  {
        span;
        data;

} -> failwith "dump164"


and (dumpEventControlData:('asta)astEventControlData -> unit) = function 
    | SV_Implicit -> failwith "dump165"
    | SV_Expr (astEventExpr1) -> failwith "dump166"


and (dumpCycleDelay:astCycleDelay -> unit) = function  _ -> failwith "dump167"

and (dumpTimingControl:('asta)astTimingControl -> unit) = function 
    | SV_Delay (astDelayControl1) -> failwith "dump168"
    | SV_Event (astEventControl1) -> failwith "dump169"
    | SV_Cycle (astCycleDelay1) -> failwith "dump170"


and (dumpAssignOp:astAssignOp -> unit) = function 
    | SV_Identity -> failwith "dump171"
    | SV_Add -> failwith "dump172"
    | SV_Sub -> failwith "dump173"
    | SV_Mul -> failwith "dump174"
    | SV_Div -> failwith "dump175"
    | SV_Mod -> failwith "dump176"
    | SV_BitAnd -> failwith "dump177"
    | SV_BitOr -> failwith "dump178"
    | SV_BitXor -> failwith "dump179"
    | SV_LogicShL -> failwith "dump180"
    | SV_LogicShR -> failwith "dump181"
    | SV_ArithShL -> failwith "dump182"
    | SV_ArithShR -> failwith "dump183"

(* /// A variable declaration. *)
(* /// *)
(* /// For example `logic x, y, z`. *)

and (dumpVarDecl:('asta)astVarDecl -> string) = function  {
        konst;
        var;
        lifetime;
        ty;
        names;

} -> (dump_array (dumpVarDeclName) "; " names)

(* /// A variable or net declaration name. *)
(* /// *)
(* /// For example the `x` in `logic x, y, z`. *)

and (dumpVarDeclName:('asta)astVarDeclName -> string) = function  {
        name;
        name_span;
        dims;
        init;

} -> name

(* /// A generate variable declaration. *)

and (dumpGenvarDecl:('asta)astGenvarDecl -> unit) = function  {
        name;
        init;

} -> failwith "dump186"

(* /// A foreach-loop index variable. *)

and (dumpForeachIndex:('asta)astForeachIndex -> unit) = function  {
    (* /// The name of the index. *)
        name;
    (* /// At which index nesting level this index is located. *)
        index;

} -> failwith "dump187"

(* /// An expression. *)

and (dumpExpr:('asta)astExpr -> string) = function 
    | SV_DummyExpr -> failwith "dumpExpr1"
    | SV_LiteralExpr (astLit1) -> dumpLit astLit1
    (* /// An identifier, like `foo`. *)
    | SV_IdentExpr (astName1) -> astName1
    (* /// A system identifier, like `$foo`. *)
    | SV_SysIdentExpr (astName1) -> astName1
    | SV_ThisExpr -> failwith "dumpExpr5"
    | SV_DollarExpr -> failwith "dumpExpr6"
    | SV_NullExpr -> failwith "dumpExpr7"
    | SV_ScopeExpr (astExpr1, astName2) -> failwith "dumpExpr8"
    | SV_IndexExpr {
        indexee;
        index;
    } -> dumpExpr indexee ^ "[" ^ dumpExpr index ^ "]"
    | SV_UnaryExpr {
        op;
        expr;
        postfix;
    } -> sprintf "%s (%s)" (dumpOp op) (dumpExpr expr)
    | SV_BinaryExpr {
        op;
        lhs;
        rhs;
    } -> sprintf "(%s) %s (%s)" (dumpExpr lhs) (dumpOp op) (dumpExpr rhs)
    | SV_TernaryExpr {
        cond;
        true_expr;
        false_expr;
    } -> sprintf "(%s) ? (%s) : (%s)" (dumpExpr cond) (dumpExpr true_expr) (dumpExpr false_expr)
    | SV_AssignExpr {
        op;
        lhs;
        rhs;
    } -> failwith "dumpExpr13"
    | SV_CallExpr (astExpr1, astCallArg2) -> dumpExpr astExpr1 ^ dump_array (dumpCallArg) ", " astCallArg2
    | SV_TypeExpr (astType1) -> failwith "dumpExpr15"
    (* // TODO: Check if this is still needed, otherwise remove *)
    | SV_ConstructorCallExpr (astCallArg1) -> failwith "dumpExpr16"
    | SV_ClassNewExpr (astExpr1) -> failwith "dumpExpr17"
    | SV_ArrayNewExpr (astExpr1, astExpr2) -> failwith "dumpExpr18"
    | SV_EmptyQueueExpr -> failwith "dumpExpr19"
    | SV_StreamConcatExpr {
        slice;
        exprs;
    } -> failwith "dumpExpr20"
    | SV_ConcatExpr {
        repeat;
        exprs;
    } -> "{" ^ dump_array dumpExpr ", " exprs ^ "}"
    | SV_MinTypMaxExpr {
        min;
        typ;
        max;
    } -> failwith "dumpExpr22"
    | SV_RangeExpr {
        mode;
        lhs;
        rhs;
    } -> "[" ^ dumpExpr lhs ^ ":" ^ dumpExpr rhs ^ "]"
    (* /// A member expression, like `a.b`. *)
    | SV_MemberExpr {
        expr;
        name;
    } -> failwith "dumpExpr24"
    | SV_PatternExpr (astPatternField1) -> failwith "dumpExpr25"
    | SV_InsideExpr (astExpr1, astValueRange2) -> failwith "dumpExpr26"
    | SV_CastExpr (astType1, astExpr2) -> failwith "dumpExpr27"
    | SV_CastSizeExpr (astExpr1, astExpr2) -> failwith "dumpExpr28"
    | SV_CastSignExpr (astTypeSign1, astExpr2) -> failwith "dumpExpr29"
    (* /// A `$bits` call. *)
    | SV_BitsExpr {
        name;
        arg;
    } -> failwith "dumpExpr30"

(* /// An ambiguous node that can either be a type or and expression. *)
(* /// *)
(* /// Use the `disamb_type_or_expr` query to disambiguate based on name *)
(* /// resolution. *)

and (dumpTypeOrExpr:('asta)astTypeOrExpr -> unit) = function 
    | SV_Type (astType1) -> failwith "dump218"
    | SV_Expr (astExpr1) -> failwith "dump219"


and (dumpValueRange:('asta)astValueRange -> unit) = function 
    | SV_Single (astExpr1) -> failwith "dump220"
    | SV_Range {
        lo;
        hi;
        span;
    } -> failwith "dump221"


and (dumpRangeMode:astRangeMode -> unit) = function 
    | SV_Absolute -> failwith "dump222"
    | SV_RelativeDown -> failwith "dump223"
    | SV_RelativeUp -> failwith "dump224"


and (dumpIdentifier:astIdentifier -> unit) = function  {
        span;
        name;

} -> failwith "dump225"


and (dumpCallArg:('asta)astCallArg -> string) = function  {
        span;
        name_span;
        name;
        expr;

} -> match name with Some nam -> nam | None -> ""


and (dumpStreamConcatSlice:('asta)astStreamConcatSlice -> unit) = function 
    | SV_Expr (astExpr1) -> failwith "dump227"
    | SV_Type (astType1) -> failwith "dump228"


and (dumpStreamExpr:('asta)astStreamExpr -> unit) = function  {
        expr;
        range;

} -> failwith "dump229"


and (dumpEventExpr:('asta)astEventExpr -> unit) = function 
    | SV_Edge {
        span;
        edge;
        value;
    } -> failwith "dump230"
    | SV_Iff {
        span;
        expr;
        cond;
    } -> failwith "dump231"
    | SV_Or {
        span;
        lhs;
        rhs;
    } -> failwith "dump232"


and (dumpEdgeIdent:astEdgeIdent -> unit) = function 
    | SV_Implicit -> failwith "dump233"
    | SV_Edge -> failwith "dump234"
    | SV_Posedge -> failwith "dump235"
    | SV_Negedge -> failwith "dump236"

(* /// A class declaration. *)

and (dumpClassDecl:('asta)astClassDecl -> unit) = function  {
        virt;
        lifetime;
    (* // default static *)
        name;
        params;
        extends;
        impls;
        items;

} -> failwith "dump237"


and (dumpClassItem:('asta)astClassItem -> unit) = function  {
        span;
        qualifiers;
        data;

} -> failwith "dump238"


and (dumpClassItemQualifier:astClassItemQualifier -> unit) = function 
    | SV_Static -> failwith "dump239"
    | SV_Protected -> failwith "dump240"
    | SV_Local -> failwith "dump241"
    | SV_Rand -> failwith "dump242"
    | SV_Randc -> failwith "dump243"
    | SV_Pure -> failwith "dump244"
    | SV_Virtual -> failwith "dump245"
    | SV_Const -> failwith "dump246"


and (dumpClassItemData:('asta)astClassItemData -> unit) = function 
    | SV_Property -> failwith "dump247"
    | SV_Typedef (astTypedef1) -> failwith "dump248"
    | SV_SubroutineDecl (astSubroutineDecl1) -> failwith "dump249"
    | SV_ExternSubroutine (astSubroutinePrototype1) -> failwith "dump250"
    | SV_Constraint (astConstraint1) -> failwith "dump251"
    | SV_ClassDecl -> failwith "dump252"
    | SV_CovergroupDecl -> failwith "dump253"
    | SV_ParamDecl (astParamDecl1) -> failwith "dump254"
    | SV_Null -> failwith "dump255"


and (dumpRandomQualifier:astRandomQualifier -> unit) = function 
    | SV_Rand -> failwith "dump256"
    | SV_Randc -> failwith "dump257"

(* /// A type definition. *)
(* /// *)
(* /// For example `typedef int my_type_t`. *)

and (dumpTypedef:('asta)astTypedef -> unit) = function  {
        name;
        ty;
        dims;

} -> failwith "dump258"


and (dumpConstraint:('asta)astConstraint -> unit) = function  {
        span;
        kind;
        statik;
        name;
        name_span;
        items;

} -> failwith "dump259"


and (dumpConstraintKind:astConstraintKind -> unit) = function 
    | SV_Decl -> failwith "dump260"
    | SV_Proto -> failwith "dump261"
    | SV_ExternProto -> failwith "dump262"
    | SV_PureProto -> failwith "dump263"


and (dumpConstraintItem:('asta)astConstraintItem -> unit) = function  {
        span;
        data;

} -> failwith "dump264"


and (dumpConstraintItemData:('asta)astConstraintItemData -> unit) = function 
    | SV_If -> failwith "dump265"
    | SV_Foreach -> failwith "dump266"
    | SV_Expr (astExpr1) -> failwith "dump267"

(* /// A function or task declaration. *)

and (dumpSubroutineDecl:('asta)astSubroutineDecl -> string) = function  {
        prototype;
        items;

} ->
dumpSubroutinePrototype prototype ^
(dump_array dumpSubroutineItem ";\n" items)

(* /// A function or task prototype. *)

and (dumpSubroutinePrototype:('asta)astSubroutinePrototype -> string) = function  {
        kind;
        lifetime;
        name;
        args;
        retty;

} -> (dumpSubroutineKind kind);


and (dumpSubroutineKind:astSubroutineKind -> string) = function 
    | SV_Func -> "function"
    | SV_Task -> "task"

(* /// A function or task port. *)

and (dumpSubroutinePort:('asta)astSubroutinePort -> unit) = function  {
        dir;
        var;
        ty;
        name;

} -> failwith "dump272"


and (dumpSubroutinePortName:('asta)astSubroutinePortName -> unit) = function  {
        name;
        dims;
        expr;

} -> failwith "dump273"


and (dumpSubroutineItem:('asta)astSubroutineItem -> string) = function 
    | SV_PortDecl (astSubroutinePortDecl1) -> dumpSubroutinePortDecl  (astSubroutinePortDecl1)
    | SV_Stmt (astStmt1) -> dumpStmt astStmt1


and (dumpSubroutinePortDecl:('asta)astSubroutinePortDecl -> string) = function  {
        span;
        dir;
        var;
        ty;
        names;

} -> dump_array dumpVarDeclName ", " names


and (dumpSubroutinePortDir:astSubroutinePortDir -> unit) = function 
    | SV_Input -> failwith "dump277"
    | SV_Output -> failwith "dump278"
    | SV_Inout -> failwith "dump279"
    | SV_Ref -> failwith "dump280"
    | SV_ConstRef -> failwith "dump281"

(* /// A net declaration. *)
(* /// *)
(* /// For example `wire x, y, z`. *)

and (dumpNetDecl:('asta)astNetDecl -> string) = function  {
        net_type;
        strength;
        kind;
        ty;
        delay;
        names;

} -> (dump_array (dumpVarDeclName) ";\n" names)


and (dumpNetKind:astNetKind -> unit) = function 
    | SV_Vectored -> failwith "dump283"
    | SV_Scalared -> failwith "dump284"
    | SV_None -> failwith "dump285"


and (dumpNetStrength:astNetStrength -> unit) = function 
    | SV_Drive (astDriveStrength1, astDriveStrength2) -> failwith "dump286"
    | SV_Charge (astChargeStrength1) -> failwith "dump287"


and (dumpDriveStrength:astDriveStrength -> unit) = function 
    | SV_Supply0 -> failwith "dump288"
    | SV_Strong0 -> failwith "dump289"
    | SV_Pull0 -> failwith "dump290"
    | SV_Weak0 -> failwith "dump291"
    | SV_HighZ0 -> failwith "dump292"
    | SV_Supply1 -> failwith "dump293"
    | SV_Strong1 -> failwith "dump294"
    | SV_Pull1 -> failwith "dump295"
    | SV_Weak1 -> failwith "dump296"
    | SV_HighZ1 -> failwith "dump297"


and (dumpChargeStrength:astChargeStrength -> unit) = function 
    | SV_Small -> failwith "dump298"
    | SV_Medium -> failwith "dump299"
    | SV_Large -> failwith "dump300"

(* /// A field in a `'{...}` pattern. *)

and (dumpPatternField:('asta)astPatternField -> unit) = function 
    | SV_Default (astExpr1) -> failwith "dump301"
    | SV_Member (astExpr1, astExpr2) -> failwith "dump302"
    | SV_Type (astType1, astExpr2) -> failwith "dump303"
    | SV_Expr (astExpr1) -> failwith "dump304"
    | SV_Repeat (astExpr1, astExpr2) -> failwith "dump305"

(* /// An import declaration. *)
(* /// *)
(* /// For example `import a::b, c::*`. *)

and (dumpImportDecl:('asta)astImportDecl -> unit) = function  {
        items;

} -> failwith "dump306"

(* /// A single import. *)
(* /// *)
(* /// For example the `a::b` in `import a::b, c::*`. *)

and (dumpImportItem:('asta)astImportItem -> unit) = function  {
        pkg;
        name;
    (* // None means `import pkg::*` *)

} -> failwith "dump307"


and (dumpAssertion:('asta)astAssertion -> unit) = function  {
        span;
        label;
        data;

} -> failwith "dump308"


and (dumpAssertionData:('asta)astAssertionData -> unit) = function 
    | SV_Immediate (astBlockingAssertion1) -> failwith "dump309"
    | SV_Deferred (astAssertionDeferred1, astBlockingAssertion2) -> failwith "dump310"
    | SV_Concurrent (astConcurrentAssertion1) -> failwith "dump311"


and (dumpAssertionDeferred:astAssertionDeferred -> unit) = function 
    (* /// `assert #0` *)
    | SV_Observed -> failwith "dump312"
    (* /// `assert final` *)
    | SV_Final -> failwith "dump313"


and (dumpBlockingAssertion:('asta)astBlockingAssertion -> unit) = function 
    | SV_Assert (astExpr1, astAssertionActionBlock2) -> failwith "dump314"
    | SV_Assume (astExpr1, astAssertionActionBlock2) -> failwith "dump315"
    | SV_Cover (astExpr1, astStmt2) -> failwith "dump316"


and (dumpConcurrentAssertion:('asta)astConcurrentAssertion -> unit) = function 
    | SV_AssertProperty (astPropSpec1, astAssertionActionBlock2) -> failwith "dump317"
    | SV_AssumeProperty (astPropSpec1, astAssertionActionBlock2) -> failwith "dump318"
    | SV_CoverProperty (astPropSpec1, astStmt2) -> failwith "dump319"
    | SV_CoverSequence -> failwith "dump320"
    | SV_ExpectProperty (astPropSpec1, astAssertionActionBlock2) -> failwith "dump321"
    | SV_RestrictProperty (astPropSpec1) -> failwith "dump322"


and (dumpAssertionActionBlock:('asta)astAssertionActionBlock -> unit) = function 
    | SV_Positive (astStmt1) -> failwith "dump323"
    | SV_Negative (astStmt1) -> failwith "dump324"
    | SV_Both (astStmt1, astStmt2) -> failwith "dump325"


and (dumpSeqExpr:('asta)astSeqExpr -> unit) = function  {
        span;
        data;

} -> failwith "dump326"


and (dumpSeqExprData:('asta)astSeqExprData -> unit) = function 
    | SV_Expr (astExpr1, astSeqRep2) -> failwith "dump327"
    | SV_BinOp (astSeqBinOp1, astSeqExpr2, astSeqExpr3) -> failwith "dump328"
    | SV_Throughout (astExpr1, astSeqExpr2) -> failwith "dump329"
    | SV_Clocked (astEventExpr1, astSeqExpr2) -> failwith "dump330"


and (dumpSeqRep:('asta)astSeqRep -> unit) = function 
    | SV_Consec (astExpr1) -> failwith "dump331"
    (* // [* expr] *)
    | SV_ConsecStar -> failwith "dump332"
    (* // [*] *)
    | SV_ConsecPlus -> failwith "dump333"
    (* // [+] *)
    | SV_Nonconsec (astExpr1) -> failwith "dump334"
    (* // [= expr] *)
    | SV_Goto (astExpr1) -> failwith "dump335"
    (* // [-> expr] *)


and (dumpSeqBinOp:astSeqBinOp -> unit) = function 
    | SV_Or -> failwith "dump336"
    | SV_And -> failwith "dump337"
    | SV_Intersect -> failwith "dump338"
    | SV_Within -> failwith "dump339"


and (dumpPropExpr:('asta)astPropExpr -> unit) = function  {
        span;
        data;

} -> failwith "dump340"


and (dumpPropExprData:('asta)astPropExprData -> unit) = function 
    | SV_SeqOp (astPropSeqOp1, astSeqExpr2) -> failwith "dump341"
    | SV_SeqBinOp (astPropSeqBinOp1, astPropSeqOp2, astSeqExpr3, astPropExpr4) -> failwith "dump342"
    | SV_Not (astPropExpr1) -> failwith "dump343"
    | SV_BinOp (astPropBinOp1, astPropExpr2, astPropExpr3) -> failwith "dump344"
    | SV_Clocked (astEventExpr1, astPropExpr2) -> failwith "dump345"


and (dumpPropSeqOp:astPropSeqOp -> unit) = function 
    | SV_None -> failwith "dump346"
    | SV_Weak -> failwith "dump347"
    | SV_Strong -> failwith "dump348"


and (dumpPropSeqBinOp:astPropSeqBinOp -> unit) = function 
    | SV_ImplOverlap -> failwith "dump349"
    | SV_ImplNonoverlap -> failwith "dump350"
    | SV_FollowOverlap -> failwith "dump351"
    | SV_FollowNonoverlap -> failwith "dump352"


and (dumpPropBinOp:astPropBinOp -> unit) = function 
    | SV_Or -> failwith "dump353"
    | SV_And -> failwith "dump354"
    | SV_Until -> failwith "dump355"
    | SV_SUntil -> failwith "dump356"
    | SV_UntilWith -> failwith "dump357"
    | SV_SUntilWith -> failwith "dump358"
    | SV_Impl -> failwith "dump359"
    | SV_Iff -> failwith "dump360"
    | SV_SeqImplOl -> failwith "dump361"
    | SV_SeqImplNol -> failwith "dump362"
    | SV_SeqFollowOl -> failwith "dump363"
    | SV_SeqFollowNol -> failwith "dump364"

(* /// An instantiation of a module. *)
(* /// *)
(* /// For example `foo u0(), u1();`. *)

and (dumpInst:('asta)astInst -> string) = function  {
    (* /// The name of the module to instantiate. *)
        target;
    (* /// The parameters in the module to be assigned. *)
        params;
    (* /// The names and ports of the module instantiations. *)
        names;

} -> target

(* /// A single module instance. *)
(* /// *)
(* /// For example the `u0()` in `foo u0(), u1();`. *)

and (dumpInstName:('asta)astInstName -> unit) = function  {
    (* /// The name of the instance. *)
        name;
    (* /// The unpacked dimensions. *)
        dims;
    (* /// The port connections. *)
        conns;

} -> failwith "dump366"

(* /// A modport declaration in an interface. *)
(* /// *)
(* /// For example `modport in (...), out (...);`. *)

and (dumpModport:('asta)astModport -> unit) = function  {
    (* /// The names of the modports. *)
        names;

} -> failwith "dump367"

(* /// A single modport declaration. *)
(* /// *)
(* /// For example the `in (...)` in `modport in (...), out (...);`. *)

and (dumpModportName:('asta)astModportName -> unit) = function  {
    (* /// The name of the modport. *)
        name;
    (* /// The individual port specifications. *)
        ports;

} -> failwith "dump368"

(* /// A modport ports declaration. *)
(* /// *)
(* /// For example `input a, .b(expr)`, or `import ...`, or `clocking foo`. *)

and (dumpModportPort:('asta)astModportPort -> unit) = function 
    (* /// A simple port, for example `input a, .b(expr)`. *)
    | SV_Simple {
        dir;
        port;
    } -> failwith "dump369"

(* /// A single simple modport port. *)
(* /// *)
(* /// For example the `a` or `.b(expr)` in `input a, .b(expr)`. *)

and (dumpModportSimplePort:('asta)astModportSimplePort -> unit) = function  {
    (* /// The name of the port. *)
        name;
    (* /// The optional parenthesized expression of the port. *)
        expr;

} -> failwith "dump370"

(* /// A parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// "localparam" data_type_or_implicit list_of_param_assignments *)
(* /// "localparam" "type" list_of_type_assignments *)
(* /// "parameter" data_type_or_implicit list_of_param_assignments *)
(* /// "parameter" "type" list_of_type_assignments *)
(* /// ``` *)

and (dumpParamDecl:('asta)astParamDecl -> string) = function  {
        local;
        kind;

} -> dumpParamKind kind


and (dumpParamKind:('asta)astParamKind -> string) = function 
    | SV_Type (astParamTypeDecl1) -> failwith "dump372"
    | SV_Value (astParamValueDecl1) -> dump_array dumpParamValueDecl ";\n" astParamValueDecl1

(* /// A single type assignment within a parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// ident ["=" type] *)
(* /// ``` *)

and (dumpParamTypeDecl:('asta)astParamTypeDecl -> unit) = function  {
        name;
        ty;

} -> failwith "dump374"

(* /// A single value assignment within a parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// [type_or_implicit] ident {dimension} ["=" expr] *)
(* /// ``` *)

and (dumpParamValueDecl:('asta)astParamValueDecl -> string) = function  {
        ty;
        name;
        dims;
        expr;

} -> name

(* /// A continuous assignment statement. *)
(* /// *)
(* /// ```text *)
(* /// "assign" [drive_strength] [delay3] list_of_assignments ";" *)
(* /// "assign" [delay_control] list_of_assignments ";" *)
(* /// ``` *)

and (dumpContAssign:('asta)astContAssign -> string) = function  {
        strength;
        delay;
        delay_control;
        assignments;

} -> String.concat "" (Array.to_list (Array.map (fun (lhs,rhs) -> "assign "^dumpExpr lhs^" = "^dumpExpr rhs^";\n") assignments))

(* /// A `for` generate statement. *)

and (dumpGenerateFor:('asta)astGenerateFor -> unit) = function  {
        init;
        cond;
        step;
        block;

} -> failwith "dump377"

(* /// An `if` generate statement. *)

and (dumpGenerateIf:('asta)astGenerateIf -> string) = function  {
        cond;
        main_block;
        else_block;

} -> sprintf "if (%s) begin %s end%s" (dumpExpr cond) (dumpGenerateBlock main_block) (match else_block with
        | Some else_block' -> " else begin " ^ dumpGenerateBlock else_block' ^ " end "
        | None -> "")

(* /// A `case` generate statement. *)

and (dumpGenerateCase:('asta)astGenerateCase -> unit) = function     (* // TODO *)
 _ -> ()
(* /// A body of a generate construct. *)
(* /// *)
(* /// May contains hierarchy items or more generate constructs. *)

and (dumpGenerateBlock:('asta)astGenerateBlock -> string) = function  {
        label;
        items;

} -> dump_array dumpItem ";\n" items


and (dumpParamAssignment:('asta)astParamAssignment -> unit) = function  {
        span;
        name;
        expr;

} -> failwith "dump381"

(* /// A port connection in an instantiation. *)
(* /// *)
(* /// For example: *)
(* /// ```verilog *)
(* /// foo bar ( *)
(* ///     .*, *)
(* ///     .name, *)
(* ///     .name(), *)
(* ///     .name(expr), *)
(* ///     expr, *)
(* /// ); *)
(* /// ``` *)

and (dumpPortConn:('asta)astPortConn -> unit) = function 
    (* /// The `.*` case, *)
    | SV_Auto -> failwith "dump382"
    (* /// The `.name`, `.name()`, or `.name(expr)` cases, *)
    | SV_Named (astName1, astPortConnMode2) -> failwith "dump383"
    (* /// The `expr` case, *)
    | SV_Positional (astExpr1) -> failwith "dump384"

(* /// How a named port connection is made. *)

and (dumpPortConnMode:('asta)astPortConnMode -> unit) = function 
    (* /// The `.name` case. *)
    | SV_Auto -> failwith "dump385"
    (* /// The `.name()` case. *)
    | SV_Unconnected -> failwith "dump386"
    (* /// The `.name(expr)` case. *)
    | SV_Connected (astExpr1) -> failwith "dump387"

(* /// A DPI declaration such as `import "DPI-C"` or `export "DPI-C"`. *)

and (dumpDpiDecl:('asta)astDpiDecl -> unit) = function 
    (* /// An `import`. *)
    | SV_Import {
        spec;
        property;
        cident;
        prototype;
    } -> failwith "dump388"
    (* /// An `export`. *)
    | SV_Export {
        spec;
        cident;
        kind;
        name;
    } -> failwith "dump389"

(* /// A DPI function/task property. *)

and (dumpDpiProperty:astDpiProperty -> unit) = function 
    (* /// `context` *)
    | SV_Context -> failwith "dump390"
    (* /// `pure` *)
    | SV_Pure -> failwith "dump391"

(* /// A data type. *)
(* /// *)
(* /// From §A.2.2.1 (extended to be context-free): *)
(* /// ```text *)
(* /// data_type ::= *)
(* ///     integer_vector_type signing? packed_dimension* *)
(* ///     integer_atom_type signing? *)
(* ///     non_integer_type *)
(* ///     ("struct"|"union") ("packed" signing?)? "{" struct_union_member+ "}" packed_dimension* *)
(* ///     "enum" enum_base_type? "{" enum_name_declaration ("," enum_name_declaration)* "}" packed_dimension* *)
(* ///     "string" *)
(* ///     "chandle" *)
(* ///     path_segment ("::" path_segment)* packed_dimension* *)
(* ///     "event" *)
(* ///     type_reference *)
(* /// *)
(* /// path_segment ::= *)
(* ///     "$unit" *)
(* ///     package_identifier *)
(* ///     class_identifier (param_value_assignment)? *)
(* ///     type_identifier *)
(* /// ``` *)

and (dumpDataType:('asta)astDataType -> unit) = function 
    (* /// An integer type, like `bit`, `logic signed`, `reg signed [42:0]`, `int`, *)
    (* /// or `int unsigned`. *)
    | SV_Int {
        ty;
        signing;
        packed_dims;
    } -> failwith "dump392"
    (* /// A real type. *)
    | SV_Real (astRealType1) -> failwith "dump393"
    (* /// A struct or union type. *)
    | SV_Struct {
        def;
        packed_dims;
    } -> failwith "dump394"
    (* /// An enum type. *)
    | SV_Enum {
        def;
        packed_dims;
    } -> failwith "dump395"
    (* /// A `string`. *)
    | SV_String -> failwith "dump396"
    (* /// A `chandle`. *)
    | SV_Chandle -> failwith "dump397"
    (* /// A named type. *)
    | SV_Named {
        path;
        packed_dims;
    } -> failwith "dump398"
    (* /// An `event`. *)
    | SV_Event -> failwith "dump399"
    (* /// A type reference, like `type(<type>)` or `type(<expr>)`. *)
    | SV_TypeRef (astTypeOrExpr1) -> failwith "dump400"

(* /// An integer type. *)

and (dumpIntType:astIntType -> unit) = function 
    (* /// A `bit`. *)
    | SV_Bit -> failwith "dump401"
    (* /// A `logic`. *)
    | SV_Logic -> failwith "dump402"
    (* /// A `reg`. *)
    | SV_Reg -> failwith "dump403"
    (* /// A `byte`. *)
    | SV_Byte -> failwith "dump404"
    (* /// A `shortint`. *)
    | SV_ShortInt -> failwith "dump405"
    (* /// An `int`. *)
    | SV_Int -> failwith "dump406"
    (* /// A `longint`. *)
    | SV_LongInt -> failwith "dump407"
    (* /// An `integer`. *)
    | SV_Integer -> failwith "dump408"
    (* /// A `time`. *)
    | SV_Time -> failwith "dump409"

(* /// A real type. *)

and (dumpRealType:astRealType -> unit) = function 
    (* /// A `shortreal`. *)
    | SV_ShortReal -> failwith "dump410"
    (* /// A `real`. *)
    | SV_Real -> failwith "dump411"
    (* /// A `realtime`. *)
    | SV_RealTime -> failwith "dump412"

(* /// An implicit data type. *)
(* /// *)
(* /// From §A.2.2.1: *)
(* /// ```text *)
(* /// implicit_data_type ::= *)
(* ///     signing? packed_dimension* *)
(* /// ``` *)

and (dumpImplicitDataType:('asta)astImplicitDataType -> unit) = function  {
        signing;
        packed_dims;

} -> failwith "dump413"

(* /// A possible implicit data type. *)
(* /// *)
(* /// From §A.2.2.1: *)
(* /// ```text *)
(* /// data_type_or_implicit ::= *)
(* ///     data_type *)
(* ///     implicit_data_type *)
(* /// ``` *)

and (dumpDataTypeOrImplicit:('asta)astDataTypeOrImplicit -> unit) = function 
    (* /// An explicit data type. *)
    | SV_Explicit (astDataType1) -> failwith "dump414"
    (* /// An implicit data type. *)
    | SV_Implicit (astImplicitDataType1) -> failwith "dump415"

(* /// A variable dimension. *)
(* /// *)
(* /// From §A.2.5: *)
(* /// ```text *)
(* /// unsized_dimension *)
(* /// unpacked_dimension *)
(* /// associative_dimension *)
(* /// queue_dimension *)
(* /// ``` *)

and (dumpVarDim:('asta)astVarDim -> unit) = function 
    (* /// An unsized dimension, like `[]`. *)
    | SV_Unsized -> failwith "dump416"
    (* /// An unpacked dimension. *)
    | SV_Unpacked (astUnpackedDim1) -> failwith "dump417"
    (* /// An associative dimension, like `[<type>]` or `[*]`. *)
    | SV_Assoc (astType1) -> failwith "dump418"
    (* /// A queue dimension, like `[$]` or `[$:42]`. *)
    | SV_Queue (astExpr1) -> failwith "dump419"

(* /// A packed dimension. *)
(* /// *)
(* /// From §A.2.5: *)
(* /// ```text *)
(* /// "[" constant_range "]" *)
(* /// unsized_dimension *)
(* /// ``` *)

and (dumpPackedDim:('asta)astPackedDim -> unit) = function 
    (* /// Such as `[41:0]`. *)
    | SV_Range (astExpr1, astExpr2) -> failwith "dump420"
    (* /// Such as `[]`. *)
    | SV_Unsized -> failwith "dump421"

(* /// An unpacked dimension. *)
(* /// *)
(* /// From §A.2.5: *)
(* /// ```text *)
(* /// "[" constant_range "]" *)
(* /// "[" constant_expression "]" *)
(* /// ``` *)

and (dumpUnpackedDim:('asta)astUnpackedDim -> unit) = function 
    (* /// Such as `[41:0]`. *)
    | SV_Range (astExpr1, astExpr2) -> failwith "dump422"
    (* /// Such as `[42]`. *)
    | SV_Expr (astExpr1) -> failwith "dump423"

(* /// A segment of a type name. *)
(* /// *)
(* /// Adapted from §A.2.2.1: *)
(* /// ```text *)
(* /// path_segment ::= *)
(* ///     "$unit" *)
(* ///     package_identifier *)
(* ///     class_identifier (param_value_assignment)? *)
(* ///     type_identifier *)
(* ///     covergroup_identifier *)
(* /// ``` *)

and (dumpPathSegment:('asta)astPathSegment -> unit) = function 
    (* /// A `$unit`. *)
    | SV_Unit -> failwith "dump424"
    (* /// A package, type, covergroup, or class identifier. *)
    | SV_Ident (astName1) -> failwith "dump425"
    (* /// A class identifier with specializations. *)
    | SV_Class (astName1, astParamAssignment2) -> failwith "dump426"

