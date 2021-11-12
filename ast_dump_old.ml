open Ast_types_old
open Printf

let dump = function _ -> ()
let dumpchar = function _ -> ()
let dumpSpan = function _ -> ()
let dumpSpanned = function _ -> ()
let dumpbool = function _ -> ()
let dumpName = function _ -> ()
let dumpKw = function _ -> ()
let dumpusize = function _ -> ()
let dumpdyn = function _ -> ()
let dumpPropSpec = function _ -> ()
let dumpNodeId = function _ -> ()
let dumpCell = function _ -> ()
let dumpAnyNode = function _ -> ()
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

} -> ()

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

and (dumpRoot:('asta)astRoot -> unit) = function  {
        files;

} -> Array.iter (dumpSourceFile) files

(* /// An entire source file. *)

and (dumpSourceFile:('asta)astSourceFile -> unit) = function  {
        timeunits;
        items;

} -> Array.iter (dumpItem) items

(* /// An item that may appear in a hierarchical scope. *)
(* /// *)
(* /// This includes the following scopes: *)
(* /// - file root *)
(* /// - modules *)
(* /// - interfaces *)
(* /// - packages *)
(* /// - classes *)
(* /// - generates *)

and (dumpItem:('asta)astItem -> unit) = function 
    | SV_Dummy -> ()
    | SV_ModuleDecl (astModule1) -> dumpModule (astModule1)
    | SV_InterfaceDecl (astInterface1) -> failwith "dumpItem1"
    | SV_PackageDecl (astPackage1) -> failwith "dumpItem2"
    | SV_ClassDecl (astClassDecl1) -> failwith "dumpItem3"
    | SV_ProgramDecl -> failwith "dumpItem4"
    | SV_ImportDecl (astImportDecl1) -> failwith "dumpItem5"
    | SV_DpiDecl (astDpiDecl1) -> failwith "dumpItem6"
    | SV_ParamDecl (astParamDecl1) -> dumpParamDecl(astParamDecl1)
    | SV_ModportDecl (astModport1) -> failwith "dumpItem8"
    | SV_Typedef (astTypedef1) -> failwith "dumpItem9"
    | SV_PortDecl (astPortDecl1) -> failwith "dumpItem10"
    | SV_Procedure (astProcedure1) -> dumpProcedure (astProcedure1)
    | SV_SubroutineDecl (astSubroutineDecl1) -> dumpSubroutineDecl (astSubroutineDecl1)
    | SV_ContAssign (astContAssign1) -> dumpContAssign (astContAssign1)
    | SV_GenvarDecl (astGenvarDecl1) -> failwith "dumpItem14"
    | SV_GenerateRegion (astSpan1, astItem2) -> Array.iter dumpItem (astItem2)
    | SV_GenerateFor (astGenerateFor1) -> failwith "dumpItem16"
    | SV_GenerateIf (astGenerateIf1) -> dumpGenerateIf (astGenerateIf1)
    | SV_GenerateCase (astGenerateCase1) -> failwith "dumpItem18"
    | SV_Assertion (astAssertion1) -> failwith "dumpItem19"
    | SV_NetDecl (astNetDecl1) -> dumpNetDecl astNetDecl1
    | SV_VarDecl (astVarDecl1) -> dumpVarDecl astVarDecl1
    | SV_Inst (astInst1) -> dumpInst astInst1
    | SV_Timeunit (astTimeunit1) -> failwith "dumpItem23"

(* /// A module. *)

and (dumpModule:('asta)astModule -> unit) = function  {
        lifetime;
    (* // default static *)
        name;
        imports;
        params;
        ports;
        items;

} -> 
print_endline (name^"("^dump_array dumpPort ", " ports^");");
Array.iter (dumpItem) items;
()

(* /// An interface. *)

and (dumpInterface:('asta)astInterface -> unit) = function  {
        lifetime;
    (* // default static *)
        name;
        params;
        ports;
        items;

} -> ()

(* /// A package. *)

and (dumpPackage:('asta)astPackage -> unit) = function  {
        lifetime;
        name;
        items;

} -> ()

(* /// Lifetime specifier for variables, tasks, and functions. Defaults to static. *)

and (dumpLifetime:astLifetime -> unit) = function 
    | SV_Static -> ()
    | SV_Automatic -> ()

(* /// A time unit specification. *)
(* /// *)
(* /// ```text *)
(* /// "timeunit" time_literal ["/" time_literal] ";" *)
(* /// "timeprecision" time_literal ";" *)
(* /// ``` *)

and (dumpTimeunit:astTimeunit -> unit) = function  {
        unit;
        prec;

} -> ()

(* /// A type. *)

and (dumpType:('asta)astType -> unit) = function  {
        kind;
        sign;
        dims;

} -> ()

(* /// A type without sign and packed dimensions. *)

and (dumpTypeKind:('asta)astTypeKind -> unit) = function 
    | SV_ImplicitType -> ()
    | SV_VoidType -> ()
    | SV_NamedType (astName1) -> ()
    | SV_StringType -> ()
    | SV_ChandleType -> ()
    | SV_VirtIntfType (astName1) -> ()
    | SV_EventType -> ()
    | SV_MailboxType -> ()
    | SV_ImplicitSignedType -> ()
    | SV_ImplicitUnsignedType -> ()
    (* // Scoping *)
    | SV_ScopedType {
        ty;
        member;
        name;
    } -> ()
    (* // Forward declarations *)
    | SV_ForwardType {
        kind;
    } -> ()
    (* // Integer Vector Types *)
    | SV_BitType -> ()
    | SV_LogicType -> ()
    | SV_RegType -> ()
    (* // Integer Atom Types *)
    | SV_ByteType -> ()
    | SV_ShortIntType -> ()
    | SV_IntType -> ()
    | SV_IntegerType -> ()
    | SV_LongIntType -> ()
    | SV_TimeType -> ()
    (* // Non-integer Types *)
    | SV_ShortRealType -> ()
    | SV_RealType -> ()
    | SV_RealtimeType -> ()
    (* // Enumerations *)
    | SV_EnumType (astEnum1) -> ()
    | SV_StructType (astStruct1) -> ()
    (* // Specialization *)
    | SV_SpecializedType (astType1, astParamAssignment2) -> ()
    (* /// Type reference, such as `type(x)` or `type(int)`. *)
    | SV_TypeRef (astTypeOrExpr1) -> ()


and (dumpTypeSign:astTypeSign -> unit) = function 
    | SV_None -> ()
    | SV_Signed -> ()
    | SV_Unsigned -> ()


and (dumpTypeDim:('asta)astTypeDim -> unit) = function 
    | SV_Expr (astExpr1) -> ()
    | SV_Range (astExpr1, astExpr2) -> ()
    | SV_Queue (astExpr1) -> ()
    | SV_Unsized -> ()
    | SV_Associative (astType1) -> ()

(* /// An enum definition. *)
(* /// *)
(* /// For example `enum { FOO = 42 }`. *)

and (dumpEnum:('asta)astEnum -> unit) = function  {
        base_type;
        variants;

} -> ()

(* /// A single entry in an enum. *)
(* /// *)
(* /// For example the `FOO = 42` in `enum { FOO = 42 }`. *)

and (dumpEnumName:('asta)astEnumName -> unit) = function  {
        name;
        range;
        value;

} -> ()

(* /// A struct definition. *)
(* /// *)
(* /// For example `struct packed { byte foo; }`. *)

and (dumpStruct:('asta)astStruct -> unit) = function  {
        kind;
        packed;
        signing;
        members;

} -> ()


and (dumpStructKind:astStructKind -> unit) = function 
    (* /// A `struct`. *)
    | SV_Struct -> ()
    (* /// A `union`. *)
    | SV_Union -> ()
    (* /// A `union tagged`. *)
    | SV_TaggedUnion -> ()

(* /// A struct field. *)
(* /// *)
(* /// For example the `byte foo;` in `struct packed { byte foo; }`. *)

and (dumpStructMember:('asta)astStructMember -> unit) = function  {
        rand_qualifier;
        ty;
        names;

} -> ()

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

} -> ()

(* /// Whether a declaration is a variable or a net. *)

and (dumpVarKind:astVarKind -> unit) = function 
    (* /// A variable declaration. *)
    | SV_Var -> ()
    (* /// A net declaration. *)
    | SV_Net {
        ty;
    (* /// The net type, such as `wire` or `trireg`. *)
        kind;
    (* /// Additional `vectored` or `scalared` specifier. *)
    } -> ()


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

and (dumpProcedure:('asta)astProcedure -> unit) = function  {
        kind;
        stmt;

} -> print_endline (dumpProcedureKind kind)


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
    | SV_All -> ()
    | SV_Any -> ()
    | SV_None -> ()


and (dumpUniquePriority:astUniquePriority -> unit) = function 
    | SV_Unique -> ()
    | SV_Unique0 -> ()
    | SV_Priority -> ()


and (dumpCaseKind:astCaseKind -> unit) = function 
    | SV_Normal -> ()
    | SV_DontCareZ -> ()
    | SV_DontCareXZ -> ()


and (dumpCaseMode:astCaseMode -> unit) = function 
    | SV_Normal -> ()
    | SV_Inside -> ()
    | SV_Pattern -> ()


and (dumpCaseItem:('asta)astCaseItem -> unit) = function 
    | SV_Default (astStmt1) -> ()
    | SV_Expr (astExpr1, astStmt2) -> ()


and (dumpDelayControl:('asta)astDelayControl -> unit) = function  {
        span;
        expr;

} -> ()


and (dumpEventControl:('asta)astEventControl -> unit) = function  {
        span;
        data;

} -> ()


and (dumpEventControlData:('asta)astEventControlData -> unit) = function 
    | SV_Implicit -> ()
    | SV_Expr (astEventExpr1) -> ()


and (dumpCycleDelay:astCycleDelay -> unit) = function  _ -> ()

and (dumpTimingControl:('asta)astTimingControl -> unit) = function 
    | SV_Delay (astDelayControl1) -> ()
    | SV_Event (astEventControl1) -> ()
    | SV_Cycle (astCycleDelay1) -> ()


and (dumpAssignOp:astAssignOp -> unit) = function 
    | SV_Identity -> ()
    | SV_Add -> ()
    | SV_Sub -> ()
    | SV_Mul -> ()
    | SV_Div -> ()
    | SV_Mod -> ()
    | SV_BitAnd -> ()
    | SV_BitOr -> ()
    | SV_BitXor -> ()
    | SV_LogicShL -> ()
    | SV_LogicShR -> ()
    | SV_ArithShL -> ()
    | SV_ArithShR -> ()

(* /// A variable declaration. *)
(* /// *)
(* /// For example `logic x, y, z`. *)

and (dumpVarDecl:('asta)astVarDecl -> unit) = function  {
        konst;
        var;
        lifetime;
        ty;
        names;

} -> print_endline (dump_array (dumpVarDeclName) "; " names)

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

} -> ()

(* /// A foreach-loop index variable. *)

and (dumpForeachIndex:('asta)astForeachIndex -> unit) = function  {
    (* /// The name of the index. *)
        name;
    (* /// At which index nesting level this index is located. *)
        index;

} -> ()

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
    } -> failwith "dumpExpr10"
    | SV_BinaryExpr {
        op;
        lhs;
        rhs;
    } -> sprintf "(%s) %s (%s)" (dumpExpr lhs) (dumpOp op) (dumpExpr rhs)
    | SV_TernaryExpr {
        cond;
        true_expr;
        false_expr;
    } -> failwith "dumpExpr12"
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
    } -> failwith "dumpExpr21"
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
    | SV_Type (astType1) -> ()
    | SV_Expr (astExpr1) -> ()


and (dumpValueRange:('asta)astValueRange -> unit) = function 
    | SV_Single (astExpr1) -> ()
    | SV_Range {
        lo;
        hi;
        span;
    } -> ()


and (dumpRangeMode:astRangeMode -> unit) = function 
    | SV_Absolute -> ()
    | SV_RelativeDown -> ()
    | SV_RelativeUp -> ()


and (dumpIdentifier:astIdentifier -> unit) = function  {
        span;
        name;

} -> ()


and (dumpCallArg:('asta)astCallArg -> string) = function  {
        span;
        name_span;
        name;
        expr;

} -> match name with Some nam -> nam | None -> ""


and (dumpStreamConcatSlice:('asta)astStreamConcatSlice -> unit) = function 
    | SV_Expr (astExpr1) -> ()
    | SV_Type (astType1) -> ()


and (dumpStreamExpr:('asta)astStreamExpr -> unit) = function  {
        expr;
        range;

} -> ()


and (dumpEventExpr:('asta)astEventExpr -> unit) = function 
    | SV_Edge {
        span;
        edge;
        value;
    } -> ()
    | SV_Iff {
        span;
        expr;
        cond;
    } -> ()
    | SV_Or {
        span;
        lhs;
        rhs;
    } -> ()


and (dumpEdgeIdent:astEdgeIdent -> unit) = function 
    | SV_Implicit -> ()
    | SV_Edge -> ()
    | SV_Posedge -> ()
    | SV_Negedge -> ()

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

} -> ()


and (dumpClassItem:('asta)astClassItem -> unit) = function  {
        span;
        qualifiers;
        data;

} -> ()


and (dumpClassItemQualifier:astClassItemQualifier -> unit) = function 
    | SV_Static -> ()
    | SV_Protected -> ()
    | SV_Local -> ()
    | SV_Rand -> ()
    | SV_Randc -> ()
    | SV_Pure -> ()
    | SV_Virtual -> ()
    | SV_Const -> ()


and (dumpClassItemData:('asta)astClassItemData -> unit) = function 
    | SV_Property -> ()
    | SV_Typedef (astTypedef1) -> ()
    | SV_SubroutineDecl (astSubroutineDecl1) -> ()
    | SV_ExternSubroutine (astSubroutinePrototype1) -> ()
    | SV_Constraint (astConstraint1) -> ()
    | SV_ClassDecl -> ()
    | SV_CovergroupDecl -> ()
    | SV_ParamDecl (astParamDecl1) -> ()
    | SV_Null -> ()


and (dumpRandomQualifier:astRandomQualifier -> unit) = function 
    | SV_Rand -> ()
    | SV_Randc -> ()

(* /// A type definition. *)
(* /// *)
(* /// For example `typedef int my_type_t`. *)

and (dumpTypedef:('asta)astTypedef -> unit) = function  {
        name;
        ty;
        dims;

} -> ()


and (dumpConstraint:('asta)astConstraint -> unit) = function  {
        span;
        kind;
        statik;
        name;
        name_span;
        items;

} -> ()


and (dumpConstraintKind:astConstraintKind -> unit) = function 
    | SV_Decl -> ()
    | SV_Proto -> ()
    | SV_ExternProto -> ()
    | SV_PureProto -> ()


and (dumpConstraintItem:('asta)astConstraintItem -> unit) = function  {
        span;
        data;

} -> ()


and (dumpConstraintItemData:('asta)astConstraintItemData -> unit) = function 
    | SV_If -> ()
    | SV_Foreach -> ()
    | SV_Expr (astExpr1) -> ()

(* /// A function or task declaration. *)

and (dumpSubroutineDecl:('asta)astSubroutineDecl -> unit) = function  {
        prototype;
        items;

} ->
dumpSubroutinePrototype prototype;
print_endline (dump_array dumpSubroutineItem ";\n" items)

(* /// A function or task prototype. *)

and (dumpSubroutinePrototype:('asta)astSubroutinePrototype -> unit) = function  {
        kind;
        lifetime;
        name;
        args;
        retty;

} -> print_endline (dumpSubroutineKind kind);


and (dumpSubroutineKind:astSubroutineKind -> string) = function 
    | SV_Func -> "function"
    | SV_Task -> "task"

(* /// A function or task port. *)

and (dumpSubroutinePort:('asta)astSubroutinePort -> unit) = function  {
        dir;
        var;
        ty;
        name;

} -> ()


and (dumpSubroutinePortName:('asta)astSubroutinePortName -> unit) = function  {
        name;
        dims;
        expr;

} -> ()


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
    | SV_Input -> ()
    | SV_Output -> ()
    | SV_Inout -> ()
    | SV_Ref -> ()
    | SV_ConstRef -> ()

(* /// A net declaration. *)
(* /// *)
(* /// For example `wire x, y, z`. *)

and (dumpNetDecl:('asta)astNetDecl -> unit) = function  {
        net_type;
        strength;
        kind;
        ty;
        delay;
        names;

} -> print_endline (dump_array (dumpVarDeclName) ";\n" names)


and (dumpNetKind:astNetKind -> unit) = function 
    | SV_Vectored -> ()
    | SV_Scalared -> ()
    | SV_None -> ()


and (dumpNetStrength:astNetStrength -> unit) = function 
    | SV_Drive (astDriveStrength1, astDriveStrength2) -> ()
    | SV_Charge (astChargeStrength1) -> ()


and (dumpDriveStrength:astDriveStrength -> unit) = function 
    | SV_Supply0 -> ()
    | SV_Strong0 -> ()
    | SV_Pull0 -> ()
    | SV_Weak0 -> ()
    | SV_HighZ0 -> ()
    | SV_Supply1 -> ()
    | SV_Strong1 -> ()
    | SV_Pull1 -> ()
    | SV_Weak1 -> ()
    | SV_HighZ1 -> ()


and (dumpChargeStrength:astChargeStrength -> unit) = function 
    | SV_Small -> ()
    | SV_Medium -> ()
    | SV_Large -> ()

(* /// A field in a `'{...}` pattern. *)

and (dumpPatternField:('asta)astPatternField -> unit) = function 
    | SV_Default (astExpr1) -> ()
    | SV_Member (astExpr1, astExpr2) -> ()
    | SV_Type (astType1, astExpr2) -> ()
    | SV_Expr (astExpr1) -> ()
    | SV_Repeat (astExpr1, astExpr2) -> ()

(* /// An import declaration. *)
(* /// *)
(* /// For example `import a::b, c::*`. *)

and (dumpImportDecl:('asta)astImportDecl -> unit) = function  {
        items;

} -> ()

(* /// A single import. *)
(* /// *)
(* /// For example the `a::b` in `import a::b, c::*`. *)

and (dumpImportItem:('asta)astImportItem -> unit) = function  {
        pkg;
        name;
    (* // None means `import pkg::*` *)

} -> ()


and (dumpAssertion:('asta)astAssertion -> unit) = function  {
        span;
        label;
        data;

} -> ()


and (dumpAssertionData:('asta)astAssertionData -> unit) = function 
    | SV_Immediate (astBlockingAssertion1) -> ()
    | SV_Deferred (astAssertionDeferred1, astBlockingAssertion2) -> ()
    | SV_Concurrent (astConcurrentAssertion1) -> ()


and (dumpAssertionDeferred:astAssertionDeferred -> unit) = function 
    (* /// `assert #0` *)
    | SV_Observed -> ()
    (* /// `assert final` *)
    | SV_Final -> ()


and (dumpBlockingAssertion:('asta)astBlockingAssertion -> unit) = function 
    | SV_Assert (astExpr1, astAssertionActionBlock2) -> ()
    | SV_Assume (astExpr1, astAssertionActionBlock2) -> ()
    | SV_Cover (astExpr1, astStmt2) -> ()


and (dumpConcurrentAssertion:('asta)astConcurrentAssertion -> unit) = function 
    | SV_AssertProperty (astPropSpec1, astAssertionActionBlock2) -> ()
    | SV_AssumeProperty (astPropSpec1, astAssertionActionBlock2) -> ()
    | SV_CoverProperty (astPropSpec1, astStmt2) -> ()
    | SV_CoverSequence -> ()
    | SV_ExpectProperty (astPropSpec1, astAssertionActionBlock2) -> ()
    | SV_RestrictProperty (astPropSpec1) -> ()


and (dumpAssertionActionBlock:('asta)astAssertionActionBlock -> unit) = function 
    | SV_Positive (astStmt1) -> ()
    | SV_Negative (astStmt1) -> ()
    | SV_Both (astStmt1, astStmt2) -> ()


and (dumpSeqExpr:('asta)astSeqExpr -> unit) = function  {
        span;
        data;

} -> ()


and (dumpSeqExprData:('asta)astSeqExprData -> unit) = function 
    | SV_Expr (astExpr1, astSeqRep2) -> ()
    | SV_BinOp (astSeqBinOp1, astSeqExpr2, astSeqExpr3) -> ()
    | SV_Throughout (astExpr1, astSeqExpr2) -> ()
    | SV_Clocked (astEventExpr1, astSeqExpr2) -> ()


and (dumpSeqRep:('asta)astSeqRep -> unit) = function 
    | SV_Consec (astExpr1) -> ()
    (* // [* expr] *)
    | SV_ConsecStar -> ()
    (* // [*] *)
    | SV_ConsecPlus -> ()
    (* // [+] *)
    | SV_Nonconsec (astExpr1) -> ()
    (* // [= expr] *)
    | SV_Goto (astExpr1) -> ()
    (* // [-> expr] *)


and (dumpSeqBinOp:astSeqBinOp -> unit) = function 
    | SV_Or -> ()
    | SV_And -> ()
    | SV_Intersect -> ()
    | SV_Within -> ()


and (dumpPropExpr:('asta)astPropExpr -> unit) = function  {
        span;
        data;

} -> ()


and (dumpPropExprData:('asta)astPropExprData -> unit) = function 
    | SV_SeqOp (astPropSeqOp1, astSeqExpr2) -> ()
    | SV_SeqBinOp (astPropSeqBinOp1, astPropSeqOp2, astSeqExpr3, astPropExpr4) -> ()
    | SV_Not (astPropExpr1) -> ()
    | SV_BinOp (astPropBinOp1, astPropExpr2, astPropExpr3) -> ()
    | SV_Clocked (astEventExpr1, astPropExpr2) -> ()


and (dumpPropSeqOp:astPropSeqOp -> unit) = function 
    | SV_None -> ()
    | SV_Weak -> ()
    | SV_Strong -> ()


and (dumpPropSeqBinOp:astPropSeqBinOp -> unit) = function 
    | SV_ImplOverlap -> ()
    | SV_ImplNonoverlap -> ()
    | SV_FollowOverlap -> ()
    | SV_FollowNonoverlap -> ()


and (dumpPropBinOp:astPropBinOp -> unit) = function 
    | SV_Or -> ()
    | SV_And -> ()
    | SV_Until -> ()
    | SV_SUntil -> ()
    | SV_UntilWith -> ()
    | SV_SUntilWith -> ()
    | SV_Impl -> ()
    | SV_Iff -> ()
    | SV_SeqImplOl -> ()
    | SV_SeqImplNol -> ()
    | SV_SeqFollowOl -> ()
    | SV_SeqFollowNol -> ()

(* /// An instantiation of a module. *)
(* /// *)
(* /// For example `foo u0(), u1();`. *)

and (dumpInst:('asta)astInst -> unit) = function  {
    (* /// The name of the module to instantiate. *)
        target;
    (* /// The parameters in the module to be assigned. *)
        params;
    (* /// The names and ports of the module instantiations. *)
        names;

} -> ()

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

} -> ()

(* /// A modport declaration in an interface. *)
(* /// *)
(* /// For example `modport in (...), out (...);`. *)

and (dumpModport:('asta)astModport -> unit) = function  {
    (* /// The names of the modports. *)
        names;

} -> ()

(* /// A single modport declaration. *)
(* /// *)
(* /// For example the `in (...)` in `modport in (...), out (...);`. *)

and (dumpModportName:('asta)astModportName -> unit) = function  {
    (* /// The name of the modport. *)
        name;
    (* /// The individual port specifications. *)
        ports;

} -> ()

(* /// A modport ports declaration. *)
(* /// *)
(* /// For example `input a, .b(expr)`, or `import ...`, or `clocking foo`. *)

and (dumpModportPort:('asta)astModportPort -> unit) = function 
    (* /// A simple port, for example `input a, .b(expr)`. *)
    | SV_Simple {
        dir;
        port;
    } -> ()

(* /// A single simple modport port. *)
(* /// *)
(* /// For example the `a` or `.b(expr)` in `input a, .b(expr)`. *)

and (dumpModportSimplePort:('asta)astModportSimplePort -> unit) = function  {
    (* /// The name of the port. *)
        name;
    (* /// The optional parenthesized expression of the port. *)
        expr;

} -> ()

(* /// A parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// "localparam" data_type_or_implicit list_of_param_assignments *)
(* /// "localparam" "type" list_of_type_assignments *)
(* /// "parameter" data_type_or_implicit list_of_param_assignments *)
(* /// "parameter" "type" list_of_type_assignments *)
(* /// ``` *)

and (dumpParamDecl:('asta)astParamDecl -> unit) = function  {
        local;
        kind;

} -> ()


and (dumpParamKind:('asta)astParamKind -> unit) = function 
    | SV_Type (astParamTypeDecl1) -> ()
    | SV_Value (astParamValueDecl1) -> ()

(* /// A single type assignment within a parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// ident ["=" type] *)
(* /// ``` *)

and (dumpParamTypeDecl:('asta)astParamTypeDecl -> unit) = function  {
        name;
        ty;

} -> ()

(* /// A single value assignment within a parameter or localparam declaration. *)
(* /// *)
(* /// ```text *)
(* /// [type_or_implicit] ident {dimension} ["=" expr] *)
(* /// ``` *)

and (dumpParamValueDecl:('asta)astParamValueDecl -> unit) = function  {
        ty;
        name;
        dims;
        expr;

} -> ()

(* /// A continuous assignment statement. *)
(* /// *)
(* /// ```text *)
(* /// "assign" [drive_strength] [delay3] list_of_assignments ";" *)
(* /// "assign" [delay_control] list_of_assignments ";" *)
(* /// ``` *)

and (dumpContAssign:('asta)astContAssign -> unit) = function  {
        strength;
        delay;
        delay_control;
        assignments;

} -> ()

(* /// A `for` generate statement. *)

and (dumpGenerateFor:('asta)astGenerateFor -> unit) = function  {
        init;
        cond;
        step;
        block;

} -> ()

(* /// An `if` generate statement. *)

and (dumpGenerateIf:('asta)astGenerateIf -> unit) = function  {
        cond;
        main_block;
        else_block;

} -> ()

(* /// A `case` generate statement. *)

and (dumpGenerateCase:('asta)astGenerateCase -> unit) = function     (* // TODO *)
 _ -> ()
(* /// A body of a generate construct. *)
(* /// *)
(* /// May contains hierarchy items or more generate constructs. *)

and (dumpGenerateBlock:('asta)astGenerateBlock -> unit) = function  {
        label;
        items;

} -> ()


and (dumpParamAssignment:('asta)astParamAssignment -> unit) = function  {
        span;
        name;
        expr;

} -> ()

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
    | SV_Auto -> ()
    (* /// The `.name`, `.name()`, or `.name(expr)` cases, *)
    | SV_Named (astName1, astPortConnMode2) -> ()
    (* /// The `expr` case, *)
    | SV_Positional (astExpr1) -> ()

(* /// How a named port connection is made. *)

and (dumpPortConnMode:('asta)astPortConnMode -> unit) = function 
    (* /// The `.name` case. *)
    | SV_Auto -> ()
    (* /// The `.name()` case. *)
    | SV_Unconnected -> ()
    (* /// The `.name(expr)` case. *)
    | SV_Connected (astExpr1) -> ()

(* /// A DPI declaration such as `import "DPI-C"` or `export "DPI-C"`. *)

and (dumpDpiDecl:('asta)astDpiDecl -> unit) = function 
    (* /// An `import`. *)
    | SV_Import {
        spec;
        property;
        cident;
        prototype;
    } -> ()
    (* /// An `export`. *)
    | SV_Export {
        spec;
        cident;
        kind;
        name;
    } -> ()

(* /// A DPI function/task property. *)

and (dumpDpiProperty:astDpiProperty -> unit) = function 
    (* /// `context` *)
    | SV_Context -> ()
    (* /// `pure` *)
    | SV_Pure -> ()

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
    } -> ()
    (* /// A real type. *)
    | SV_Real (astRealType1) -> ()
    (* /// A struct or union type. *)
    | SV_Struct {
        def;
        packed_dims;
    } -> ()
    (* /// An enum type. *)
    | SV_Enum {
        def;
        packed_dims;
    } -> ()
    (* /// A `string`. *)
    | SV_String -> ()
    (* /// A `chandle`. *)
    | SV_Chandle -> ()
    (* /// A named type. *)
    | SV_Named {
        path;
        packed_dims;
    } -> ()
    (* /// An `event`. *)
    | SV_Event -> ()
    (* /// A type reference, like `type(<type>)` or `type(<expr>)`. *)
    | SV_TypeRef (astTypeOrExpr1) -> ()

(* /// An integer type. *)

and (dumpIntType:astIntType -> unit) = function 
    (* /// A `bit`. *)
    | SV_Bit -> ()
    (* /// A `logic`. *)
    | SV_Logic -> ()
    (* /// A `reg`. *)
    | SV_Reg -> ()
    (* /// A `byte`. *)
    | SV_Byte -> ()
    (* /// A `shortint`. *)
    | SV_ShortInt -> ()
    (* /// An `int`. *)
    | SV_Int -> ()
    (* /// A `longint`. *)
    | SV_LongInt -> ()
    (* /// An `integer`. *)
    | SV_Integer -> ()
    (* /// A `time`. *)
    | SV_Time -> ()

(* /// A real type. *)

and (dumpRealType:astRealType -> unit) = function 
    (* /// A `shortreal`. *)
    | SV_ShortReal -> ()
    (* /// A `real`. *)
    | SV_Real -> ()
    (* /// A `realtime`. *)
    | SV_RealTime -> ()

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

} -> ()

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
    | SV_Explicit (astDataType1) -> ()
    (* /// An implicit data type. *)
    | SV_Implicit (astImplicitDataType1) -> ()

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
    | SV_Unsized -> ()
    (* /// An unpacked dimension. *)
    | SV_Unpacked (astUnpackedDim1) -> ()
    (* /// An associative dimension, like `[<type>]` or `[*]`. *)
    | SV_Assoc (astType1) -> ()
    (* /// A queue dimension, like `[$]` or `[$:42]`. *)
    | SV_Queue (astExpr1) -> ()

(* /// A packed dimension. *)
(* /// *)
(* /// From §A.2.5: *)
(* /// ```text *)
(* /// "[" constant_range "]" *)
(* /// unsized_dimension *)
(* /// ``` *)

and (dumpPackedDim:('asta)astPackedDim -> unit) = function 
    (* /// Such as `[41:0]`. *)
    | SV_Range (astExpr1, astExpr2) -> ()
    (* /// Such as `[]`. *)
    | SV_Unsized -> ()

(* /// An unpacked dimension. *)
(* /// *)
(* /// From §A.2.5: *)
(* /// ```text *)
(* /// "[" constant_range "]" *)
(* /// "[" constant_expression "]" *)
(* /// ``` *)

and (dumpUnpackedDim:('asta)astUnpackedDim -> unit) = function 
    (* /// Such as `[41:0]`. *)
    | SV_Range (astExpr1, astExpr2) -> ()
    (* /// Such as `[42]`. *)
    | SV_Expr (astExpr1) -> ()

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
    | SV_Unit -> ()
    (* /// A package, type, covergroup, or class identifier. *)
    | SV_Ident (astName1) -> ()
    (* /// A class identifier with specializations. *)
    | SV_Class (astName1, astParamAssignment2) -> ()

