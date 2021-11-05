(** Copyright (c) 2016-2021 Fabian Schuiki
**!
**! A generalized SystemVerilog type system.
**!
**! This module covers all types that may arise in a SystemVerilog source file,
**! plus some additional things like modules/interfaces to streamline the
**! handling of hierarchical names and interface signals/modports throughout the
**! compiler.
**!
**! ## Packed Types
**!
**! Packed types are the core types of SystemVerilog. They combine a core packed
**! type with an optional sign and zero or more packed dimensions. The core
**! packed types are:
**!
**! - Integer vector types: `bit`, `logic`, `reg`
**! - Integer atom types: `byte`, `shortint`, `int`, `longint`, `integer`,
**!   `time`
**! - Packed structs and unions
**! - Enums
**! - Packed named types
**! - Packed type references
**!
**! The packed dimensions can be:
**!
**! - Unsized (`[]`)
**! - Ranges (`[x:y]`)
**!
**! Packed types are implemented by the [`PackedType`](struct.PackedType.html)
**! struct.
**!
**! ## Unpacked Types
**!
**! Unpacked types are a second level of types in SystemVerilog. They extend a
**! core unpacked type with a variety of unpacked dimensions, depending on which
**! syntactic construct generated the type (variable or otherwise). The core
**! unpacked types are:
**!
**! - Packed types
**! - Non-integer types: `shortreal`, `real`, `realtime`
**! - Unpacked structs and unions
**! - `string`, `chandle`, `event`
**! - Virtual interfaces
**! - Class types
**! - Covergroups
**! - Unpacked named types
**! - Unpacked type references
**! - Modules and interfaces
**!
**! The unpacked dimensions are:
**!
**! - Unsized (`[]`)
**! - Arrays (`[x]`)
**! - Ranges (`[x:y]`)
**! - Associative (`[T]` or `[*]`)
**! - Queues (`[$]` or `[$:x]`)
**!
**! Unpacked types are implemented by the [`UnpackedType`](struct.UnpackedType.html)
**! struct.
**!
**! ## Simple Bit Vector Types
**!
**! Some packed types may be converted into an equivalent Simple Bit Vector Type
**! (SBVT), which has an identical bit pattern as the original type, but
**! consists only of an integer vector type with a single optional packed
**! dimension. An SBVT can be converted back into a packed type. SBVTs can be
**! range-cast, which changes the width of their single dimension.
**!
**! SBVTs track whether the original packed type explicitly had a dimension or
**! used an integer atom type, or was named. When converting back to a packed
**! type, the SBVT attempts to restore this information, depending on how many
**! changes were applied.
**!
**! ## Sign
**!
**! All packed types have an associated sign, indicating whether they are
**! *signed* or *unsigned*. The types have a default sign, which means that
**! the sign may have been omitted in the source file. Packed types can be
**! sign-cast, which changes only their sign.
**!
**! ## Domain
**!
**! All packed types consist of bits that can either carry two or four values.
**! An aggregate type is two-valued iff *all* its constituent types are
**! two-valued, otherwise it is four-valued. Packed types can be domain-cast,
**! which changes only their value domain.
**)

type isize = int
type usize = int
type svSpanned = Spanned of string
type svSpan = int
type svParamEnv = unit
type svName = string
type svEnum = int
type svIdentifier = string
type svModule = unit
type svInterface = unit
type svModportName = string

type enumTimeUnit =
    Second|
    MilliSecond|
    MicroSecond|
    NanoSecond|
    PicoSecond|
    FemtoSecond

type enumLit =
    Str of svName|
    BasedInteger of (svName option * bool * char * svName)|
    (* One of `'0`, `'1`, `'x`, and `'z`. *)
    UnbasedUnsized of (char)|
    (* A number given as integer and optional fractional part. *)
    Number of (svName * svName option)|
    (* A time literal given as integer part, fractional part, and unit. *)
    Time of (svName * svName option * enumTimeUnit)

type enumOp =
    (* Assignment *)
    Assign|
    AssignAdd|
    AssignSub|
    AssignMul|
    AssignDiv|
    AssignMod|
    AssignBitAnd|
    AssignBitOr|
    AssignBitXor|
    AssignLogicShL|
    AssignLogicShR|
    AssignArithShL|
    AssignArithShR|

    (* Arithmetic *)
    Add|
    Sub|
    Mul|
    Div|
    Mod|
    Pow|
    Inc|
    Dec|

    (* Equality *)
    LogicEq|
    LogicNeq|
    CaseEq|
    CaseNeq|
    WildcardEq|
    WildcardNeq|

    (* Relational *)
    Lt|
    Leq|
    Gt|
    Geq|

    (* Logic *)
    LogicNot|
    LogicAnd|
    LogicOr|
    LogicImpl|
    LogicEquiv|

    (* Bitwise *)
    BitNot|
    BitAnd|
    BitNand|
    BitOr|
    BitNor|
    BitXor|
    BitXnor|
    BitNxor|

    (* Shift *)
    LogicShL|
    LogicShR|
    ArithShL|
    ArithShR|

    (* Sequence *)
    SeqImplOl|
    SeqImplNol|
    SeqFollowOl|
    SeqFollowNol

type enumAssignOp =
    Identity|
    Add|
    Sub|
    Mul|
    Div|
    Mod|
    BitAnd|
    BitOr|
    BitXor|
    LogicShL|
    LogicShR|
    ArithShL|
    ArithShR

type enumStructKind =
    (* A `struct`. *)
    Struct|
    (* A `union`. *)
    Union|
    (* A `union tagged`. *)
    TaggedUnion

type enumTypeSign =
    None|
    Signed|
    Unsigned

type enumRandomQualifier =
    Rand|
    Randc

type enumTypeKind =
    ImplicitType|
    VoidType|
    NamedType of svSpanned|
    StringType|
    ChandleType|
    VirtIntfTyp of svName|
    EventType|
    MailboxType|
    ImplicitSignedType|
    ImplicitUnsignedType|

    (* Scoping *)
    ScopedType of {
        ty: structType;
        member: bool;
        name: svSpanned;
    }|

    (* Forward declarations *)
    ForwardType of {
        kind: enumTypeKind;
    }|

    (* Integer Vector Types *)
    BitType|
    LogicType|
    RegType|

    (* Integer Atom Types *)
    ByteType|
    ShortIntType|
    IntType|
    IntegerType|
    LongIntType|
    TimeType|

    (* Non-integer Types *)
    ShortRealType|
    RealType|
    RealtimeType|

    (* Enumerations *)
    EnumType of svEnum|
    StructType of structStruct|

    (* Specialization *)
    SpecializedType of (structType * structParamAssignment array)|

    (* Type reference, such as `type(x)` or `type(int)`. *)
    TypeRef of enumTypeOrExpr

and astStructMember = {
    rand_qualifier: enumRandomQualifier;
    ty: structType;
    names: structVarDeclName array;
}

and structParamAssignment = {
    span: svSpan;
    name: svIdentifier option;
    expr: enumTypeOrExpr;
}

and enumTypeOrExpr =
    Type of structType|
    Expr of enumExpr

and structStruct = {
    kind: enumStructKind;
    packed: bool;
    signing: enumTypeSign;
    members: structStructMember array;
}

and structType = {
    kind: enumTypeKind;
    sign: enumTypeSign;
    dims: enumTypeDim array;
}

and enumExpr =
    DummyExpr|
    LiteralExpr of enumLit|
    (* An identifier, like `foo`. *)
    IdentExpr of svSpanned|
    (* A system identifier, like `$foo`. *)
    SysIdentExpr of svSpanned|
    ThisExpr|
    DollarExpr|
    NullExpr|
    ScopeExpr of enumExpr * svSpanned|
    IndexExpr of {
        indexee: enumExpr;
        index: enumExpr;
    }|
    UnaryExpr of {
        op: enumOp;
        expr: enumExpr;
        postfix: bool;
    }|
    BinaryExpr of {
        op: enumOp;
        lhs: enumExpr;
        rhs: enumExpr;
    }|
    TernaryExpr of {
        cond: enumExpr;
        true_expr: enumExpr;
        false_expr: enumExpr;
    }|
    AssignExpr of {
        op: enumAssignOp;
        lhs: enumExpr;
        rhs: enumExpr;
    }|
    CallExpr of (enumExpr * structCallArg array)|
    ConstructorCallExpr of (structCallArg array)|
    ClassNewExpr of (enumExpr option)|
    ArrayNewExpr of (enumExpr * enumExpr option)|
    EmptyQueueExpr|
    StreamConcatExpr of {
        slice: enumStreamConcatSlice;
        exprs: structStreamExpr array;
    }|
    ConcatExpr of {
        repeat: enumExpr option;
        exprs: enumExpr array;
    }|
    MinTypMaxExpr of {
        min: enumExpr;
        typ: enumExpr;
        max: enumExpr;
    }|
    RangeExpr of {
        mode: enumRangeMode;
        lhs: enumExpr;
        rhs: enumExpr;
    }|
    (* A member expression, like `a.b`. *)
    MemberExpr of {
        expr: enumExpr;
        name: svSpanned;
    }|
    PatternExpr of (enumPatternField array)|
    InsideExpr of (enumExpr * enumValueRange array)|
    CastExpr of (structType * enumExpr)|
    CastSizeExpr of (enumExpr * enumExpr)|
    CastSignExpr of (svSpanned * enumExpr)|
    (* A `$bits` call. *)
    BitsExpr of {
        name: svSpanned;
        arg: enumTypeOrExpr;
    }

and enumTypeDim =
    Expr of (enumExpr)|
    Range of (enumExpr * enumExpr)|
    Queue of (enumExpr option)|
    Unsized|
    Associative of (structType option)

and structCallArg = {
    span: svSpan;
    name_span: svSpan;
    name: svName option;
    expr: enumExpr option;
}

and enumStreamConcatSlice =
    Expr of enumExpr |
    Type of structType

and structStreamExpr = {
    expr: enumExpr;
    range: enumExpr option;
}

and enumRangeMode =
    Absolute|
    RelativeDown|
    RelativeUp

and enumPatternField =
    Default of enumExpr|
    Member of enumExpr * enumExpr|
    Type of structType * enumExpr|
    Expr of enumExpr|
    Repeat of (enumExpr * enumExpr array)

and enumValueRange =
    Single of enumExpr |
    Range of {
        lo: enumExpr;
        hi: enumExpr;
        span: svSpan;
    }

and structVarDeclName = {
    name: svName;
    name_span: svSpan;
    dims: enumTypeDim array;
    init: enumExpr option;
}

(* A packed type. *)
and structPackedType = {
    (* The core packed type. *)
    core: enumPackedCore;
    (* The type sign. *)
    sign: enumSign;
    (* Whether the sign was explicit in the source code. *)
    sign_explicit: bool;
    (* The packed dimensions. *)
    dims: enumPackedDim array;
    (* This type with one level of name/reference resolved. *)
    resolved: structPackedType option;
    (* This type with all names/references recursively resolved. *)
    resolved_full: structPackedType option;
}

(* A core packed type. *)
and enumPackedCore =
    (* An error occurred during type computation. *)
    Error |
    (* Void. *)
    Void |
    (* An integer vector type. *)
    IntVec of enumIntVecType |
    (* An integer atom type. *)
    IntAtom of enumIntAtomType |
    (* A packed struct. *)
    Struct of structStructType |
    (* An enum. *)
    Enum of structEnumType |
    (* A named type. *)
    Named of {
        (* How the user originally called the type. *)
        name: svSpanned;
        (* The type this name expands to. *)
        ty: structPackedType;
    } |
    (* A type reference. *)
    Ref of {
        (* Location of the `type(...)` in the source file. *)
        span: svSpan;
        (* The type that this reference expands to. *)
        ty: structPackedType;
    }

(* A packed dimension. *)
and enumPackedDim =
    (* A range dimension, like `[a:b]`. *)
    Range of structRange |
    (* A unsized dimension, like `[]`. *)
    Unsized

(* An integer vector type. *)
(* *)
(* These are the builtin single-bit integer types. *)
and enumIntVecType =
    (* A `bit`. *)
    Bit |
    (* A `logic`. *)
    Logic |
    (* A `reg`. *)
    Reg

(* An integer atom type. *)
(* *)
(* These are the builtin multi-bit integer types. *)
and enumIntAtomType =
    (* A `byte`. *)
    Byte |
    (* A `shortint`. *)
    ShortInt |
    (* An `int`. *)
    Int |
    (* A `longint`. *)
    LongInt |
    (* An `integer`. *)
    Integer |
    (* A `time`. *)
    Time

(* The number of values each bit of a type can assume. *)
and enumDomain =
    (* Two-valued types such as `bit` or `int`. *)
    TwoValued |
    (* Four-valued types such as `logic` or `integer`. *)
    FourValued

(* Whether a type is signed or unsigned. *)
and enumSign =
    (* A `signed` type. *)
    Signed |
    (* An `unsigned` type. *)
    Unsigned

(* The `[a:b]` part in a vector/array type such as `logic [a:b]`. *)
and structRange = {
    (* The total number of bits, given as `|a-b|+1`. *)
    size: usize;
    (* The direction of the vector, i.e. whether `a > b` or `a < b`. *)
    dir: enumRangeDir;
    (* The starting offset of the range. *)
    offset: isize;
}

(* Which side is greater in a range `[a:b]`. *)
and enumRangeDir =
    (* `a < b` *)
    Up|
    (* `a > b` *)
    Down

(* An unpacked type. *)
and structUnpackedType = {
    (* The core unpacked type. *)
    core: enumUnpackedCore;
    (* The unpacked dimensions. *)
    dims: enumUnpackedDim array;
    (* This type with one level of name/reference resolved. *)
    resolved: structUnpackedType option;
    (* This type with all names/references recursively resolved. *)
    resolved_full: structUnpackedType option;
}

(* A core unpacked type. *)
and enumUnpackedCore =
    (* An error occurred during type computation. *)
    Error|
    (* A packed type. *)
    Packed of structPackedType|
    (* A real type. *)
    Real of enumRealType|
    (* A unpacked struct. *)
    Struct of structStructType|
    (* A string. *)
    String|
    (* A chandle. *)
    Chandle|
    (* An event. *)
    Event|
    (* TODO: Add virtual interfaces
     * TODO: Add class types
     * TODO: Add covergroups *)
    (* A named type. *)
    Named of {
        (* How the user originally called the type. *)
        name: svSpanned;
        (* The type this name expands to. *)
        ty: structUnpackedType;
    }|
    (* A type reference. *)
    Ref of {
        (* Location of the `type(...)` in the source file. *)
        span: svSpan;
        (* The type that this reference expands to. *)
        ty: structUnpackedType;
    }|
    (* A module instance. *)
    Module of structModuleType|
    (* An interface instance. *)
    Interface of structInterfaceType

(* An unpacked dimension. *)
and enumUnpackedDim =
    (* A unsized dimension, like `[]`. *)
    Unsized|
    (* An array dimension, like `[a]`. *)
    Array of usize|
    (* A range dimension, like `[a:b]`. *)
    Range of structRange|
    (* An associative dimension, like `[T]` or `[*]`. *)
    Assoc of structUnpackedType option|
    (* A queue dimension, like `[$]` or `[$:a]`. *)
    Queue of usize option

(* A real type. *)
and enumRealType =
    (* A `shortreal`. *)
    ShortReal|
    (* A `real`. *)
    Real|
    (* A `realtime`. *)
    RealTime

(* A struct type. *)
(* *)
(* This represents both packed and unpacked structs. Which one it is depends on *)
(* whether this struct is embedded in a `PackedType` or `UnpackedType`. For the *)
(* packed version the struct inherits its sign from its parent `PackedType`. *)
and structStructType = {
    (* The corresponding AST node of this struct definition. *)
    ast: structStruct;
    (* The AST node of the corresponding type. *)
    ast_type: structType;
    (* Whether this is a `struct`, `union` or `union tagged`. *)
    kind: enumStructKind;
    (* The list of members. *)
    members: structStructMember list;
}

(* A member of a struct type. *)
and structStructMember = {
    (* The name. *)
    name: svSpanned;
    (* The type. *)
    ty: structUnpackedType;
    (* The AST node of the member declaration. *)
    ast_member: astStructMember;
    (* The AST node of the member name. *)
    ast_name: structVarDeclName;
}

(* An enum type. *)
and structEnumType = {
    (* The corresponding AST node of this enum definition. *)
    ast: structEnum;
    (* The base type of this enum. *)
    base: structPackedType;
    (* Whether the base type was explicit in the source code. *)
    base_explicit: bool;
    (* The list of variants. *)
    variants: (svSpanned * structEnumName) array;
}

and structEnum = {
  base_type: structType option;
  variants: structEnumName array;
}

and structEnumName = {
    name: svSpanned;
    range: enumExpr;
    value: enumExpr option;
}
(* A module instance. *)
and structModuleType = {
    (* The AST node of the module. *)
    ast: svModule;
    (* The parametrization of the module. *)
    env: svParamEnv;
}

(* An interface instance. *)
and structInterfaceType = {
    (* The AST node of the interface. *)
    ast: svInterface;
    (* The parametrization of the interface. *)
    env: svParamEnv;
    (* The optional modport that was specified together with the interface. *)
    modport: svModportName option;
}

(* A simple bit vector type. *)
and structSbvType = {
    (* The domain, which dictates whether this is a `bit` or `logic` vector. *)
    domain: enumDomain;
    (* Whether the type used an integer atom like `int` in the source text. *)
    used_atom: bool;
    (* The sign. *)
    sign: enumSign;
    (* Whether the sign was explicit in the source text. *)
    sign_explicit: bool;
    (* The size of the vector. *)
    size: usize;
    (* Whether the single-bit vector had an explicit range in the source text. *)
    (* Essentially whether it was `bit` or `bit[a:a]`. *)
    size_explicit: bool;
}

(* A packed or unpacked dimension. *)
and enumDim =
    (* A packed dimension. *)
    Packed of enumPackedDim|
    (* An unpacked dimension. *)
    Unpacked of enumUnpackedDim

and astPackedDim =
    (* Such as `[41:0]`. *)
    Range of (enumExpr * enumExpr)|
    (* Such as `[]`. *)
    Unsized
