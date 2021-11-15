open Token_types_old

let dump = function _ -> failwith "dump427"
let dumpchar = function _ -> failwith "dump428"
let dumpSpan = function _ -> failwith "dump429"
let dumpSpanned = function _ -> failwith "dump430"
let dumpbool = function _ -> failwith "dump431"
let dumpName = function _ -> failwith "dump432"
let dumpKw = function _ -> failwith "dump433"
let dumpusize = function _ -> failwith "dump434"
let dumpdyn = function _ -> failwith "dump435"
let dumpPropSpec = function _ -> failwith "dump436"
let dumpNodeId = function _ -> failwith "dump437"
let dumpCell = function _ -> failwith "dump438"
let dumpAnyNode = function _ -> failwith "dump439"
(* // Copyright (c) 2016-2021 Fabian Schuiki *)
(* //! Defines all tokens that may result from performing lexical analysis on a *)
(* //! SystemVerilog source file. This module is inspired heavily by the tokens *)
(* //! used in the Rust compiler. *)
(* /// A primary token emitted by the lexer. *)

and (dumpToken:tokenToken -> unit) = function 
    (* // Symbols *)
    | SV_Comma -> failwith "dump440"
    | SV_Period -> failwith "dump441"
    | SV_Colon -> failwith "dump442"
    | SV_Semicolon -> failwith "dump443"
    | SV_At -> failwith "dump444"
    | SV_Hashtag -> failwith "dump445"
    | SV_DoubleHashtag -> failwith "dump446"
    | SV_Namespace -> failwith "dump447"
    | SV_Ternary -> failwith "dump448"
    | SV_AddColon -> failwith "dump449"
    | SV_SubColon -> failwith "dump450"
    | SV_Apostrophe -> failwith "dump451"
    | SV_Dollar -> failwith "dump452"
    | SV_Operator (tokenOp1) -> failwith "dump453"
    (* /// An opening delimiter *)
    | SV_OpenDelim (tokenDelimToken1) -> failwith "dump454"
    (* /// A closing delimiter *)
    | SV_CloseDelim (tokenDelimToken1) -> failwith "dump455"
    (* /// A literal *)
    | SV_Literal (tokenLit1) -> failwith "dump456"
    (* /// A system task or function identifier, e.g. "$display" *)
    | SV_SysIdent (tokenName1) -> failwith "dump457"
    (* /// A compiler directive, e.g. "`timescale" *)
    | SV_CompDir (tokenName1) -> failwith "dump458"
    (* /// An identifier *)
    | SV_Ident (tokenName1) -> failwith "dump459"
    (* /// An escaped identifier *)
    | SV_EscIdent (tokenName1) -> failwith "dump460"
    (* /// An unsigned number *)
    (* // UnsignedNumber(Name), *)
    (* /// A keyword *)
    | SV_Keyword (tokenKw1) -> failwith "dump461"
    (* // The end of the input file *)
    | SV_Eof -> failwith "dump462"

(* /// A delimiter token such as parentheses or brackets. *)

and (dumpDelimToken:tokenDelimToken -> unit) = function 
    (* /// A round paranthesis `(` or `)` *)
    | SV_Paren -> failwith "dump463"
    (* /// A square bracket `[` or `]` *)
    | SV_Brack -> failwith "dump464"
    (* /// A curly brace `{` or `}` *)
    | SV_Brace -> failwith "dump465"
    (* /// A `begin` or `end` *)
    | SV_Bgend -> failwith "dump466"

(* /// Abstract literals such as strings. *)

and (dumpLit:tokenLit -> string) = function 
    | SV_Str (tokenName1) -> tokenName1
    | SV_BasedInteger (tokenName1, tokenbool2, tokenchar3, tokenName4) -> tokenName4
    (* /// One of `'0`, `'1`, `'x`, and `'z`. *)
    | SV_UnbasedUnsized (tokenchar1) -> String.make 1 tokenchar1
    (* /// A number given as integer and optional fractional part. *)
    | SV_Number (tokenName1, tokenName2) -> tokenName1
    (* /// A time literal given as integer part, fractional part, and unit. *)
    | SV_Time (tokenName1, tokenName2, tokenTimeUnit3) -> tokenName1

(* /// The unit of a time literal. *)

and (dumpTimeUnit:tokenTimeUnit -> unit) = function 
    | SV_Second -> failwith "dump472"
    | SV_MilliSecond -> failwith "dump473"
    | SV_MicroSecond -> failwith "dump474"
    | SV_NanoSecond -> failwith "dump475"
    | SV_PicoSecond -> failwith "dump476"
    | SV_FemtoSecond -> failwith "dump477"

(* /// Operator symbols. *)

and (dumpOp:tokenOp -> string) = function 
    (* // Assignment *)
    | SV_Assign -> failwith "dump478"
    | SV_AssignAdd -> failwith "dump479"
    | SV_AssignSub -> failwith "dump480"
    | SV_AssignMul -> failwith "dump481"
    | SV_AssignDiv -> failwith "dump482"
    | SV_AssignMod -> failwith "dump483"
    | SV_AssignBitAnd -> failwith "dump484"
    | SV_AssignBitOr -> failwith "dump485"
    | SV_AssignBitXor -> failwith "dump486"
    | SV_AssignLogicShL -> failwith "dump487"
    | SV_AssignLogicShR -> failwith "dump488"
    | SV_AssignArithShL -> failwith "dump489"
    | SV_AssignArithShR -> failwith "dump490"
    (* // Arithmetic *)
    | SV_Add -> "+"
    | SV_Sub -> "-"
    | SV_Mul -> "*"
    | SV_Div -> "/"
    | SV_Mod -> "%"
    | SV_Pow -> "**"
    | SV_Inc -> "++"
    | SV_Dec -> "--"
    (* // Equality *)
    | SV_LogicEq -> "=="
    | SV_LogicNeq -> "!="
    | SV_CaseEq -> "==="
    | SV_CaseNeq -> "!=="
    | SV_WildcardEq -> failwith "dumpOp14"
    | SV_WildcardNeq -> failwith "dumpOp15"
    (* // Relational *)
    | SV_Lt -> "<"
    | SV_Leq -> "<="
    | SV_Gt -> ">"
    | SV_Geq -> ">="
    (* // Logic *)
    | SV_LogicNot -> "!"
    | SV_LogicAnd -> "&&"
    | SV_LogicOr -> "||"
    | SV_LogicImpl -> failwith "dumpOp23"
    | SV_LogicEquiv -> failwith "dumpOp24"
    (* // Bitwise *)
    | SV_BitNot -> "~"
    | SV_BitAnd -> "&"
    | SV_BitNand -> "~&"
    | SV_BitOr -> "|"
    | SV_BitNor -> "~|"
    | SV_BitXor -> "^"
    | SV_BitXnor -> "~^"
    | SV_BitNxor -> failwith "dumpOp32"
    (* // Shift *)
    | SV_LogicShL -> "<<"
    | SV_LogicShR -> ">>"
    | SV_ArithShL -> "<<<"
    | SV_ArithShR -> ">>>"
    (* // Sequence *)
    | SV_SeqImplOl -> failwith "dumpOp37"
    | SV_SeqImplNol -> failwith "dumpOp38"
    | SV_SeqFollowOl -> failwith "dumpOp39"
    | SV_SeqFollowNol -> failwith "dumpOp40"

(* /// Expression precedence. Note that a few kinds of expression are *)
(* /// right-associative rather than the default left-associative. *)

and (dumpPrecedence:tokenPrecedence -> unit) = function 
    | SV_Min -> failwith "dump530"
    | SV_MinTypMax -> failwith "dump531"
    | SV_Concatenation -> failwith "dump532"
    (* // no associativity *)
    | SV_Assignment -> failwith "dump533"
    (* // no associativity *)
    | SV_Implication -> failwith "dump534"
    (* // right-associative *)
    | SV_Ternary -> failwith "dump535"
    (* // right-associative *)
    | SV_LogicOr -> failwith "dump536"
    | SV_LogicAnd -> failwith "dump537"
    | SV_BitOr -> failwith "dump538"
    | SV_BitXor -> failwith "dump539"
    | SV_BitAnd -> failwith "dump540"
    | SV_Equality -> failwith "dump541"
    | SV_Relational -> failwith "dump542"
    | SV_Shift -> failwith "dump543"
    | SV_Add -> failwith "dump544"
    | SV_Mul -> failwith "dump545"
    | SV_Pow -> failwith "dump546"
    | SV_Unary -> failwith "dump547"
    | SV_Postfix -> failwith "dump548"
    | SV_Scope -> failwith "dump549"
    | SV_Max -> failwith "dump550"

