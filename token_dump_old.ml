open Token_types_old

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
(* // Copyright (c) 2016-2021 Fabian Schuiki *)
(* //! Defines all tokens that may result from performing lexical analysis on a *)
(* //! SystemVerilog source file. This module is inspired heavily by the tokens *)
(* //! used in the Rust compiler. *)
(* /// A primary token emitted by the lexer. *)

and (dumpToken:tokenToken -> unit) = function 
    (* // Symbols *)
    | SV_Comma -> ()
    | SV_Period -> ()
    | SV_Colon -> ()
    | SV_Semicolon -> ()
    | SV_At -> ()
    | SV_Hashtag -> ()
    | SV_DoubleHashtag -> ()
    | SV_Namespace -> ()
    | SV_Ternary -> ()
    | SV_AddColon -> ()
    | SV_SubColon -> ()
    | SV_Apostrophe -> ()
    | SV_Dollar -> ()
    | SV_Operator (tokenOp1) -> ()
    (* /// An opening delimiter *)
    | SV_OpenDelim (tokenDelimToken1) -> ()
    (* /// A closing delimiter *)
    | SV_CloseDelim (tokenDelimToken1) -> ()
    (* /// A literal *)
    | SV_Literal (tokenLit1) -> ()
    (* /// A system task or function identifier, e.g. "$display" *)
    | SV_SysIdent (tokenName1) -> ()
    (* /// A compiler directive, e.g. "`timescale" *)
    | SV_CompDir (tokenName1) -> ()
    (* /// An identifier *)
    | SV_Ident (tokenName1) -> ()
    (* /// An escaped identifier *)
    | SV_EscIdent (tokenName1) -> ()
    (* /// An unsigned number *)
    (* // UnsignedNumber(Name), *)
    (* /// A keyword *)
    | SV_Keyword (tokenKw1) -> ()
    (* // The end of the input file *)
    | SV_Eof -> ()

(* /// A delimiter token such as parentheses or brackets. *)

and (dumpDelimToken:tokenDelimToken -> unit) = function 
    (* /// A round paranthesis `(` or `)` *)
    | SV_Paren -> ()
    (* /// A square bracket `[` or `]` *)
    | SV_Brack -> ()
    (* /// A curly brace `{` or `}` *)
    | SV_Brace -> ()
    (* /// A `begin` or `end` *)
    | SV_Bgend -> ()

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
    | SV_Second -> ()
    | SV_MilliSecond -> ()
    | SV_MicroSecond -> ()
    | SV_NanoSecond -> ()
    | SV_PicoSecond -> ()
    | SV_FemtoSecond -> ()

(* /// Operator symbols. *)

and (dumpOp:tokenOp -> string) = function 
    (* // Assignment *)
    | SV_Assign -> failwith "dumpOp1"
    | SV_AssignAdd -> failwith "dumpOp2"
    | SV_AssignSub -> failwith "dumpOp3"
    | SV_AssignMul -> failwith "dumpOp4"
    | SV_AssignDiv -> failwith "dumpOp5"
    | SV_AssignMod -> failwith "dumpOp6"
    | SV_AssignBitAnd -> failwith "dumpOp7"
    | SV_AssignBitOr -> failwith "dumpOp8"
    | SV_AssignBitXor -> failwith "dumpOp9"
    | SV_AssignLogicShL -> failwith "dumpOp10"
    | SV_AssignLogicShR -> failwith "dumpOp11"
    | SV_AssignArithShL -> failwith "dumpOp12"
    | SV_AssignArithShR -> failwith "dumpOp13"
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
    | SV_BitNand -> failwith "dumpOp27"
    | SV_BitOr -> "|"
    | SV_BitNor -> failwith "dumpOp29"
    | SV_BitXor -> "^"
    | SV_BitXnor -> failwith "dumpOp31"
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
    | SV_Min -> ()
    | SV_MinTypMax -> ()
    | SV_Concatenation -> ()
    (* // no associativity *)
    | SV_Assignment -> ()
    (* // no associativity *)
    | SV_Implication -> ()
    (* // right-associative *)
    | SV_Ternary -> ()
    (* // right-associative *)
    | SV_LogicOr -> ()
    | SV_LogicAnd -> ()
    | SV_BitOr -> ()
    | SV_BitXor -> ()
    | SV_BitAnd -> ()
    | SV_Equality -> ()
    | SV_Relational -> ()
    | SV_Shift -> ()
    | SV_Add -> ()
    | SV_Mul -> ()
    | SV_Pow -> ()
    | SV_Unary -> ()
    | SV_Postfix -> ()
    | SV_Scope -> ()
    | SV_Max -> ()

