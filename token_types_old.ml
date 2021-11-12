open Sexplib
open Sexplib.Std
(*
open Ppx_sexp_conv_lib.Sexp
*)
type token = unit [@@deriving sexp]
type tokenchar = char [@@deriving sexp]
type tokenSpan = unit [@@deriving sexp]
type tokenSpanned = int [@@deriving sexp]
type tokenbool = bool [@@deriving sexp]
type tokenName = string [@@deriving sexp]
type tokenKw = string [@@deriving sexp]
type tokenusize = int [@@deriving sexp]
type tokendyn = int [@@deriving sexp]
type tokenPropSpec = int [@@deriving sexp]
type tokenNodeId = int [@@deriving sexp]
type tokenCell = int [@@deriving sexp]
type tokenAnyNode = unit [@@deriving sexp]
(* // Copyright (c) 2016-2021 Fabian Schuiki *)
(* //! Defines all tokens that may result from performing lexical analysis on a *)
(* //! SystemVerilog source file. This module is inspired heavily by the tokens *)
(* //! used in the Rust compiler. *)
(* /// A primary token emitted by the lexer. *)

and tokenToken =
    (* // Symbols *)
    | SV_Comma
    | SV_Period
    | SV_Colon
    | SV_Semicolon
    | SV_At
    | SV_Hashtag
    | SV_DoubleHashtag
    | SV_Namespace
    | SV_Ternary
    | SV_AddColon
    | SV_SubColon
    | SV_Apostrophe
    | SV_Dollar
    | SV_Operator of (tokenOp)
    (* /// An opening delimiter *)
    | SV_OpenDelim of (tokenDelimToken)
    (* /// A closing delimiter *)
    | SV_CloseDelim of (tokenDelimToken)
    (* /// A literal *)
    | SV_Literal of (tokenLit)
    (* /// A system task or function identifier, e.g. "$display" *)
    | SV_SysIdent of (tokenName)
    (* /// A compiler directive, e.g. "`timescale" *)
    | SV_CompDir of (tokenName)
    (* /// An identifier *)
    | SV_Ident of (tokenName)
    (* /// An escaped identifier *)
    | SV_EscIdent of (tokenName)
    (* /// An unsigned number *)
    (* // UnsignedNumber(Name), *)
    (* /// A keyword *)
    | SV_Keyword of (tokenKw)
    (* // The end of the input file *)
    | SV_Eof
 [@@deriving sexp]
(* /// A delimiter token such as parentheses or brackets. *)

and tokenDelimToken =
    (* /// A round paranthesis `(` or `)` *)
    | SV_Paren
    (* /// A square bracket `[` or `]` *)
    | SV_Brack
    (* /// A curly brace `{` or `}` *)
    | SV_Brace
    (* /// A `begin` or `end` *)
    | SV_Bgend
 [@@deriving sexp]
(* /// Abstract literals such as strings. *)

and tokenLit =
    | SV_Str of (tokenName)
    | SV_BasedInteger of (tokenName option * tokenbool * tokenchar * tokenName)
    (* /// One of `'0`, `'1`, `'x`, and `'z`. *)
    | SV_UnbasedUnsized of (tokenchar)
    (* /// A number given as integer and optional fractional part. *)
    | SV_Number of (tokenName * tokenName option)
    (* /// A time literal given as integer part, fractional part, and unit. *)
    | SV_Time of (tokenName * tokenName option * tokenTimeUnit)
 [@@deriving sexp]
(* /// The unit of a time literal. *)

and tokenTimeUnit =
    | SV_Second
    | SV_MilliSecond
    | SV_MicroSecond
    | SV_NanoSecond
    | SV_PicoSecond
    | SV_FemtoSecond
 [@@deriving sexp]
(* /// Operator symbols. *)

and tokenOp =
    (* // Assignment *)
    | SV_Assign
    | SV_AssignAdd
    | SV_AssignSub
    | SV_AssignMul
    | SV_AssignDiv
    | SV_AssignMod
    | SV_AssignBitAnd
    | SV_AssignBitOr
    | SV_AssignBitXor
    | SV_AssignLogicShL
    | SV_AssignLogicShR
    | SV_AssignArithShL
    | SV_AssignArithShR
    (* // Arithmetic *)
    | SV_Add
    | SV_Sub
    | SV_Mul
    | SV_Div
    | SV_Mod
    | SV_Pow
    | SV_Inc
    | SV_Dec
    (* // Equality *)
    | SV_LogicEq
    | SV_LogicNeq
    | SV_CaseEq
    | SV_CaseNeq
    | SV_WildcardEq
    | SV_WildcardNeq
    (* // Relational *)
    | SV_Lt
    | SV_Leq
    | SV_Gt
    | SV_Geq
    (* // Logic *)
    | SV_LogicNot
    | SV_LogicAnd
    | SV_LogicOr
    | SV_LogicImpl
    | SV_LogicEquiv
    (* // Bitwise *)
    | SV_BitNot
    | SV_BitAnd
    | SV_BitNand
    | SV_BitOr
    | SV_BitNor
    | SV_BitXor
    | SV_BitXnor
    | SV_BitNxor
    (* // Shift *)
    | SV_LogicShL
    | SV_LogicShR
    | SV_ArithShL
    | SV_ArithShR
    (* // Sequence *)
    | SV_SeqImplOl
    | SV_SeqImplNol
    | SV_SeqFollowOl
    | SV_SeqFollowNol
 [@@deriving sexp]
(* /// Expression precedence. Note that a few kinds of expression are *)
(* /// right-associative rather than the default left-associative. *)

and tokenPrecedence =
    | SV_Min
    | SV_MinTypMax
    | SV_Concatenation
    (* // no associativity *)
    | SV_Assignment
    (* // no associativity *)
    | SV_Implication
    (* // right-associative *)
    | SV_Ternary
    (* // right-associative *)
    | SV_LogicOr
    | SV_LogicAnd
    | SV_BitOr
    | SV_BitXor
    | SV_BitAnd
    | SV_Equality
    | SV_Relational
    | SV_Shift
    | SV_Add
    | SV_Mul
    | SV_Pow
    | SV_Unary
    | SV_Postfix
    | SV_Scope
    | SV_Max
 [@@deriving sexp]
