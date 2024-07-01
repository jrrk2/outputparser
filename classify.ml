open Source_text
open Source_text_rewrite_types

let rec classify chash = function
| STRING _ -> STRING ""
| IDENTIFIER _ -> IDENTIFIER ""
| INTEGER_NUMBER _ -> INTEGER_NUMBER ""
| TYPE_HYPHEN_IDENTIFIER _ -> TYPE_HYPHEN_IDENTIFIER ""
| IDENTIFIER_HYPHEN_COLON_COLON _ -> IDENTIFIER_HYPHEN_COLON_COLON ""
| FLOATING_HYPHEN_POINT_NUMBER _ -> FLOATING_HYPHEN_POINT_NUMBER 0.0
| ELIST lst -> ELIST []
| TLIST lst -> List.iter (fun itm -> Hashtbl.replace chash (classify chash itm) ()) lst; TLIST []
| TUPLE2(arg1,arg2) -> TUPLE2 ((classify chash) arg1, (classify chash) arg2)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8, (classify chash) arg9)
| ((AT|EMPTY_TOKEN|LPAREN|RPAREN|LBRACK|RBRACK|LBRACE|RBRACE
| COLON|SEMICOLON|COMMA|CARET|TILDE|QUERY|QUOTE
| PLUS|HYPHEN|STAR|SLASH|HASH|PLING
| AMPERSAND|AMPERSAND_AMPERSAND|AMPERSAND_EQ
| GT_GT_GT|PLUS_COLON|PLUS_PLUS|COLON_COLON
| EQUALS|LT_EQ|VBAR_VBAR|LT_LT|GT_GT|GT_EQ|EQ_EQ|LESS|GREATER|VBAR
| TILDE_VBAR|TILDE_AMPERSAND
| CARET_TILDE
| HYPHEN_HYPHEN
| VBAR_EQ|PLUS_EQ
| PLING_EQ|DOT_STAR|STAR_STAR
| QUOTE_LBRACE
| DLR_display|DLR_stop|DLR_finish|DLR_write
| DLR_signed|DLR_unsigned|DLR_time|DLR_readmemh|DLR_clog2|DLR_bits|DLR_error
| Module|Always|Assign|Reg|Wire|Logic|Bit|Int|Integer
| Unsigned|Signed
| Output|Input|Posedge|Negedge|Or|DOT
| Parameter|Localparam|Initial
| If|Else|Modport|For
| Begin|End|Endmodule
| Interface|Endinterface
| Task|Endtask
| Package|Endpackage
| Generate|Endgenerate
| Function|Endfunction
| Struct
| Typedef
| Enum
| Case|Casez|Casex|Default|Endcase
| Packed
| Import
| Genvar
| Always_comb|Always_ff|Always_latch
| Return
| Automatic
| Union
| Assert
| Const
| Inout
| DLR_fatal
| DLR_warning
| DLR_random
| DLR_sformatf
| DLR_fscanf
| DLR_fwrite
| DLR_fclose
| DLR_fopen
| DLR_feof
| DLR_size
| DLR_high
| DLR_low
| Break
| Type
| Property
| While
| String
| Byte
| Longint
| Void
| Inside
| Unique
| Foreach
| Final
| HYPHEN_COLON
| HYPHEN_EQ
| EOF_TOKEN) as oth) -> oth
| oth -> failwith ("classify fail: "^Source_text_tokens.getstr oth)

let chash = Hashtbl.create 255;;

let classify'' p' =
  Hashtbl.replace chash (classify chash p') ();
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  ()

let classify' () =
  let lst = ref [] in
  Hashtbl.iter (fun k _ -> lst := k :: !lst) chash;
  List.sort compare !lst
