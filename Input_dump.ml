open Input
open Input_rewrite_types

let esc string = (if string.[0] <> '\\' then "\\" else "")^string

let rec dump_ilang ind = function
| Assign_stmt67(ilang_lst,ilang_lst') -> "assign "^dump_ilst ind ilang_lst^" "^dump_ilst ind ilang_lst'^"\n"
| Attr_list56(ilang_lst,ilang_lst') -> "Attr_list56("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst' ^")"
| Attr_stmt(string,ilang_lst') -> "attribute "^esc string^" "^dump_ilst ind ilang_lst'^"\n"
| Autoidx_stmt26(int') -> "autoidx "^string_of_int int'
| Case_body63(ilang_lst,ilang_lst') -> "Case_body63("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Case_body64(ilang_lst,ilang_lst') -> "Case_body64("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Case_body65(ilang_lst,ilang_lst') -> "Case_body65("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Cell_bodyconnect(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodyconnect("^dump_ilst ind ilang_lst^", "^esc string^", "^dump_ilst ind ilang_lst'^", "^dump_ilst ind ilang_lst2^")"
| Cell_bodyparam(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodyparam("^dump_ilst ind ilang_lst^", "^esc string^", "^dump_ilst ind ilang_lst'^", "^dump_ilst ind ilang_lst2 ^")"
| Cell_bodypreal(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodypreal("^dump_ilst ind ilang_lst^", "^esc string^", "^dump_ilst ind ilang_lst'^", "^dump_ilst ind ilang_lst2 ^")"
| Cell_bodypsigned(ilang_lst,string,ilang_lst',ilang_lst2) -> "Cell_bodypsigned("^dump_ilst ind ilang_lst^", "^esc string^", "^dump_ilst ind ilang_lst'^", "^dump_ilst ind ilang_lst2^")"
| Cell_stmt(string,string',ilang_lst,ilang_lst') -> "cell "^string^" "^esc string'^"\n"^(dump_ilst "\n  " ilang_lst)^(dump_ilst "\n  " ilang_lst')^"\nend\n"
| Compare_list61(ilang_lst,ilang_lst') -> " "^dump_ilst ind ilang_lst^" , "^dump_ilst ind ilang_lst'^" "
| Conn_stmt96(ilang_lst,ilang_lst') -> "connect "^dump_ilst ind ilang_lst^" "^dump_ilst ind ilang_lst'^"\n"
| Design6(ilang_lst,ilang_lst') -> "Design6("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Design7(ilang_lst,ilang_lst') -> "Design7("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Design8(ilang_lst,ilang_lst') -> "Design8("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Input2(ilang_lst,ilang_lst') -> "Input2("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'^")"
| Memory_optionsoffset(int') -> " offset "^string_of_int int'
| Memory_optionssize(int') -> " size "^string_of_int int'
| Memory_optionswidth(int') -> " width "^string_of_int int'
| Memory_stmt39(ilang_lst,string') -> "memory "^dump_ilst " " ilang_lst^" "^esc string'^"\n"
| Module12(string,ilang_lst') -> "module "^esc string^"\n"^dump_ilst ind ilang_lst'^"\nend\n"
| Module_body13(ilang_lst,ilang_lst') -> "Module_body13("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst ^")"
| Param_defval_stmt24(string,ilang_lst') -> "parameter "^esc string^" "^dump_ilst ", " ilang_lst'^"\n"
| Param_stmt23(string,ilang_lst') -> "Param_stmt23("^esc string^", "^dump_ilst ind ilang_lst'^")"
| Proc_stmt(string,ilang_lst,ilang_lst',ilang_lst2) -> "process "^esc string^"\n"^dump_ilst "\n  " ilang_lst^"\n  "^dump_ilst "\n  " ilang_lst'^"\n  "^dump_ilst "\n  " ilang_lst2^"\nend\n"
| Signed -> " signed"
| Sigspec90(string, int') -> esc string^" ["^string_of_int int'^"]"
| Sigspec92(ilang_lst') -> "{ "^dump_ilst " " ilang_lst'^" }"
| Sigspec_list_reversed93(ilang_lst,ilang_lst') -> "Sigspec_list_reversed93("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst ^")"
| Sigspecrange(string,int,int') -> esc string^" ["^string_of_int int^":"^string_of_int int'^"]"
| Switch_bodycase(ilang_lst,ilang_lst',ilang_lst2) -> "case "^dump_ilst ind ilang_lst^"\n  "^dump_ilst ind ilang_lst'^"\n  "^dump_ilst ind ilang_lst2^"\n"
| Switch_stmt(ilang_lst,ilang_lst',ilang_lst2,ilang_lst3) -> "switch "^dump_ilst ind ilang_lst^"\n"^dump_ilst ind ilang_lst'^"\n"^dump_ilst ind ilang_lst2^"\n"^dump_ilst ind ilang_lst3^"\nend\n"
| Sync_list69(ilang_lst',ilang_lst2,ilang_lst3,ilang_lst4) -> "sync "^dump_ilst ind ilang_lst'^" "^dump_ilst ind ilang_lst2^" "^dump_ilst ind ilang_lst3^"\n    "^dump_ilst ind ilang_lst4^"\n"
| Sync_listalways(ilang_lst',ilang_lst2) -> "sync always\n  "^dump_ilst ind ilang_lst'^" "^dump_ilst ind ilang_lst2^"\n"
| Sync_listglobal(ilang_lst',ilang_lst2) -> "sync global "^dump_ilst ind ilang_lst'^" "^dump_ilst ind ilang_lst2
| Sync_listinit(ilang_lst',ilang_lst2) -> "sync init "^dump_ilst ind ilang_lst'^" "^dump_ilst ind ilang_lst2^"\n"
| Update_list82(ilang_lst',ilang_lst2) -> "update "^dump_ilst ind ilang_lst'^", "^dump_ilst ind ilang_lst2^"\n"
| Update_listmemwr(string,ilang_lst2,ilang_lst3,ilang_lst4,ilang_lst5) -> "memwr "^esc string^" "^dump_ilst ind ilang_lst2^" "^dump_ilst ind ilang_lst3^" "^dump_ilst ind ilang_lst4^" "^dump_ilst ind ilang_lst5^"\n"
| Upto -> " upto "
| Wire_optionsinout(int) -> " inout "^string_of_int int
| Wire_optionsinput(int) -> " input "^string_of_int int
| Wire_optionsinvalid -> " optionsinvalid"
| Wire_optionsoffset(int') -> " offset "^string_of_int int'
| Wire_optionsoutput(int') -> " output "^string_of_int int'
| Wire_optionswidth(int') -> " width "^string_of_int int'
| Wire_stmt(ilang_lst,string') -> "wire"^dump_ilst " " ilang_lst^" "^esc string'^"\n"
| TokCase(ilang_lst,ilang_lst') -> "TokCase("^dump_ilst ind ilang_lst^", "^dump_ilst ind ilang_lst'
| TokConn(ilang_lst,ilang_lst') -> "connect "^dump_ilst " " ilang_lst^" "^dump_ilst " " ilang_lst'
| TokParam(ilang_lst,ilang_lst') -> "parameter "^dump_ilst " " ilang_lst^" "^dump_ilst " " ilang_lst'
| TokUpdate(ilang_lst,ilang_lst') -> "update "^dump_ilst ind ilang_lst^" "^dump_ilst ind ilang_lst'^"\n"
| TokInt(int) -> string_of_int int
| TokID(string) -> esc string
| TokVal(string) -> string
| TokStr(string) -> "\""^string^"\""
| TokPos -> "posedge"
| TokNeg -> "negedge"
| TokEdge -> "edge"

and dump_ilst ind lst = String.concat ind (List.map (dump_ilang "") lst)
