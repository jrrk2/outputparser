open Output_types
open Output_parser
open Printf

let othlst = ref []

let capitalise start = String.uppercase_ascii (String.sub start 0 1) ^ String.sub start 1 (String.length start - 1)

let primary gramlst = match List.hd gramlst with
  | GRAMITM (ACCEPT, [ID start; END]) -> start
  | oth -> failwith (Ord.getstr oth)

let reserved = function
  | "if" -> "if_rule"
  | "then" -> "then_rule"
  | "else" -> "else_rule"
  | "initializer" -> "initializer_rule"
  | "module" -> "module_rule"
  | oth -> oth

let typ' str = try Sys.getenv str with _ -> "unit"

let termhash = Hashtbl.create 257

let typ is_terminal str =
  if not (Hashtbl.mem termhash str) then
    Hashtbl.add termhash str (is_terminal, typ' str);
  Hashtbl.find termhash str

let rec munge s = let s0 = String.sub s 0 1 in (match s.[0] with
  | 'A' .. 'Z' -> s0
  | 'a' .. 'z' -> s0
  | '0' .. '9' -> s0
  | _ -> "_")^(let l = String.length s in if l > 1 then munge (String.sub s 1 (l - 1)) else "")

let rec munge0 s = let s0 = String.sub s 0 1 in (match s.[0] with
  | 'A' .. 'Z' -> s0
  | 'a' .. 'z' -> String.uppercase_ascii s0
  | '0' .. '9' -> s0
  | _ -> "_")^(let l = String.length s in if l > 1 then munge (String.sub s 1 (l - 1)) else "")

let mungeh = Hashtbl.create 255

let munge' s = match s.[0] with
  | 'A' .. 'Z' -> munge s
  | 'a' .. 'z' -> munge0 s
  | _ -> "TOK_"^munge s

let munge s = 
  let m = munge' s in
  if Hashtbl.mem mungeh m then
    (if Hashtbl.find mungeh m <> s then m^"_"^string_of_int (Hashtbl.length mungeh) else m)
  else
    (Hashtbl.replace mungeh m s; m)

let items mlyfile oldlhs txt rulst ix =
              let used = ref 0 in
	      let len = String.length !oldlhs in
	      let islist1 = len > 8 && String.sub !oldlhs 0 8 = "list_of_" in
	      let islist2 = len > 4 && String.sub !oldlhs (len-4) 4 = "List" in
	      let cons = islist1 || islist2 in
	      let tuple = if cons then "CONS" else "TUPLE" in
              List.iter (function 
                  | EMPTY -> fprintf mlyfile "/* empty */ { EMPTY_TOKEN }\n"
                  | ID "error" -> incr used; fprintf mlyfile "ERROR_TOKEN "
                  | END -> incr used; fprintf mlyfile "EOF_TOKEN "
                  | ID id -> incr used; fprintf mlyfile "%s " (reserved id)
		  | DOLLAR_AT n -> fprintf mlyfile "/* %d */ " n
		  | STRING_LITERAL nam -> incr used;
		    let m = munge nam in fprintf mlyfile "%s " m;
                  | oth -> incr used; fprintf mlyfile "%s " (Ord.getstr oth)) rulst;
              if txt <> "" then fprintf mlyfile "{\n%s }\n" txt
              else if !used <> 0 then
	          begin
                  let delim = if cons then 
                     begin
		     if !used > 1 then fprintf mlyfile "{ %s%d" tuple !used
                     else fprintf mlyfile "{ CONS1 ";
                     ref '('
                     end
                  else
                     begin
		     if !used > 1 then begin fprintf mlyfile "{ %s%d(STRING(\"%s%d\")" tuple (1 + !used) !oldlhs ix; ref ','; end
                     else begin fprintf mlyfile "{ "; ref '('; end
                     end in 
		  let ix = ref 0 in List.iter (function 
		      | EMPTY -> incr ix; fprintf mlyfile "%c ()" !delim; delim := ','
		      | END -> incr ix; fprintf mlyfile "%cEOF_TOKEN" !delim; delim := ','
		      | ID "error" -> incr ix; fprintf mlyfile "%cERROR_TOKEN" !delim; delim := ','
		      | ID id -> incr ix;
			  let (is_term,typ') = typ false id in
			  if typ' <> "unit" then
			    fprintf mlyfile "%c%s $%d" !delim (reserved id) !ix
			  else if is_term then
			    fprintf mlyfile "%c%s" !delim (reserved id)
			  else
			    fprintf mlyfile "%c$%d" !delim !ix;
			  delim := ','
		      | DOLLAR_AT n -> ()
		      | STRING_LITERAL nam -> incr ix; 
			  let m = munge nam in
			  let (is_term,typ') = typ false m in
			  if false then print_endline (typ' ^ " " ^ m);
			  if typ' <> "unit" then
			    fprintf mlyfile "%c%s $%d" !delim m !ix
			  else if is_term || true then
			    fprintf mlyfile "%c%s" !delim m
			  else
			    fprintf mlyfile "%c$%d" !delim !ix;
			  delim := ','
		      | oth -> incr ix; fprintf mlyfile "%c%s" !delim (Ord.getstr oth); delim := ',') rulst;
		  fprintf mlyfile ") }\n"
	          end

let tokens = List.map (function
    | TERMITM(STRING_LITERAL nam, _) -> munge nam
    | TERMITM(ID nam, _) -> nam
    | TERMITM(oth, _) -> Ord.getstr oth
    | err -> failwith (Ord.getstr err))

let template toklst gramlst =
    let toklst' = ref (tokens toklst) in
    List.iter (fun itm -> if not (List.mem itm !toklst') then toklst' := itm :: !toklst')
    ["EOF_TOKEN";
    "LINEFEED";
    "EMPTY_TOKEN";
    "ERROR_TOKEN";
    "AMPERSAND";
    "AT";
    "BACKQUOTE";
    "BACKSLASH";
    "CARET";
    "COLON";
    "COMMA";
    "ACCEPT";
    "DEFAULT";
    "DOLLAR";
    "DOT";
    "DOUBLEQUOTE";
    "HASH";
    "LBRACE";
    "LBRACK";
    "PERCENT";
    "PLING";
    "QUERY";
    "QUOTE";
    "RBRACE";
    "RBRACK";
    "TILDE";
    "UNDERSCORE";
    "VBAR";
    "LESS";
    "GREATER";
    ];
    let stem = primary gramlst in
    let mlyfile = open_out (capitalise stem^".mly") in
    let typfile = open_out (capitalise stem^"_types.ml") in
(*
    let auxh = open_out (capitalise stem^"_aux.mli") in
    let auxf = open_out (capitalise stem^"_aux.ml") in
    fprintf auxh "open %s\n" (capitalise stem);
    fprintf auxh "val declst:token list ref\n";
    close_out auxh;
    fprintf auxf "open %s\n" (capitalise stem);
    fprintf auxf "let (declst:token list ref) = ref []\n";
    close_out auxf;
*)
    fprintf mlyfile "%%{\n";
    fprintf mlyfile "  open Parsing\n";
    fprintf mlyfile "  open %s_types\n" (capitalise stem);
    fprintf mlyfile "  let declst = ref []\n";
    fprintf mlyfile "  let packhash_add id_t = Hashtbl.add packhash id_t ()\n";
    fprintf mlyfile "  let typehash_add id_t = Hashtbl.add typehash id_t ()\n";
    fprintf typfile "  open %s\n" (capitalise stem);
    fprintf typfile "let getstr = function\n";
    let maxlen = ref 0 in List.iter (function
        | GRAMITM (_, rulst) -> let len = List.length rulst in if !maxlen < len then maxlen := len
        | oth -> failwith (Ord.getstr oth)) gramlst;
    List.iter (fun (str,min,max) -> for i = min to max do
      let itm = str^string_of_int i in
      Hashtbl.add termhash itm (true, (String.concat "*" (Array.to_list (Array.make i "token"))));
      toklst' := itm :: !toklst'
    done) ["TUPLE",2,1 + !maxlen;"CONS",1,4];
    List.iter (fun (typ,itm) -> Hashtbl.add termhash itm (true, typ);
               if not (List.mem itm !toklst') then toklst' := itm :: !toklst')
      [
      "string", "STRING";
      "token list", "ELIST";
      "string list", "SLIST";
      "token list", "TLIST"];
    let toklst' = List.sort compare !toklst' in
    List.iter (fun itm ->
		   let typ' = match snd (typ true itm) with "unit" -> "" | oth -> " _" in
                   fprintf typfile "| %s %s -> \"%s\"\n" itm typ' itm) toklst';
    fprintf typfile "\nlet (typehash:(string,unit)Hashtbl.t) = Hashtbl.create 257\n";
    fprintf typfile "\nlet (packhash:(string,unit)Hashtbl.t) = Hashtbl.create 257\n";
    close_out typfile;
    fprintf mlyfile "%%}\n";
    fprintf mlyfile "\n";
    List.iter (fun itm ->
		   let typ' = match snd (typ true itm) with "unit" -> "" | oth -> "<"^oth^">" in
                   fprintf mlyfile "%%token %s %s\n" typ' itm) toklst';
    fprintf mlyfile "%%type <token> ml_start\n";
    fprintf mlyfile "%%start ml_start\n";
    fprintf mlyfile "%%%%\n";
    fprintf mlyfile "\n";
    let oldlhs = ref "" in List.iteri (fun ix ->
      let searchf = capitalise stem^"_"^string_of_int ix in
      if false then print_endline searchf;
      let txt = if Sys.file_exists searchf then 
        begin
        let fd = open_in searchf in
        let buf = Bytes.create 4096 in
        let len = input fd buf 0 4096 in
        close_in fd;
        let txt = String.sub (Bytes.to_string buf) 0 len in
        if false then print_endline txt;
        txt
        end
      else "" in function
        | GRAMITM (ACCEPT, rulst) -> fprintf mlyfile "\nml_start: ";
              oldlhs := "ml_start";
	      items mlyfile oldlhs txt rulst ix
        | GRAMITM (ID lhs, rulst) ->
              if lhs <> !oldlhs then fprintf mlyfile "\n%s: " (reserved lhs) else fprintf mlyfile "\t|\t";
              oldlhs := lhs;
              items mlyfile oldlhs txt rulst ix
        | GRAMITM (VBAR, rulst) -> 
              fprintf mlyfile "\t|\t";
              items mlyfile oldlhs txt rulst ix
        | GRAMITM (DOLLAR_AT n, [EMPTY]) -> ()
        | oth -> othlst := oth :: !othlst; failwith (Ord.getstr oth)) gramlst;
    fprintf mlyfile "\n\n";
    close_out mlyfile
