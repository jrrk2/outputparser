open Output_types
open Output_parser
open Printf

let othlst = ref []

let primary gramlst = match List.hd gramlst with
        | GRAMITM (ACCEPT, [ID start; END]) -> start
        | oth -> failwith (Ord.getstr oth)

let reserved = function
  | "if" -> "if_rule"
  | "then" -> "then_rule"
  | "else" -> "else_rule"
  | oth -> oth

let typ' str = try Sys.getenv str with _ -> "unit"

let typhash = Hashtbl.create 257

let typ is_terminal str =
  if not (Hashtbl.mem typhash str) then
    Hashtbl.add typhash str (is_terminal, typ' str);
  Hashtbl.find typhash str

let items mlyfile rulst =
              let used = ref 0 in
              List.iter (function 
                  | EMPTY -> fprintf mlyfile "/* empty */ { EMPTY_TOKEN }\n"
                  | ID "error" -> fprintf mlyfile "error { failwith \"parse_error\" ) }\n"
                  | ID id -> incr used; fprintf mlyfile "%s " (reserved id)
		  | DOLLAR_AT n -> fprintf mlyfile "/* %d */ " n
                  | oth -> incr used; fprintf mlyfile "%s " (Ord.getstr oth)) rulst;
              if !used <> 0 then
	          begin
		  fprintf mlyfile "{ TUPLE%d" !used;
		  let delim = ref '(' and ix = ref 0 in List.iter (function 
		      | EMPTY -> incr ix; fprintf mlyfile "%c ()" !delim; delim := ','
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
		      | oth -> incr ix; fprintf mlyfile "%c%s" !delim (Ord.getstr oth); delim := ',') rulst;
		  fprintf mlyfile ") }\n"
	          end

let template mlyfile toklst gramlst =
    fprintf mlyfile "%%{\n";
    fprintf mlyfile "  open Parsing\n";
    fprintf mlyfile "  open Output_types\n";
    fprintf mlyfile "let getstr = function\n";
    List.iter (fun itm ->
		   let typ' = match snd (typ true itm) with "unit" -> "" | oth -> " _" in
                   fprintf mlyfile "| %s %s -> \"%s\"\n" itm typ' itm) toklst;
    fprintf mlyfile "\n";
    fprintf mlyfile "%%}\n";
    fprintf mlyfile "\n";
    List.iter (fun itm ->
		   let typ' = match snd (typ true itm) with "unit" -> "" | oth -> "<"^oth^">" in
                   fprintf mlyfile "%%token %s %s\n" typ' itm) toklst;
    fprintf mlyfile "%%token EOF_TOKEN\n";
    fprintf mlyfile "%%token EMPTY_TOKEN\n";
    fprintf mlyfile "%%token ERROR_TOKEN\n";
    fprintf mlyfile "%%token AMPERSAND\n";
    fprintf mlyfile "%%token AT\n";
    fprintf mlyfile "%%token BACKQUOTE\n";
    fprintf mlyfile "%%token BACKSLASH\n";
    fprintf mlyfile "%%token CARET\n";
    fprintf mlyfile "%%token COLON\n";
    fprintf mlyfile "%%token COMMA\n";
    fprintf mlyfile "%%token ACCEPT\n";
    fprintf mlyfile "%%token DEFAULT\n";
    fprintf mlyfile "%%token DOLLAR\n";
    fprintf mlyfile "%%token DOT\n";
    fprintf mlyfile "%%token DOUBLEQUOTE\n";
    fprintf mlyfile "%%token HASH\n";
    fprintf mlyfile "%%token LBRACE\n";
    fprintf mlyfile "%%token LBRACK\n";
    fprintf mlyfile "%%token PERCENT\n";
    fprintf mlyfile "%%token PLING\n";
    fprintf mlyfile "%%token QUERY\n";
    fprintf mlyfile "%%token QUOTE\n";
    fprintf mlyfile "%%token RBRACE\n";
    fprintf mlyfile "%%token RBRACK\n";
    fprintf mlyfile "%%token TILDE\n";
    fprintf mlyfile "%%token UNDERSCORE\n";
    fprintf mlyfile "%%token VBAR\n";
    fprintf mlyfile "%%token <int> INT\n";
    fprintf mlyfile "%%token <string list> SLIST\n";
    fprintf mlyfile "%%token <token list> TLIST\n";
    for i = 1 to 25 do
      fprintf mlyfile "%%token <%s> TUPLE%d\n" (String.concat "*" (Array.to_list (Array.make i "token"))) i
    done;
    let start = primary gramlst in
    fprintf mlyfile "%%type <token> %s\n" start;
    fprintf mlyfile "%%start %s\n" start;
    fprintf mlyfile "%%%%\n";
    fprintf mlyfile "\n";
    let oldlhs = ref "" in List.iter (function
        | GRAMITM (ID lhs, rulst) ->
              if lhs <> !oldlhs then fprintf mlyfile "\n%s: " (reserved lhs) else fprintf mlyfile "\t|\t";
              oldlhs := lhs;
              items mlyfile rulst
        | GRAMITM (VBAR, rulst) -> 
              fprintf mlyfile "\t|\t";
              items mlyfile rulst
        | GRAMITM (DOLLAR_AT n, [EMPTY]) -> ()
        | oth -> othlst := oth :: !othlst; failwith (Ord.getstr oth)) (List.tl gramlst);
    fprintf mlyfile "\n\n";
