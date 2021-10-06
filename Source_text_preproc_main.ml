open Source_text_preproc_grammar
open Source_text_preproc_lexer
open Source_text_preproc

let rec cat = function
  | [] -> ""
  | IDSTR oth :: tl -> oth ^ cat tl

let parse' str str' =
  let psuccess = ref true in
      begin
        Stack.push (str, my_openin str) includes;
        Stack.push true ifdef_stk; (* toplevel ifdef default *)
        preproc_open();
        begin
            let lexbuf = Lexing.from_function (from_func stdout) in
            let looping = ref true in
            while !looping do
              let rslt = start token lexbuf in
              match rslt with
              | toklst,ENDOFFILE -> looping := false;
                let outfile = open_out str' in
                output_string outfile (cat (List.rev toklst));
                close_out outfile
              | oth -> failwith "start"
            done
        end;
        preproc_close();
      end;
  !psuccess

let _ = parse' Sys.argv.(1) Sys.argv.(2)
