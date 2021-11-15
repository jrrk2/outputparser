let _ = match Array.length Sys.argv with
    | 1 -> print_endline "Usage:"
    | 2 -> let _ = Moore_main.dmp true Sys.argv.(1) in ()
    | 3 -> let _ = Moore_main.dmp false Sys.argv.(1) in ()
    | _ -> print_endline "Usage:"

