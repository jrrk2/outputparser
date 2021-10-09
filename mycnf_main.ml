open Msat_tseitin.MakeCNF

let ep form =
    let m = make_cnf form in
    let _ = Printf.printf "CNF length = %d\n" (List.length m) in
    let solver = Msat_sat_slit.create () in
    Msat_sat_slit.assume solver m ();
    match Msat_sat_slit.solve solver with
      | Msat_sat_slit.Sat _ -> print_endline ("SATISFIABLE (endpoint mismatched)"); false
      | Msat_sat_slit.Unsat _ -> print_endline ("UNSATISFIABLE (endpoint matched)"); true

let _ = ep Mycnf.mycnf'
