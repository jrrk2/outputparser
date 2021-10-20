
module E = Msat_sat_slit.String_lit
module F = Msat_tseitin.MakeCNF

type ind = {
  wires:(E.signal, F.t option) Hashtbl.t;
  inffop:(E.signal, unit) Hashtbl.t;
  stash:(E.signal, string * string * Input_rewrite_types.ilang list) Hashtbl.t;
  wid:(string, int) Hashtbl.t;
}
