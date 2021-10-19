open Input_rewrite_types
open Source_text_rewrite_types
open Source_text_rewrite

let verbose = try int_of_string (Sys.getenv "CNF_VERBOSE") with err -> 0

let dbgfunc = ref []
let othst = ref []
let othclst = ref []
let othwlst = ref []
let othconn = ref None

let rec func ind klst kind conns = match kind, pinmap ind klst conns with
  | "", ["",E.GND,None] -> failwith "GND" (* dummy to force type inference *)

  | "$_ALDFFE_NNN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (ad) (knot (l))) (q)
  | "$_ALDFFE_NNP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (ad) (knot (l))) (q)
  | "$_ALDFFE_NPN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (ad) ((l))) (q)
  | "$_ALDFFE_NPP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (ad) ((l))) (q)
  | "$_ALDFFE_PNN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (ad) (knot (l))) (q)
  | "$_ALDFFE_PNP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (ad) (knot (l))) (q)
  | "$_ALDFFE_PPN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (ad) ((l))) (q)
  | "$_ALDFFE_PPP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (ad) ((l))) (q)
  | "$_ALDFF_NN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (d) (ad) (knot (l))) (q)
  | "$_ALDFF_NP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (d) (ad) ((l))) (q)
  | "$_ALDFF_PN_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (d) (ad) (knot (l))) (q)
  | "$_ALDFF_PP_",("\\AD", _, Some ad) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\L", _, Some l) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (d) (ad) ((l))) (q)
  | "$_ANDNOT_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 (a) (knot (b)))
  | "$_AND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (and2 (a) (b))
  | "$_AOI3_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (or2 (and2 (a) (b)) (c)))
  | "$_AOI4_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (or2 (and2 (a) (b)) (and2 (c) (d))))
  | "$_BUF_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (a)
  | "$_DFFE_NN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) (knot (r))) (q)
  | "$_DFFE_NN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) (knot (r))) (q)
  | "$_DFFE_NN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (r))) (q)
  | "$_DFFE_NN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (r))) (q)
  | "$_DFFE_NN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) (knot (e))) (q)
  | "$_DFFE_NP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) ((r))) (q)
  | "$_DFFE_NP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) ((r))) (q)
  | "$_DFFE_NP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((r))) (q)
  | "$_DFFE_NP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((r))) (q)
  | "$_DFFE_NP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) (e)) (q)
  | "$_DFFE_PN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) (knot (r))) (q)
  | "$_DFFE_PN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) (knot (r))) (q)
  | "$_DFFE_PN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (r))) (q)
  | "$_DFFE_PN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (r))) (q)
  | "$_DFFE_PN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) (knot (e))) (q)
  | "$_DFFE_PP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) ((r))) (q)
  | "$_DFFE_PP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) ((r))) (q)
  | "$_DFFE_PP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((r))) (q)
  | "$_DFFE_PP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((r))) (q)
  | "$_DFFE_PP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) (e)) (q)
  | "$_DFFSRE_NNNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_NNNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_NNPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_NNPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_NPNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_NPNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_NPPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_NPPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_PNNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_PNNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_PNPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_PNPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_PPNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_PPNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSRE_PPPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFFSRE_PPPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFFSR_NNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSR_NNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSR_NPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSR_NPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFFSR_PNN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSR_PNP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DFFSR_PPN_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DFFSR_PPP_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DFF_NN0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) (knot (r))) (q)
  | "$_DFF_NN1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) (knot (r))) (q)
  | "$_DFF_NP0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) ((r))) (q)
  | "$_DFF_NP1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) ((r))) (q)
  | "$_DFF_N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (d) (q)
  | "$_DFF_PN0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) (knot (r))) (q)
  | "$_DFF_PN1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) (knot (r))) (q)
  | "$_DFF_PP0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) ((r))) (q)
  | "$_DFF_PP1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) ((r))) (q)
  | "$_DFF_P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (d) (q)
  | "$_DLATCHSR_NNN_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DLATCHSR_NNP_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DLATCHSR_NPN_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DLATCHSR_NPP_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DLATCHSR_PNN_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_DLATCHSR_PNP_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_DLATCHSR_PPN_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_DLATCHSR_PPP_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_DLATCH_NN0_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) (knot (r))) (q)
  | "$_DLATCH_NN1_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (r))) (q)
  | "$_DLATCH_NP0_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) ((r))) (q)
  | "$_DLATCH_NP1_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((r))) (q)
  | "$_DLATCH_N_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) (knot (e))) (q)
  | "$_DLATCH_PN0_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) (knot (r))) (q)
  | "$_DLATCH_PN1_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (r))) (q)
  | "$_DLATCH_PP0_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) ((r))) (q)
  | "$_DLATCH_PP1_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((r))) (q)
  | "$_DLATCH_P_",("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (mux2 (atom q) (d) ((e))) (q)
  | "$_FF_",("\\D", _, Some d) :: ("\\Q", q, Some _) :: [] -> addnxt "nxt" ind (d) (q)
  | "$_MUX16_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\F", _, Some f) :: ("\\G", _, Some g) :: ("\\H", _, Some h) :: ("\\I", _, Some i) :: ("\\J", _, Some j) :: ("\\K", _, Some k) :: ("\\L", _, Some l) :: ("\\M", _, Some m) :: ("\\N", _, Some n) :: ("\\O", _, Some o) :: ("\\P", _, Some p) :: ("\\S", _, Some s) :: ("\\T", _, Some t) :: ("\\U", _, Some u) :: ("\\V", _, Some v) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (mux2 (mux2 (mux2 (a) (b) (s)) (mux2 (c) (d) (s)) (t)) (mux2 (mux2 (e) (f) (s)) (mux2 (g) (h) (s)) (t)) (u)) (mux2 (mux2 (mux2 (i) (j) (s)) (mux2 (k) (l) (s)) (t)) (mux2 (mux2 (m) (n) (s)) (mux2 (o) (p) (s)) (t)) (u)) (v))
  | "$_MUX4_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\S", _, Some s) :: ("\\T", _, Some t) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (mux2 (a) (b) (s)) (mux2 (c) (d) (s)) (t))
  | "$_MUX8_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\F", _, Some f) :: ("\\G", _, Some g) :: ("\\H", _, Some h) :: ("\\S", _, Some s) :: ("\\T", _, Some t) :: ("\\U", _, Some u) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (mux2 (mux2 (a) (b) (s)) (mux2 (c) (d) (s)) (t)) (mux2 (mux2 (e) (f) (s)) (mux2 (g) (h) (s)) (t)) (u))
  | "$_MUX_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\S", _, Some s) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (a) (b) (s))
  | "$_NAND_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (and2 (a) (b)))
  | "$_NMUX_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\S", _, Some s) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (knot (a)) (knot (b)) (s))
  | "$_NOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (or2 (a) (b)))
  | "$_NOT_",("\\A", _, Some a) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (a))
  | "$_OAI3_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (and2 (or2 (a) (b)) (c)))
  | "$_OAI4_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (and2 (or2 (a) (b)) (or2 (c) (d))))
  | "$_ORNOT_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (or2 (a) (knot (b)))
  | "$_OR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (or2 (a) (b))
  | "$_SDFFCE_NN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) (knot (r))) (knot (e))) (q)
  | "$_SDFFCE_NN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) (knot (r))) ((e))) (q)
  | "$_SDFFCE_NN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) (knot (r))) (knot (e))) (q)
  | "$_SDFFCE_NN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) (knot (r))) ((e))) (q)
  | "$_SDFFCE_NP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) ((r))) (knot (e))) (q)
  | "$_SDFFCE_NP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) ((r))) ((e))) (q)
  | "$_SDFFCE_NP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) ((r))) (knot (e))) (q)
  | "$_SDFFCE_NP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) ((r))) ((e))) (q)
  | "$_SDFFCE_PN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) (knot (r))) (knot (e))) (q)
  | "$_SDFFCE_PN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) (knot (r))) ((e))) (q)
  | "$_SDFFCE_PN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) (knot (r))) (knot (e))) (q)
  | "$_SDFFCE_PN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) (knot (r))) ((e))) (q)
  | "$_SDFFCE_PP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) ((r))) (knot (e))) (q)
  | "$_SDFFCE_PP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_false) ((r))) ((e))) (q)
  | "$_SDFFCE_PP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) ((r))) (knot (e))) (q)
  | "$_SDFFCE_PP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (atom q) (mux2 (d) (F.f_true) ((r))) ((e))) (q)
  | "$_SDFFE_NN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) (knot (r))) (q)
  | "$_SDFFE_NN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) (knot (r))) (q)
  | "$_SDFFE_NN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (r))) (q)
  | "$_SDFFE_NN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (r))) (q)
  | "$_SDFFE_NP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) ((r))) (q)
  | "$_SDFFE_NP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) ((r))) (q)
  | "$_SDFFE_NP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((r))) (q)
  | "$_SDFFE_NP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((r))) (q)
  | "$_SDFFE_PN0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) (knot (r))) (q)
  | "$_SDFFE_PN0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) (knot (r))) (q)
  | "$_SDFFE_PN1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) (knot (r))) (q)
  | "$_SDFFE_PN1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) (knot (r))) (q)
  | "$_SDFFE_PP0N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_false) ((r))) (q)
  | "$_SDFFE_PP0P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_false) ((r))) (q)
  | "$_SDFFE_PP1N_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) (knot (e))) (F.f_true) ((r))) (q)
  | "$_SDFFE_PP1P_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\E", _, Some e) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (d) ((e))) (F.f_true) ((r))) (q)
  | "$_SDFF_NN0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) (knot (r))) (q)
  | "$_SDFF_NN1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) (knot (r))) (q)
  | "$_SDFF_NP0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) ((r))) (q)
  | "$_SDFF_NP1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) ((r))) (q)
  | "$_SDFF_PN0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) (knot (r))) (q)
  | "$_SDFF_PN1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) (knot (r))) (q)
  | "$_SDFF_PP0_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_false) ((r))) (q)
  | "$_SDFF_PP1_",("\\C", _, Some c) :: ("\\D", _, Some d) :: ("\\Q", q, Some _) :: ("\\R", _, Some r) :: [] -> addnxt "nxt" ind (mux2 (d) (F.f_true) ((r))) (q)
  | "$_SR_NN_",("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) (knot (r))) (q)
  | "$_SR_NP_",("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) (knot (s))) (F.f_false) ((r))) (q)
  | "$_SR_PN_",("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) (knot (r))) (q)
  | "$_SR_PP_",("\\Q", q, Some _) :: ("\\R", _, Some r) :: ("\\S", _, Some s) :: [] -> addnxt "nxt" ind (mux2 (mux2 (atom q) (F.f_true) ((s))) (F.f_false) ((r))) (q)
  | "$_TBUF_",("\\A", _, Some a) :: ("\\E", _, Some e) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (mux2 (F.make_atom (E.make (E.SCALAR "Z"))) (a) (e))
  | "$_XNOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (knot (xor2 (a) (b)))
  | "$_XOR_",("\\A", _, Some a) :: ("\\B", _, Some b) :: ("\\Y", y, found) :: [] -> if found = None then addfunc ind y (xor2 (a) (b))
  | oth,lst ->
      othclst := lst;
      othwlst := [];
      Hashtbl.iter (fun k -> function
          | Some x -> othwlst := (k,fpp x) :: !othwlst
          | None -> othwlst := (k,"") :: !othwlst) ind.wires;
      othwlst := List.sort compare !othwlst;
      othst := List.map (fun (pin, net, _) -> Hashtbl.find_opt ind.stash net) lst;
      failwith ("func: unmatched "^oth)

and recurse ind klst signal = function
| Some (kind, inst, conns) ->
  if not (List.mem inst klst) then
    begin
    if verbose > 2 then print_endline ("recurse into " ^ E.string_of_signal signal ^ " (instance: " ^ inst ^ ", kind: " ^ kind ^ " )");
    func ind (inst :: klst) kind conns;
    if verbose > 2 then print_endline ("recurse into "^E.string_of_signal signal^" returned from " ^ kind);
    end
  else
    begin
    dbgfunc := (kind,conns,signal,Hashtbl.find ind.wires signal) :: !dbgfunc;
    if verbose > 2 then print_endline ("instance: " ^ inst ^ " (kind : " ^ kind ^ " ) already searched")
    end;
  Hashtbl.find ind.wires signal
| None ->
  print_endline ("recurse into "^E.string_of_signal signal^" failed"); None

and getcon ind klst pin = function
  | E.GND -> Some F.f_false
  | PWR -> Some F.f_true
  | signal -> match Hashtbl.find_opt ind.wires signal with
    | Some (Some _ as x) -> x
    | Some None -> if pin <> "Y" then recurse ind klst signal (Hashtbl.find_opt ind.stash signal) else None
    | None -> failwith (E.string_of_signal signal ^" not declared")

and conn' ind klst = function
  | TokConn ([TokID pin], [Sigspec90 (signal, ix)]) -> let s = idx signal ix in pin, s, getcon ind klst pin s
  | TokConn ([TokID pin], [TokID signal]) -> let s = (scalar signal) in pin, s, getcon ind klst pin s
  | TokConn ([TokID pin], [TokVal lev]) -> let s = (cnv_pwr lev) in pin, s, getcon ind klst pin s
  | oth -> othconn := Some oth; failwith "conn'"

and pinmap ind klst conns = List.sort compare (List.map (conn' ind klst) conns)

