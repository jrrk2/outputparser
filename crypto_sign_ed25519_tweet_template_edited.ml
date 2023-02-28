
let dump msg (v,v') siz =
  for i = 0 to siz-1 do Printf.printf "%.2x " (int_of_char (Bytes.get v (v'+i))) done;
  print_newline ()

let dump64 v =
  Array.iter (fun itm -> Printf.printf "%.16Lx " itm) v;
  print_newline ()

let char_trunc x = char_of_int (Int.logand x 255)

let _D2 = [|
61785L;
9906L;
39828L;
60374L;
45398L;
33411L;
5274L;
224L;
53552L;
61171L;
33010L;
6542L;
64743L;
22239L;
55772L;
9222L;
|]

let _K = [|
4794697086780616226L;
8158064640168781261L;
-5349999486874862801L;
-1606136188198331460L;
4131703408338449720L;
6480981068601479193L;
-7908458776815382629L;
-6116909921290321640L;
-2880145864133508542L;
1334009975649890238L;
2608012711638119052L;
6128411473006802146L;
8268148722764581231L;
-9160688886553864527L;
-7215885187991268811L;
-4495734319001033068L;
-1973867731355612462L;
-1171420211273849373L;
1135362057144423861L;
2597628984639134821L;
3308224258029322869L;
5365058923640841347L;
6679025012923562964L;
8573033837759648693L;
-7476448914759557205L;
-6327057829258317296L;
-5763719355590565569L;
-4658551843659510044L;
-4116276920077217854L;
-3051310485924567259L;
489312712824947311L;
1452737877330783856L;
2861767655752347644L;
3322285676063803686L;
5560940570517711597L;
5996557281743188959L;
7280758554555802590L;
8532644243296465576L;
-9096487096722542874L;
-7894198246740708037L;
-6719396339535248540L;
-6333637450476146687L;
-4446306890439682159L;
-4076793802049405392L;
-3345356375505022440L;
-2983346525034927856L;
-860691631967231958L;
1182934255886127544L;
1847814050463011016L;
2177327727835720531L;
2830643537854262169L;
3796741975233480872L;
4115178125766777443L;
5681478168544905931L;
6601373596472566643L;
7507060721942968483L;
8399075790359081724L;
8693463985226723168L;
-8878714635349349518L;
-8302665154208450068L;
-8016688836872298968L;
-6606660893046293015L;
-4685533653050689259L;
-4147400797238176981L;
-3880063495543823972L;
-3348786107499101689L;
-1523767162380948706L;
-757361751448694408L;
500013540394364858L;
748580250866718886L;
1242879168328830382L;
1977374033974150939L;
2944078676154940804L;
3659926193048069267L;
4368137639120453308L;
4836135668995329356L;
5532061633213252278L;
6448918945643986474L;
6902733635092675308L;
7801388544844847127L;
|]

let _L = [|
237L;
211L;
245L;
92L;
26L;
99L;
18L;
88L;
214L;
156L;
247L;
162L;
222L;
249L;
222L;
20L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
16L;
|]

let _X = [|
54554L;
36645L;
11616L;
51542L;
42930L;
38181L;
51040L;
26924L;
56412L;
64982L;
57905L;
49316L;
21502L;
52590L;
14035L;
8553L;
|]

let _Y = [|
26200L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
26214L;
|]

let gf0 = [|
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
|]

let gf1 = [|
1L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
0L;
|]

let iv = [|
106;
9;
230;
103;
243;
188;
201;
8;
187;
103;
174;
133;
132;
202;
167;
59;
60;
110;
243;
114;
254;
148;
248;
43;
165;
79;
245;
58;
95;
29;
54;
241;
81;
14;
82;
127;
173;
230;
130;
209;
155;
5;
104;
140;
43;
62;
108;
31;
31;
131;
217;
171;
251;
65;
189;
107;
91;
224;
205;
25;
19;
126;
33;
121;
|]

let rec omega n = omega n

and sel25519 (p: Int64.t array) (q: Int64.t array) (b: int) : unit =
let (t: Int64.t ref) = ref 0L in
let (i: Int64.t ref) = ref 0L in
let (c: Int64.t ref) = ref 0L in
c := Int64.of_int (Int.lognot ((b-1)));
i := Int64.of_int (0);
while !i < 16L do t := Int64.logand (!c) ((Int64.logxor (p.((Int64.to_int !i))) (q.((Int64.to_int !i)))));
p.((Int64.to_int !i)) <- Int64.logxor (p.((Int64.to_int !i))) (!t);
q.((Int64.to_int !i)) <- Int64.logxor (q.((Int64.to_int !i))) (!t);
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
()

and dl64 ((x: Bytes.t), (x' : int)) : Int64.t =
let (i: Int64.t ref) = ref 0L in
let (u: Int64.t ref) = ref 0L in
u := Int64.of_int (0);
i := Int64.of_int (0);
while !i < (Int64.of_int (8)) do u := Int64.logor (Int64.shift_left !u (8)) (Int64.of_int (int_of_char (Bytes.get x (x' + (Int64.to_int !i)))));
i := (Int64.add (*5*) (!i) (Int64.of_int (1))); done;
let rslt = !u in rslt

and _Sigma1 (x: Int64.t) : Int64.t =
let (_'184: Int64.t ref) = ref 0L in
let (_'183: Int64.t ref) = ref 0L in
let (_'182: Int64.t ref) = ref 0L in
_'182 := ( _R  (x) (*Int64.t*)   (14) (*int*) ) ;
_'183 := ( _R  (x) (*Int64.t*)   (18) (*int*) ) ;
_'184 := ( _R  (x) (*Int64.t*)   (41) (*int*) ) ;
let rslt = Int64.logxor (Int64.logxor (!_'182) (!_'183)) (!_'184) in rslt

and _Ch (x: Int64.t) (y: Int64.t) (z: Int64.t) : Int64.t =
let rslt = Int64.logxor (Int64.logand (x) (y)) (Int64.logand (Int64.lognot (x)) (z)) in rslt

and _Sigma0 (x: Int64.t) : Int64.t =
let (_'184: Int64.t ref) = ref 0L in
let (_'183: Int64.t ref) = ref 0L in
let (_'182: Int64.t ref) = ref 0L in
_'182 := ( _R  (x) (*Int64.t*)   (28) (*int*) ) ;
_'183 := ( _R  (x) (*Int64.t*)   (34) (*int*) ) ;
_'184 := ( _R  (x) (*Int64.t*)   (39) (*int*) ) ;
let rslt = Int64.logxor (Int64.logxor (!_'182) (!_'183)) (!_'184) in rslt

and _Maj (x: Int64.t) (y: Int64.t) (z: Int64.t) : Int64.t =
let rslt = Int64.logxor (Int64.logxor (Int64.logand (x) (y)) (Int64.logand (x) (z))) (Int64.logand (y) (z)) in rslt

and sigma0 (x: Int64.t) : Int64.t =
let (_'183: Int64.t ref) = ref 0L in
let (_'182: Int64.t ref) = ref 0L in
_'182 := ( _R  (x) (*Int64.t*)   (1) (*int*) ) ;
_'183 := ( _R  (x) (*Int64.t*)   (8) (*int*) ) ;
let rslt = Int64.logxor (Int64.logxor (!_'182) (!_'183)) (Int64.shift_right (x) (7)) in rslt

and _R (x: Int64.t) (c: int) : Int64.t =
let rslt = Int64.logor (Int64.shift_right (x) (c)) (Int64.shift_left x (64-c)) in rslt

and sigma1 (x: Int64.t) : Int64.t =
let (_'183: Int64.t ref) = ref 0L in
let (_'182: Int64.t ref) = ref 0L in
_'182 := ( _R  (x) (*Int64.t*)   (19) (*int*) ) ;
_'183 := ( _R  (x) (*Int64.t*)   (61) (*int*) ) ;
let rslt = Int64.logxor (Int64.logxor (!_'182) (!_'183)) (Int64.shift_right (x) (6)) in rslt

and crypto_hashblocks_sha512_tweet ((x: Bytes.t), (x' : int)) ((m: Bytes.t), (m' : int)) (n: Int64.t) : int =
let m' = ref m' in
let n = ref n in
let (z: Int64.t array) = Array.make 8 0L in
let (b: Int64.t array) = Array.make 8 0L in
let (a: Int64.t array) = Array.make 8 0L in
let (w: Int64.t array) = Array.make 16 0L in
let (t: Int64.t ref) = ref 0L in
let (i: int ref) = ref 0 in
let (j: int ref) = ref 0 in
let (_'190: Int64.t ref) = ref 0L in
let (_'189: Int64.t ref) = ref 0L in
let (_'188: Int64.t ref) = ref 0L in
let (_'187: Int64.t ref) = ref 0L in
let (_'186: Int64.t ref) = ref 0L in
let (_'185: Int64.t ref) = ref 0L in
let (_'184: Int64.t ref) = ref 0L in
let (_'183: Int64.t ref) = ref 0L in
let (_'182: Int64.t ref) = ref 0L in
i := 0;
while !i < 8 do _'182 := ( dl64  ((x, x' + (*6*) 8 * !i)) (*Bytes.t*) ) ;
_'183 := (!_'182);
a.((!i)) <- !(_'183);
z.((!i)) <- !(_'183);
i := !i + (*2*) 1; done;
while !n >= Int64.of_int (128) do i := 0;
while !i < 16 do _'184 := ( dl64  ((m, (*6*) 8 * !i)) (*Bytes.t*) ) ;
w.((!i)) <- !(_'184);
i := !i + (*2*) 1; done;
i := 0;
while !i < 80 do j := 0;
while !j < 8 do b.((!j)) <- a.((!j));
j := !j + (*2*) 1; done;
_'185 := ( _Sigma1  (a.(4)) (*Int64.t*) ) ;
_'186 := ( _Ch  (a.(4)) (*Int64.t*)   (a.(5)) (*Int64.t*)   (a.(6)) (*Int64.t*) ) ;
t := Int64.add (*9*) (Int64.add (*9*) (Int64.add (*11*) (Int64.add (*11*) (a.(7)) (!_'185)) (!_'186)) (_K.((!i)))) (w.(!i mod 16));
_'187 := ( _Sigma0  (a.(0)) (*Int64.t*) ) ;
_'188 := ( _Maj  (a.(0)) (*Int64.t*)   (a.(1)) (*Int64.t*)   (a.(2)) (*Int64.t*) ) ;
b.(7) <- Int64.add (*11*) (Int64.add (*12*) !t !_'187) (!_'188);
b.(3) <- Int64.add (*11*) (b.(3)) (!t);
j := 0;
while !j < 8 do a.((!j + (*2*) 1) mod 8) <- b.((!j));
j := !j + (*2*) 1; done;
if !i mod 16 == 15 then begin j := 0;
while !j < 16 do _'189 := ( sigma0  (w.((!j + (*2*) 1) mod 16)) (*Int64.t*) ) ;
_'190 := ( sigma1  (w.((!j + (*2*) 14) mod 16)) (*Int64.t*) ) ;
w.((!j)) <- Int64.add (*9*) (w.((!j))) ((Int64.add (*11*) (Int64.add (*11*) (w.((!j + (*2*) 9) mod 16)) (!_'189)) (!_'190)));
j := !j + (*2*) 1; done; end;
i := !i + (*2*) 1; done;
i := 0;
while !i < 8 do a.((!i)) <- Int64.add (*9*) (a.((!i))) (z.((!i)));
z.((!i)) <- a.((!i));
i := !i + (*2*) 1; done;
m' := (!m' + 128);
n := Int64.sub !n (Int64.of_int (128)); done;
i := 0;
while !i < 8 do ts64 (x, x' + (*6*) 8 * !i) z.((!i));
i := !i + (*2*) 1; done;
let rslt = !n in Int64.to_int rslt

and ts64 ((x: Bytes.t), (x' : int)) (u: Int64.t) : unit =
let (u: Int64.t ref) = ref u in
let (i: int ref) = ref 0 in
i := 7;
while !i >= 0 do Bytes.set x (x'+(!i)) (char_trunc (Int64.to_int !u));
u := Int64.shift_right (!u) (8);
i := !i-1; done;
print_endline (string_of_int x');
dump "ts64: " (x,x') 8

and crypto_hash_sha512_tweet ((out: Bytes.t), (out' : int)) ((m: Bytes.t), (m' : int)) (n: Int64.t) : int =
let m' = ref m' in
let n = ref n in
let (h: Bytes.t) = Bytes.make 64 ' 'in
let (x: Bytes.t) = Bytes.make 256 ' ' in
let (i: Int64.t ref) = ref 0L in
let (b: Int64.t ref) = ref 0L in
b := !n;
i := Int64.of_int (0);
while !i < (Int64.of_int (64)) do Bytes.set h ((Int64.to_int !i)) (char_trunc iv.((Int64.to_int !i)));
i := (Int64.add (*5*) (!i) (Int64.of_int (1))); done;
ignore(crypto_hashblocks_sha512_tweet (h,0) (m,!m') !n);
m' := Int64.to_int (Int64.add (Int64.of_int !m') !n);
n := Int64.logand (!n) (Int64.of_int (127));
m' := Int64.to_int (Int64.sub (Int64.of_int !m') !n);
i := Int64.of_int (0);
while !i < (Int64.of_int (256)) do Bytes.set x ((Int64.to_int !i)) (char_trunc 0);
i := (Int64.add (*5*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < !n do Bytes.set x ((Int64.to_int !i)) (Bytes.get m (!m'+(Int64.to_int !i)));
i := (Int64.add (*5*) (!i) (Int64.of_int (1))); done;
Bytes.set x ((Int64.to_int !n)) (char_trunc 128);
n := Int64.of_int (256-128 * (if !n < 112L then 1 else 0));
Bytes.set x ((Int64.to_int (Int64.sub !n (Int64.of_int (9))))) (char_trunc (Int64.to_int (Int64.shift_right (!b) ( (61)))));
(*
ts64 (Int64.sub (x, n)) (Int64.of_int (8)) (Int64.shift_left !b (3));
*)
ignore (crypto_hashblocks_sha512_tweet (h,0) (x,0) !n);
i := Int64.of_int (0);
while !i < (Int64.of_int (64)) do Bytes.set out ((Int64.to_int !i)) (Bytes.get h (Int64.to_int !i));
i := (Int64.add (*5*) (!i) (Int64.of_int (1))); done;
let rslt = 0 in rslt

and modL ((r: Bytes.t), (r' : int)) (x: Int64.t array) : unit =
let (carry: Int64.t ref) = ref 0L in
let (i: Int64.t ref) = ref 0L in
let (j: Int64.t ref) = ref 0L in
i := Int64.of_int (63);
while !i >= Int64.of_int (32) do carry := Int64.of_int (0);
j := Int64.sub !i (Int64.of_int (32));
while !j < Int64.sub !i (Int64.of_int (12)) do
 x.((Int64.to_int !j)) <- Int64.add (*9*) (x.((Int64.to_int !j))) ((Int64.sub !carry ( (Int64.mul ( (Int64.mul (Int64.of_int (16)) x.((Int64.to_int !i)))) _L.((Int64.to_int (Int64.sub !j (Int64.sub !i (Int64.of_int (32))))))))));
carry := Int64.shift_right ((Int64.add (*4*) (x.((Int64.to_int !j))) (Int64.of_int (128)))) (8);
x.((Int64.to_int !j)) <- Int64.sub x.((Int64.to_int !j)) (Int64.shift_left !carry (8));
j := (Int64.add (*4*) (!j) (Int64.of_int (1))); done;
x.((Int64.to_int !j)) <- Int64.add (*9*) (x.((Int64.to_int !j))) (!carry);
x.((Int64.to_int !i)) <- 0L;
i := Int64.sub !i (Int64.of_int (1)); done;
carry := Int64.of_int (0);
j := Int64.of_int (0);
while !j < 32L do
 x.((Int64.to_int !j)) <- Int64.add (*9*) (x.((Int64.to_int !j))) ((Int64.sub !carry ( (Int64.mul (Int64.shift_right (x.(31)) (4)) _L.((Int64.to_int !j))))));
carry := Int64.shift_right (x.((Int64.to_int !j))) (8);
x.((Int64.to_int !j)) <- Int64.logand (x.((Int64.to_int !j))) (Int64.of_int (255));
j := (Int64.add (*4*) (!j) (Int64.of_int (1))); done;
j := Int64.of_int (0);
while !j < 32L do x.((Int64.to_int !j)) <- Int64.sub x.((Int64.to_int !j)) ( (Int64.mul !carry _L.((Int64.to_int !j))));
j := (Int64.add (*4*) (!j) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 32L do x.((Int64.to_int ((Int64.add (*4*) (!i) (Int64.of_int (1)))))) <- Int64.add (*9*) (x.((Int64.to_int ((Int64.add (*4*) (!i) (Int64.of_int (1))))))) ((Int64.shift_right (x.((Int64.to_int !i))) (8)));
Bytes.set r ((Int64.to_int !i)) (char_trunc (Int64.to_int (Int64.logand (x.((Int64.to_int !i))) (Int64.of_int (255)))));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
()

and reduce ((r: Bytes.t), (r' : int)) : unit =
let (x: Int64.t array) = Array.make 64 0L in
let (i: Int64.t ref) = ref 0L in
i := Int64.of_int (0);
while !i < 64L do x.((Int64.to_int !i)) <- Int64.of_int (int_of_char (Bytes.get r (Int64.to_int !i)));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 64L do Bytes.set r ((Int64.to_int !i)) (char_trunc 0);
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
modL (r,r') x;
()

and set25519 (r: Int64.t array) (a: Int64.t array) : unit =
let (i: int ref) = ref 0 in
i := 0;
(*
dump64 r;
dump64 a;
*)
while !i < 16 do
(*
print_endline (string_of_int !i);
*)
r.((!i)) <- a.((!i));
i := !i + (*2*) 1; done;
()

and cswap (p: Int64.t array array) (q: Int64.t array array) (b: char) : unit =
let (i: int ref) = ref 0 in
i := 0;
while !i < 4 do sel25519 p.((!i)) q.((!i)) (int_of_char b);
i := !i + (*2*) 1; done;
()

and _Z (o: Int64.t array) (a: Int64.t array) (b: Int64.t array) : unit =
let (i: int ref) = ref 0 in
i := 0;
while !i < 16 do o.((!i)) <- Int64.sub a.((!i)) b.((!i));
i := !i + (*2*) 1; done;
()

and _A (o: Int64.t array) (a: Int64.t array) (b: Int64.t array) : unit =
let (i: int ref) = ref 0 in
i := 0;
while !i < 16 do o.((!i)) <- Int64.add (*9*) (a.((!i))) (b.((!i)));
i := !i + (*2*) 1; done;
()

and add (p: Int64.t array array) (q: Int64.t array array) : unit =
let (a: Int64.t array) = Array.make 16 0L in
let (b: Int64.t array) = Array.make 16 0L in
let (c: Int64.t array) = Array.make 16 0L in
let (d: Int64.t array) = Array.make 16 0L in
let (t: Int64.t array) = Array.make 16 0L in
let (e: Int64.t array) = Array.make 16 0L in
let (f: Int64.t array) = Array.make 16 0L in
let (g: Int64.t array) = Array.make 16 0L in
let (h: Int64.t array) = Array.make 16 0L in
_Z a p.(1) p.(0);
_Z t q.(1) q.(0);
_M a a t;
_A b p.(0) p.(1);
_A t q.(0) q.(1);
_M b b t;
_M c p.(3) q.(3);
_M c c _D2;
_M d p.(2) q.(2);
_A d d d;
_Z e b a;
_Z f d c;
_A g d c;
_A h b a;
_M p.(0) e f;
_M p.(1) h g;
_M p.(2) g f;
_M p.(3) e h;
()

and scalarmult (p: Int64.t array array) (q: Int64.t array array) ((s: Bytes.t), (s' : int)) : unit =
let (i: int ref) = ref 0 in
let (b: char ref) = ref ' ' in
set25519 p.(0) gf0;
set25519 p.(1) gf1;
set25519 p.(2) gf1;
set25519 p.(3) gf0;
i := 255;
while !i >= 0 do b := char_trunc (Int.logand (Int.shift_right (int_of_char (Bytes.get s (!i / 8))) ((Int.logand (!i) (7)))) (1));
cswap p q !b;
add q p;
add p p;
cswap p q !b;
i := !i-1; done;
()

and scalarbase (p: Int64.t array array) ((s: Bytes.t), (s' : int)) : unit =
let (q: Int64.t array array) = Array.make_matrix 4 16 0L in
set25519 q.(0) _X;
set25519 q.(1) _Y;
set25519 q.(2) gf1;
_M q.(3) _X _Y;
scalarmult p q (s,0);
()

and _S (o: Int64.t array) (a: Int64.t array) : unit =
_M o a a;
()

and inv25519 (o: Int64.t array) (i: Int64.t array) : unit =
let (c: Int64.t array) = Array.make 16 0L in
let (a: int ref) = ref 0 in
let (_'182: int ref) = ref 0 in
a := 0;
while !a < 16 do c.((!a)) <- i.((!a));
a := !a + (*2*) 1; done;
a := 253;
while !a >= 0 do _S c c;
if !a <> 2 then begin _'182 := (if (!a <> 4) then 1 else 0); end else begin _'182 := 0; end;
if !_'182 <> 0 then begin _M c c i; end;
a := !a-1; done;
a := 0;
while !a < 16 do o.((!a)) <- c.((!a));
a := !a + (*2*) 1; done;
()

and _M (o: Int64.t array) (a: Int64.t array) (b: Int64.t array) : unit =
let (i: Int64.t ref ) = ref 0L in
let (j: Int64.t ref ) = ref 0L in
let (t: Int64.t array) = Array.make 31 0L in
i := Int64.of_int (0);
while !i < 31L do t.((Int64.to_int !i)) <- 0L;
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 16L do j := Int64.of_int (0);
while !j < 16L do t.((Int64.to_int (Int64.add (*9*) (!i) (!j)))) <- (Int64.add (*4*) (t.((Int64.to_int (Int64.add (*9*) (!i) (!j))))) ((Int64.mul a.((Int64.to_int !i)) b.((Int64.to_int !j)))));
j := (Int64.add (*4*) (!j) (Int64.of_int (1))); done;
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 15L do t.((Int64.to_int !i)) <- (Int64.add (*4*) (t.((Int64.to_int !i))) ( (Int64.mul (Int64.of_int (38)) t.((Int64.to_int ((Int64.add (*4*) (!i) (Int64.of_int (16)))))))));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 16L do o.((Int64.to_int !i)) <- t.((Int64.to_int !i));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
car25519 o;
car25519 o;
()

and car25519 (o: Int64.t array) : unit =
let (i: int ref) = ref 0 in
let (c: Int64.t ref) = ref 0L in
i := 0;
while !i < 16 do o.((!i)) <- Int64.add (*9*) (o.((!i))) ((Int64.shift_left 1L (16)));
c := Int64.shift_right (o.((!i))) (16);
o.((!i + (*2*) 1) * (if !i < 15 then 1 else 0)) <- Int64.add (*9*) (o.((!i + (*2*) 1) * (if !i < 15 then 1 else 0))) (((Int64.add (*4*) (Int64.sub !c (Int64.of_int (1))) (Int64.mul (Int64.mul (Int64.of_int (37)) (Int64.sub !c (Int64.of_int (1))))  (if !i == 15 then 1L else 0L)))));
o.((!i)) <- Int64.sub o.((!i)) (Int64.shift_left !c (16));
i := !i + (*2*) 1; done;
()

and pack25519 ((o: Bytes.t), (o' : int)) (n: Int64.t array) : unit =
let (i: int ref) = ref 0 in
let (j: int ref) = ref 0 in
let (b: int ref) = ref 0 in
let (m: Int64.t array) = Array.make 16 0L in
let (t: Int64.t array) = Array.make 16 0L in
i := 0;
while !i < 16 do t.((!i)) <- n.((!i));
i := !i + (*2*) 1; done;
car25519 t;
car25519 t;
car25519 t;
j := 0;
while !j < 2 do m.(0) <- Int64.sub t.(0) (Int64.of_int (65517));
i := 1;
while !i < 15 do m.((!i)) <- Int64.sub (Int64.sub t.((!i)) (Int64.of_int (65535))) (Int64.logand (Int64.shift_right (m.((!i-1))) (16)) (Int64.of_int (1)));
m.((!i-1)) <- Int64.logand (m.((!i-1))) (Int64.of_int (65535));
i := !i + (*2*) 1; done;
m.(15) <- Int64.sub (Int64.sub t.(15) (Int64.of_int (32767))) (Int64.logand (Int64.shift_right (m.(14)) (16)) (Int64.of_int (1)));
b := Int64.to_int (Int64.logand (Int64.shift_right (m.(15)) (16)) (Int64.of_int (1)));
m.(14) <- Int64.logand (m.(14)) (Int64.of_int (65535));
sel25519 t m (1 - !b);
j := !j + (*2*) 1; done;
i := 0;
while !i < 16 do Bytes.set o (2 * !i) (char_trunc (Int64.to_int ((Int64.logand (t.((!i))) (Int64.of_int (255))))));
Bytes.set o ((2 * !i + (*1*) 1)) (char_trunc (Int64.to_int (Int64.shift_right (t.((!i))) (8))));
i := !i + (*2*) 1; done;
()

and par25519 (a: Int64.t array) : char =
let (d: Bytes.t) = Bytes.make 32 ' ' in
pack25519 (d,0) a;
let rslt = Int.logand (int_of_char (Bytes.get d 0)) (1) in char_trunc rslt

and pack ((r: Bytes.t), (r' : int)) (p: Int64.t array array) : unit =
let (tx: Int64.t array) = Array.make 16 0L in
let (ty: Int64.t array) = Array.make 16 0L in
let (zi: Int64.t array) = Array.make 16 0L in
let (_'182: char ref) = ref ' ' in
inv25519 zi p.(2);
_M tx p.(0) zi;
_M ty p.(1) zi;
pack25519 (r,r') ty;
_'182 := ( par25519  (tx) (*Int64.t array*) ) ;
Bytes.set r (31) (char_trunc (Int.logxor (int_of_char (Bytes.get r 31)) (Int.shift_left (int_of_char !_'182) 7)));
()

and crypto_sign_ed25519_tweet ((sm: Bytes.t), (sm' : int)) (smlen: Int64.t array) ((m: Bytes.t), (m' : int)) (n: Int64.t) ((sk: Bytes.t), (sk' : int)) : int =
let (d: Bytes.t) = Bytes.make 64 ' ' in
let (h: Bytes.t) = Bytes.make 64 ' ' in
let (r: Bytes.t) = Bytes.make 64 ' ' in
let (i: Int64.t ref) = ref 0L in
let (j: Int64.t ref) = ref 0L in
let (x: Int64.t array) = Array.make 64 0L in
let (p: Int64.t array array) = Array.make_matrix 4 16 0L in
ignore (crypto_hash_sha512_tweet (d,0) (sk,sk') 32L);
Bytes.set d (0) (char_trunc (Int.logand (int_of_char (Bytes.get d 0)) (248)));
Bytes.set d (31) (char_trunc (Int.logand (int_of_char (Bytes.get d 31)) (127)));
Bytes.set d (31) (char_trunc (Int.logor (int_of_char (Bytes.get d 31)) (64)));
smlen.(0) <- (Int64.add (*4*) (n) (Int64.of_int (64)));
i := Int64.of_int (0);
while !i < n do Bytes.set sm (Int64.to_int (((Int64.add (*8*) (Int64.of_int (64)) (!i))))) (Bytes.get m (Int64.to_int !i));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 32L do Bytes.set sm (Int64.to_int (((Int64.add (*8*) (Int64.of_int (32)) (!i))))) (Bytes.get d (Int64.to_int ((Int64.add (*8*) (Int64.of_int (32)) (!i)))));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
ignore (crypto_hash_sha512_tweet (r,0) (sm, (*6*) 32) (Int64.add (*4*) (n) (Int64.of_int (32))));
reduce (r,0);
scalarbase p (r,0);
pack (sm,sm') p;
i := Int64.of_int (0);
while !i < 32L do Bytes.set sm ((Int64.to_int ((Int64.add (*4*) (!i) (Int64.of_int (32)))))) (Bytes.get sk (Int64.to_int ((Int64.add (*4*) (!i) (Int64.of_int (32))))));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
ignore(crypto_hash_sha512_tweet (h,0) (sm,sm') (Int64.add (*4*) (n) (Int64.of_int (64))));
reduce (h,0);
i := Int64.of_int (0);
while !i < 64L do x.((Int64.to_int !i)) <- 0L;
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 32L do x.((Int64.to_int !i)) <- Int64.of_int (int_of_char (Bytes.get r (Int64.to_int !i)));
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
i := Int64.of_int (0);
while !i < 32L do j := Int64.of_int (0);
while !j < 32L do
x.((Int64.to_int (Int64.add (*9*) (!i) (!j)))) <- (Int64.add (*4*) (x.((Int64.to_int (Int64.add (*9*) (!i) (!j))))) ( (Int64.mul (Int64.of_int (int_of_char (Bytes.get h (Int64.to_int !i)))) (Int64.of_int (int_of_char (Bytes.get d (Int64.to_int !j)))))));
j := (Int64.add (*4*) (!j) (Int64.of_int (1))); done;
i := (Int64.add (*4*) (!i) (Int64.of_int (1))); done;
modL (sm, (*6*) 32) x;
let rslt = 0 in rslt

