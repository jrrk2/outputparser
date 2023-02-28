open Crypto_sign_ed25519_tweet_template_edited

(*
let dump v =
  Bytes.iter (fun itm -> Printf.printf "%.2x " (int_of_char itm)) v
*)

let test_main ((): unit) : unit =
let (mlen: Int64.t ref) = ref 0L in
let (smlen: Int64.t array) = Array.make 1 0L in
let (n: Int64.t ref) = ref 0L in
let (secret_key: Bytes.t) = Bytes.make 64 ' ' in
let (plain_text: Bytes.t) = Bytes.make 64 ' ' in
let (signed_text: Bytes.t) = Bytes.make 128 ' ' in
let (actual: Bytes.t) = Bytes.make 128 ' ' in
mlen := Int64.of_int (- (1));
smlen.(0) <- Int64.of_int (- (1));
n := Int64.of_int (8);
Bytes.set secret_key (0) (char_of_int 59);
Bytes.set secret_key (1) (char_of_int 193);
Bytes.set secret_key (2) (char_of_int 131);
Bytes.set secret_key (3) (char_of_int 246);
Bytes.set secret_key (4) (char_of_int 219);
Bytes.set secret_key (5) (char_of_int 92);
Bytes.set secret_key (6) (char_of_int 230);
Bytes.set secret_key (7) (char_of_int 150);
Bytes.set secret_key (8) (char_of_int 193);
Bytes.set secret_key (9) (char_of_int 253);
Bytes.set secret_key (10) (char_of_int 11);
Bytes.set secret_key (11) (char_of_int 194);
Bytes.set secret_key (12) (char_of_int 43);
Bytes.set secret_key (13) (char_of_int 85);
Bytes.set secret_key (14) (char_of_int 187);
Bytes.set secret_key (15) (char_of_int 77);
Bytes.set secret_key (16) (char_of_int 245);
Bytes.set secret_key (17) (char_of_int 78);
Bytes.set secret_key (18) (char_of_int 153);
Bytes.set secret_key (19) (char_of_int 222);
Bytes.set secret_key (20) (char_of_int 25);
Bytes.set secret_key (21) (char_of_int 98);
Bytes.set secret_key (22) (char_of_int 88);
Bytes.set secret_key (23) (char_of_int 14);
Bytes.set secret_key (24) (char_of_int 103);
Bytes.set secret_key (25) (char_of_int 109);
Bytes.set secret_key (26) (char_of_int 144);
Bytes.set secret_key (27) (char_of_int 191);
Bytes.set secret_key (28) (char_of_int 62);
Bytes.set secret_key (29) (char_of_int 134);
Bytes.set secret_key (30) (char_of_int 132);
Bytes.set secret_key (31) (char_of_int 198);
Bytes.set secret_key (32) (char_of_int 104);
Bytes.set secret_key (33) (char_of_int 35);
Bytes.set secret_key (34) (char_of_int 198);
Bytes.set secret_key (35) (char_of_int 236);
Bytes.set secret_key (36) (char_of_int 77);
Bytes.set secret_key (37) (char_of_int 96);
Bytes.set secret_key (38) (char_of_int 188);
Bytes.set secret_key (39) (char_of_int 224);
Bytes.set secret_key (40) (char_of_int 67);
Bytes.set secret_key (41) (char_of_int 193);
Bytes.set secret_key (42) (char_of_int 103);
Bytes.set secret_key (43) (char_of_int 99);
Bytes.set secret_key (44) (char_of_int 243);
Bytes.set secret_key (45) (char_of_int 99);
Bytes.set secret_key (46) (char_of_int 156);
Bytes.set secret_key (47) (char_of_int 122);
Bytes.set secret_key (48) (char_of_int 135);
Bytes.set secret_key (49) (char_of_int 155);
Bytes.set secret_key (50) (char_of_int 99);
Bytes.set secret_key (51) (char_of_int 64);
Bytes.set secret_key (52) (char_of_int 17);
Bytes.set secret_key (53) (char_of_int 49);
Bytes.set secret_key (54) (char_of_int 216);
Bytes.set secret_key (55) (char_of_int 158);
Bytes.set secret_key (56) (char_of_int 209);
Bytes.set secret_key (57) (char_of_int 113);
Bytes.set secret_key (58) (char_of_int 190);
Bytes.set secret_key (59) (char_of_int 250);
Bytes.set secret_key (60) (char_of_int 218);
Bytes.set secret_key (61) (char_of_int 63);
Bytes.set secret_key (62) (char_of_int 1);
Bytes.set secret_key (63) (char_of_int 247);
Bytes.set plain_text (0) (char_of_int 90);
Bytes.set plain_text (1) (char_of_int 164);
Bytes.set plain_text (2) (char_of_int 159);
Bytes.set plain_text (3) (char_of_int 132);
Bytes.set plain_text (4) (char_of_int 206);
Bytes.set plain_text (5) (char_of_int 75);
Bytes.set plain_text (6) (char_of_int 53);
Bytes.set plain_text (7) (char_of_int 185);
Bytes.set plain_text (8) (char_of_int 44);
Bytes.set plain_text (9) (char_of_int 165);
Bytes.set plain_text (10) (char_of_int 222);
Bytes.set plain_text (11) (char_of_int 132);
Bytes.set plain_text (12) (char_of_int 175);
Bytes.set plain_text (13) (char_of_int 188);
Bytes.set plain_text (14) (char_of_int 235);
Bytes.set plain_text (15) (char_of_int 193);
Bytes.set plain_text (16) (char_of_int 69);
Bytes.set plain_text (17) (char_of_int 0);
Bytes.set plain_text (18) (char_of_int 127);
Bytes.set plain_text (19) (char_of_int 243);
Bytes.set plain_text (20) (char_of_int 23);
Bytes.set plain_text (21) (char_of_int 212);
Bytes.set plain_text (22) (char_of_int 170);
Bytes.set plain_text (23) (char_of_int 132);
Bytes.set plain_text (24) (char_of_int 6);
Bytes.set plain_text (25) (char_of_int 27);
Bytes.set plain_text (26) (char_of_int 172);
Bytes.set plain_text (27) (char_of_int 123);
Bytes.set plain_text (28) (char_of_int 206);
Bytes.set plain_text (29) (char_of_int 45);
Bytes.set plain_text (30) (char_of_int 124);
Bytes.set plain_text (31) (char_of_int 80);
Bytes.set plain_text (32) (char_of_int 194);
Bytes.set plain_text (33) (char_of_int 243);
Bytes.set plain_text (34) (char_of_int 109);
Bytes.set plain_text (35) (char_of_int 194);
Bytes.set plain_text (36) (char_of_int 145);
Bytes.set plain_text (37) (char_of_int 233);
Bytes.set plain_text (38) (char_of_int 5);
Bytes.set plain_text (39) (char_of_int 12);
Bytes.set plain_text (40) (char_of_int 251);
Bytes.set plain_text (41) (char_of_int 93);
Bytes.set plain_text (42) (char_of_int 19);
Bytes.set plain_text (43) (char_of_int 104);
Bytes.set plain_text (44) (char_of_int 155);
Bytes.set plain_text (45) (char_of_int 210);
Bytes.set plain_text (46) (char_of_int 207);
Bytes.set plain_text (47) (char_of_int 109);
Bytes.set plain_text (48) (char_of_int 72);
Bytes.set plain_text (49) (char_of_int 88);
Bytes.set plain_text (50) (char_of_int 172);
Bytes.set plain_text (51) (char_of_int 157);
Bytes.set plain_text (52) (char_of_int 233);
Bytes.set plain_text (53) (char_of_int 15);
Bytes.set plain_text (54) (char_of_int 213);
Bytes.set plain_text (55) (char_of_int 228);
Bytes.set plain_text (56) (char_of_int 40);
Bytes.set plain_text (57) (char_of_int 229);
Bytes.set plain_text (58) (char_of_int 200);
Bytes.set plain_text (59) (char_of_int 229);
Bytes.set plain_text (60) (char_of_int 95);
Bytes.set plain_text (61) (char_of_int 163);
Bytes.set plain_text (62) (char_of_int 79);
Bytes.set plain_text (63) (char_of_int 251);
Bytes.set signed_text (0) (char_of_int 205);
Bytes.set signed_text (1) (char_of_int 199);
Bytes.set signed_text (2) (char_of_int 186);
Bytes.set signed_text (3) (char_of_int 178);
Bytes.set signed_text (4) (char_of_int 205);
Bytes.set signed_text (5) (char_of_int 97);
Bytes.set signed_text (6) (char_of_int 158);
Bytes.set signed_text (7) (char_of_int 184);
Bytes.set signed_text (8) (char_of_int 22);
Bytes.set signed_text (9) (char_of_int 79);
Bytes.set signed_text (10) (char_of_int 198);
Bytes.set signed_text (11) (char_of_int 219);
Bytes.set signed_text (12) (char_of_int 93);
Bytes.set signed_text (13) (char_of_int 241);
Bytes.set signed_text (14) (char_of_int 249);
Bytes.set signed_text (15) (char_of_int 48);
Bytes.set signed_text (16) (char_of_int 187);
Bytes.set signed_text (17) (char_of_int 32);
Bytes.set signed_text (18) (char_of_int 219);
Bytes.set signed_text (19) (char_of_int 211);
Bytes.set signed_text (20) (char_of_int 12);
Bytes.set signed_text (21) (char_of_int 180);
Bytes.set signed_text (22) (char_of_int 184);
Bytes.set signed_text (23) (char_of_int 129);
Bytes.set signed_text (24) (char_of_int 190);
Bytes.set signed_text (25) (char_of_int 182);
Bytes.set signed_text (26) (char_of_int 79);
Bytes.set signed_text (27) (char_of_int 121);
Bytes.set signed_text (28) (char_of_int 222);
Bytes.set signed_text (29) (char_of_int 247);
Bytes.set signed_text (30) (char_of_int 88);
Bytes.set signed_text (31) (char_of_int 84);
Bytes.set signed_text (32) (char_of_int 101);
Bytes.set signed_text (33) (char_of_int 20);
Bytes.set signed_text (34) (char_of_int 104);
Bytes.set signed_text (35) (char_of_int 85);
Bytes.set signed_text (36) (char_of_int 20);
Bytes.set signed_text (37) (char_of_int 138);
Bytes.set signed_text (38) (char_of_int 9);
Bytes.set signed_text (39) (char_of_int 188);
Bytes.set signed_text (40) (char_of_int 94);
Bytes.set signed_text (41) (char_of_int 110);
Bytes.set signed_text (42) (char_of_int 98);
Bytes.set signed_text (43) (char_of_int 106);
Bytes.set signed_text (44) (char_of_int 122);
Bytes.set signed_text (45) (char_of_int 163);
Bytes.set signed_text (46) (char_of_int 25);
Bytes.set signed_text (47) (char_of_int 40);
Bytes.set signed_text (48) (char_of_int 81);
Bytes.set signed_text (49) (char_of_int 180);
Bytes.set signed_text (50) (char_of_int 42);
Bytes.set signed_text (51) (char_of_int 56);
Bytes.set signed_text (52) (char_of_int 128);
Bytes.set signed_text (53) (char_of_int 117);
Bytes.set signed_text (54) (char_of_int 201);
Bytes.set signed_text (55) (char_of_int 14);
Bytes.set signed_text (56) (char_of_int 205);
Bytes.set signed_text (57) (char_of_int 134);
Bytes.set signed_text (58) (char_of_int 137);
Bytes.set signed_text (59) (char_of_int 214);
Bytes.set signed_text (60) (char_of_int 32);
Bytes.set signed_text (61) (char_of_int 146);
Bytes.set signed_text (62) (char_of_int 203);
Bytes.set signed_text (63) (char_of_int 12);
Bytes.set signed_text (64) (char_of_int 90);
Bytes.set signed_text (65) (char_of_int 164);
Bytes.set signed_text (66) (char_of_int 159);
Bytes.set signed_text (67) (char_of_int 132);
Bytes.set signed_text (68) (char_of_int 206);
Bytes.set signed_text (69) (char_of_int 75);
Bytes.set signed_text (70) (char_of_int 53);
Bytes.set signed_text (71) (char_of_int 185);
Bytes.set signed_text (72) (char_of_int 44);
Bytes.set signed_text (73) (char_of_int 165);
Bytes.set signed_text (74) (char_of_int 222);
Bytes.set signed_text (75) (char_of_int 132);
Bytes.set signed_text (76) (char_of_int 175);
Bytes.set signed_text (77) (char_of_int 188);
Bytes.set signed_text (78) (char_of_int 235);
Bytes.set signed_text (79) (char_of_int 193);
Bytes.set signed_text (80) (char_of_int 69);
Bytes.set signed_text (81) (char_of_int 0);
Bytes.set signed_text (82) (char_of_int 127);
Bytes.set signed_text (83) (char_of_int 243);
Bytes.set signed_text (84) (char_of_int 23);
Bytes.set signed_text (85) (char_of_int 212);
Bytes.set signed_text (86) (char_of_int 170);
Bytes.set signed_text (87) (char_of_int 132);
Bytes.set signed_text (88) (char_of_int 6);
Bytes.set signed_text (89) (char_of_int 27);
Bytes.set signed_text (90) (char_of_int 172);
Bytes.set signed_text (91) (char_of_int 123);
Bytes.set signed_text (92) (char_of_int 206);
Bytes.set signed_text (93) (char_of_int 45);
Bytes.set signed_text (94) (char_of_int 124);
Bytes.set signed_text (95) (char_of_int 80);
Bytes.set signed_text (96) (char_of_int 194);
Bytes.set signed_text (97) (char_of_int 243);
Bytes.set signed_text (98) (char_of_int 109);
Bytes.set signed_text (99) (char_of_int 194);
Bytes.set signed_text (100) (char_of_int 145);
Bytes.set signed_text (101) (char_of_int 233);
Bytes.set signed_text (102) (char_of_int 5);
Bytes.set signed_text (103) (char_of_int 12);
Bytes.set signed_text (104) (char_of_int 251);
Bytes.set signed_text (105) (char_of_int 93);
Bytes.set signed_text (106) (char_of_int 19);
Bytes.set signed_text (107) (char_of_int 104);
Bytes.set signed_text (108) (char_of_int 155);
Bytes.set signed_text (109) (char_of_int 210);
Bytes.set signed_text (110) (char_of_int 207);
Bytes.set signed_text (111) (char_of_int 109);
Bytes.set signed_text (112) (char_of_int 72);
Bytes.set signed_text (113) (char_of_int 88);
Bytes.set signed_text (114) (char_of_int 172);
Bytes.set signed_text (115) (char_of_int 157);
Bytes.set signed_text (116) (char_of_int 233);
Bytes.set signed_text (117) (char_of_int 15);
Bytes.set signed_text (118) (char_of_int 213);
Bytes.set signed_text (119) (char_of_int 228);
Bytes.set signed_text (120) (char_of_int 40);
Bytes.set signed_text (121) (char_of_int 229);
Bytes.set signed_text (122) (char_of_int 200);
Bytes.set signed_text (123) (char_of_int 229);
Bytes.set signed_text (124) (char_of_int 95);
Bytes.set signed_text (125) (char_of_int 163);
Bytes.set signed_text (126) (char_of_int 79);
Bytes.set signed_text (127) (char_of_int 251);
ignore(crypto_sign_ed25519_tweet (actual,0) smlen (plain_text,0) 64L (secret_key,0));
dump "actual: " (actual,0) 128
