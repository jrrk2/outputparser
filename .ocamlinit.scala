open CompilationUnit
open CompilationUnit_types
open CompilationUnit_main
open CompilationUnit_filt
open CompilationUnit_transform

let chan = open_out ("test.scala");;
let main = "Adder";;
let args = [| ""; "../chisel-tutorial/src/main/scala/solutions/Adder.scala" |];;
let rslts = dump parse chan main args;;
close_out chan;;
let (hd::tl) = rslts;;
