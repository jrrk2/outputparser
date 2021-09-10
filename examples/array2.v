/*
    PicoRV32 -- A Small RISC-V (RV32I) Processor Core
 
    Copyright (C) 2015  Clifford Wolf <clifford@clifford.at>
 
    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.
 
    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 
 */

module array2 (
	input             clk,
	input       [2:0] sel,
 	input             latched_rinsn,
	output reg  [7:0] osel
);

always @(posedge clk)
  case (sel)
    0: osel[0] <= latched_rinsn;
    1: osel[1] <= latched_rinsn;
    2: osel[2] <= latched_rinsn;
    3: osel[3] <= latched_rinsn;
    4: osel[4] <= latched_rinsn;
    5: osel[5] <= latched_rinsn;
    6: osel[6] <= latched_rinsn;
    7: osel[7] <= latched_rinsn;
  endcase // case (sel)
   
endmodule
