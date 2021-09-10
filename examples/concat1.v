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

module concat1 (
        input clk,
        input axi_test,
        input [7:0] xorshift64_state,
	output reg [4:0] fast_axi_transaction,
	output reg [4:0] async_axi_transaction,
	output reg [4:0] delay_axi_transaction);

	always @(posedge clk) begin
		if (axi_test) begin
				{fast_axi_transaction, async_axi_transaction, delay_axi_transaction} <= xorshift64_state;
		end
	end
endmodule
