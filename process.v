/*
 */

module process(output reg [7:0] upcounter,
	       output reg [7:0] downcounter,
	       input wire 	clk,
	       input wire 	reset);

   always @(posedge clk)
     if (reset)
       begin
	  upcounter <= 8'b0;
	  downcounter <= 8'b0;
       end
     else
       begin
	  upcounter <= upcounter + 1;
	  downcounter <= downcounter - 1;
       end

endmodule // process
