/* blocking module */

module blocking(output wire rdy, input wire clk, input wire reset);

   reg [4:0] count, inc;
   assign rdy = count[4];

   always @(posedge clk or posedge reset)
     if (reset)
       count <= 15;
     else
       begin
	  inc = -1;
	  count <= count + inc;
       end

endmodule // blocking
