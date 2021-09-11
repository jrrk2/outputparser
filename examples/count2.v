module count2(input clk, input rst, input en, output reg [7:0] q);

   always @(posedge clk)
     if (rst)
       q = 8'b0;
     else if (en)
       begin
	  q = q + 8'b1;
	  q = q + 8'b1;
       end
   
endmodule // query
