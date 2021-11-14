package types;

  parameter [7:0] wid = 8;

   typedef logic [wid-1:0] sz;

endpackage // types

module count8 
import types::*;
 # (parameter type sz = sz) (input clk, input rst, input en, output sz q);

   always @(posedge clk or posedge rst)
     if (rst)
       q = {{types::wid}{1'b0}};
     else if (en)
       begin
	  q = q + 1;
       end

endmodule
