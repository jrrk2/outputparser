module slib_counter # (parameter WIDTH = 4) (
	input wire		CLK,
	input wire		RST,
	input wire		CLEAR,
	input wire		LOAD,
	input wire		ENABLE,
	input wire		DOWN,
	input wire	[WIDTH - 1:0] 	D,
	output reg	[WIDTH - 1:0] 	Q,
	output reg		OVERFLOW);  
 
reg [WIDTH:0] iCounter;  

always @(posedge CLK or posedge RST)
if (RST)
  begin
      
     iCounter <= 0;
  end
else
  begin
     if ((CLEAR ==  1'b1))
       begin
           
          iCounter <= 0;
       end
     else if ((LOAD ==  1'b1))
       begin
          iCounter <= $unsigned({ 1'b0, D});  
       end
     
     else if ((ENABLE ==  1'b1))
       begin
          if ((DOWN ==  1'b0))
            begin
               iCounter <= iCounter + 1;  
            end
          else
            begin
               iCounter <= iCounter - 1;  
            end
       end
     
     if ((iCounter[WIDTH] ==  1'b1))
       begin
          iCounter[WIDTH] <= 0;
       end
  end
  
assign   Q = iCounter[WIDTH - 1:0];  
assign   OVERFLOW = iCounter[WIDTH];  

endmodule  
