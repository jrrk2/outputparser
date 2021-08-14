module slib_edge_detect(
	input wire		CLK,
	input wire		RST,
	input wire		D,
	output reg		RE,
	output reg		FE);  
 
reg iDd;  

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  iDd <=  1'b0;  
      end
  else
  begin
  iDd <= D;  
      end
  
end

assign   RE = iDd ==  1'b0 && D ==  1'b1 ?  1'b1 :   1'b0;  
assign   FE = iDd ==  1'b1 && D ==  1'b0 ?  1'b1 :   1'b0;  

endmodule  
