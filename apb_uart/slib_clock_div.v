module slib_clock_div #(parameter RATIO = 8) (
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	output reg		Q);  
 
reg iQ;  
reg [$clog2(RATIO-1)-1:0] iCounter;
   
always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  iCounter <= 0;  
    iQ <=  1'b0;  
      end
  else
  begin
  iQ <=  1'b0;  
    if ((CE ==  1'b1))
          begin
      if ((iCounter == (RATIO - 1)))
                  begin
          iQ <=  1'b1;  
            iCounter <= 0;  
                      end
           else
          begin
          iCounter <= iCounter + 1;  
                      end
                end
      
  end
  
end

assign   Q = iQ;  

endmodule  
