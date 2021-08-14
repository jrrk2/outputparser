module uart_baudgen(
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	input wire		CLEAR,
	input wire	[15:0] 	DIVIDER,
	output reg		BAUDTICK);  
 
reg [15:0] iCounter;  

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
   
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
BAUDTICK <=  1'b0;  
      end
  else
  begin
  if ((CLEAR ==  1'b1))
          begin
       
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
      end
          else if     ((CE ==  1'b1))
          begin
      iCounter <= iCounter + 1;  
              end
      BAUDTICK <=  1'b0;  
    if ((iCounter == $unsigned(DIVIDER)))
          begin
       
iCounter <= (0<<15)|(0<<14)|(0<<13)|(0<<12)|(0<<11)|(0<<10)|(0<<9)|(0<<8)|(0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
BAUDTICK <=  1'b1;  
              end
      
  end
  
end

endmodule  
