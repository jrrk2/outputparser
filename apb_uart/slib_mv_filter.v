module slib_mv_filter #(parameter WIDTH = 4, THRESHOLD = 10) (
	input wire		CLK,
	input wire		RST,
	input wire		SAMPLE,
	input wire		CLEAR,
	input wire		D,
	output logic		Q);  
 
 
typedef enum {FALSE,TRUE} bool_t;  
reg [WIDTH:0] iCounter;  
reg iQ;  

always @(posedge CLK or posedge RST)
  if (RST)
    begin
        
       iCounter <= 0;
       iQ <=  1'b0;  
    end
  else
    begin
       if (iCounter >= THRESHOLD)
         begin
            iQ <=  1'b1;  
         end
       else
         begin
            if ((SAMPLE ==  1'b1 && D ==  1'b1))
              begin
                 iCounter <= iCounter + 1;  
              end
         end
       if ((CLEAR ==  1'b1))
         begin
             
            iCounter <= 0;
            iQ <=  1'b0;  
         end
    end

assign   Q = iQ;  

endmodule  
