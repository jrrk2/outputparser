module slib_input_filter #(parameter SIZE = 2) (
	input wire		CLK,
	input wire		RST,
	input wire		CE,
	input wire		D,
	output logic		Q);  
 
 
typedef enum {FALSE,TRUE} bool_t;  

reg [$clog2(SIZE+1)-1:0] iCount;
   
always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
          iCount <= 0;  
          Q <=  1'b0;  
       end
     else
       begin
          if ((CE ==  1'b1))
            begin
               if ((D ==  1'b1) && (iCount != SIZE))
                 begin
                    iCount <= iCount + 1;  
                 end
               else if         ((D ==  1'b0) && (iCount != 0))
                 begin
                    iCount <= iCount - 1;  
                 end
            end
          
          if ((iCount == SIZE))
            begin
               Q <=  1'b1;  
            end
          else if     ((iCount == 0))
            begin
               Q <=  1'b0;  
            end
       end
     
  end
   

endmodule
