module slib_input_sync(
	input wire		CLK,
	input wire		RST,
	input wire		D,
	output logic		Q);  
 
 
typedef enum {FALSE,TRUE} bool_t;  
reg [1:0] iD;  

always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
           
          iD <= (0<<1)|(0<<0);
       end
     else
       begin
          iD[0] <= D;
          iD[1] <= iD[0];       
       end
  end

assign   Q = iD[1];  
endmodule  
