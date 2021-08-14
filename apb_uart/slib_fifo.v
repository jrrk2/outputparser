module slib_fifo # (parameter WIDTH = 8, parameter SIZE_E=6) (
	input wire		CLK,
	input wire		RST,
	input wire		CLEAR,
	input wire		WRITE,
	input wire		READ,
	input wire	[WIDTH - 1:0] 	D,
	output logic	[WIDTH - 1:0] 	Q,
	output logic		EMPTY,
	output logic		FULL,
	output logic	[SIZE_E - 1:0] 	USAGE);  
 
 
typedef enum {FALSE,TRUE} bool_t;  
reg iEMPTY;  
reg iFULL;  
reg [SIZE_E:0] iWRAddr;  
reg [SIZE_E:0] iRDAddr;  
reg [SIZE_E:0] init;  
reg [SIZE_E - 1:0] iUSAGE;  

       reg [WIDTH-1:0] iFIFOMem [0:2**SIZE_E-1];

       always @(posedge CLK or posedge RST)
       begin
       if ((RST ==  1'b1))
         begin
            Q <= (0<<0);
         end
         else
         begin
         if ((WRITE ==  1'b1 && iFULL ==  1'b0))
           begin
              iFIFOMem[iWRAddr[SIZE_E-1:0]] <= D;
           end

       Q <= iFIFOMem[iRDAddr[SIZE_E - 1:0]];  
             end

       end
   
assign   iFULL = (iRDAddr[SIZE_E - 1:0]  == iWRAddr[SIZE_E - 1:0] ) && (iRDAddr[SIZE_E] != iWRAddr[SIZE_E]) ?  1'b1 :   1'b0;  

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
   
iWRAddr <= 0;
 
iRDAddr <= 0;
iEMPTY <=  1'b1;  
      end
  else
  begin
  if ((WRITE ==  1'b1 && iFULL ==  1'b0))
          begin
      iWRAddr <= iWRAddr + 1;  
              end
      
if ((READ ==  1'b1 && iEMPTY ==  1'b0))
          begin
      iRDAddr <= iRDAddr + 1;  
              end
      
if ((CLEAR ==  1'b1))
          begin
       
iWRAddr <= 0;
 
iRDAddr <= 0;
      end
      
if ((iRDAddr == iWRAddr))
          begin
      iEMPTY <=  1'b1;  
              end
       else
      begin
      iEMPTY <=  1'b0;  
              end
        end
  
end

always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
   
iUSAGE <= 0;
  end
  else
  begin
  if ((CLEAR ==  1'b1))
          begin
       
iUSAGE <= 0;
      end
       else
      begin
      if (((READ ==  1'b0 && WRITE ==  1'b1) && iFULL ==  1'b0))
                  begin
          iUSAGE <= iUSAGE + 1;  
                      end
          
if (((WRITE ==  1'b0 && READ ==  1'b1) && iEMPTY ==  1'b0))
                  begin
          iUSAGE <= iUSAGE - 1;  
                      end
          
      end
        end
  
end

assign   EMPTY = iEMPTY;  
assign   FULL = iFULL;  
assign   USAGE = iUSAGE;  

endmodule  
