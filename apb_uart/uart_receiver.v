module uart_receiver(
	input wire		CLK,
	input wire		RST,
	input wire		RXCLK,
	input wire		RXCLEAR,
	input wire	[1:0] 	WLS,
	input wire		STB,
	input wire		PEN,
	input wire		EPS,
	input wire		SP,
	input wire		SIN,
	output logic		PE,
	output logic		FE,
	output logic		BI,
	output logic	[7:0] 	DOUT,
	output logic		RXFINISHED);  
 
typedef enum logic [2:0] {IDLE,
START,
DATA,
PAR,
STOP,
MWAIT} state_type;  
state_type CState, NState;  
reg [3:0] iBaudCount;  
reg iBaudCountClear;  
reg iBaudStep;  
reg iBaudStepD;  
reg iFilterClear;  
reg iFSIN;  
reg iFStopBit;  
reg iParity;  
reg iParityReceived;  
reg [3:0] iDataCount;  
reg iDataCountInit;  
reg iDataCountFinish;  
reg iRXFinished;  
reg iFE;  
reg iBI;  
reg iNoStopReceived;  
reg [7:0] iDOUT;  
slib_counter #(.WIDTH(4)) RX_BRC (
	.CLK(CLK),
	.RST(RST),
	.CLEAR(iBaudCountClear),
	.LOAD( 1'b0),
	.ENABLE(RXCLK),
	.DOWN( 1'b0),
	.D(4'b0000),
	.Q(iBaudCount),
	.OVERFLOW(iBaudStep));  
slib_mv_filter #(.WIDTH(4),.THRESHOLD(10)) RX_MVF (
	.CLK(CLK),
	.RST(RST),
	.SAMPLE(RXCLK),
	.CLEAR(iFilterClear),
	.D(SIN),
	.Q(iFSIN));  
slib_input_filter #(.SIZE(4)) RX_IFSB (
	.CLK(CLK),
	.RST(RST),
	.CE(RXCLK),
	.D(SIN),
	.Q(iFStopBit));  

always @(posedge CLK or posedge RST)
  begin
     if ((RST ==  1'b1))
       begin
          iBaudStepD <=  1'b0;  
       end
     else
       begin
          iBaudStepD <= iBaudStep;  
       end
  end

assign   iFilterClear = iBaudStepD | iBaudCountClear;  

always @(iDOUT or EPS)
begin
iParity <= (((((((iDOUT[7] ^ iDOUT[6]) ^ iDOUT[5]) ^ iDOUT[4]) ^ iDOUT[3]) ^ iDOUT[2]) ^ iDOUT[1]) ^ iDOUT[0]) ^  ~ EPS;  

end


always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       iDataCount <= 0;  
        
       iDOUT <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iDataCountInit ==  1'b1))
         begin
            iDataCount <= 0;  
             
            iDOUT <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
         end
       else
         begin
            if ((iBaudStep ==  1'b1 && iDataCountFinish ==  1'b0))
              begin
                 iDOUT[iDataCount] <= iFSIN;
                 iDataCount <= iDataCount + 1;  
              end
         end
    end

assign   iDataCountFinish = (((WLS == 2'b00 && iDataCount == 5) | (WLS == 2'b01 && iDataCount == 6)) | (WLS == 2'b10 && iDataCount == 7)) | (WLS == 2'b11 && iDataCount == 8) ?  1'b1 :   1'b0;  

always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       CState <= IDLE;  
    end
  else
    begin
       CState <= NState;  
    end

always @(CState or SIN or iFSIN or iFStopBit or iBaudStep or iBaudCount or iDataCountFinish or PEN or WLS or STB)
begin
NState <= IDLE;  
iBaudCountClear <=  1'b0;  
iDataCountInit <=  1'b0;  
iRXFinished <=  1'b0;  
case (CState)
  IDLE:
    begin
  if ((SIN ==  1'b0))
          begin
      NState <= START;  
              end
      
iBaudCountClear <=  1'b1;  
    iDataCountInit <=  1'b1;  
      end
  
  START:
    begin
  iDataCountInit <=  1'b1;  
    if ((iBaudStep ==  1'b1))
          begin
      if ((iFSIN ==  1'b0))
                  begin
          NState <= DATA;  
                      end
          
      end
       else
      begin
      NState <= START;  
              end
        end
  
  DATA:
    begin
  if ((iDataCountFinish ==  1'b1))
          begin
      if ((PEN ==  1'b1))
                  begin
          NState <= PAR;  
                      end
           else
          begin
          NState <= STOP;  
                      end
                end
       else
      begin
      NState <= DATA;  
              end
        end
  
  PAR:
    begin
  if ((iBaudStep ==  1'b1))
          begin
      NState <= STOP;  
              end
       else
      begin
      NState <= PAR;  
              end
        end
  
  STOP:
    begin
  if ((iBaudCount[3] ==  1'b1))
          begin
      if ((iFStopBit ==  1'b0))
                  begin
          iRXFinished <=  1'b1;  
            NState <= MWAIT;  
                      end
           else
          begin
          iRXFinished <=  1'b1;  
            NState <= IDLE;  
                      end
                end
       else
      begin
      NState <= STOP;  
              end
        end
  
  MWAIT:
    begin
  if ((SIN ==  1'b0))
          begin
      NState <= MWAIT;  
              end
      
  end
  
  default:
    begin
  begin end  end
  
endcase

end


always @(posedge CLK or posedge RST)
begin
if ((RST ==  1'b1))
  begin
  PE <=  1'b0;  
    iParityReceived <=  1'b0;  
      end
  else
  begin
  if ((CState == PAR && iBaudStep ==  1'b1))
          begin
      iParityReceived <= iFSIN;  
              end
      
if ((PEN ==  1'b1))
          begin
      PE <=  1'b0;  
        if ((SP ==  1'b1))
                  begin
          if (((EPS ^ iParityReceived) ==  1'b0))
                          begin
              PE <=  1'b1;  
                              end
              
          end
           else
          begin
          if ((iParity != iParityReceived))
                          begin
              PE <=  1'b1;  
                              end
              
          end
                end
       else
      begin
      PE <=  1'b0;  
        iParityReceived <=  1'b0;  
              end
        end
  
end

assign   iNoStopReceived = iFStopBit ==  1'b0 && (CState == STOP) ?  1'b1 :   1'b0;  
assign   iBI = (iDOUT == 8'b00000000 && iParityReceived ==  1'b0) && iNoStopReceived ==  1'b1 ?  1'b1 :   1'b0;  
assign   iFE = iNoStopReceived ==  1'b1 ?  1'b1 :   1'b0;  
assign   DOUT = iDOUT;  
assign   BI = iBI;  
assign   FE = iFE;  
assign   RXFINISHED = iRXFinished;  

endmodule
