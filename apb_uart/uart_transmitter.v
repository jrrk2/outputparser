module uart_transmitter(
	input wire		CLK,
	input wire		RST,
	input wire		TXCLK,
	input wire		TXSTART,
	input wire		CLEAR,
	input wire	[1:0] 	WLS,
	input wire		STB,
	input wire		PEN,
	input wire		EPS,
	input wire		SP,
	input wire		BC,
	input wire	[7:0] 	DIN,
	output logic		TXFINISHED,
	output logic		SOUT);  
 
typedef enum logic [3:0] {IDLE,
START,
BIT0,
BIT1,
BIT2,
BIT3,
BIT4,
BIT5,
BIT6,
BIT7,
PAR,
STOP,
STOP2} state_type;  
state_type CState, NState;  
reg iTx2;  
reg iSout;  
reg iParity;  
reg iFinished;  

always @(posedge CLK or posedge RST)
  if ((RST ==  1'b1))
    begin
       CState <= IDLE;  
       iTx2 <=  1'b0;  
    end
  else
    begin
       if ((TXCLK ==  1'b1))
         begin
            if ((iTx2 ==  1'b0))
              begin
                 CState <= NState;  
                 iTx2 <=  1'b1;  
              end
            else
              begin
                 if ((((WLS == 2'b00) && (STB ==  1'b1)) && CState == STOP2))
                   begin
                      CState <= NState;  
                      iTx2 <=  1'b1;  
                   end
                 else
                   begin
                      CState <= CState;  
                      iTx2 <=  1'b0;  
                   end
              end
         end
    end

always @(CState or TXSTART or DIN or WLS or PEN or SP or EPS or STB or iParity)
  begin
     NState <= IDLE;  
     iSout <=  1'b1;  
     case (CState)
       IDLE:
         begin
            if ((TXSTART ==  1'b1))
              begin
                 NState <= START;  
              end
            
         end
       
       START:
         begin
            iSout <=  1'b0;  
            NState <= BIT0;  
         end
       
       BIT0:
         begin
            iSout <= DIN[0];  
            NState <= BIT1;  
         end
       
       BIT1:
         begin
            iSout <= DIN[1];  
            NState <= BIT2;  
         end
       
       BIT2:
         begin
            iSout <= DIN[2];  
            NState <= BIT3;  
         end
       
       BIT3:
         begin
            iSout <= DIN[3];  
            NState <= BIT4;  
         end
       
       BIT4:
         begin
            iSout <= DIN[4];  
            if ((WLS == 2'b00))
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
                 NState <= BIT5;  
              end
         end
       
       BIT5:
         begin
            iSout <= DIN[5];  
            if ((WLS == 2'b01))
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
                 NState <= BIT6;  
              end
         end
       
       BIT6:
         begin
            iSout <= DIN[6];  
            if ((WLS == 2'b10))
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
                 NState <= BIT7;  
              end
         end
       
       BIT7:
         begin
            iSout <= DIN[7];  
            if ((PEN ==  1'b1))
              begin
                 NState <= PAR;  
              end
            else
              begin
                 NState <= STOP;  
              end
         end
       
       PAR:
         begin
            if ((SP ==  1'b1))
              begin
                 if ((EPS ==  1'b1))
                   begin
                      iSout <=  1'b0;  
                   end
                 else
                   begin
                      iSout <=  1'b1;  
                   end
              end
            else
              begin
                 if ((EPS ==  1'b1))
                   begin
                      iSout <= iParity;  
                   end
                 else
                   begin
                      iSout <=  ~ iParity;  
                   end
              end
            NState <= STOP;  
         end
       
       STOP:
         begin
            if ((STB ==  1'b1))
              begin
                 NState <= STOP2;  
              end
            else
              begin
                 if ((TXSTART ==  1'b1))
                   begin
                      NState <= START;  
                   end
                 
              end
         end
       
  STOP2:
    begin
       if ((TXSTART ==  1'b1))
         begin
            NState <= START;  
         end
       
    end
       
       default:
         begin
            begin end  end
       
     endcase
     
  end
   
    logic iP40, iP50, iP60, iP70;
     
    always @ (DIN or WLS)
    begin:TX_PAR
        iP40 = DIN[4] ^ DIN[3] ^ DIN[2] ^ DIN[1] ^ DIN[0];
        iP50 = DIN[5] ^ iP40;
        iP60 = DIN[6] ^ iP50;
        iP70 = DIN[7] ^ iP60;

        case(WLS)
            2'b00: iParity <= iP40;
            2'b01: iParity <= iP50;
            2'b10: iParity <= iP60;
            default: iParity <= iP70;
        endcase;
    end

    reg iLast;
    always @(posedge CLK or posedge RST)
    begin:TX_FIN
        if (RST)
          begin
             iFinished <= 1'b0;
             iLast <= 1'b0;
          end
        else
          begin
             iFinished <= 1'b0;
             if (iLast == 1'b0 && CState == STOP)
               iFinished <= 1'b1;
             if (CState == STOP)
               iLast <= 1'b1;
             else
               iLast <= 1'b0;
          end
    end

assign   SOUT = BC ==  1'b0 ? iSout :   1'b0;  
assign   TXFINISHED = iFinished;  

endmodule  
