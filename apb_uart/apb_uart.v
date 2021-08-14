module apb_uart(
	input wire		CLK,
	input wire		RSTN,
	input wire		PSEL,
	input wire		PENABLE,
	input wire		PWRITE,
	input wire	[2:0] 	PADDR,
	input wire	[31:0] 	PWDATA,
	output logic	[31:0] 	PRDATA,
	output logic		PREADY,
	output logic		PSLVERR,
	output logic		INT,
	output logic		OUT1N,
	output logic		OUT2N,
	output logic		RTSN,
	output logic		DTRN,
	input wire		CTSN,
	input wire		DSRN,
	input wire		DCDN,
	input wire		RIN,
	input wire		SIN,
	output logic		SOUT);  
 
 
typedef enum {FALSE,TRUE} bool_t;  
 
 
 
 
 
 
reg iWrite;  
reg iRead;  
reg iRST;  
reg iRBRRead;  
reg iTHRWrite;  
reg iDLLWrite;  
reg iDLMWrite;  
reg iIERWrite;  
reg iIIRRead;  
reg iFCRWrite;  
reg iLCRWrite;  
reg iMCRWrite;  
reg iLSRRead;  
reg iMSRRead;  
reg iSCRWrite;  
reg [7:0] iTSR;  
reg [7:0] iRBR;  
reg [7:0] iDLL;  
reg [7:0] iDLM;  
reg [7:0] iIER;  
reg [7:0] iIIR;  
reg [7:0] iFCR;  
reg [7:0] iLCR;  
reg [7:0] iMCR;  
reg [7:0] iLSR;  
reg [7:0] iMSR;  
reg [7:0] iSCR;  
reg iIER_ERBI;  
reg iIER_ETBEI;  
reg iIER_ELSI;  
reg iIER_EDSSI;  
reg iIIR_PI;  
reg iIIR_ID0;  
reg iIIR_ID1;  
reg iIIR_ID2;  
reg iIIR_FIFO64;  
reg iFCR_FIFOEnable;  
reg iFCR_RXFIFOReset;  
reg iFCR_TXFIFOReset;  
reg iFCR_DMAMode;  
reg iFCR_FIFO64E;  
reg [1:0] iFCR_RXTrigger;  
reg [1:0] iLCR_WLS;  
reg iLCR_STB;  
reg iLCR_PEN;  
reg iLCR_EPS;  
reg iLCR_SP;  
reg iLCR_BC;  
reg iLCR_DLAB;  
reg iMCR_DTR;  
reg iMCR_RTS;  
reg iMCR_OUT1;  
reg iMCR_OUT2;  
reg iMCR_LOOP;  
reg iMCR_AFE;  
reg iLSR_DR;  
reg iLSR_OE;  
reg iLSR_PE;  
reg iLSR_FE;  
reg iLSR_BI;  
reg iLSR_THRE;  
reg iLSR_THRNF;  
reg iLSR_TEMT;  
reg iLSR_FIFOERR;  
reg iMSR_dCTS;  
reg iMSR_dDSR;  
reg iMSR_TERI;  
reg iMSR_dDCD;  
reg iMSR_CTS;  
reg iMSR_DSR;  
reg iMSR_RI;  
reg iMSR_DCD;  
reg iCTSNs;  
reg iDSRNs;  
reg iDCDNs;  
reg iRINs;  
reg iCTSn;  
reg iDSRn;  
reg iDCDn;  
reg iRIn;  
reg iCTSnRE;  
reg iCTSnFE;  
reg iDSRnRE;  
reg iDSRnFE;  
reg iDCDnRE;  
reg iDCDnFE;  
reg iRInRE;  
reg iRInFE;  
reg [15:0] iBaudgenDiv;  
reg iBaudtick16x;  
reg iBaudtick2x;  
reg iRCLK;  
reg iBAUDOUTN;  
reg iTXFIFOClear;  
reg iTXFIFOWrite;  
reg iTXFIFORead;  
reg iTXFIFOEmpty;  
reg iTXFIFOFull;  
reg iTXFIFO16Full;  
reg iTXFIFO64Full;  
reg [5:0] iTXFIFOUsage;  
reg [7:0] iTXFIFOQ;  
reg iRXFIFOClear;  
reg iRXFIFOWrite;  
reg iRXFIFORead;  
reg iRXFIFOEmpty;  
reg iRXFIFOFull;  
reg iRXFIFO16Full;  
reg iRXFIFO64Full;  
reg [10:0] iRXFIFOD;  
reg [10:0] iRXFIFOQ;  
reg [5:0] iRXFIFOUsage;  
reg iRXFIFOTrigger;  
reg iRXFIFO16Trigger;  
reg iRXFIFO64Trigger;  
reg iRXFIFOPE;  
reg iRXFIFOFE;  
reg iRXFIFOBI;  
reg iSOUT;  
reg iTXStart;  
reg iTXClear;  
reg iTXFinished;  
reg iTXRunning;  
reg iSINr;  
reg iSIN;  
reg iRXFinished;  
reg iRXClear;  
reg [7:0] iRXData;  
reg iRXPE;  
reg iRXFE;  
reg iRXBI;  
reg iFERE;  
reg iPERE;  
reg iBIRE;  
reg [6:0] iFECounter;  
reg iFEIncrement;  
reg iFEDecrement;  
reg iRDAInterrupt;  
reg [5:0] iTimeoutCount;  
reg iCharTimeout;  
reg iLSR_THRERE;  
reg iTHRInterrupt;  
reg iTXEnable;  
reg iRTS;  
assign   iWrite = (PSEL ==  1'b1 && PENABLE ==  1'b1) && PWRITE ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRead = (PSEL ==  1'b1 && PENABLE ==  1'b1) && PWRITE ==  1'b0 ?  1'b1 :   1'b0;  
assign   iRST = RSTN ==  1'b0 ?  1'b1 :   1'b0;  
assign   iRBRRead = (iRead ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0;  
assign   iTHRWrite = (iWrite ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0;  
assign   iDLLWrite = (iWrite ==  1'b1 && PADDR == 3'b000) && iLCR_DLAB ==  1'b1 ?  1'b1 :   1'b0;  
assign   iDLMWrite = (iWrite ==  1'b1 && PADDR == 3'b001) && iLCR_DLAB ==  1'b1 ?  1'b1 :   1'b0;  
assign   iIERWrite = (iWrite ==  1'b1 && PADDR == 3'b001) && iLCR_DLAB ==  1'b0 ?  1'b1 :   1'b0;  
assign   iIIRRead = iRead ==  1'b1 && PADDR == 3'b010 ?  1'b1 :   1'b0;  
assign   iFCRWrite = iWrite ==  1'b1 && PADDR == 3'b010 ?  1'b1 :   1'b0;  
assign   iLCRWrite = iWrite ==  1'b1 && PADDR == 3'b011 ?  1'b1 :   1'b0;  
assign   iMCRWrite = iWrite ==  1'b1 && PADDR == 3'b100 ?  1'b1 :   1'b0;  
assign   iLSRRead = iRead ==  1'b1 && PADDR == 3'b101 ?  1'b1 :   1'b0;  
assign   iMSRRead = iRead ==  1'b1 && PADDR == 3'b110 ?  1'b1 :   1'b0;  
assign   iSCRWrite = iWrite ==  1'b1 && PADDR == 3'b111 ?  1'b1 :   1'b0;  
slib_input_sync UART_IS_SIN (CLK,iRST,SIN,iSINr);  
slib_input_sync UART_IS_CTS (CLK,iRST,CTSN,iCTSNs);  
slib_input_sync UART_IS_DSR (CLK,iRST,DSRN,iDSRNs);  
slib_input_sync UART_IS_DCD (CLK,iRST,DCDN,iDCDNs);  
slib_input_sync UART_IS_RI (CLK,iRST,RIN,iRINs);  
slib_input_filter #(.SIZE(2)) UART_IF_CTS (CLK,iRST,iBaudtick2x,iCTSNs,iCTSn);  
slib_input_filter #(.SIZE(2)) UART_IF_DSR (CLK,iRST,iBaudtick2x,iDSRNs,iDSRn);  
slib_input_filter #(.SIZE(2)) UART_IF_DCD (CLK,iRST,iBaudtick2x,iDCDNs,iDCDn);  
slib_input_filter #(.SIZE(2)) UART_IF_RI (CLK,iRST,iBaudtick2x,iRINs,iRIn);  

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
        
       iDLL <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
        
       iDLM <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iDLLWrite ==  1'b1))
         begin
            iDLL <= PWDATA[7:0] ;  
         end
       if ((iDLMWrite ==  1'b1))
         begin
            iDLM <= PWDATA[7:0] ;  
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iIER[3:0] <= 0;
    end
  else
    begin
       if ((iIERWrite ==  1'b1))
         begin
            iIER[3:0] <= PWDATA[3:0];
         end
    end

assign   iIER_ERBI = iIER[0];  
assign   iIER_ETBEI = iIER[1];  
assign   iIER_ELSI = iIER[2];  
assign   iIER_EDSSI = iIER[3];  
assign iIER[7:4] = 0;

uart_interrupt UART_IIC (
	.CLK(CLK),
	.RST(iRST),
	.IER(iIER[3:0] ),
	.LSR(iLSR[4:0] ),
	.THI(iTHRInterrupt),
	.RDA(iRDAInterrupt),
	.CTI(iCharTimeout),
	.AFE(iMCR_AFE),
	.MSR(iMSR[3:0] ),
	.IIR(iIIR[3:0] ),
	.INT(INT));  
slib_edge_detect UART_IIC_THRE_ED (
	.CLK(CLK),
	.RST(iRST),
	.D(iLSR_THRE),
	.RE(iLSR_THRERE),
        .FE());  

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iTHRInterrupt <=  1'b0;  
    end
  else
    begin
       if (((iLSR_THRERE ==  1'b1 | iFCR_TXFIFOReset ==  1'b1) | ((iIERWrite ==  1'b1 && PWDATA[1] ==  1'b1) && iLSR_THRE ==  1'b1)))
         begin
            iTHRInterrupt <=  1'b1;  
         end
       else if     (((iIIRRead ==  1'b1 && iIIR[3:1]  == 3'b001) | iTHRWrite ==  1'b1))
         begin
            iTHRInterrupt <=  1'b0;  
         end
    end

assign   iRDAInterrupt = (iFCR_FIFOEnable ==  1'b0 && iLSR_DR ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iRXFIFOTrigger ==  1'b1) ?  1'b1 :   1'b0;  
assign   iIIR_PI = iIIR[0];  
assign   iIIR_ID0 = iIIR[1];  
assign   iIIR_ID1 = iIIR[2];  
assign   iIIR_ID2 = iIIR[3];  
assign   iIIR_FIFO64 = iIIR[5];  
assign iIIR[4] = 0;
assign iIIR[5] = iFCR_FIFOEnable ? iFCR_FIFO64E : 1'b0;
assign iIIR[6] = iFCR_FIFOEnable;
assign iIIR[7] = iFCR_FIFOEnable;
   
always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
        
       iTimeoutCount <= (0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
       iCharTimeout <=  1'b0;  
    end
  else
    begin
       if (((iRXFIFOEmpty ==  1'b1 | iRBRRead ==  1'b1) | iRXFIFOWrite ==  1'b1))
         begin
             
            iTimeoutCount <= (0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
         end
       else if     (((iRXFIFOEmpty ==  1'b0 && iBaudtick2x ==  1'b1) && iTimeoutCount[5] ==  1'b0))
         begin
            iTimeoutCount <= iTimeoutCount + 1;  
         end
       if ((iFCR_FIFOEnable ==  1'b1))
         begin
            if ((iRBRRead ==  1'b1))
              begin
                 iCharTimeout <=  1'b0;  
              end
            else if         ((iTimeoutCount[5] ==  1'b1))
              begin
                 iCharTimeout <=  1'b1;  
              end
         end
       else
         begin
            iCharTimeout <=  1'b0;  
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iFCR_FIFOEnable <=  1'b0;  
       iFCR_RXFIFOReset <=  1'b0;  
       iFCR_TXFIFOReset <=  1'b0;  
       iFCR_DMAMode <=  1'b0;  
       iFCR_FIFO64E <=  1'b0;  
        
       iFCR_RXTrigger <= (0<<1)|(0<<0);
    end
  else
    begin
       iFCR_RXFIFOReset <=  1'b0;  
       iFCR_TXFIFOReset <=  1'b0;  
       if ((iFCRWrite ==  1'b1))
         begin
            iFCR_FIFOEnable <= PWDATA[0];  
            iFCR_DMAMode <= PWDATA[3];  
            iFCR_RXTrigger <= PWDATA[7:6] ;  
            if ((iLCR_DLAB ==  1'b1))
              begin
                 iFCR_FIFO64E <= PWDATA[5];  
              end
            
            if (((PWDATA[1] ==  1'b1 | (iFCR_FIFOEnable ==  1'b0 && PWDATA[0] ==  1'b1)) | (iFCR_FIFOEnable ==  1'b1 && PWDATA[0] ==  1'b0)))
              begin
                 iFCR_RXFIFOReset <=  1'b1;  
              end
            
            if (((PWDATA[2] ==  1'b1 | (iFCR_FIFOEnable ==  1'b0 && PWDATA[0] ==  1'b1)) | (iFCR_FIFOEnable ==  1'b1 && PWDATA[0] ==  1'b0)))
              begin
                 iFCR_TXFIFOReset <=  1'b1;  
              end
         end
    end

assign    iFCR[0] = iFCR_FIFOEnable;
assign    iFCR[1] = iFCR_RXFIFOReset;
assign    iFCR[2] = iFCR_TXFIFOReset;
assign    iFCR[3] = iFCR_DMAMode;
assign    iFCR[4] = 1'b0;
assign    iFCR[5] = iFCR_FIFO64E;
assign    iFCR[7:6] = iFCR_RXTrigger;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
        
       iLCR <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iLCRWrite ==  1'b1))
         begin
            iLCR <= PWDATA[7:0] ;  
         end
    end

assign   iLCR_WLS = iLCR[1:0] ;  
assign   iLCR_STB = iLCR[2];  
assign   iLCR_PEN = iLCR[3];  
assign   iLCR_EPS = iLCR[4];  
assign   iLCR_SP = iLCR[5];  
assign   iLCR_BC = iLCR[6];  
assign   iLCR_DLAB = iLCR[7];  

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iMCR[5:0] <= 0;
    end
  else
    begin
       if ((iMCRWrite ==  1'b1))
         begin
            iMCR[5:0] <= PWDATA[5:0];
         end
    end

assign   iMCR_DTR = iMCR[0];  
assign   iMCR_RTS = iMCR[1];  
assign   iMCR_OUT1 = iMCR[2];  
assign   iMCR_OUT2 = iMCR[3];  
assign   iMCR_LOOP = iMCR[4];  
assign   iMCR_AFE = iMCR[5];  
assign iMCR[7:6] = 0;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iLSR_OE <=  1'b0;  
       iLSR_PE <=  1'b0;  
       iLSR_FE <=  1'b0;  
       iLSR_BI <=  1'b0;  
       iFECounter <= 0;  
       iLSR_FIFOERR <=  1'b0;  
    end
  else
    begin
       if ((((iFCR_FIFOEnable ==  1'b0 && iLSR_DR ==  1'b1) && iRXFinished ==  1'b1) | ((iFCR_FIFOEnable ==  1'b1 && iRXFIFOFull ==  1'b1) && iRXFinished ==  1'b1)))
         begin
            iLSR_OE <=  1'b1;  
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_OE <=  1'b0;  
         end
       if ((iPERE ==  1'b1))
         begin
            iLSR_PE <=  1'b1;  
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_PE <=  1'b0;  
         end
       if ((iFERE ==  1'b1))
         begin
            iLSR_FE <=  1'b1;  
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_FE <=  1'b0;  
         end
       if ((iBIRE ==  1'b1))
         begin
            iLSR_BI <=  1'b1;  
         end
       else if     ((iLSRRead ==  1'b1))
         begin
            iLSR_BI <=  1'b0;  
         end
       if ((iFECounter != 0))
         begin
            iLSR_FIFOERR <=  1'b1;  
         end
       else if     ((iRXFIFOEmpty ==  1'b1 | iRXFIFOQ[10:8]  == 3'b000))
         begin
            iLSR_FIFOERR <=  1'b0;  
         end
       if ((iRXFIFOClear ==  1'b1))
         begin
            iFECounter <= 0;  
         end
       else
         begin
            if ((iFEIncrement ==  1'b1 && iFEDecrement ==  1'b0))
              begin
                 iFECounter <= iFECounter + 1;  
              end
            else if         ((iFEIncrement ==  1'b0 && iFEDecrement ==  1'b1))
              begin
                 iFECounter <= iFECounter - 1;  
              end
         end
    end

assign   iRXFIFOPE = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[8] ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRXFIFOFE = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[9] ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRXFIFOBI = iRXFIFOEmpty ==  1'b0 && iRXFIFOQ[10] ==  1'b1 ?  1'b1 :   1'b0;  
slib_edge_detect UART_PEDET (.CLK,.RST(iRST),.D(iRXFIFOPE),.RE(iPERE),.FE());  
slib_edge_detect UART_FEDET (.CLK,.RST(iRST),.D(iRXFIFOFE),.RE(iFERE),.FE());  
slib_edge_detect UART_BIDET (.CLK,.RST(iRST),.D(iRXFIFOBI),.RE(iBIRE),.FE());  
assign   iFEIncrement = iRXFIFOWrite ==  1'b1 && iRXFIFOD[10:8]  != 3'b000 ?  1'b1 :   1'b0;  
assign   iFEDecrement = (iFECounter != 0 && iRXFIFOEmpty ==  1'b0) && ((iPERE ==  1'b1 | iFERE ==  1'b1) | iBIRE ==  1'b1) ?  1'b1 :   1'b0;  
assign     iLSR[0]         = iLSR_DR;
assign     iLSR[1]         = iLSR_OE;
assign     iLSR[2]         = iLSR_PE;
assign     iLSR[3]         = iLSR_FE;
assign     iLSR[4]         = iLSR_BI;
assign     iLSR[5]         = iLSR_THRNF;
assign     iLSR[6]         = iLSR_TEMT;
assign     iLSR[7]         = iFCR_FIFOEnable && iLSR_FIFOERR;
   
assign   iLSR_DR = iRXFIFOEmpty ==  1'b0 | iRXFIFOWrite ==  1'b1 ?  1'b1 :   1'b0;  
assign   iLSR_THRE = iTXFIFOEmpty ==  1'b1 ?  1'b1 :   1'b0;  
assign   iLSR_TEMT = iTXRunning ==  1'b0 && iLSR_THRE ==  1'b1 ?  1'b1 :   1'b0;  
assign   iLSR_THRNF = ((iFCR_FIFOEnable ==  1'b0 && iTXFIFOEmpty ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iTXFIFOFull ==  1'b0)) ?  1'b1 :   1'b0;  
assign   iMSR_CTS = (iMCR_LOOP ==  1'b1 && iRTS ==  1'b1) | (iMCR_LOOP ==  1'b0 && iCTSn ==  1'b0) ?  1'b1 :   1'b0;  
assign   iMSR_DSR = (iMCR_LOOP ==  1'b1 && iMCR_DTR ==  1'b1) | (iMCR_LOOP ==  1'b0 && iDSRn ==  1'b0) ?  1'b1 :   1'b0;  
assign   iMSR_RI = (iMCR_LOOP ==  1'b1 && iMCR_OUT1 ==  1'b1) | (iMCR_LOOP ==  1'b0 && iRIn ==  1'b0) ?  1'b1 :   1'b0;  
assign   iMSR_DCD = (iMCR_LOOP ==  1'b1 && iMCR_OUT2 ==  1'b1) | (iMCR_LOOP ==  1'b0 && iDCDn ==  1'b0) ?  1'b1 :   1'b0;  
slib_edge_detect UART_ED_CTS (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_CTS),
	.RE(iCTSnRE),
	.FE(iCTSnFE));  
slib_edge_detect UART_ED_DSR (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_DSR),
	.RE(iDSRnRE),
	.FE(iDSRnFE));  
slib_edge_detect UART_ED_RI (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_RI),
	.RE(iRInRE),
	.FE(iRInFE));  
slib_edge_detect UART_ED_DCD (
	.CLK(CLK),
	.RST(iRST),
	.D(iMSR_DCD),
	.RE(iDCDnRE),
	.FE(iDCDnFE));  

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iMSR_dCTS <=  1'b0;  
       iMSR_dDSR <=  1'b0;  
       iMSR_TERI <=  1'b0;  
       iMSR_dDCD <=  1'b0;  
    end
  else
    begin
       if ((iCTSnRE ==  1'b1 | iCTSnFE ==  1'b1))
         begin
            iMSR_dCTS <=  1'b1;  
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dCTS <=  1'b0;  
         end
       if ((iDSRnRE ==  1'b1 | iDSRnFE ==  1'b1))
         begin
            iMSR_dDSR <=  1'b1;  
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dDSR <=  1'b0;  
         end
       if ((iRInFE ==  1'b1))
         begin
            iMSR_TERI <=  1'b1;  
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_TERI <=  1'b0;  
         end
       if ((iDCDnRE ==  1'b1 | iDCDnFE ==  1'b1))
         begin
            iMSR_dDCD <=  1'b1;  
         end
       else if     ((iMSRRead ==  1'b1))
         begin
            iMSR_dDCD <=  1'b0;  
         end
    end

assign    iMSR[0]     = iMSR_dCTS;
assign    iMSR[1]     = iMSR_dDSR;
assign    iMSR[2]     = iMSR_TERI;
assign    iMSR[3]     = iMSR_dDCD;
assign    iMSR[4]     = iMSR_CTS;
assign    iMSR[5]     = iMSR_DSR;
assign    iMSR[6]     = iMSR_RI;
assign    iMSR[7]     = iMSR_DCD;

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
        
       iSCR <= (0<<7)|(0<<6)|(0<<5)|(0<<4)|(0<<3)|(0<<2)|(0<<1)|(0<<0);
    end
  else
    begin
       if ((iSCRWrite ==  1'b1))
         begin
            iSCR <= PWDATA[7:0] ;  
         end
    end

assign   iBaudgenDiv = {iDLM, iDLL}
;  
uart_baudgen UART_BG16 (
	.CLK(CLK),
	.RST(iRST),
	.CE( 1'b1),
	.CLEAR( 1'b0),
	.DIVIDER(iBaudgenDiv),
	.BAUDTICK(iBaudtick16x));  
slib_clock_div #(.RATIO(8)) UART_BG2 (
	.CLK(CLK),
	.RST(iRST),
	.CE(iBaudtick16x),
	.Q(iBaudtick2x));  
slib_edge_detect UART_RCLK (
	.CLK(CLK),
	.RST(iRST),
	.D(iBAUDOUTN),
	.RE(iRCLK),
        .FE());  
slib_fifo #(.WIDTH(8), .SIZE_E(6)) UART_TXFF (
	.CLK(CLK),
	.RST(iRST),
	.CLEAR(iTXFIFOClear),
	.WRITE(iTXFIFOWrite),
	.READ(iTXFIFORead),
	.D(PWDATA[7:0] ),
	.Q(iTXFIFOQ),
	.EMPTY(iTXFIFOEmpty),
	.FULL(iTXFIFO64Full),
	.USAGE(iTXFIFOUsage));  
assign   iTXFIFO16Full = iTXFIFOUsage[4];  
assign   iTXFIFOFull = iFCR_FIFO64E ==  1'b0 ? iTXFIFO16Full :  iTXFIFO64Full;  
assign   iTXFIFOWrite = ((iFCR_FIFOEnable ==  1'b0 && iTXFIFOEmpty ==  1'b1) | (iFCR_FIFOEnable ==  1'b1 && iTXFIFOFull ==  1'b0)) && iTHRWrite ==  1'b1 ?  1'b1 :   1'b0;  
assign   iTXFIFOClear = iFCR_TXFIFOReset ==  1'b1 ?  1'b1 :   1'b0;  
slib_fifo #(.WIDTH(11), .SIZE_E(6)) UART_RXFF (
	.CLK(CLK),
	.RST(iRST),
	.CLEAR(iRXFIFOClear),
	.WRITE(iRXFIFOWrite),
	.READ(iRXFIFORead),
	.D(iRXFIFOD),
	.Q(iRXFIFOQ),
	.EMPTY(iRXFIFOEmpty),
	.FULL(iRXFIFO64Full),
	.USAGE(iRXFIFOUsage));  
assign   iRXFIFORead = iRBRRead ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRXFIFO16Full = iRXFIFOUsage[4];  
assign   iRXFIFOFull = iFCR_FIFO64E ==  1'b0 ? iRXFIFO16Full :  iRXFIFO64Full;  
assign   iRBR = iRXFIFOQ[7:0] ;  
assign   iRXFIFO16Trigger = ((((iFCR_RXTrigger == 2'b00 && iRXFIFOEmpty ==  1'b0) | (iFCR_RXTrigger == 2'b01 && (iRXFIFOUsage[2] ==  1'b1 | iRXFIFOUsage[3] ==  1'b1))) | (iFCR_RXTrigger == 2'b10 && iRXFIFOUsage[3] ==  1'b1)) | (((iFCR_RXTrigger == 2'b11 && iRXFIFOUsage[3] ==  1'b1) && iRXFIFOUsage[2] ==  1'b1) && iRXFIFOUsage[1] ==  1'b1)) | iRXFIFO16Full ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRXFIFO64Trigger = ((((iFCR_RXTrigger == 2'b00 && iRXFIFOEmpty ==  1'b0) | (iFCR_RXTrigger == 2'b01 && (iRXFIFOUsage[4] ==  1'b1 | iRXFIFOUsage[5] ==  1'b1))) | (iFCR_RXTrigger == 2'b10 && iRXFIFOUsage[5] ==  1'b1)) | (((iFCR_RXTrigger == 2'b11 && iRXFIFOUsage[5] ==  1'b1) && iRXFIFOUsage[4] ==  1'b1) && iRXFIFOUsage[3] ==  1'b1)) | iRXFIFO64Full ==  1'b1 ?  1'b1 :   1'b0;  
assign   iRXFIFOTrigger = iFCR_FIFO64E ==  1'b0 ? iRXFIFO16Trigger :  iRXFIFO64Trigger;  
uart_transmitter UART_TX (
	.CLK(CLK),
	.RST(iRST),
	.TXCLK(iBaudtick2x),
	.TXSTART(iTXStart),
	.CLEAR(iTXClear),
	.WLS(iLCR_WLS),
	.STB(iLCR_STB),
	.PEN(iLCR_PEN),
	.EPS(iLCR_EPS),
	.SP(iLCR_SP),
	.BC(iLCR_BC),
	.DIN(iTSR),
	.TXFINISHED(iTXFinished),
	.SOUT(iSOUT));  
assign   iTXClear =  1'b0;  
uart_receiver UART_RX (
	.CLK(CLK),
	.RST(iRST),
	.RXCLK(iRCLK),
	.RXCLEAR(iRXClear),
	.WLS(iLCR_WLS),
	.STB(iLCR_STB),
	.PEN(iLCR_PEN),
	.EPS(iLCR_EPS),
	.SP(iLCR_SP),
	.SIN(iSIN),
	.PE(iRXPE),
	.FE(iRXFE),
	.BI(iRXBI),
	.DOUT(iRXData),
	.RXFINISHED(iRXFinished));  
assign   iRXClear =  1'b0;  
assign   iSIN = iMCR_LOOP ==  1'b0 ? iSINr :  iSOUT;  
assign   iTXEnable = iTXFIFOEmpty ==  1'b0 && (iMCR_AFE ==  1'b0 | (iMCR_AFE ==  1'b1 && iMSR_CTS ==  1'b1)) ?  1'b1 :   1'b0;  

   typedef enum logic [1:0] {TXIDLE, TXSTART, TXRUN, TXEND} tx_state_type;
   typedef enum logic {RXIDLE, RXSAVE} rx_state_type;
   
   rx_state_type rx_State;
   tx_state_type tx_State;
   
     
    always @ (posedge CLK or posedge iRST)
        if (iRST == 1'b1)
          begin
            tx_State    <= TXIDLE;
            iTSR        <= 0;
            iTXStart    <= 1'b0;
            iTXFIFORead <= 1'b0;
            iTXRunning  <= 1'b0;
          end
        else
          begin
             
            iTXStart    <= 1'b0;
            iTXFIFORead <= 1'b0;
            iTXRunning  <= 1'b0;

            case(tx_State)
                TXIDLE       :
                  begin
                     if (iTXEnable == 1'b1)
                       begin
                          iTXStart <= 1'b1;             
                          tx_State <= TXSTART;
                       end
                     else
                       tx_State <= TXIDLE;
                  end
                TXSTART    :
                  begin
                     iTSR <= iTXFIFOQ;
                     iTXStart <= 1'b1;                 
                     iTXFIFORead <= 1'b1;              
                     tx_State <= TXRUN;
                  end
                TXRUN      :
                  begin
                     if (iTXFinished == 1'b1)      
                       tx_State <= TXEND;
                     else
                       tx_State <= TXRUN;
                     iTXRunning <= 1'b1;
                     iTXStart   <= 1'b1;
                  end
                TXEND      :  tx_State <= TXIDLE;
                default    :  tx_State <= TXIDLE;
            endcase;
    end

     
    always @(posedge CLK or posedge iRST)
        if (iRST == 1'b1)
          begin
            rx_State        <= RXIDLE;
            iRXFIFOWrite <= 1'b0;
            iRXFIFOClear <= 1'b0;
            iRXFIFOD     <= 0;
          end
        else
          begin
             
            iRXFIFOWrite <= 1'b0;
            iRXFIFOClear <= iFCR_RXFIFOReset;

            case (rx_State)
                RXIDLE       :
                  begin
                     if (iRXFinished == 1'b1)
                       begin  
                          iRXFIFOD <= {iRXBI, iRXFE, iRXPE, iRXData};
                          if (iFCR_FIFOEnable == 1'b0)
                            iRXFIFOClear <= 1'b1;     
                          rx_State <= RXSAVE;
                       end
                     else
                       rx_State <= RXIDLE;
                  end
                RXSAVE    :
                  begin
                     if (iFCR_FIFOEnable == 1'b0)
                       iRXFIFOWrite <= 1'b1;         
                     else if (iRXFIFOFull == 1'b0)
                       iRXFIFOWrite <= 1'b1;         
                     rx_State <= RXIDLE;
                  end
                default    :  rx_State <= RXIDLE;
             endcase;  
          end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iRTS <=  1'b0;  
    end
  else
    begin
       if ((iMCR_RTS ==  1'b0 | (iMCR_AFE ==  1'b1 && iRXFIFOTrigger ==  1'b1)))
         begin
            iRTS <=  1'b0;  
         end
       else if     ((iMCR_RTS ==  1'b1 && (iMCR_AFE ==  1'b0 | (iMCR_AFE ==  1'b1 && iRXFIFOEmpty ==  1'b1))))
         begin
            iRTS <=  1'b1;  
         end
    end

always @(posedge CLK or posedge iRST)
  if ((iRST ==  1'b1))
    begin
       iBAUDOUTN <=  1'b1;  
       OUT1N <=  1'b1;  
       OUT2N <=  1'b1;  
       RTSN <=  1'b1;  
       DTRN <=  1'b1;  
       SOUT <=  1'b1;  
    end
  else
    begin
       iBAUDOUTN <=  1'b0;  
       OUT1N <=  1'b0;  
       OUT2N <=  1'b0;  
       RTSN <=  1'b0;  
       DTRN <=  1'b0;  
       SOUT <=  1'b0;  
       if ((iBaudtick16x ==  1'b0))
         begin
            iBAUDOUTN <=  1'b1;  
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_OUT1 ==  1'b0))
         begin
            OUT1N <=  1'b1;  
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_OUT2 ==  1'b0))
         begin
            OUT2N <=  1'b1;  
         end
       
       if ((iMCR_LOOP ==  1'b1 | iRTS ==  1'b0))
         begin
            RTSN <=  1'b1;  
         end
       
       if ((iMCR_LOOP ==  1'b1 | iMCR_DTR ==  1'b0))
         begin
            DTRN <=  1'b1;  
         end
       
       if ((iMCR_LOOP ==  1'b1 | iSOUT ==  1'b1))
         begin
            SOUT <=  1'b1;  
              end
    end

always @(PADDR or iLCR_DLAB or iRBR or iDLL or iDLM or iIER or iIIR or iLCR or iMCR or iLSR or iMSR or iSCR)
  begin
     case (PADDR)
       3'b000:
         begin
            if ((iLCR_DLAB ==  1'b0))
              begin
                 PRDATA[7:0] <= iRBR;
              end
            else
              begin
                 PRDATA[7:0] <= iDLL;
              end
         end
       
       3'b001:
         begin
            if ((iLCR_DLAB ==  1'b0))
              begin
                 PRDATA[7:0] <= iIER;
              end
            else
              begin
                 PRDATA[7:0] <= iDLM;
              end
         end
       
       3'b010:
         begin
            PRDATA[7:0] <= iIIR;
         end
       
       3'b011:
         begin
            PRDATA[7:0] <= iLCR;
         end
       
       3'b100:
         begin
            PRDATA[7:0] <= iMCR;
         end
       
       3'b101:
         begin
            PRDATA[7:0] <= iLSR;
         end
       
       3'b110:
         begin
            PRDATA[7:0] <= iMSR;
         end
       
       3'b111:
         begin
            PRDATA[7:0] <= iSCR;
         end
       
       default:
         begin
            PRDATA[7:0] <= iRBR;
         end
       
     endcase

  end

assign PRDATA[31:8] = 24'b0;
assign   PREADY =  1'b1;  
assign   PSLVERR =  1'b0;  

endmodule  
