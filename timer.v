/* timer module */

module timer(output wire rdy, input wire clk, input wire reset);

   reg [4:0] count;
   assign rdy = count[4];

   always @(posedge clk or posedge reset)
     if (reset)
       count <= 15;
     else
       count <= count - 1;

endmodule // timer
