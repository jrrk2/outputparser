module count(input clk, input rst, input en, input dir, output reg [7:0] q);

   enum {hold,up,down} e_t;

   reg [1:0] state;

   always @(posedge clk)
     if (rst)
       begin
       q = 8'b0;
       state = hold;
       end
     else
       begin
          if (en && dir) state = up;
          else if (en && !dir) state = down;
          else state = hold;
       case (state)
	  up:
	     q = q + 8'b1;
	  down:
	     q = q - 8'b1;
	  hold:;
	  endcase
       end

endmodule // query
