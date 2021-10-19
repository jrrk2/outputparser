module count3(input clk, input rst, input en, input dir, output reg [1:0] state, output reg [3:0] q);

   enum logic [1:0] {hold,up,down} e_t;

   always @(posedge clk)
     if (rst)
       begin
       q = 4'b0;
       state = hold;
       end
     else
       begin
          if (en && dir) state = up;
          else if (en && !dir) state = down;
          else state = hold;
       case (state)
	  up:
	     q = q + 4'b1;
	  down:
	     q = q - 4'b1;
	  hold:;
	  endcase
       end

endmodule // query
