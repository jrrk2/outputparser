module rhs(input a, input [2:0] b, output [7:0] z);

   assign z[b] = a;

endmodule // query
