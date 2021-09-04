module add(input [3:0] a,input[3:0] b, output [7:0] z);

   assign z = a+b;

// dummy #(.Y_WIDTH(8)) inst1(.A(a),.B(b),.Y(z));

endmodule // query
