module arith(input [3:0] a,input[3:0] b, output f, output g, output [3:0] m, output[3:0] n, output [3:0] o,
output [7:0] p, output [7:0] q, output [7:0] r, output [7:0] s,
output t,output u,output v,output w,output x,output y);

   assign f = a && b;
   assign g = a || b;

   assign m = a & b;
   assign n = a | b;
   assign o = a ^ b;

   assign p = a+b;
   assign q = a-b;
   assign r = a*b;
/*
   assign s = a/b;
*/
   assign t = a == b;
   assign u = a != b;
   assign v = a < b;
   assign w = a <= b;
   assign x = a >= b;
   assign y = a > b;

// dummy #(.Y_WIDTH(8)) inst1(.A(a),.B(b),.Y(z));

endmodule // query
