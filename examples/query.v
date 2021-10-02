module query(input [7:0] a,b, output z);

   assign z = a[b[2:0]];

endmodule // query
