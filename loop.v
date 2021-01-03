/*
   Copyright (c) 2003 Stephen Williams (steve@icarus.com)
  
      This source code is free software; you can redistribute it
      and/or modify it in source code form under the terms of the GNU
      General Public License as published by the Free Software
      Foundation; either version 2 of the License, or (at your option)
      any later version.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU General Public License for more details.
  
      You should have received a copy of the GNU General Public License
      along with this program; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
  
     $Id: process.v,v 1.1 2003/04/01 05:55:24 stevewilliams Exp $
 */

module loop(output reg [11:0] cnt,
	    input [7:0] in,
	       input wire clk,
	       input wire reset);

   reg [2:0]			i;
   reg [7:0] 			filt [4:0];
   
   always @(posedge clk)
     if (reset)
       begin
	  for (i = 1; i <= 4; i++)
	    filt[i] = 8'b0;
	  cnt = 8'b0;
       end
     else
       begin
	  for (i = 1; i <= 4; i++)
	    begin
	       filt[i] <= filt[i-1];
	    end
	  filt[0] <= in;
	  cnt = 0;
	  for (i = 0; i <= 4; i++)
	    begin
	       cnt = cnt + filt[i];
	    end
       end

endmodule // process
