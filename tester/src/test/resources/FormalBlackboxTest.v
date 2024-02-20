module FormalBlackboxTest(/*AUTOARG*/
   // Outputs
   o,
   // Inputs
   clk, a, b
   );
   input clk;

   input wire [7:0] a;
   input wire [7:0] b;
   output wire [7:0] o;

   reg [7:0]         output_reg = 0;

   assign o = output_reg;

   always @(posedge clk)
     output_reg <= a + b;


endmodule // FormalBlackboxTest
