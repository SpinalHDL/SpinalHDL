module PLL
(
  input  clk_in,
  output  clk_out = 0
);


  always @ (posedge clk_in)
  begin
      clk_out <= ! clk_out;
  end
endmodule
