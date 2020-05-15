
`timescale 1ns/1ns

module __simulation_def;
initial
 begin
  $dumpfile("test.vcd");
  $dumpvars(0,TestMemIVerilog);
  $readmempath("./rtl/");
 end
endmodule