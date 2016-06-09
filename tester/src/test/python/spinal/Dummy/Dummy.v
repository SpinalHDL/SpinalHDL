module Dummy();
  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(1, Dummy);
  end
endmodule
