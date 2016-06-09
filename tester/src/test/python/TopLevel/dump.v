module dump();
  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(1, TopLevel);
  end
endmodule
