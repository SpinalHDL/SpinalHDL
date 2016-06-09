module dump();
  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(1, CommonTester);
  end
endmodule
