module dump();
  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(1, WhenTester);
  end
endmodule
