// Generator : SpinalHDL v1.4.0    git head : ecb5a80b713566f417ea3ea061f9969e73770a7f
// Date      : 14/05/2020, 16:13:04
// Component : TestMemIVerilog



module TestMemVerilatorW (
  input      [9:0]    io_addr,
  input      [127:0]    io_wvalue,
  input               io_wenable,
  output     [127:0]    io_rvalue,
  input               clk,
  input               reset 
);
  reg        [127:0]    _zz_2_;
  wire                _zz_3_;
  wire       [127:0]    _zz_1_;
  reg [127:0] mem [0:1023] /* verilator public */;

  assign _zz_3_ = 1'b1;
  always @ (posedge clk) begin
    if(_zz_3_) begin
      _zz_2_ <= mem[io_addr];
    end
  end

  always @ (posedge clk) begin
    if(_zz_3_ && io_wenable ) begin
      mem[io_addr] <= _zz_1_;
    end
  end

  assign _zz_1_ = io_wvalue;
  assign io_rvalue = _zz_2_;

endmodule
