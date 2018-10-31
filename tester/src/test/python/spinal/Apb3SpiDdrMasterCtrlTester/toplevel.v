module toplevel();





  wire  [7:0] io_apb_PADDR;
  wire  [0:0] io_apb_PSEL;
  wire   io_apb_PENABLE;
  wire  io_apb_PREADY;
  wire   io_apb_PWRITE;
  wire  [31:0] io_apb_PWDATA;
  wire reg [31:0] io_apb_PRDATA;
  wire   io_xip_cmd_valid;
  wire  io_xip_cmd_ready;
  wire  [23:0] io_xip_cmd_payload;
  wire  io_xip_rsp_valid;
  wire [31:0] io_xip_rsp_payload;
  wire [0:0] io_spi_ss;
  wire   earlyClk;
  wire   clk;
  wire  io_spi_sclk;
  wire  io_spi_data0;
  wire  io_spi_data1;
  wire  io_spi_data2;
  wire  io_spi_data3;
  wire   reset;

Apb3SpiDdrMasterCtrl ctrl(
      .io_apb_PADDR(io_apb_PADDR),
      .io_apb_PSEL(io_apb_PSEL),
      .io_apb_PENABLE(io_apb_PENABLE),
      .io_apb_PREADY(io_apb_PREADY),
      .io_apb_PWRITE(io_apb_PWRITE),
      .io_apb_PWDATA(io_apb_PWDATA),
      .io_apb_PRDATA(io_apb_PRDATA),
      .io_xip_cmd_valid(io_xip_cmd_valid),
      .io_xip_cmd_ready(io_xip_cmd_ready),
      .io_xip_cmd_payload(io_xip_cmd_payload),
      .io_xip_rsp_valid(io_xip_rsp_valid),
      .io_xip_rsp_payload(io_xip_rsp_payload),
      .io_spi_ss(io_spi_ss),
      .clkEarly(clkEarly),
      .clk(clk),
      .io_spi_sclk(io_spi_sclk),
      .io_spi_data0(io_spi_data0),
      .io_spi_data1(io_spi_data1),
      .io_spi_data2(io_spi_data2),
      .io_spi_data3(io_spi_data3),
      .reset(reset));

  reg [31:0]vcc;
  wire DQ1;
  wire pullUp = 1;
  N25Qxxx flash(
    .S(io_spi_ss[0]),
    .C_(io_spi_sclk),
    .HOLD_DQ3(pullUp),
    .DQ0(io_spi_data0),
    .DQ1(io_spi_data1),
    .Vcc(vcc),
    .Vpp_W_DQ2(pullUp));

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, toplevel);
  end
endmodule
