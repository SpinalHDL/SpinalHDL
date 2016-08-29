
module SdramCtrlTester_tb
(
  input   io_bus_cmd_valid,
  output  io_bus_cmd_ready,
  input  [23:0] io_bus_cmd_payload_address,
  input   io_bus_cmd_payload_write,
  input  [15:0] io_bus_cmd_payload_data,
  input  [1:0] io_bus_cmd_payload_mask,
  input  [7:0] io_bus_cmd_payload_context,
  output  io_bus_rsp_valid,
  input   io_bus_rsp_ready,
  output [15:0] io_bus_rsp_payload_data,
  output [7:0] io_bus_rsp_payload_context,
  input   clk,
  input   reset
);

  wire [12:0] io_sdram_ADDR;
  wire [1:0] io_sdram_BA;
  wire [15:0] io_sdram_DQ;
  wire [15:0] io_sdram_DQ_read;
  wire [15:0] io_sdram_DQ_write;
  wire  io_sdram_DQ_writeEnable;
  wire [1:0] io_sdram_DQM;
  wire  io_sdram_CASn;
  wire  io_sdram_CKE;
  wire  io_sdram_CSn;
  wire  io_sdram_RASn;
  wire  io_sdram_WEn;

  assign io_sdram_DQ_read = io_sdram_DQ;
  assign io_sdram_DQ = io_sdram_DQ_writeEnable ? io_sdram_DQ_write : 16'bZZZZZZZZZZZZZZZZ;

  mt48lc16m16a2 sdram(
    .Dq(io_sdram_DQ),
    .Addr(io_sdram_ADDR),
    .Ba(io_sdram_BA),
    .Clk(clk),
    .Cke(io_sdram_CKE),
    .Cs_n(io_sdram_CSn),
    .Ras_n(io_sdram_RASn),
    .Cas_n(io_sdram_CASn),
    .We_n(io_sdram_WEn),
    .Dqm(io_sdram_DQM)
  );


  SdramCtrlTester uut (
    .io_sdram_ADDR(io_sdram_ADDR),
    .io_sdram_BA(io_sdram_BA),
    .io_sdram_DQ_read(io_sdram_DQ_read),
    .io_sdram_DQ_write(io_sdram_DQ_write),
    .io_sdram_DQ_writeEnable(io_sdram_DQ_writeEnable),
    .io_sdram_DQM(io_sdram_DQM),
    .io_sdram_CASn(io_sdram_CASn),
    .io_sdram_CKE(io_sdram_CKE),
    .io_sdram_CSn(io_sdram_CSn),
    .io_sdram_RASn(io_sdram_RASn),
    .io_sdram_WEn(io_sdram_WEn),
    .io_bus_cmd_valid(io_bus_cmd_valid),
    .io_bus_cmd_ready(io_bus_cmd_ready),
    .io_bus_cmd_payload_address(io_bus_cmd_payload_address),
    .io_bus_cmd_payload_write(io_bus_cmd_payload_write),
    .io_bus_cmd_payload_data(io_bus_cmd_payload_data),
    .io_bus_cmd_payload_mask(io_bus_cmd_payload_mask),
    .io_bus_cmd_payload_context(io_bus_cmd_payload_context),
    .io_bus_rsp_valid(io_bus_rsp_valid),
    .io_bus_rsp_ready(io_bus_rsp_ready),
    .io_bus_rsp_payload_data(io_bus_rsp_payload_data),
    .io_bus_rsp_payload_context(io_bus_rsp_payload_context),
    .clk(clk),
    .reset(reset)
  );

 // initial begin
  //  $dumpfile("wave.vcd");
  //  $dumpvars(0, SdramCtrlTester_tb);
 // end
endmodule

