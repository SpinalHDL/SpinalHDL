
module Axi4SharedSdramCtrlTester_tb
(
  input   io_axi_arw_valid,
  output  io_axi_arw_ready,
  input  [24:0] io_axi_arw_payload_addr,
  input  [1:0] io_axi_arw_payload_id,
  input  [7:0] io_axi_arw_payload_len,
  input  [2:0] io_axi_arw_payload_size,
  input  [1:0] io_axi_arw_payload_burst,
  input   io_axi_arw_payload_write,
  input   io_axi_w_valid,
  output  io_axi_w_ready,
  input  [31:0] io_axi_w_payload_data,
  input  [3:0] io_axi_w_payload_strb,
  input   io_axi_w_payload_last,
  output  io_axi_b_valid,
  input   io_axi_b_ready,
  output [1:0] io_axi_b_payload_id,
  output [1:0] io_axi_b_payload_resp,
  output  io_axi_r_valid,
  input   io_axi_r_ready,
  output [31:0] io_axi_r_payload_data,
  output [1:0] io_axi_r_payload_id,
  output [1:0] io_axi_r_payload_resp,
  output  io_axi_r_payload_last,
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


  Axi4SharedSdramCtrlTester uut (
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
    .io_axi_arw_valid(io_axi_arw_valid),
    .io_axi_arw_ready(io_axi_arw_ready),
    .io_axi_arw_payload_addr(io_axi_arw_payload_addr),
    .io_axi_arw_payload_id(io_axi_arw_payload_id),
    .io_axi_arw_payload_len(io_axi_arw_payload_len),
    .io_axi_arw_payload_size(io_axi_arw_payload_size),
    .io_axi_arw_payload_burst(io_axi_arw_payload_burst),
    .io_axi_arw_payload_write(io_axi_arw_payload_write),
    .io_axi_w_valid(io_axi_w_valid),
    .io_axi_w_ready(io_axi_w_ready),
    .io_axi_w_payload_data(io_axi_w_payload_data),
    .io_axi_w_payload_strb(io_axi_w_payload_strb),
    .io_axi_w_payload_last(io_axi_w_payload_last),
    .io_axi_b_valid(io_axi_b_valid),
    .io_axi_b_ready(io_axi_b_ready),
    .io_axi_b_payload_id(io_axi_b_payload_id),
    .io_axi_b_payload_resp(io_axi_b_payload_resp),
    .io_axi_r_valid(io_axi_r_valid),
    .io_axi_r_ready(io_axi_r_ready),
    .io_axi_r_payload_data(io_axi_r_payload_data),
    .io_axi_r_payload_id(io_axi_r_payload_id),
    .io_axi_r_payload_resp(io_axi_r_payload_resp),
    .io_axi_r_payload_last(io_axi_r_payload_last),
    .clk(clk),
    .reset(reset)
  );

 // initial begin
 //   $dumpfile("wave.vcd");
 //   $dumpvars(0, SdramCtrlTester_tb);
  //end
endmodule

