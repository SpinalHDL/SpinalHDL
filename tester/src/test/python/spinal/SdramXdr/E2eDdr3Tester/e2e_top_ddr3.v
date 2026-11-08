// e2e_top_ddr3.v
//
// Thin, committable wrapper tying the SpinalHDL-generated DFI command encoder
// (DfiLegacySimPhy.v, DDR3 config) to the ORIGINAL, UNMODIFIED Micron ddr3.v
// behavioral model. cocotb drives the ctl_* / data inputs and observes
// dq_obs/dq_valid; everything else is plain structural wiring.
//
// Mirrors tb/e2e_tb.sv (the validated standalone SV bench) pin-for-pin:
//   * ddr3 #(.check_strict_timing(0), .STOP_ON_ERROR(0), .DEBUG(1))
//   * dm_tdqs tied to 2'b00 (mask disabled so writes land)
//   * tri-state DQ/DQS: controller drives when wr_en, else high-Z (the model
//     drives them during reads)
//   * dq_valid computed HERE in 4-state Verilog so the Python side never
//     interprets x/z on the databus itself.

`timescale 1ns / 1ps

module e2e_top_ddr3 (
  input  wire        mcRst,
  input  wire        ck,
  input  wire [25:0] ctl_addr,
  input  wire [5:0]  ctl_bank,
  input  wire [1:0]  ctl_rasN,
  input  wire [1:0]  ctl_casN,
  input  wire [1:0]  ctl_weN,
  input  wire [1:0]  ctl_csN,
  input  wire [1:0]  ctl_cke,
  input  wire [1:0]  ctl_odt,
  input  wire        wr_en,
  input  wire [15:0] wb_dq,
  input  wire [1:0]  wb_dqs,
  output wire [15:0] dq_obs,
  output wire        dq_valid
);

  wire        e_ck, e_ck_n, e_cke, e_cs_n, e_ras_n, e_cas_n, e_we_n, e_odt;
  wire [2:0]  e_ba;
  wire [13:0] e_addr;
  wire [15:0] dq_bus;
  wire [1:0]  dqs_w, dqs_n_w, rdqs_unused;
  wire [1:0]  dm_tdqs_w;

  assign dm_tdqs_w = 2'b00;
  assign dq_bus    = wr_en ? wb_dq  : 16'bz;
  assign dqs_w     = wr_en ? wb_dqs : 2'bz;
  assign dqs_n_w   = wr_en ? ~wb_dqs : 2'bz;
  assign dq_obs    = dq_bus;
  assign dq_valid  = (dq_bus !== 16'hzzzz) && (dq_bus !== 16'hxxxx);

  // Unit under test: SpinalHDL DFI encoder (generated Verilog)
  DfiLegacySimPhy u_enc (
    .io_mcClk           (ck),
    .io_mcRst           (mcRst),
    .io_dramClk         (ck),
    .io_control_address (ctl_addr),
    .io_control_bank    (ctl_bank),
    .io_control_rasN    (ctl_rasN),
    .io_control_casN    (ctl_casN),
    .io_control_weN     (ctl_weN),
    .io_control_csN     (ctl_csN),
    .io_control_cke     (ctl_cke),
    .io_control_odt     (ctl_odt),
    .io_ck              (e_ck),
    .io_ck_n            (e_ck_n),
    .io_cke             (e_cke),
    .io_cs_n            (e_cs_n),
    .io_ras_n           (e_ras_n),
    .io_cas_n           (e_cas_n),
    .io_we_n            (e_we_n),
    .io_odt             (e_odt),
    .io_ba              (e_ba),
    .io_addr            (e_addr)
  );

  // Reference model: ORIGINAL Micron ddr3.v (unmodified, by path)
  ddr3 #(.check_strict_timing(0), .STOP_ON_ERROR(0), .DEBUG(1)) u_mem (
    .rst_n   (~mcRst),
    .ck      (e_ck),
    .ck_n    (e_ck_n),
    .cke     (e_cke),
    .cs_n    (e_cs_n),
    .ras_n   (e_ras_n),
    .cas_n   (e_cas_n),
    .we_n    (e_we_n),
    .odt     (e_odt),
    .dm_tdqs (dm_tdqs_w),
    .ba      (e_ba),
    .addr    (e_addr),
    .dq      (dq_bus),
    .dqs     (dqs_w),
    .dqs_n   (dqs_n_w),
    .tdqs_n  ()
  );

endmodule
