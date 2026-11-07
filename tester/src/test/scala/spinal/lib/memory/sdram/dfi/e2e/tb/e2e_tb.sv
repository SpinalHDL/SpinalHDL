//=============================================================================
// e2e_tb.sv
//
// Standalone Icarus-Verilog co-simulation testbench for the SpinalHDL DFI DDR3
// command encoder (DfiLegacySimPhy, exported to Verilog via SpinalVerilog) wired
// LIVE to the ORIGINAL, UNMODIFIED Micron ddr3.v behavioral model.
//
// Design notes:
//   * No Verilator, no patched model derivative: the pristine ddr3.v is compiled
//     directly. iverilog's full 4-state event engine handles the model's #delay /
//     z / x / bufif1 / advancing-$time natively, so the model's OWN built-in
//     timing checker is exercised for real.
//   * The TB drives the DQ/DQS tri-state data bus (write strobes + fill data) and
//     samples it back (reads), absorbing the DQS-vs-CK phase here.
//   * Round-trip: ACT -> WR (constant fill) -> RD -> verify 8 contiguous fill
//     beats came back, for TWO different columns, then re-read column 0 to
//     prove the reads are location-specific (col0 keeps its fill after col8).
//   * Pin settling: the model mirrors every control pin into an `_in` copy via
//     `always @(sig) _in <= #0 sig`, so each `_in` copy stays X until that pin
//     first CHANGES. Control regs therefore start X (no static init) and are
//     settled procedurally at t=0; that one change event latches all `_in`
//     copies to defined values before the model's reset / CKE-active checks run.
//
// Data-mask: DM is an INPUT (mask) for the x16 config. The model masks each
//   write byte by ~dm_in, so a floating dm_tdqs (zz -> ~zz = x) makes every
//   write store x. dm_tdqs is tied to 2'b00 (mask disabled) so writes land.
//
// Timing chosen to satisfy the model's live (ungated) checks (sg25 bin):
//   * tCK = 3.0 ns  -> inside the SG25 (DDR3-800E) window [2500, 3340] ps.
//   * CL = 5 (MR0 addr[4]=1; CL = {addr[2],addr[6:4]}+4), CWL = 5 (MR2 addr[5:3]=0).
//     MR0 = 0x110 (CL=5 + addr[8] DLL-reset); MR1 = 0x004 (DLL en, ODT, AL=0);
//     MR2 = 0x000 (CWL=5); MR3 = 0x000 (clears the GLOBAL mpr_en bit, which
//     MR0's CL field would otherwise leave set and make the model reject ACT/WR).
//   * Commands are driven on the CK *negedge* (setup 1.5 ns >> tIS=350 ps).
//   * Init gaps: tXPR (170 ns) after CKE-active, tZQinit (~1024 tCK) after ZQCL,
//     tDLLK (512 tCK) after the MR0 DLL-reset, tMRD/tMOD (4/12 tCK) between MRS.
//   * check_strict_timing=0 relaxes the $time-based fine-grained checks;
//     STOP_ON_ERROR=0 keeps the model from $stop-ing on a soft error.
//=============================================================================
`timescale 1ns / 1ps

module e2e_tb;

  //--------------------------------------------------------------------------
  // Parameters
  //--------------------------------------------------------------------------
  localparam int BA_W    = 3;
  localparam int ADDR_W  = 14;
  localparam int DQ_W    = 16;
  localparam int DQS_W   = 2;
  localparam int CTRL_AW = 26;          // address slot width (13 * fr=2)
  localparam int CTRL_BW = 6;           // bank slot width  (3  * fr=2)
  localparam [15:0] FILL0 = 16'hA000;   // fill for column 0
  localparam [15:0] FILL1 = 16'hB000;   // fill for column 8
  localparam int  BL      = 8;          // burst length

  //--------------------------------------------------------------------------
  // Clocks / reset.  One 0.75 ns base tick; all clocks phase-locked:
  //   dqs_clk = cnt[0]  edge every 0.75 ns  (DQS strobe rate)
  //   dramClk = cnt[1]  period 3.0 ns       (DDR3 CK, tCK = 3000 ps)
  //   mcClk   = cnt[2]  period 6.0 ns       (controller MC clock)
  // NOTE: control regs deliberately have NO static initializer so they start X;
  //       the t=0 settle below is what latches the model's `_in` input copies.
  //--------------------------------------------------------------------------
  reg [2:0] cnt = 3'd0;
  wire       dqs_clk = cnt[0];
  wire       dramClk = cnt[1];
  wire       mcClk   = cnt[2];
  always #0.75 cnt <= #0 cnt + 3'd1;

  reg mcRst;                                    // X until t=0 settle

  //--------------------------------------------------------------------------
  // Encoder (generated from SpinalHDL) <-> model command pins
  //--------------------------------------------------------------------------
  reg  [CTRL_AW-1:0] ctl_addr;                  // X until t=0 settle
  reg  [CTRL_BW-1:0] ctl_bank;
  reg  [1:0] ctl_rasN, ctl_casN, ctl_weN, ctl_csN, ctl_cke, ctl_odt;

  wire       e_ck, e_ck_n, e_cke, e_cs_n, e_ras_n, e_cas_n, e_we_n, e_odt;
  wire [BA_W-1:0]   e_ba;
  wire [ADDR_W-1:0] e_addr;

  DfiLegacySimPhy u_enc (
    .io_mcClk        (mcClk),
    .io_mcRst        (mcRst),
    .io_dramClk      (dramClk),
    .io_control_address(ctl_addr),
    .io_control_bank (ctl_bank),
    .io_control_rasN (ctl_rasN),
    .io_control_casN (ctl_casN),
    .io_control_weN  (ctl_weN),
    .io_control_csN  (ctl_csN),
    .io_control_cke  (ctl_cke),
    .io_control_odt  (ctl_odt),
    .io_ck           (e_ck),
    .io_ck_n         (e_ck_n),
    .io_cke          (e_cke),
    .io_cs_n         (e_cs_n),
    .io_ras_n        (e_ras_n),
    .io_cas_n        (e_cas_n),
    .io_we_n         (e_we_n),
    .io_odt          (e_odt),
    .io_ba           (e_ba),
    .io_addr         (e_addr)
  );

  //--------------------------------------------------------------------------
  // Tri-state DQ / DQS data bus (TB drives write, samples read)
  //--------------------------------------------------------------------------
  reg [DQ_W-1:0] wb_dq   = {DQ_W{1'b0}};
  reg            wb_dqs  = 1'b0;
  reg            wr_enable = 1'b0;

  wire [DQ_W-1:0]  dq_bus;
  wire [DQS_W-1:0] dqs_bus, dqs_n_bus;
  wire [1:0]       dm_tdqs_wire = 2'b00;      // DM=0 => data mask disabled (writes enabled)
  assign dq_bus    = wr_enable ? wb_dq            : {DQ_W{1'bz}};
  assign dqs_bus   = wr_enable ? {wb_dqs,wb_dqs}  : {DQS_W{1'bz}};
  assign dqs_n_bus = wr_enable ? {!wb_dqs,!wb_dqs} : {DQS_W{1'bz}};

  //--------------------------------------------------------------------------
  // Micron ddr3.v behavioral model (UNMODIFIED)
  //--------------------------------------------------------------------------
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
    .dm_tdqs (dm_tdqs_wire),
    .ba      (e_ba),
    .addr    (e_addr),
    .dq      (dq_bus),
    .dqs     (dqs_bus),
    .dqs_n   (dqs_n_bus),
    .tdqs_n  ()
  );

  //--------------------------------------------------------------------------
  // Command helpers.  Both DFI phase slots are driven identically so the
  // encoder output is phase-independent.  Commands are active-LOW and are
  // driven on the CK negedge (stable well ahead of the sampling posedge).
  //--------------------------------------------------------------------------
  task drive(input rasN, input casN, input weN, input cke,
             input [2:0] bank, input [12:0] addr_x);
    begin
      ctl_rasN = {rasN,rasN};  ctl_casN = {casN,casN};
      ctl_weN  = {weN,weN};    ctl_cke  = {cke,cke};
      ctl_csN  = 2'b00;        ctl_odt  = 2'b11;
      ctl_bank = {bank,bank};  ctl_addr = {addr_x,addr_x};
    end
  endtask

  // Single-cycle command (decoded on the next CK posedge), then release NOP.
  task issue(input rasN, input casN, input weN,
             input [2:0] bank, input [12:0] addr_x);
    begin
      @(negedge dramClk);
      drive(rasN,casN,weN,1'b1, bank, addr_x);
      @(posedge dramClk);                       // model decodes here
      @(negedge dramClk);
      drive(1'b1,1'b1,1'b1,1'b1, 3'd0, 13'd0);  // NOP
    end
  endtask

  // Wait n CK cycles (NOP held).
  task gap(input integer n);
    integer i;
    begin
      for (i = 0; i < n; i = i + 1) @(posedge dramClk);
    end
  endtask

  //--------------------------------------------------------------------------
  // Round-trip: ACT -> WR(fill) -> RD -> capture -> PRECHARGE
  // Event-driven read capture (collect the model's non-high-Z DQ beats) is
  // immune to the exact CL latency.
  //--------------------------------------------------------------------------
  reg [15:0] rd_cap [0:31];
  integer rd_cnt, run_len, j;

  // READ col and capture BL beats; ok = a contiguous run of `exp` beats exists.
  task read_verify(input [12:0] col, input [15:0] exp, output integer ok);
    integer i;
    begin
      issue(1'b1,1'b0,1'b1, 3'd0, col);
      rd_cnt = 0;
      for (i = 0; i < 64 && rd_cnt < 16; i = i + 1) begin
        @(posedge dqs_clk);
        if (dq_bus !== 16'hzzzz && dq_bus !== 16'hxxxx) begin
          rd_cap[rd_cnt] = dq_bus;
          rd_cnt = rd_cnt + 1;
        end
      end
      ok = 0; run_len = 0;
      for (j = 0; j < rd_cnt; j = j + 1) begin
        if (rd_cap[j] === exp) run_len = run_len + 1; else run_len = 0;
        if (run_len >= BL) ok = 1;
      end
      $display("[e2e] col=%03h captured=%0d ok=%0d", col, rd_cnt, ok);
    end
  endtask

  // Round-trip: ACTIVATE -> WR(fill) -> (gap) -> PRECHARGE-all.
  task round_trip(input [12:0] col, input [15:0] fill, output integer ok);
    integer i;
    begin
      // ACTIVATE bank0 row 0x10
      issue(1'b0,1'b1,1'b1, 3'd0, 13'h010);
      gap(20);                                   // tRCD margin

      // WRITE col: constant-fill DQ + 400 MHz DQS square wave
      issue(1'b1,1'b0,1'b0, 3'd0, col);
      wr_enable = 1'b1; wb_dqs = 1'b0;
      for (i = 0; i < 24; i = i + 1) begin       // ~12 tCK of DQS (lead+data+trail)
        @(posedge dqs_clk);
        wb_dq  = fill;
        wb_dqs = ~wb_dqs;                        // toggle -> DQS square wave
      end
      wr_enable = 1'b0;                          // release DQ/DQS (high-Z)
      gap(20);                                   // burst + tWTR margin

      read_verify(col, fill, ok);

      // PRECHARGE all banks so the next ACT is legal
      issue(1'b0,1'b1,1'b0, 3'd0, 13'h400);
      gap(20);                                   // tRP margin
    end
  endtask

  //--------------------------------------------------------------------------
  // Main
  //--------------------------------------------------------------------------
  integer ok0, ok1, okc;

  initial begin
    // ---- t=0 settle: X -> defined. This single change event latches the
    //      model's BUS_DELAY `_in` input copies (otherwise they stay X and
    //      trip the ungated reset / CKE-active checks). CKE inactive, NOP. ----
    mcRst = 1'b1;
    drive(1'b1,1'b1,1'b1,1'b0, 3'd0, 13'd0);

    // Hold CKE inactive + reset asserted for > 10 ns before releasing reset.
    repeat (20) @(posedge dramClk);
    mcRst = 1'b0;                                 // RST_N inactive (CKE still 0)
    gap(8);                                        // CKE low post-reset

    // ---- CKE active with a clean NOP ----
    @(negedge dramClk);
    drive(1'b1,1'b1,1'b1,1'b1, 3'd0, 13'd0);
    gap(64);                                       // tXPR = 170 ns (~57 tCK) exit-reset -> valid cmd

    // ---- init: ZQCL, then MRS banks (MR2, MR1, MR0) + MR3 housekeeping ----
    issue(1'b1,1'b1,1'b0, 3'd0, 13'h400);         // ZQCAL long (addr A10=1)
    gap(1100);                                      // tZQinit ~ 1024 tCK: ZQ-cal -> first valid cmd
    issue(1'b0,1'b0,1'b0, 3'd2, 13'h000);         // MR2 (bank2): CWL=5 (addr[5:3]=0)
    gap(16);                                        // tMRD/tMOD margin
    issue(1'b0,1'b0,1'b0, 3'd1, 13'h004);         // MR1 (bank1): DLL enable, ODT, AL=0
    gap(16);
    issue(1'b0,1'b0,1'b0, 3'd0, 13'h110);         // MR0 (bank0): CL=5 (addr[4]) + DLL reset (addr[8])
    gap(16);

    // MR0's CL bit (addr[2]=1 for CL=5) also latches the model's GLOBAL mpr_en
    // (ddr3.v sets mpr_en = addr[2] on EVERY MRS, not just MR3). Since bank0 is
    // written last, mpr_en stays 1 and the model rejects ACT/WR. Issue a final
    // MR3 (bank3) write with addr[2]=0 to clear it, then wait tDLLK (512 tCK)
    // for the DLL to re-lock after the MR0 reset.
    issue(1'b0,1'b0,1'b0, 3'd3, 13'h000);         // MR3: clear global mpr_en
    gap(512);                                       // tDLLK = 512 tCK DLL locking after MR0 reset

    // ---- round-trip column 0, then column 8 (next burst, no overlap) ----
    round_trip(13'h000, FILL0, ok0);
    round_trip(13'h008, FILL1, ok1);

    // Location-specificity: re-read col0 -> must STILL be FILL0, NOT FILL1.
    issue(1'b0,1'b1,1'b1, 3'd0, 13'h010);        // ACTivate bank0 row0x10
    gap(20);
    read_verify(13'h000, FILL0, okc);
    issue(1'b0,1'b1,1'b0, 3'd0, 13'h400);        // PRECHARGE all
    gap(20);

    $display("[e2e] col0 ok=%0d  col8 ok=%0d  col0-still-A000 ok=%0d", ok0, ok1, okc);
    if (ok0 && ok1 && okc)
      $display("[e2e] PASS: WR/RD round-trip vs original ddr3.v (2 locations)");
    else
      $display("[e2e] FAIL: round-trip incomplete (col0=%0d col8=%0d)", ok0, ok1);
    $finish;
  end

  // Safety net: abort if something wedges.
  initial begin
    #200000;
    $display("[e2e] TIMEOUT (200 us) -- aborting");
    $finish;
  end

endmodule
