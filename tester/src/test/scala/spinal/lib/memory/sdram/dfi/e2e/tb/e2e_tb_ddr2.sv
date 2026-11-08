//=============================================================================
// e2e_tb_ddr2.sv
//
// Standalone Icarus-Verilog co-simulation testbench for the SpinalHDL DFI DDR2
// command encoder (DfiLegacySimPhy, exported via SpinalVerilog) wired LIVE to
// the ORIGINAL, UNMODIFIED Micron ddr2.v behavioral model.
//
// Design notes:
//   * No Verilator, no patched model derivative: pristine ddr2.v compiled
//     directly; iverilog's 4-state engine runs the model's built-in timing
//     checker for real.
//   * tCK = 5.0 ns: the model's default speed bin (no `define) is sg5E with
//     TCK_MIN = 5000 ps, so a faster clock would trip the tCK(min) checker.
//   * Command encoding is the model's own (ddr2.v decodes
//     cmd_n_in = {ras_n, cas_n, we_n}):
//       ACTIVATE  = {0,1,1}   WRITE = {1,0,0}   READ = {1,0,1}
//       PRECHARGE = {0,1,0}   REFRESH = {0,0,1} LOAD_MODE = {0,0,0}
//       PRECHARGE-ALL adds addr[AP=10]=1.
//   * Mode registers (decoded per ddr2.v):
//       MR0: BL = 1<<addr[2:0] (011 => BL8), burst_order = addr[3] (0 = seq),
//            CL = addr[6:4] (101 => 5), DLL reset = addr[8],
//            WR = addr[11:9]+1 (100 => 5).
//            DLL-reset value = 0x953, normal value = 0x853.
//            CL* tCK = 25 ns >= CL_TIME (15 ns); WR* tCK = 25 ns >= TWR (15 ns).
//       MR1: DLL enable = !addr[0] (0x000 enables), OCD = addr[9:7]
//            (0x380 = 111 for the init handshake, 0x000 = 000 final).
//       MR2 / MR3: 0x000.
//   * Init mirrors the model's own `initialize` task order (pin-driven):
//       PRE-ALL, MRS MR3/MR2/MR1/MR0(reset), PRE-ALL, REF, REF,
//       MRS MR0(normal), MRS MR1(OCD=111), MRS MR1(OCD=000), then 250 tCK
//       for the DLL to lock (TDLLK = 200 tCK).
//   * CKE is raised only after 200 us (the model WARNs otherwise).
//   * Write DQS: 50%-duty square wave, period 2.5 ns, phase-locked so DQS
//     posedges coincide with CK posedges. Verified against the model's
//     wdqs_cntr flag machinery (WDQS_PRE/PST = 1): every check_write_dqs_high
//     flag is cleared by a DQS negedge and every check_write_dqs_low flag by a
//     DQS posedge within its CK window; tWPRE/tWPST/tDSS/tDQSS/tDSH margins all
//     pass with 2.5 ns separations.
//   * Read capture: sample DQ on a free-running 2.5 ns sampler (beat-center
//     phase, no boundary races), keep non-x/z values, scan for a contiguous
//     run of BL=8 fill beats.
//   * Pin settling: the model mirrors every control pin into an `_in` copy via
//     `always @(sig) _in <= #0 sig`, so each copy stays X until the pin first
//     CHANGES. Control regs start X and are settled procedurally at t~0.2 ns.
//   * Width: the model's default (no x4/x8 `define) is x16 -> dq[15:0],
//     dqs[1:0] (two 8-bit DQS groups), dm_rdqs[1:0]. DM tied to 0 (mask
//     disabled) so writes land; both DQS groups are driven in phase.
//=============================================================================
`timescale 1ns / 1ps

module e2e_tb;

  //--------------------------------------------------------------------------
  // Parameters
  //--------------------------------------------------------------------------
  localparam int CTL_AW  = 26;           // address slot width (13 * fr=2)
  localparam int CTL_BW  = 6;            // bank slot width  (3  * fr=2)
  localparam [15:0] FILL0 = 16'hA000;    // fill for column 0
  localparam [15:0] FILL1 = 16'hB000;    // fill for column 8
  localparam int  BL     = 8;            // burst length
  localparam [12:0] ROW  = 13'h010;      // test row
  localparam [12:0] PREA = 13'h400;      // PRECHARGE-ALL (addr[10]=1)

  // Mode register values
  localparam [12:0] MR0_RST  = 13'h953;  // BL8, seq, CL5, WR5, DLL reset
  localparam [12:0] MR0_NORM = 13'h853;  // BL8, seq, CL5, WR5
  localparam [12:0] MR1_OCD  = 13'h380;  // OCD = 111
  localparam [12:0] MR_ZERO  = 13'h000;

  //--------------------------------------------------------------------------
  // Clocks (all phase-locked from t=0):
  //   ck   : period 5.0 ns  (tCK = 5000 ps, posedges at 2.5 + 5k ns)
  //   sclk : period 2.5 ns  (DQS-rate read sampler, posedges at 1.25 + 2.5k)
  //--------------------------------------------------------------------------
  reg ck   = 1'b0;
  reg sclk = 1'b0;
  always #2.5  ck   = ~ck;
  always #1.25 sclk = ~sclk;
  wire ck_n = ~ck;

  //--------------------------------------------------------------------------
  // Encoder (generated from SpinalHDL) <-> model command pins
  //--------------------------------------------------------------------------
  reg [CTL_AW-1:0] ctl_addr;             // X until t=0 settle
  reg [CTL_BW-1:0] ctl_bank;
  reg [1:0] ctl_rasN, ctl_casN, ctl_weN, ctl_csN, ctl_cke, ctl_odt;

  wire       e_ck, e_ck_n, e_cke, e_cs_n, e_ras_n, e_cas_n, e_we_n, e_odt;
  wire [2:0] e_ba;
  wire [12:0] e_addr;

  DfiLegacySimPhy u_enc (
    .io_mcClk         (ck),
    .io_mcRst         (1'b0),
    .io_dramClk       (ck),
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

  //--------------------------------------------------------------------------
  // Tri-state DQ / DQS data bus (TB drives writes, model drives reads)
  //--------------------------------------------------------------------------
  reg [15:0] wb_dq   = 16'h0000;
  reg [1:0]  wb_dqs  = 2'b00;
  reg        wr_enable = 1'b0;

  wire [15:0] dq_bus;
  wire [1:0]  dqs_wire, dqs_n_wire, rdqs_n_wire;
  wire [1:0]  dm_wire = 2'b00;           // DM=0 => data mask disabled

  assign dq_bus     = wr_enable ? wb_dq   : 16'bz;
  assign dqs_wire   = wr_enable ? wb_dqs  : 2'bz;
  assign dqs_n_wire = wr_enable ? ~wb_dqs : 2'bz;

  //--------------------------------------------------------------------------
  // Micron ddr2.v behavioral model (UNMODIFIED)
  //--------------------------------------------------------------------------
  ddr2 #(.DEBUG(1), .STOP_ON_ERROR(0)) u_mem (
    .ck      (e_ck),
    .ck_n    (e_ck_n),
    .cke     (e_cke),
    .cs_n    (e_cs_n),
    .ras_n   (e_ras_n),
    .cas_n   (e_cas_n),
    .we_n    (e_we_n),
    .dm_rdqs (dm_wire),
    .ba      (e_ba),
    .addr    (e_addr),
    .dq      (dq_bus),
    .dqs     (dqs_wire),
    .dqs_n   (dqs_n_wire),
    .rdqs_n  (rdqs_n_wire),
    .odt     (e_odt)
  );

  //--------------------------------------------------------------------------
  // Command helpers. Both DFI phase slots are driven identically (the
  // encoder slices slot 0). Commands are driven on the CK negedge so they are
  // stable well ahead of the sampling posedge (tIS = 350 ps).
  //--------------------------------------------------------------------------
  task drive_nop;
    begin
      ctl_rasN = 2'b11; ctl_casN = 2'b11; ctl_weN = 2'b11;
      ctl_cke  = 2'b11; ctl_csN  = 2'b00; ctl_odt  = 2'b11;
      ctl_bank = {CTL_BW{1'b0}}; ctl_addr = {CTL_AW{1'b0}};
    end
  endtask

  // Single-cycle command (decoded on the next CK posedge), then release NOP.
  task issue(input rasN, input casN, input weN,
             input [2:0] bank, input [12:0] a);
    begin
      @(negedge ck);
      ctl_rasN = {rasN,rasN}; ctl_casN = {casN,casN}; ctl_weN = {weN,weN};
      ctl_cke  = 2'b11;       ctl_csN  = 2'b00;       ctl_odt  = 2'b11;
      ctl_bank = {bank,bank}; ctl_addr = {a,a};
      @(posedge ck);                          // model decodes here
      @(negedge ck);
      drive_nop;
    end
  endtask

  // Wait n CK cycles (NOP held).
  task gap(input integer n);
    integer i;
    begin
      for (i = 0; i < n; i = i + 1) @(posedge ck);
    end
  endtask

  //--------------------------------------------------------------------------
  // READ `col`, capture beats on the free-running sampler, verify a
  // contiguous run of BL beats equals `exp`.
  //--------------------------------------------------------------------------
  reg [15:0] rd_cap [0:31];
  integer rd_cnt, run_len, j;

  task read_verify(input [12:0] col, input [15:0] exp, output integer ok);
    integer i;
    begin
      issue(1'b1, 1'b0, 1'b1, 3'd0, col);     // READ
      rd_cnt = 0;
      for (i = 0; i < 40 && rd_cnt < 16; i = i + 1) begin
        @(posedge sclk);
        if (dq_bus !== 16'hxxxx && dq_bus !== 16'hzzzz) begin
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

  //--------------------------------------------------------------------------
  // WRITE `col` with constant `fill`: drive the command and the DQS square
  // wave with exact #delays, phase-locked to ck (DQS posedges on CK posedges).
  // Timeline (T1 = WR decode posedge):
  //   T1-2.5 : command + DQS=0 presented, wr_enable
  //   T1     : WR decoded; DQS posedge (command pins held!)
  //   T1+2.5 : command released to NOP (CK negedge, satisfies tIS = 350 ps;
  //            releasing at T1 would change pins AT the sampling edge)
  //   T1+5.0k: DQS toggles (16 edges -> last at T1+45.0)
  //   T1+20.0..T1+37.5 : 8 data beats latched (wdq_cntr window)
  //   T1+40.0 : postamble posedge clears check_write_postamble
  //   T1+45.0 : bus released
  //--------------------------------------------------------------------------
  task write_fill(input [12:0] col, input [15:0] fill);
    integer i;
    begin
      @(negedge ck);                          // T1 - 2.5
      ctl_rasN = 2'b11; ctl_casN = 2'b00; ctl_weN = 2'b00;   // WRITE
      ctl_cke  = 2'b11; ctl_csN  = 2'b00; ctl_odt  = 2'b11;
      ctl_bank = {3'd0, 3'd0}; ctl_addr = {col, col};
      wr_enable = 1'b1; wb_dq = fill; wb_dqs = 2'b00;
      #2.5;                                   // T1: decode + DQS posedge
      #2.5;                                   // T1+2.5: CK negedge
      drive_nop;
      repeat (16) begin
        #2.5;
        wb_dqs = ~wb_dqs;  // both DQS groups toggled in phase
      end
      wr_enable = 1'b0;                       // T1 + 45.0
    end
  endtask

  //--------------------------------------------------------------------------
  // Round-trip: ACT -> WR(fill) -> RD -> verify -> PRE-ALL
  //--------------------------------------------------------------------------
  task round_trip(input [12:0] col, input [15:0] fill, output integer ok);
    begin
      issue(1'b0, 1'b1, 1'b1, 3'd0, ROW);     // ACTIVATE bank0
      gap(10);                                 // tRCD = 15 ns margin
      write_fill(col, fill);
      read_verify(col, fill, ok);              // issues READ + captures
      issue(1'b0, 1'b1, 1'b0, 3'd0, PREA);     // PRECHARGE ALL
      gap(10);                                 // tRP = 15 ns margin
    end
  endtask

  //--------------------------------------------------------------------------
  // Main
  //--------------------------------------------------------------------------
  integer ok0, ok1, okc;

  initial begin
    // ---- t=0 settle: X -> defined. This single change event latches the
    //      model's BUS_DELAY `_in` input copies. CKE inactive, NOP. ----
    ctl_rasN = 2'b11; ctl_casN = 2'b11; ctl_weN = 2'b11;
    ctl_cke  = 2'b00; ctl_csN  = 2'b00; ctl_odt  = 2'b11;
    ctl_bank = {CTL_BW{1'b0}}; ctl_addr = {CTL_AW{1'b0}};

    // ---- wait out the 200 us post-power-up requirement ----
    while ($time < 201000) @(posedge ck);

    // ---- CKE active with a clean NOP ----
    @(negedge ck);
    ctl_cke = 2'b11;
    gap(4);

    // ---- init sequence (mirrors the model's `initialize` task) ----
    issue(1'b0, 1'b1, 1'b0, 3'd0, PREA);      // PRECHARGE ALL
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd3, MR_ZERO);   // MRS MR3
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd2, MR_ZERO);   // MRS MR2
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd1, MR_ZERO);   // MRS MR1 (DLL enable)
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd0, MR0_RST);   // MRS MR0 (CL5, WR5, DLL reset)
    gap(4);
    issue(1'b0, 1'b1, 1'b0, 3'd0, PREA);      // PRECHARGE ALL (banks forced active)
    gap(30);                                 // >= TRFC_MIN (127.5 ns)
    issue(1'b0, 1'b0, 1'b1, 3'd0, 13'd0);     // REFRESH 1
    gap(30);                                 // >= TRFC_MIN (127.5 ns)
    issue(1'b0, 1'b0, 1'b1, 3'd0, 13'd0);     // REFRESH 2
    gap(30);                                 // >= TRFC_MIN (127.5 ns)
    issue(1'b0, 1'b0, 1'b0, 3'd0, MR0_NORM);  // MRS MR0 (no DLL reset)
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd1, MR1_OCD);   // MRS MR1 (OCD = 111)
    gap(4);
    issue(1'b0, 1'b0, 1'b0, 3'd1, MR_ZERO);   // MRS MR1 (OCD = 000) -> init done
    gap(250);                                  // > TDLLK (200 tCK) for DLL lock

    // ---- round-trip column 0, then column 8 (disjoint bursts) ----
    round_trip(13'h000, FILL0, ok0);
    round_trip(13'h008, FILL1, ok1);

    // Location-specificity: re-read col0 -> must STILL be FILL0, NOT FILL1.
    issue(1'b0, 1'b1, 1'b1, 3'd0, ROW);        // ACTIVATE bank0
    gap(10);
    read_verify(13'h000, FILL0, okc);
    issue(1'b0, 1'b1, 1'b0, 3'd0, PREA);       // PRECHARGE ALL
    gap(10);

    $display("[e2e] col0 ok=%0d  col8 ok=%0d  col0-still-A000 ok=%0d", ok0, ok1, okc);
    if (ok0 && ok1 && okc)
      $display("[e2e] PASS: WR/RD round-trip vs original ddr2.v (2 locations)");
    else
      $display("[e2e] FAIL: round-trip incomplete (col0=%0d col8=%0d)", ok0, ok1);
    $finish;
  end

  // Safety net: abort if something wedges.
  initial begin
    #400000;
    $display("[e2e] TIMEOUT (400 us) -- aborting");
    $finish;
  end

endmodule
