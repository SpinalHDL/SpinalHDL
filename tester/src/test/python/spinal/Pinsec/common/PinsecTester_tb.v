
module PinsecTester_tb
(
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

  reg   io_asyncReset;
  reg   io_axiClk;
  reg   io_vgaClk;

  reg  loader_valid;
  reg  [15 : 0]loader_data;
  reg  [1 : 0]loader_bank;
  reg  [31 : 0]loader_address;

  assign io_sdram_DQ_read = io_sdram_DQ;
  assign io_sdram_DQ = io_sdram_DQ_writeEnable ? io_sdram_DQ_write : 16'bZZZZZZZZZZZZZZZZ;

  mt48lc16m16a2 sdram(
    .Dq(io_sdram_DQ),
    .Addr(io_sdram_ADDR),
    .Ba(io_sdram_BA),
    .Clk(io_axiClk),
    .Cke(io_sdram_CKE),
    .Cs_n(io_sdram_CSn),
    .Ras_n(io_sdram_RASn),
    .Cas_n(io_sdram_CASn),
    .We_n(io_sdram_WEn),
    .Dqm(io_sdram_DQM),
    .loader_valid(loader_valid),
    .loader_data(loader_data),
    .loader_bank(loader_bank),
    .loader_address(loader_address)
  );


  Pinsec uut (
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
    .io_asyncReset(io_asyncReset),
    .io_axiClk(io_axiClk),
    .io_vgaClk(io_vgaClk)
  );

 initial begin
   // $dumpfile("E:/waves/wave.vcd");
   // $dumpvars(0, uut);
   // $dumpvars(0, uut.axi_vgaCtrl);
   // $dumpvars(0, uut.axi_core);
   //  $dumpvars(0, sdram);
    //$dumpvars(0, uut.axi_sdramCtrl);
   // $dumpvars(0, uut.axi_core.core.execute0_inInst_payload_pc);
 end
endmodule

