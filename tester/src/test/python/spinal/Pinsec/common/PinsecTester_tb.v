
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
/*
  initial begin
    io_axiClk = 0;
    io_asyncReset = 1;
  end

  initial begin
    while(1) begin
      #3750  io_axiClk =  ! io_axiClk;
    end
  end

  initial begin
      io_asyncReset = 1;
      #20000;
      io_asyncReset = 0;
  end*/



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
    .Dqm(io_sdram_DQM)
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
    .io_axiClk(io_axiClk)
  );

//  initial begin
 //   $dumpfile("wave.vcd");
 //   $dumpvars(0, PinsecTester_tb);
 // end
endmodule

