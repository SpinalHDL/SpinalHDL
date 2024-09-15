package spinal.lib.memory.sdram.dfi.PHY

import spinal.lib.memory.sdram.dfi.CtrlWithBmb.{DfiController, BmbPortParameter, CtrlParameter}
import spinal.lib.memory.sdram.dfi.Interface._
import spinal.lib.memory.sdram.dfi._
import spinal.lib._
import spinal.core._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}

class DDR3IO(pl:PhyConfig, config: DfiConfig) extends Bundle{
  import config._
  val ck_p_o = out Bits(chipSelectNumber bits)
  val ck_n_o = out Bits(chipSelectNumber bits)
  val cke_o = out Bits(chipSelectNumber bits)
  val reset_n_o = out Bits(chipSelectNumber bits)
  val ras_n_o = out Bits(chipSelectNumber bits)
  val cas_n_o = out Bits(chipSelectNumber bits)
  val we_n_o = out Bits(chipSelectNumber bits)
  val cs_n_o = out Bits(chipSelectNumber bits)
  val ba_o = out Bits(bankWidth * chipSelectNumber bits)
  val addr_o = out Bits(addressWidth * chipSelectNumber bits)
  val odt_o = out Bits(chipSelectNumber bits)
  val dm_o = out Bits(pl.sdram.bytePerWord * chipSelectNumber bits)
  val dqs_p_io = inout (Analog(Bits(2 * chipSelectNumber bits)))
  val dqs_n_io = inout (Analog(Bits(2 * chipSelectNumber bits)))
  val dq_io = inout (Analog(Bits(pl.sdram.dataWidth*chipSelectNumber bits)))
}

case class bmb_dfi_ddr3(pl : PhyConfig, config: DfiConfig) extends Component{
  val tp:TaskParameter = TaskParameter(timingWidth=5,refWidth=23)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  import config._
  val io = new Bundle{
    val clk1 = in Bool()
    val clk2 = in Bool()
    val clk3 = in Bool()
    val clk4 = in Bool()
    val bmb = slave(Bmb(bmbp))
    val ddr3 = new DDR3IO(pl,config)
    val initDone = out Bool()
  }
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,ClockDomain.current,cmdBufferSize=64,dataBufferSize=64,rspBufferSize=64)
  val ctp = CtrlParameter(tp, bmbpp)
  val tpa = TaskParameterAggregate(tp,pl,BmbAdapter.corePortParameter(bmbpp, pl),config)

  val clockArea = new ClockingArea(ClockDomain.current) {
    val bmb2dfi = DfiController(ctp, pl, config)
    bmb2dfi.io.bmb <> io.bmb
  }

  if(config.frequencyRatio == 1){
    val ddr3Chips = for(i <- 0 until(chipSelectNumber)) yield new Area {
      val sel = i
      val phy = dfi_phy_ddr3(tpa)
    }

    ddr3Chips.map(_.phy.io.dfi.rddata_valid_o).orR <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddataValid
    ddr3Chips.map(_.phy.io.dfi.rddata_o).reduceBalancedTree(_|_) <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddata

    val rst = ~ClockDomain.readResetWire
    val adapter = for(ddr3Chip <- ddr3Chips) yield new Area{
      ddr3Chip.phy.io.clk.i <> io.clk1
      ddr3Chip.phy.io.clk.ddr_i <> io.clk2
      ddr3Chip.phy.io.clk.ddr90_i <> io.clk3
      ddr3Chip.phy.io.clk.ref_i <> io.clk4
      ddr3Chip.phy.io.rst_i := rst

      ddr3Chip.phy.io.dfi.cke_i <> clockArea.bmb2dfi.io.dfi.control.cke(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.odt_i.clear()
      ddr3Chip.phy.io.dfi.reset_n_i.set()
      ddr3Chip.phy.io.dfi.cs_n_i <> clockArea.bmb2dfi.io.dfi.control.csN(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.ras_n_i <> clockArea.bmb2dfi.io.dfi.control.rasN.orR
      ddr3Chip.phy.io.dfi.cas_n_i <> clockArea.bmb2dfi.io.dfi.control.casN.orR
      ddr3Chip.phy.io.dfi.we_n_i <> clockArea.bmb2dfi.io.dfi.control.weN.orR
      ddr3Chip.phy.io.dfi.bank_i <> clockArea.bmb2dfi.io.dfi.control.bank
      ddr3Chip.phy.io.dfi.address_i <> clockArea.bmb2dfi.io.dfi.control.address

      ddr3Chip.phy.io.dfi.wrdata_en_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdataEn
      ddr3Chip.phy.io.dfi.wrdata_mask_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdataMask
      ddr3Chip.phy.io.dfi.wrdata_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdata

      ddr3Chip.phy.io.dfi.rddata_en_i <> clockArea.bmb2dfi.io.dfi.read.rden(0)


      ddr3Chip.phy.io.ddr3.ck_p_o <> io.ddr3.ck_p_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ck_n_o <> io.ddr3.ck_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cke_o  <> io.ddr3.cke_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.reset_n_o <> io.ddr3.reset_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ras_n_o <> io.ddr3.ras_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cas_n_o <> io.ddr3.cas_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.we_n_o  <> io.ddr3.we_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cs_n_o  <> io.ddr3.cs_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ba_o  <> io.ddr3.ba_o(bankWidth*ddr3Chip.sel,bankWidth bits)
      ddr3Chip.phy.io.ddr3.addr_o  <> io.ddr3.addr_o(addressWidth*ddr3Chip.sel,addressWidth bits)
      ddr3Chip.phy.io.ddr3.odt_o  <> io.ddr3.odt_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.dm_o  <> io.ddr3.dm_o(pl.sdram.bytePerWord*ddr3Chip.sel,pl.sdram.bytePerWord bits)
      ddr3Chip.phy.io.ddr3.dqs_p_io <> io.ddr3.dqs_p_io(2*ddr3Chip.sel,2 bits)
      ddr3Chip.phy.io.ddr3.dqs_n_io <> io.ddr3.dqs_n_io(2*ddr3Chip.sel,2 bits)
      ddr3Chip.phy.io.ddr3.dq_io  <> io.ddr3.dq_io(pl.sdram.dataWidth*ddr3Chip.sel,pl.sdram.dataWidth bits)
      ddr3Chip.phy.io.initDone <> io.initDone
    }
  }

}

case class Bmb_Cmd(bmbp: BmbParameter, pl: PhyConfig) extends Component{

  val io = new Bundle{
    val bmb = master(Bmb(bmbp))
    val initDone = in Bool()
  }
  val counter = Reg(UInt(io.bmb.cmd.length.getWidth + 1 bits)).init(0)
  val opcodeCount = RegInit(U(0, 2 bits))
  val idleTimer = RegInit(U(0,4 bits))
  def write(initdata: UInt, length: Int, address: Int) = {
    when(idleTimer === 0){
      counter := U(length, log2Up(length) + 1 bits).resized
    }
    when(counter =/= 0) {
      counter := counter - 1
      io.bmb.cmd.valid.set()
      io.bmb.cmd.address := address
      io.bmb.cmd.length := U(length * pl.bytePerBeat - 1).resized
      io.bmb.cmd.opcode := True.asBits
    }

    io.bmb.cmd.data := initdata.rotateLeft(((U(length) - counter) >> 1).resize(log2Up(io.bmb.cmd.data.getWidth))).asBits
    when(counter === 1) {
      io.bmb.cmd.last.set()
    }
    println("write command")
  }
  def read(length: Int, address: Int): Unit = {
    io.bmb.cmd.address := address
    io.bmb.cmd.length := U(length * pl.bytePerBeat - 1).resized
    io.bmb.cmd.opcode := False.asBits
    io.bmb.cmd.last.set()
    io.bmb.cmd.valid.set()
  }

  val start = RegInit(False).setWhen(io.initDone)
//  val start = True
  io.bmb.cmd.valid.clear()
  io.bmb.cmd.last.clear()
  io.bmb.cmd.source.clearAll()
  io.bmb.cmd.opcode.setAll()
  io.bmb.cmd.address.clearAll()
  io.bmb.cmd.length.clearAll()
  io.bmb.cmd.data.clearAll()
  io.bmb.cmd.mask.clearAll()
  io.bmb.cmd.context.clearAll()
  io.bmb.rsp.ready := RegInit(False).setWhen(io.bmb.cmd.valid && io.bmb.cmd.opcode === 0).clearWhen(io.bmb.rsp.last)


  when(idleTimer =/= 0){
    idleTimer := idleTimer - 1
  }
  when(idleTimer === 1 || io.bmb.cmd.opcode === 0){
    when(opcodeCount < opcodeCount.maxValue){opcodeCount := opcodeCount + 1}
  }
  when(io.bmb.cmd.last) {
    idleTimer := 10
  }
  when(start) {
    switch(opcodeCount) {
      is(0) {
        write(U"32'h9FD24589", 32, 128)
      }
      is(1) {
        write(U"32'h75BC2457", 32, 1024)
      }
      is(2){
        read(32,128)
      }
    }
  }

}

case class dfi_ddr3() extends Component{
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramConfig(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyConfig = PhyConfig(sdram = sdram, phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,
    tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3,tPhyRdCslat = 0,tPhyWrCsLat = 0)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=new DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  import config._
  val io = new Bundle{
    val clk = in Bool()
    val rst_n = in Bool()
    val ddr3 = new DDR3IO(pl,config)
  }
  noIoPrefix()
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work",bmbClockDomainCfg)

  val pll = pll_clk()
  pll.io.clk.in1 <> io.clk
  pll.io.reset <> ~io.rst_n
  val rst_n = io.rst_n & pll.io.locked
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rst_n

  val topClockingArea = new ClockingArea(myClockDomain){
    val bmb_cmd = Bmb_Cmd(bmbp,pl)

    val bmbddr = bmb_dfi_ddr3(pl, config)
    bmbddr.io.clk1 <> pll.io.clk.out1
    bmbddr.io.clk2 <> pll.io.clk.out2
    bmbddr.io.clk3 <> pll.io.clk.out3
    bmbddr.io.clk4 <> pll.io.clk.out4
    bmbddr.io.bmb <> bmb_cmd.io.bmb
    bmbddr.io.ddr3 <> io.ddr3
    bmbddr.io.initDone <> bmb_cmd.io.initDone
  }
}

object bmb_dfi_ddr3 extends App{
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramConfig(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyConfig = PhyConfig(sdram = sdram, phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,
    tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3,tPhyRdCslat = 0,tPhyWrCsLat = 0)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=new DDR(),timeConfig=timeConfig)
  val ver = SpinalConfig().generateVerilog(bmb_dfi_ddr3(pl, config))
  ver.mergeRTLSource("ddr3_dfi_phy")
}
object Bmb_Cmd extends App{
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramConfig(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyConfig = PhyConfig(sdram = sdram, phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,
    tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3,tPhyRdCslat = 0,tPhyWrCsLat = 0)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=new DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)

  SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)).generateVerilog(Bmb_Cmd(bmbp,pl))

}

object dfi_ddr3 extends App{
  val ver = SpinalConfig().generateVerilog(dfi_ddr3())
  ver.mergeRTLSource("ddr3_dfi_phy")
}