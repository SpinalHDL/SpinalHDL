package spinal.lib.memory.sdram.xdr.Dfi.PHY

import CtrlWithBmb.{Bmb2Dfi, BmbAdapter, BmbPortParameter, CtrlParameter}
//import PHY.dfi_ddr3.{config, pl}
import spinal.lib._
import spinal.core._
import _root_.Interface.{CoreParameter, CoreParameterAggregate, DDR, DfiConfig, DfiTimeConfig, PhyLayout, SdramGeneration, SdramLayout, SdramTiming}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}

case class bmb_dfi_ddr3(pl : PhyLayout, config: DfiConfig) extends Component{
  val core:CoreParameter = CoreParameter(timingWidth=5,refWidth=23)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val bmbClockDomainCfg = ClockDomainConfig(resetActiveLevel = LOW)
  val myClockDomain = ClockDomain.internal("work",bmbClockDomainCfg)
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,myClockDomain,cmdBufferSize=64,dataBufferSize=64,rspBufferSize=64)
//  val ctrlbmbp = BmbParameter(addressWidth = 5, dataWidth = pl.sdram.chipAddressWidth,sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val ctp = CtrlParameter(core, bmbpp)
  import config._
  val io = new Bundle{
    val clk = in Bool()
    val rstN = in Bool()
    val clkw = out Bool()
    val rstNw = out Bool()
    val bmb = slave(Bmb(ctp.port.bmb))
    val initDone = out Bool()
    val ddr3 = new Bundle{
      val ck_p_o = out Bits(chipSelectNumber bits)
      val ck_n_o = out Bits(chipSelectNumber bits)
      val cke_o = out Bits(chipSelectNumber bits)
      val reset_n_o = out Bits(chipSelectNumber bits)
      val ras_n_o = out Bits(chipSelectNumber bits)
      val cas_n_o = out Bits(chipSelectNumber bits)
      val we_n_o = out Bits(chipSelectNumber bits)
      val cs_n_o = out Bits(chipSelectNumber bits)
      val ba_o = out Bits(3*chipSelectNumber bits)
      val addr_o = out Bits(15*chipSelectNumber bits)
      val odt_o = out Bits(chipSelectNumber bits)
      val dm_o = out Bits(2*chipSelectNumber bits)
      val dqs_p_io = inout (Analog(Bits(2*chipSelectNumber bits)))
      val dqs_n_io = inout (Analog(Bits(2*chipSelectNumber bits)))
      val dq_io = inout (Analog(Bits(16*chipSelectNumber bits)))
    }
  }

  val pll = pll_clk()
  pll.io.clk.in1 <> io.clk
  pll.io.reset <> ~io.rstN

  val rstN = io.rstN & pll.io.locked

//  val bmbClockDomain = ClockDomain(pll.io.clk.out1,rstN,config = bmbClockDomainCfg)
  myClockDomain.clock := pll.io.clk.out1
  myClockDomain.reset := rstN
  io.clkw := pll.io.clk.out1
  io.rstNw := rstN

  val clockArea = new ClockingArea(myClockDomain) {
//    val bmb2dfi = Bmb2Dfi(ctp, pl, config, ctrlbmbp)
    val bmb2dfi = Bmb2Dfi(ctp, pl, config)
    bmb2dfi.io.bmb <> io.bmb
    bmb2dfi.io.initDone <> io.initDone
//    bmb2dfi.io.bmb.rsp.ready := RegInit(False).setWhen( bmb2dfi.io.bmb.cmd.valid && bmb2dfi.io.bmb.cmd.opcode === 0).clearWhen(bmb2dfi.io.bmb.rsp.last)
  }

//  val ddr3Chip = Vec(Data.,config.chipSelectNumber)

  if(config.frequencyRatio == 1){
    val ddr3Chips = for(i <- 0 until(chipSelectNumber)) yield new Area {
      val sel = i
      val phy = ddr3_dfi_phy()
    }
//    val ddrID = False.muxListDc(clockArea.bmb2dfi.io.dfi.control.csN.asBools.zipWithIndex)
//    val ddrID = Reg(Bits(log2Up(chipSelectNumber+1) bits))
//    for(i <- 0 until(chipSelectNumber)){
//      when(clockArea.bmb2dfi.io.dfi.control.csN(i)){
//        ddrID := ddr3Chips(i).sel
//      }
//    }
//
    ddr3Chips.map(_.phy.io.dfi.rddata_valid_o).orR <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddatavalid
    ddr3Chips.map(_.phy.io.dfi.rddata_o).reduceBalancedTree(_|_) <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddata


//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.ck_p_o).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.ck_p_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.ck_n_o).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.ck_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.cke_o ).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.cke_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.reset_n_o).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.reset_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.ras_n_o).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.ras_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.cas_n_o).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.cas_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.we_n_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.we_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.cs_n_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.cs_n_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.ba_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.ba_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.addr_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.addr_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.odt_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.odt_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.dm_o).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.dm_o
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.dqs_p_io).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.dqs_p_io
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.dqs_n_io).zipWithIndex.map(t=>(t._2,t._1))) <> io.ddr3.dqs_n_io
//    ddrID.muxListDc(ddr3Chips.map(_.phy.io.ddr3.dq_io).zipWithIndex.map(t=>(t._2,t._1)))  <> io.ddr3.dq_io
    val adapter = for(ddr3Chip <- ddr3Chips) yield new Area{
      ddr3Chip.phy.io.clk.i <> pll.io.clk.out1
      ddr3Chip.phy.io.clk.ddr_i <> pll.io.clk.out2
      ddr3Chip.phy.io.clk.ddr90_i <> pll.io.clk.out3
      ddr3Chip.phy.io.clk.ref_i <> pll.io.clk.out4
      ddr3Chip.phy.io.rst_i <> ~rstN

      ddr3Chip.phy.io.dfi.cke_i <> clockArea.bmb2dfi.io.dfi.control.cke(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.odt_i.clear()
      ddr3Chip.phy.io.dfi.reset_n_i.set()
      ddr3Chip.phy.io.dfi.cs_n_i <> clockArea.bmb2dfi.io.dfi.control.csN(ddr3Chip.sel)
      ddr3Chip.phy.io.dfi.ras_n_i <> clockArea.bmb2dfi.io.dfi.control.rasN.orR
      ddr3Chip.phy.io.dfi.cas_n_i <> clockArea.bmb2dfi.io.dfi.control.casN.orR
      ddr3Chip.phy.io.dfi.we_n_i <> clockArea.bmb2dfi.io.dfi.control.weN.orR
      ddr3Chip.phy.io.dfi.bank_i <> clockArea.bmb2dfi.io.dfi.control.bank
      ddr3Chip.phy.io.dfi.address_i <> clockArea.bmb2dfi.io.dfi.control.address

      ddr3Chip.phy.io.dfi.wrdata_en_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdataen
      ddr3Chip.phy.io.dfi.wrdata_mask_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdataMask
      ddr3Chip.phy.io.dfi.wrdata_i <> clockArea.bmb2dfi.io.dfi.write.wr(0).wrdata

      ddr3Chip.phy.io.dfi.rddata_en_i <> clockArea.bmb2dfi.io.dfi.read.rden(0)
//      ddr3Chip.phy.io.dfi.rddata_valid_o <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddatavalid
//      ddr3Chip.phy.io.dfi.rddata_o <> clockArea.bmb2dfi.io.dfi.read.rd(0).rddata

      ddr3Chip.phy.io.cfg.valid_i.clear()
      ddr3Chip.phy.io.cfg.i.assignDontCare()

      ddr3Chip.phy.io.ddr3.ck_p_o <> io.ddr3.ck_p_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ck_n_o <> io.ddr3.ck_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cke_o  <> io.ddr3.cke_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.reset_n_o <> io.ddr3.reset_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ras_n_o <> io.ddr3.ras_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cas_n_o <> io.ddr3.cas_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.we_n_o  <> io.ddr3.we_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.cs_n_o  <> io.ddr3.cs_n_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.ba_o  <> io.ddr3.ba_o(3*ddr3Chip.sel,3 bits)
      ddr3Chip.phy.io.ddr3.addr_o  <> io.ddr3.addr_o(15*ddr3Chip.sel,15 bits)
      ddr3Chip.phy.io.ddr3.odt_o  <> io.ddr3.odt_o(ddr3Chip.sel)
      ddr3Chip.phy.io.ddr3.dm_o  <> io.ddr3.dm_o(2*ddr3Chip.sel,2 bits)
      ddr3Chip.phy.io.ddr3.dqs_p_io <> io.ddr3.dqs_p_io(2*ddr3Chip.sel,2 bits)
      ddr3Chip.phy.io.ddr3.dqs_n_io <> io.ddr3.dqs_n_io(2*ddr3Chip.sel,2 bits)
      ddr3Chip.phy.io.ddr3.dq_io  <> io.ddr3.dq_io(16*ddr3Chip.sel,16 bits)
    }
  }

}

case class BmbCmd(bmbp: BmbParameter,pl: PhyLayout) extends Component{

  val io = new Bundle{
    val initDone = in Bool()
    val bmb = master(Bmb(bmbp))
  }
  val counter = Reg(UInt(io.bmb.cmd.length.getWidth + 1 bits)).init(0)
//  val cmdValid = RegInit(False)
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
//    when(counter === length) {
//      cmdValid := True
//    }
    //        io.bmb.cmd.data #= array(0)
    io.bmb.cmd.data := initdata.rotateLeft(((U(length) - counter) >> 1).resize(log2Up(io.bmb.cmd.data.getWidth))).asBits
    when(counter === 1) {
//      cmdValid := False
      io.bmb.cmd.last.set()
    }
    println("write command")
  }
  def read(length: Int, address: Int): Unit = {
    io.bmb.cmd.address := address
    io.bmb.cmd.length := U(length * pl.bytePerBeat - 1).resized
    io.bmb.cmd.opcode := False.asBits
//    cmdValid := True
    io.bmb.cmd.last.set()
    io.bmb.cmd.valid.set()
  }

  val start = RegInit(False).setWhen(io.initDone)
//  io.bmb.cmd.valid := cmdValid
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
  val pl:PhyLayout = PhyLayout(sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,sdramtime=SdramTiming(3)),
    phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
  import config._
  val io = new Bundle{
    val clk = in Bool()
    val rstN = in Bool()
    val ddr3 = new Bundle{
      val ck_p_o = out Bits(chipSelectNumber bits)
      val ck_n_o = out Bits(chipSelectNumber bits)
      val cke_o = out Bits(chipSelectNumber bits)
      val reset_n_o = out Bits(chipSelectNumber bits)
      val ras_n_o = out Bits(chipSelectNumber bits)
      val cas_n_o = out Bits(chipSelectNumber bits)
      val we_n_o = out Bits(chipSelectNumber bits)
      val cs_n_o = out Bits(chipSelectNumber bits)
      val ba_o = out Bits(3*chipSelectNumber bits)
      val addr_o = out Bits(15*chipSelectNumber bits)
      val odt_o = out Bits(chipSelectNumber bits)
      val dm_o = out Bits(2*chipSelectNumber bits)
      val dqs_p_io = inout (Analog(Bits(2*chipSelectNumber bits)))
      val dqs_n_io = inout (Analog(Bits(2*chipSelectNumber bits)))
      val dq_io = inout (Analog(Bits(16*chipSelectNumber bits)))
    }
  }

  val bmbddr = bmb_dfi_ddr3(pl, config)
  bmbddr.io.clk := io.clk
  bmbddr.io.rstN := io.rstN
  bmbddr.io.ddr3.ck_p_o <> io.ddr3.ck_p_o
  bmbddr.io.ddr3.ck_n_o <> io.ddr3.ck_n_o
  bmbddr.io.ddr3.cke_o <> io.ddr3.cke_o
  bmbddr.io.ddr3.reset_n_o <> io.ddr3.reset_n_o
  bmbddr.io.ddr3.ras_n_o <> io.ddr3.ras_n_o
  bmbddr.io.ddr3.cas_n_o <> io.ddr3.cas_n_o
  bmbddr.io.ddr3.we_n_o <> io.ddr3.we_n_o
  bmbddr.io.ddr3.cs_n_o <> io.ddr3.cs_n_o
  bmbddr.io.ddr3.ba_o <> io.ddr3.ba_o
  bmbddr.io.ddr3.addr_o <> io.ddr3.addr_o
  bmbddr.io.ddr3.odt_o <> io.ddr3.odt_o
  bmbddr.io.ddr3.dm_o <> io.ddr3.dm_o
  bmbddr.io.ddr3.dqs_p_io <> io.ddr3.dqs_p_io
  bmbddr.io.ddr3.dqs_n_io <> io.ddr3.dqs_n_io
  bmbddr.io.ddr3.dq_io <> io.ddr3.dq_io
  bmbddr.io.bmb.rsp.ready := RegInit(False).setWhen(bmbddr.io.bmb.cmd.valid && bmbddr.io.bmb.cmd.opcode === 0).clearWhen(bmbddr.io.bmb.rsp.last)
  bmbddr.io.bmb.cmd.valid.assignDontCare()
  bmbddr.io.bmb.cmd.last.assignDontCare()
  bmbddr.io.bmb.cmd.opcode.assignDontCare()
  bmbddr.io.bmb.cmd.length.assignDontCare()
  bmbddr.io.bmb.cmd.data.assignDontCare()
  bmbddr.io.bmb.cmd.mask.assignDontCare()
  bmbddr.io.bmb.cmd.address.assignDontCare()
  bmbddr.io.bmb.cmd.source.assignDontCare()
  bmbddr.io.bmb.cmd.context.assignDontCare()
}

object bmb_dfi_ddr3 extends App{
//  val bmbclockDomain = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,config=ClockDomainConfig(resetActiveLevel = HIGH))
//  val core:CoreParameter = CoreParameter(timingWidth=5,refWidth=23)
  val pl:PhyLayout = PhyLayout(sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,sdramtime=SdramTiming(3)),
    phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
//  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
//    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
//  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=64,dataBufferSize=64,rspBufferSize=64)
//  val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
//  val cpa = CoreParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)
  SpinalConfig().generateVerilog(bmb_dfi_ddr3(pl, config))
//  SpinalConfig().generateVerilog(dfi_ddr3())
}
object BmbCmd extends App{
  val pl:PhyLayout = PhyLayout(sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,sdramtime=SdramTiming(3)),
    phaseCount=1,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=pl.sdram.tPhyWrlat,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=pl.sdram.tRddataEn,tPhyRdlat=4,tPhyRdCsGap=3)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=1,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)

  SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)).generateVerilog(BmbCmd(bmbp,pl))

}
