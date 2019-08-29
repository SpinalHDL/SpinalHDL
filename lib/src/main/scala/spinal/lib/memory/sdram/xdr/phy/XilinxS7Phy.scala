package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.{IOBUFDS, OBUFDS, ODELAYE2, OSERDESE2}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.xdr.{Phy, PhyParameter, SdramXdrIo}


object XilinxS7Phy{
  def memoryLayoutToPhyLayout(sl : SdramLayout) = PhyParameter(
    sdram = sl,
    phaseCount = 4,
    dataRatio = 2,
    outputLatency = 1,
    inputLatency = 1,
    burstLength = 8,
    CCD = 4
  )
}

case class XilinxS7PhyIo(sl : SdramLayout) extends Bundle with IMasterSlave{
  val serdesClk0 = Bool()
  val serdesClk90 = Bool()
  val sdram = SdramXdrIo(sl)

  override def asMaster(): Unit = {
    in(serdesClk0,serdesClk90)
    master(sdram)
  }
}



case class XilinxS7Phy(sl : SdramLayout, clkRatio : Int) extends Phy[XilinxS7PhyIo](XilinxS7Phy.memoryLayoutToPhyLayout(sl)){
  override def MemoryBus(): XilinxS7PhyIo = XilinxS7PhyIo(sl)
  override def driveFrom(mapper: BusSlaveFactory): Unit = {}

  val phaseCount = clkRatio

  def valueToOutput(name : String, phase : Bool): Bool = seqToOutput(name, Seq.fill(4*2)(phase))
  def sdrToOutput(name : String, phases : Seq[Bool], outputEnable : Bool = True): Bool = seqToOutput(name, phases.map(p => Seq.fill(2)(p)).flatten, outputEnable)
  def ddrToOutput(name : String, phases : Seq[Seq[Bool]], outputEnable : Bool = True): Bool = seqToOutput(name, phases.flatten, outputEnable)
  def seqToOutput(name : String, seq : Seq[Bool], outputEnable : Bool = True): Bool ={
    val serdes = OSERDESE2(
      DATA_RATE_OQ = "DDR",
      DATA_RATE_TQ = "SDR",
      DATA_WIDTH = phaseCount*2,
      SERDES_MODE = "MASTER",
      TRISTATE_WIDTH = 1
    ).setName(s"${name}_OSERDESE2")

    for(bitId <- 0 until phaseCount*2){
      serdes.D(bitId) := seq(bitId)
    }
    for(bitId <- phaseCount*2 until 8){
      serdes.D(bitId) := False
    }
    serdes.CLK := io.memory.serdesClk0
    serdes.CLKDIV := ClockDomain.current.readClockWire

    serdes.T1 := !outputEnable
    for(i <- 1 until 4) serdes.T(i) := True
    serdes.TCE := True
    serdes.OCE := True
    serdes.TBYTEIN := True
    serdes.RST := ClockDomain.current.isResetActive
    serdes.OQ



//    val delay = ODELAYE2(
//      ODELAY_TYPE = "VARIABLE",
//      HIGH_PERFORMANCE_MODE = true,
//      SIGNAL_PATTERN = "DATA",
//      REFCLK_FREQUENCY = ClockDomain.current.frequency.getValue.toDouble/1e6,
//      CINVCTRL_SEL = false,
//      PIPE_SEL = false,
//      DELAY_SRC = "ODATAIN"
//    )
//    delay.C := ClockDomain.current.readClockWire
//    delay.REGRST := False //TODO
//    delay.CE := False //TODO
//    delay.INC := True
//    delay.LDPIPEEN := False
//    delay.ODATAIN := serdes.OQ
//    delay.DATAOUT
  }

  val clkBuf = OBUFDS()
  clkBuf.I := ddrToOutput("CK", Seq.fill(phaseCount)(Seq(False, True)))
  io.memory.sdram.CK := clkBuf.O
  io.memory.sdram.CKn := clkBuf.OB


  for(i <- 0 until sl.chipAddressWidth) io.memory.sdram.ADDR(i) := valueToOutput("ADDR", io.ctrl.ADDR(i))
  for(i <- 0 until sl.bankWidth)        io.memory.sdram.BA(i) := valueToOutput("BA", io.ctrl.BA(i))
  io.memory.sdram.CASn := sdrToOutput("CASn", io.ctrl.phases.map(_.CASn))
  io.memory.sdram.CKE  := sdrToOutput("CKE", io.ctrl.phases.map(_.CKE))
  io.memory.sdram.CSn  := sdrToOutput("CSn", io.ctrl.phases.map(_.CSn ))
  io.memory.sdram.RASn := sdrToOutput("RASn", io.ctrl.phases.map(_.RASn))
  io.memory.sdram.WEn  := sdrToOutput("WEn", io.ctrl.phases.map(_.WEn ))
  io.memory.sdram.RESETn := sdrToOutput("RESETn", io.ctrl.phases.map(_.RESETn))
  io.memory.sdram.ODT    := sdrToOutput("ODT", io.ctrl.phases.map(_.ODT ))


  for(i <- 0 until sl.dataWidth) io.memory.sdram.DQ(i) := ddrToOutput("DQ", io.ctrl.phases.map(_.DQw.map(_(i))))
  io.ctrl.phases.foreach(_.DQr.foreach(_ := 0))

  for(i <- 0 until sl.dataWidth/8) {
    val buf = IOBUFDS()
    buf.T := False
    buf.I := valueToOutput("DQS", False)
    io.memory.sdram.DQS(i) := buf.IO
    io.memory.sdram.DQSn(i) := buf.IOB
  }
}


case class PLLE2_ADV() extends BlackBox{
  addGeneric("CLKIN1_PERIOD", 10.0 )
  addGeneric("CLKFBOUT_MULT", 8 )
  addGeneric("CLKOUT0_DIVIDE", 4 )
  addGeneric("CLKOUT0_PHASE", 0 )
  addGeneric("CLKOUT1_DIVIDE", 2 )
  addGeneric("CLKOUT1_PHASE", 90 )

  val CLKFBIN = in Bool()
  val CLKIN1 = in Bool()
  val CLKFBOUT = out Bool()
  val CLKOUT0  = out Bool()
  val CLKOUT1  = out Bool()
}




object SerdesTest extends App{
  SpinalVerilog(new Component{
    val clk = in Bool()
    val pll = PLLE2_ADV()
    pll.CLKFBIN := pll.CLKFBOUT
    pll.CLKIN1 := clk

    val logic = new ClockingArea(ClockDomain(pll.CLKOUT0)){
      val counter = Reg(UInt(8 bits))
      counter := counter + 1
    }

    val serdes = OSERDESE2(
      DATA_RATE_OQ = "DDR",
      DATA_RATE_TQ = "SDR",
      DATA_WIDTH = 8,
      SERDES_MODE = "MASTER",
      TRISTATE_WIDTH = 1
    ).setName(s"${name}_OSERDESE2")

    for(bitId <- 0 until 8){
      serdes.D(bitId) := logic.counter(bitId)
    }
    serdes.CLKDIV := pll.CLKOUT0
    serdes.CLK := pll.CLKOUT1

    serdes.T1 := False
    for(i <- 1 until 4) serdes.T(i) := True
    serdes.TCE := True
    serdes.OCE := True
    serdes.TBYTEIN := True
    serdes.RST := ClockDomain.current.isResetActive

    val output = out Bool()

    output := serdes.OQ

  }.setDefinitionName("Test1"))
}