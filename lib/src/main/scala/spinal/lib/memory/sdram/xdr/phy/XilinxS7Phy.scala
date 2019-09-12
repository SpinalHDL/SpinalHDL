package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.{IDELAYE2, IOBUFDS, ISERDESE2, OBUFDS, ODELAYE2, OSERDESE2}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.xdr.{Phy, PhyParameter, SdramXdrIo}


object XilinxS7Phy{
  def memoryLayoutToPhyLayout(sl : SdramLayout, clkRatio : Int) = PhyParameter(
    sdram = sl,
    phaseCount = clkRatio,
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



case class XilinxS7Phy(sl : SdramLayout, clkRatio : Int) extends Phy[XilinxS7PhyIo](XilinxS7Phy.memoryLayoutToPhyLayout(sl, clkRatio)){
  override def MemoryBus(): XilinxS7PhyIo = XilinxS7PhyIo(sl)

  assert(clkRatio == 2)
  val phaseCount = clkRatio

  def valueToOutput(name : String, phase : Bool): Bool = seqToOutput(name, Seq.fill(4*2)(phase))
  def sdrToOutput(name : String, phases : Seq[Bool], outputEnable : Seq[Bool] = List.fill(phaseCount)(True)): Bool = seqToOutput(name, phases.map(p => Seq.fill(2)(p)).flatten, outputEnable)
  def ddrToOutput(name : String, phases : Seq[Seq[Bool]], outputEnable : Seq[Bool] = List.fill(phaseCount)(True)): Bool = seqToOutput(name, phases.flatten, outputEnable)
  def seqToOutput(name : String, seq : Seq[Bool], outputEnable : Seq[Bool] = List.fill(phaseCount)(True)): Bool ={
    val serdes = OSERDESE2(
      DATA_RATE_OQ = "DDR",
      DATA_RATE_TQ = "DDR",
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

    for(i <- 0 to phaseCount-1) serdes.T(i) := !outputEnable(i)
    for(i <- phaseCount to 3) serdes.T(i) := True
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



  val idelayClear = in Bool()
  val dqs = for(i <- 0 until (sl.dataWidth+7)/8) yield new Area{
    val buf = IOBUFDS()
    buf.T := False
    buf.I := valueToOutput("DQS", False)

    io.memory.sdram.DQS(i) := buf.IO
    io.memory.sdram.DQSn(i) := buf.IOB

    val idelay = IDELAYE2(
      HIGH_PERFORMANCE_MODE = true,
      IDELAY_TYPE = "VARIABLE",
      IDELAY_VALUE = 0,
      PIPE_SEL = false,
      REFCLK_FREQUENCY = 300.0,
      SIGNAL_PATTERN = "DATA"
    )

    idelay.C := io.memory.serdesClk0 //TODO
    idelay.IDATAIN := buf.O
    idelay.LDPIPEEN := False
    idelay.INC := True
    idelay.CNTVALUEIN := 0
    idelay.CINVCTRL := False
    idelay.DATAIN := False
    idelay.REGRST := False

    val idelayInc = in Bool()
    idelay.LD := idelayClear
    idelay.CE := idelayInc
  }
  val dq = for(i <- 0 until sl.dataWidth) yield new Area{
    io.memory.sdram.DQ(i) := ddrToOutput("DQ", io.ctrl.phases.map(_.DQw.map(_(i))), Seq.fill(phaseCount)(False /*io.ctrl.DQe*/)) //TODO

    val idelay = IDELAYE2(
      HIGH_PERFORMANCE_MODE = true,
      IDELAY_TYPE = "VARIABLE",
      IDELAY_VALUE = 0,
      PIPE_SEL = false,
      REFCLK_FREQUENCY = 300.0,
      SIGNAL_PATTERN = "DATA"
    )


    idelay.C := io.memory.serdesClk0
    idelay.IDATAIN := io.memory.sdram.DQ(i)
    idelay.LDPIPEEN := False
    idelay.INC := True
    idelay.CNTVALUEIN := 0
    idelay.CINVCTRL := False
    idelay.DATAIN := False
    idelay.REGRST := False

    val idelayInc = in Bool()
    idelay.LD := idelayClear
    idelay.CE := idelayInc

    val des = ISERDESE2(
      DATA_RATE = "DDR",
      DATA_WIDTH = phaseCount*2,
      INTERFACE_TYPE = "MEMORY",
      IOBDELAY = "IFD"
    )



    des.BITSLIP := False
    des.CE1 := True
    des.CE2 := True
    des.CLK := dqs(i/8).idelay.DATAOUT
    des.CLKB := !dqs(i/8).idelay.DATAOUT
    des.CLKDIV := ClockDomain.current.readClockWire
    des.CLKDIVP := False
    des.D := False
    des.DDLY := idelay.DATAOUT
    des.DYNCLKDIVSEL := False
    des.DYNCLKSEL := False
    des.OCLK := io.memory.serdesClk0
    des.OCLKB := False
    des.OFB := False
    des.RST := ClockDomain.current.isResetActive
    des.SHIFTIN1 := False
    des.SHIFTIN2 := False


    Vec(io.ctrl.phases.flatMap(_.DQr.map(_(i)))).assignFromBits((0 to phaseCount*2).map(des.Q(_)).asBits())
  }


  override def driveFrom(mapper: BusSlaveFactory): Unit = {
    mapper.drive(idelayClear, 0x00, 0)
    mapper.drive(Vec(dqs.map(_.idelayInc)), 0x10, 0)
    mapper.driveMultiWord(Vec(dq.map(_.idelayInc)), 0x20)
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
      DATA_RATE_TQ = "DDR",
      DATA_WIDTH = 4,
      SERDES_MODE = "MASTER",
      TRISTATE_WIDTH = 4
    ).setName(s"${name}_OSERDESE2")

    for(bitId <- 0 to 3){
      serdes.D(bitId) := logic.counter(bitId)
    }
    for(bitId <- 4 to 7){
      serdes.D(bitId) := False
    }
    serdes.CLKDIV := pll.CLKOUT0
    serdes.CLK := pll.CLKOUT1


    for(i <- 0 to 3) serdes.T(i) := False
    serdes.TCE := True
    serdes.OCE := True
    serdes.TBYTEIN := True
    serdes.RST := ClockDomain.current.isResetActive

    val output = out Bool()

    output := serdes.OQ

  }.setDefinitionName("Test1"))
}