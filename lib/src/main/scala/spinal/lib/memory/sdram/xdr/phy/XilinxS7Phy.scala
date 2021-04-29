package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.{BUFG, BUFIO, IDELAYCTRL, IDELAYE2, IOBUF, IOBUFDS, ISERDESE2, OBUFDS, ODELAYE2, OSERDESE2}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.xdr.{PhyLayout, SdramXdrIo, SdramXdrPhyCtrl}


object XilinxS7Phy{
  def phyLayout(sl : SdramLayout, clkRatio : Int) = PhyLayout(
    sdram = sl,
    phaseCount = clkRatio,
    dataRate = 2,
    outputLatency = 2,
    readDelay = 0,
    writeDelay = 0,
    cmdToDqDelayDelta = 1,
    transferPerBurst = Math.max(4, sl.generation.burstLength)
  )
}




case class XilinxS7Phy(sl : SdramLayout,
                       clkRatio : Int,
                       clk90 : ClockDomain,
                       serdesClk0 : ClockDomain,
                       serdesClk90 : ClockDomain) extends Component{
  val pl = XilinxS7Phy.phyLayout(sl, clkRatio)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val sdram = master(SdramXdrIo(sl))
  }

  assert(clkRatio == 2)
  val phaseCount = clkRatio

  val clk270Rst = ResetCtrl.asyncAssertSyncDeassert(
    input = ClockDomain.current.isResetActive,
    clockDomain = clk90.withRevertedClockEdge(),
    inputPolarity  = HIGH,
    outputPolarity = HIGH
  )
  val clk270 = clk90.withRevertedClockEdge().copy(reset = clk270Rst)

  val clk90Rst = ResetCtrl.asyncAssertSyncDeassert(
    input = clk270Rst,
    clockDomain = clk90,
    inputPolarity  = HIGH,
    outputPolarity = HIGH
  )

//  clk270.setSynchronousWith(ClockDomain.current)

  val idelayctrl = IDELAYCTRL()
  idelayctrl.REFCLK := serdesClk90.readClockWire  //TODO serdesClk90
  idelayctrl.RST := ClockDomain.isResetActive

  def valueToOutput(name : String, phase : Bool): Bool = seqToOutput(name, Seq.fill(4*2)(phase))._1
  def sdrToOutput(name : String, phases : Seq[Bool], outputEnable : Seq[Bool] = List.fill(phaseCount*2)(True)): Bool = seqToOutput(name, phases.map(p => Seq.fill(2)(p)).flatten, outputEnable)._1
  def ddrToOutput(name : String, phases : Seq[Seq[Bool]], outputEnable : Seq[Bool] = List.fill(phaseCount*2)(True)): Bool = seqToOutput(name, phases.flatten, outputEnable)._1
  def seqToOutput(name : String, seq : Seq[Bool], outputEnable : Seq[Bool] = List.fill(phaseCount*2)(True), phase90 : Boolean = false): (Bool, Bool) ={
    val serdes = OSERDESE2(
      DATA_RATE_OQ = "DDR",
      DATA_RATE_TQ = "DDR",
      DATA_WIDTH = phaseCount*2,
      SERDES_MODE = "MASTER",
      TRISTATE_WIDTH = phaseCount*2
    ).setName(s"${name}_OSERDESE2")

    for(bitId <- 0 until phaseCount*2){
      serdes.D(bitId) := seq(bitId)
    }
    for(bitId <- phaseCount*2 until 8){
      serdes.D(bitId) := False
    }

    phase90 match{
      case false =>
        serdes.CLK := serdesClk0.readClockWire
        serdes.CLKDIV := ClockDomain.current.readClockWire
        serdes.RST := ClockDomain.current.isResetActive
      case true =>
        serdes.CLK := serdesClk90.readClockWire
        serdes.CLKDIV := clk90.readClockWire
        serdes.RST := clk90Rst
    }


    for(i <- 0 to phaseCount*2-1) serdes.T(i) := !outputEnable(i)
    for(i <- phaseCount*2 to 3) serdes.T(i) := True
    serdes.TCE := True
    serdes.OCE := True
    serdes.TBYTEIN := True
    (serdes.OQ, serdes.TQ)
  }

  val clkBuf = OBUFDS()
  clkBuf.I := seqToOutput("CK", Seq.fill(phaseCount)(Seq(True, False)).flatten, phase90 = true)._1
  io.sdram.CK := clkBuf.O
  io.sdram.CKn := clkBuf.OB


  for(i <- 0 until sl.chipAddressWidth) io.sdram.ADDR(i) := valueToOutput("ADDR", RegNext(io.ctrl.ADDR(i)))
  for(i <- 0 until sl.bankWidth)        io.sdram.BA(i) := valueToOutput("BA", RegNext(io.ctrl.BA(i)))
  io.sdram.CASn := sdrToOutput("CASn", io.ctrl.phases.map(p => RegNext(p.CASn)))
  io.sdram.CKE  := sdrToOutput("CKE", io.ctrl.phases.map(p => RegNext(p.CKE)))
  io.sdram.CSn  := sdrToOutput("CSn", io.ctrl.phases.map(p => RegNext(p.CSn )))
  io.sdram.RASn := sdrToOutput("RASn", io.ctrl.phases.map(p => RegNext(p.RASn)))
  io.sdram.WEn  := sdrToOutput("WEn", io.ctrl.phases.map(p => RegNext(p.WEn )))
  if(sl.generation.RESETn) io.sdram.RESETn := sdrToOutput("RESETn", io.ctrl.phases.map(p => RegNext(p.RESETn)))
  io.sdram.ODT    := sdrToOutput("ODT", io.ctrl.phases.map(p => RegNext(p.ODT)))



  val idelayValueIn = in Bits(5 bits)

  val dqe0Reg = RegNext(io.ctrl.writeEnable) init(False)
  val dqe270Reg = clk270(RegNext(io.ctrl.writeEnable) init(False))
  val dqstReg = clk270(RegNext(Vec((io.ctrl.writeEnable ## dqe270Reg).mux(
    B"00" -> B"0000",
    B"10" -> B"0011",
    B"11" -> B"1111",
    B"01" -> B"1111"
  ).asBools.reverse)))
  dqstReg.foreach(_.init(False))

  val dqReg = io.ctrl.phases.map(p => RegNext(p.DQw))
  val dmReg = io.ctrl.phases.map(p => RegNext(p.DM))


  val dqs = for(i <- 0 until (sl.dataWidth+7)/8) yield new Area{
    val (serQ, serT) = seqToOutput("CK", Seq.fill(phaseCount)(Seq(True, False)).flatten, dqstReg, phase90 = true)

    val buf = IOBUFDS()
    buf.T := serT
    buf.I := serQ

    io.sdram.DQS(i) := buf.IO
    io.sdram.DQSn(i) := buf.IOB

    def clkin = serdesClk0.readClockWire
  }

  val dm = for(i <- 0 until (sl.dataWidth+7)/8) yield new Area{
    io.sdram.DM(i) := ddrToOutput("DM", dmReg.map(_.map(_(i))))
  }

  io.ctrl.readValid := io.ctrl.readEnable

  val dq = for(i <- 0 until sl.dataWidth) yield new Area{
    val buf = IOBUF()
    io.sdram.DQ(i) := buf.IO

    val (serQ, serT) = seqToOutput("DQ", dqReg.map(_.map(_(i))).flatten, List.fill(4)(dqe0Reg))

    buf.T := serT
    buf.I := serQ

    val idelay = IDELAYE2(
      HIGH_PERFORMANCE_MODE = true,
      IDELAY_TYPE = "VAR_LOAD",
      IDELAY_VALUE = 0,
      PIPE_SEL = false,
      REFCLK_FREQUENCY = ClockDomain.current.frequency.getValue.toDouble*clkRatio / 1e6,
      SIGNAL_PATTERN = "DATA"
    )


    idelay.C := serdesClk0.readClockWire
    idelay.IDATAIN := buf.O
    idelay.LDPIPEEN := False
    idelay.INC := True
    idelay.CNTVALUEIN := idelayValueIn
    idelay.CINVCTRL := False
    idelay.DATAIN := False
    idelay.REGRST := False

    val idelayLoad = in Bool()
    idelay.LD := idelayLoad
    idelay.CE := False

    val des = ISERDESE2(
      DATA_RATE = "DDR",
      DATA_WIDTH = phaseCount*2,
      INTERFACE_TYPE = "NETWORKING",
      IOBDELAY = "IFD"
    )


    val bitsleep = in Bool()
    des.BITSLIP := bitsleep
    des.CE1 := True
    des.CE2 := True
    des.CLKDIV := ClockDomain.current.readClockWire
    des.CLKDIVP := False
    des.D := False
    des.DDLY := idelay.DATAOUT
    des.DYNCLKDIVSEL := False
    des.DYNCLKSEL := False
    des.CLK := serdesClk0.readClockWire
    des.CLKB := !serdesClk0.readClockWire
    des.OCLK := False
    des.OCLKB := False
    des.OFB := False
    des.RST := ClockDomain.current.isResetActive
    des.SHIFTIN1 := False
    des.SHIFTIN2 := False

//    val DQrNew = B((0 until phaseCount*2).map(i => des.Q(phaseCount*2-1-i))) //.asBits.rotateLeft(bitslip.resize(widthOf(bitslip)-1))
//    val DQrLast = ClockDomain.current(RegNext(DQrNew))
//    val DQr = (DQrNew ## DQrLast) >> bitslip
//
//    for(phase <- 0 until phaseCount; ratio <- 0 until pl.dataRatio){
//      io.ctrl.phases(phase).DQr(ratio)(i) := DQr(phase*pl.dataRatio + ratio)
//    }
    for(phase <- 0 until phaseCount; ratio <- 0 until pl.dataRate){
      io.ctrl.phases(phase).DQr(ratio)(i) := des.Q((phaseCount*pl.dataRate-1) - (phase*pl.dataRate + ratio))
    }
  }


  def driveFrom(mapper: BusSlaveFactory): Unit = {
    mapper.drive(idelayValueIn, 0x00)
//    mapper.drive(Vec(dqs.map(_.idelayLoad)), 0x10)
    mapper.driveMultiWord(Vec(dq.map(_.idelayLoad)), 0x20)
//    mapper.drive(bitslip, 0x40)
    mapper.driveMultiWord(Vec(dq.map(_.bitsleep)), 0x40).foreach(_ := False)
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

