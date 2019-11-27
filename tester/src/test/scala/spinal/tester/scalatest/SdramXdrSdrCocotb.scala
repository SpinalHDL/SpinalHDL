//package spinal.tester.scalatest
//
//import spinal.core._
//import spinal.lib.bus.bmb._
//import spinal.lib.memory.sdram.sdr.MT48LC16M16A2
//import spinal.lib.memory.sdram.xdr.{BmbPortParameter, CoreParameter, CtrlWithPhy, CtrlParameter, mt48lc16m16a2_model}
//import spinal.lib._
//import spinal.lib.bus.amba3.apb.Apb3
//import spinal.lib.eda.bench.Rtl
//import spinal.lib.memory.sdram.xdr.phy.SdrInferedPhy
//
//case class SdramSdrTesterCocotbTop() extends Component{
//  val sl = MT48LC16M16A2.layout
//  val cp = CtrlParameter(
//    core = CoreParameter(
//      portTockenMin = 4,
//      portTockenMax = 8,
//      timingWidth = 4,
//      refWidth = 16,
//      writeLatencies = List(0),
//      readLatencies = List(2)
//    ),
//    ports = Seq(
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = sl.byteAddressWidth,
//          dataWidth = 16,
//          lengthWidth = 3,
//          sourceWidth = 3,
//          contextWidth = 8
//        ),
//        clockDomain = ClockDomain.current,
//        cmdBufferSize = 4,
//        dataBufferSize = 6,
//        rspBufferSize = 4
//      ),
//
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = sl.byteAddressWidth,
//          dataWidth = 16,
//          lengthWidth = 4,
//          sourceWidth = 5,
//          contextWidth = 12
//        ),
//        clockDomain = ClockDomain.current,
//        cmdBufferSize = 2,
//        dataBufferSize = 2,
//        rspBufferSize = 5
//      ),
//
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = sl.byteAddressWidth,
//          dataWidth = 16,
//          lengthWidth = 5,
//          sourceWidth = 6,
//          contextWidth = 16
//        ),
//        clockDomain = ClockDomain.current,
//        cmdBufferSize = 8,
//        dataBufferSize = 8,
//        rspBufferSize = 2
//      )
//    )
//  )
//
//  val io = new Bundle {
//    val apb = slave(Apb3(12, 32))
//    val ports = Vec(cp.ports.map(p => slave(Bmb(p.bmb))))
//  }
//
//  val dq = Analog(Bits(16 bits))
//
//  val ctrl = new CtrlWithPhy(cp, SdrInferedPhy(sl))
//  io.ports <> ctrl.io.bmb
//  io.apb <> ctrl.io.apb
//  when(ctrl.io.memory.DQ.writeEnable) {
//    dq := ctrl.io.memory.DQ.write
//  }
//  ctrl.io.memory.DQ.read := dq
//
//  val sdram = mt48lc16m16a2_model()
//  sdram.Addr  := ctrl.io.memory.ADDR
//  sdram.Ba    := ctrl.io.memory.BA
//  sdram.Clk   := ClockDomain.current.readClockWire
//  sdram.Cke   := ctrl.io.memory.CKE
//  sdram.Cs_n  := ctrl.io.memory.CSn
//  sdram.Ras_n := ctrl.io.memory.RASn
//  sdram.Cas_n := ctrl.io.memory.CASn
//  sdram.We_n  := ctrl.io.memory.WEn
//  sdram.Dqm   := ctrl.io.memory.DQM
//  dq := sdram.Dq
//
//}
//
//
////TODO REGRESSION
////class SdramSdrTesterCocotb extends SpinalTesterCocotbBase {
////  override def getName: String = "SdramSdrTesterCocotbTop"
////  override def pythonTestLocation: String = "tester/src/test/python/spinal/SdramXdr/SdrTester"
////  override def createToplevel: Component = {
////    SdramSdrTesterCocotbTop().setDefinitionName(getName)
////  }
////  override def noVhdl = true
////  withWaveform = true
////}
//
//import spinal.core._
//import spinal.lib.eda.bench._
//
////object SdramSdrSyntBench extends App{
////  val sl = MT48LC16M16A2.layout.copy(bankWidth = 3)
////  val cp = CtrlParameter(
////    core = CoreParameter(
////      portTockenMin = 4,
////      portTockenMax = 8,
////      rspFifoSize = 4,
////      timingWidth = 4,
////      refWidth = 16,
////      writeLatencies = List(0),
////      readLatencies = List(2)
////    ),
////    ports = Seq(
////      BmbPortParameter(
////        bmb = BmbParameter(
////          addressWidth = sl.byteAddressWidth,
////          dataWidth = 16,
////          lengthWidth = 3,
////          sourceWidth = 0,
////          contextWidth = 0
////        ),
////        clockDomain = ClockDomain.current,
////        cmdBufferSize = 4,
////        rspBufferSize = 4
////      ),
////
////      BmbPortParameter(
////        bmb = BmbParameter(
////          addressWidth = sl.byteAddressWidth,
////          dataWidth = 16,
////          lengthWidth = 4,
////          sourceWidth = 0,
////          contextWidth = 0
////        ),
////        clockDomain = ClockDomain.current,
////        cmdBufferSize = 2,
////        rspBufferSize = 5
////      )/*,
////
////      BmbPortParameter(
////        bmb = BmbParameter(
////          addressWidth = sl.byteAddressWidth,
////          dataWidth = 16,
////          lengthWidth = 5,
////          sourceWidth = 0,
////          contextWidth = 0
////        ),
////        clockDomain = ClockDomain.current,
////        cmdBufferSize = 8,
////        rspBufferSize = 2
////      )*//*,
////
////      BmbPortParameter(
////        bmb = BmbParameter(
////          addressWidth = sl.byteAddressWidth,
////          dataWidth = 16,
////          lengthWidth = 5,
////          sourceWidth = 0,
////          contextWidth = 0
////        ),
////        clockDomain = ClockDomain.current,
////        cmdBufferSize = 8,
////        rspBufferSize = 2
////      )*/
////    )
////  )
////
////
////  val ports4 = new Rtl {
////    override def getName(): String = "Port4"
////    override def getRtlPath(): String = "Port4.v"
////    SpinalVerilog({
////      val c = new CtrlWithPhy(cp, SdrInferedPhy(sl)).setDefinitionName(getRtlPath().split("\\.").head)
////      c
////    })
////  }
////
////
////  val rtls = List(ports4)
////
////  val targets = XilinxStdTargets(
////    vivadoArtix7Path = "/media/miaou/HD/linux/Xilinx/Vivado/2018.3/bin"
////  ) ++ AlteraStdTargets(
////    quartusCycloneIVPath = "/media/miaou/HD/linux/intelFPGA_lite/18.1/quartus/bin",
////    quartusCycloneVPath  = "/media/miaou/HD/linux/intelFPGA_lite/18.1/quartus/bin"
////  )
////
////  Bench(rtls, targets, "/media/miaou/HD/linux/tmp")
////
////
////
////
////}