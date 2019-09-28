//package spinal.tester.scalatest
//
//import spinal.core._
//import spinal.lib.bus.bmb._
//import spinal.lib.memory.sdram.sdr.{MT41K128M16JT, MT48LC16M16A2}
//import spinal.lib.memory.sdram.xdr.{BmbPortParameter, CoreParameter, CtrlWithPhy, CtrlParameter, mt41k128m16jt_model, mt48lc16m16a2_model}
//import spinal.lib._
//import spinal.lib.bus.amba3.apb.Apb3
//import spinal.lib.eda.bench.Rtl
//import spinal.lib.memory.sdram.xdr.phy.{SdrInferedPhy, XilinxS7Phy}
//
////CONFIGURE_OPTS="LDFLAGS=-m32 CFLAGS=-m32 --enable-shared" LD_LIBRARY_PATH=`pwd`/versions/2.7.13/lib:$LD_LIBRARY_PATH pyenv install 2.7.13 -vfk
//
//case class SdramXdrDdr3S7TesterCocotbTop() extends Component{
//  val sl = MT41K128M16JT.layout
//  val cp = CtrlParameter(
//    core = CoreParameter(
//      portTockenMin = 4,
//      portTockenMax = 8,
//      rspFifoSize = 4,
//      timingWidth = 4,
//      refWidth = 16,
//      writeLatencies = List(0),
//      readLatencies = List(2)
//    ),
//    ports = Seq(
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = sl.byteAddressWidth,
//          dataWidth = 64,
//          lengthWidth = 4,
//          sourceWidth = 3,
//          contextWidth = 8
//        ),
//        cmdBufferSize = 4,
//        rspBufferSize = 4
//      )/*,
//
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = sl.byteAddressWidth,
//          dataWidth = 16,
//          lengthWidth = 4,
//          sourceWidth = 5,
//          contextWidth = 12
//        ),
//        cmdBufferSize = 2,
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
//        cmdBufferSize = 8,
//        rspBufferSize = 2
//      )*/
//    )
//  )
//
//  val io = new Bundle {
//    val apb = slave(Apb3(12, 32))
//    val ports = Vec(cp.ports.map(p => slave(Bmb(p.bmb))))
//    val serdesClk0, serdesClk90 = in Bool()
//
//  }
//
//
////  val dq = Analog(Bits(16 bits))
//
//  val ctrl = (new CtrlWithPhy(cp, XilinxS7Phy(sl, clkRatio = 2, ClockDomain(io.serdesClk0))))
//  io.ports <> ctrl.io.bmb
//  io.apb <> ctrl.io.apb
//
//  val sdram = mt41k128m16jt_model()
//  sdram.rst_n := ctrl.io.memory.RESETn
//  sdram.ck  := ctrl.io.memory.CK
//  sdram.ck_n  := ctrl.io.memory.CKn
//  sdram.cke  := ctrl.io.memory.CKE
//  sdram.cs_n  := ctrl.io.memory.CSn
//  sdram.ras_n  := ctrl.io.memory.RASn
//  sdram.cas_n  := ctrl.io.memory.CASn
//  sdram.we_n  := ctrl.io.memory.WEn
//  sdram.odt  := ctrl.io.memory.ODT
//  sdram.ba  := ctrl.io.memory.BA
//  sdram.addr  := ctrl.io.memory.ADDR
//
//  val dq = Analog(Bits(16 bits))
//  val dqs = Analog(Bits(2 bits))
//  val dqs_n = Analog(Bits(2 bits))
//  val dm_tdqs = Analog(Bits(2 bits))
//
//  dq := sdram.dq
//  dqs := sdram.dqs
//  dqs_n := sdram.dqs_n
//  dm_tdqs := sdram.dm_tdqs
//
//
//  dq  := ctrl.io.memory.DQ
//  dqs  := ctrl.io.memory.DQS
//  dqs_n  := ctrl.io.memory.DQSn
//
////  dq := B(0, widthOf(sdram.dq) bits)
////  dqs := B(0, widthOf(sdram.dqs) bits)
////  dqs_n := B(0, widthOf(sdram.dqs_n) bits)
////  dm_tdqs := B(0, widthOf(sdram.dm_tdqs) bits)
//
//
//}
//
//
//class SdramXdrDdr3S7TesterCocotb extends SpinalTesterCocotbBase {
//  override def getName: String = "SdramXdrDdr3S7TesterCocotbTop"
//  override def pythonTestLocation: String = "tester/src/test/python/spinal/SdramXdr/Ddr3S7Tester"
//  override def createToplevel: Component = {
//    SdramXdrDdr3S7TesterCocotbTop().setDefinitionName(getName)
//  }
//  override def noVhdl = true
//  withWaveform = true
//}
