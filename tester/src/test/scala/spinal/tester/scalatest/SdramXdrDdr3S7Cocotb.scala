//package spinal.tester.scalatest
//
//import spinal.core._
//import spinal.lib.bus.bmb._
//import spinal.lib.memory.sdram.sdr.{MT41K128M16JT, MT48LC16M16A2}
//import spinal.lib.memory.sdram.xdr.{BmbPortParameter, CoreParameter, CtrlParameter, CtrlWithPhy, CtrlWithoutPhy, mt41k128m16jt_model, mt48lc16m16a2_model}
//import spinal.lib._
//import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
//import spinal.lib.eda.bench.Rtl
//import spinal.lib.memory.sdram.xdr.phy.{SdrInferedPhy, XilinxS7Phy}
//
////CONFIGURE_OPTS="LDFLAGS=-m32 CFLAGS=-m32 --enable-shared" LD_LIBRARY_PATH=`pwd`/versions/2.7.13/lib:$LD_LIBRARY_PATH pyenv install 2.7.13 -vfk
//
//case class SdramXdrDdr3S7TesterCocotbTop() extends Component{
//
//  val clk0, clk90, rst0 = in Bool()
//  val serdesClk0, serdesClk90 = in Bool()
//
//
//  Clock.sync(clk0, clk90)
//  Clock.sync(clk0, serdesClk0)
//  Clock.sync(clk0, serdesClk90)
//
//
//  val system = new ClockingArea(ClockDomain(clk0, rst0)) {
//    //  val dq = Analog(Bits(16 bits))
//
//    val sl = MT41K128M16JT.layout
//    val cp = CtrlParameter(
//      core = CoreParameter(
//        portTockenMin = 4,
//        portTockenMax = 8,
//        timingWidth = 4,
//        refWidth = 16,
//        writeLatencies = List(3),
//        readLatencies = List(5)
//      ),
//      ports = Seq(
//        BmbPortParameter(
//          bmb = BmbParameter(
//            addressWidth = sl.byteAddressWidth,
//            dataWidth = 64,
//            lengthWidth = 4,
//            sourceWidth = 3,
//            contextWidth = 8
//          ),
//          cmdBufferSize = 16,
//          dataBufferSize = 16,
//          rspBufferSize = 16,
//          clockDomain = ClockDomain.current
//        )/*,
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
//      )
//    )
//
//    val io = new Bundle {
//      val ctrlApb = slave(Apb3(12, 32))
//      val phyApb = slave(Apb3(12, 32))
//      val ports = Vec(cp.ports.map(p => slave(Bmb(p.bmb))))
//    }
//
//    val phy = XilinxS7Phy(sl, clkRatio = 2, ClockDomain(clk90), ClockDomain(serdesClk0), ClockDomain(serdesClk90))
//    phy.driveFrom(Apb3SlaveFactory(io.phyApb))
//
//    phy.children.foreach {
//      case bb: BlackBox => bb.clearBlackBox()
//      case _ =>
//    }
//
//    val ctrl = new CtrlWithoutPhy(cp, phy.pl)
//    ctrl.io.bmb <> io.ports
//    ctrl.io.apb <> io.ctrlApb
//    ctrl.io.phy <> phy.io.ctrl
//
//
//    val sdram = mt41k128m16jt_model()
//    sdram.rst_n := phy.io.memory.RESETn
//    sdram.ck := phy.io.memory.CK
//    sdram.ck_n := phy.io.memory.CKn
//    sdram.cke := phy.io.memory.CKE
//    sdram.cs_n := phy.io.memory.CSn
//    sdram.ras_n := phy.io.memory.RASn
//    sdram.cas_n := phy.io.memory.CASn
//    sdram.we_n := phy.io.memory.WEn
//    sdram.odt := phy.io.memory.ODT
//    sdram.ba := phy.io.memory.BA
//    sdram.addr := phy.io.memory.ADDR
//
//    val dq = Analog(Bits(16 bits))
//    val dqs = Analog(Bits(2 bits))
//    val dqs_n = Analog(Bits(2 bits))
//    val dm_tdqs = Analog(Bits(2 bits))
//
//    dq := sdram.dq
//    dqs := sdram.dqs
//    dqs_n := sdram.dqs_n
//    dm_tdqs := sdram.dm_tdqs
//
//
//    dq := phy.io.memory.DQ
//    dqs := phy.io.memory.DQS
//    dqs_n := phy.io.memory.DQSn
//
//    //  dq := B(0, widthOf(sdram.dq) bits)
//    //  dqs := B(0, widthOf(sdram.dqs) bits)
//    //  dqs_n := B(0, widthOf(sdram.dqs_n) bits)
//    //  dm_tdqs := B(0, widthOf(sdram.dm_tdqs) bits)
//
//  }
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
