package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

class RegIfGrpTester extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16, 32)))
    val int_event = new Bundle {
      val a, b, c, d, e = in Bool()
      val f, g, h, k, i = in Bool()
      val mebit = in Bits(3 bit)
    }
    val int_level = new Bundle {
      val a, b, c, d, e = in Bool()
      val f, g, h, k = in Bool()
      val int0, int1, int2 = in Bool()
      val int4, int5, int6, int7 = in Bool()
      val mbit = in Bits(5 bit)
    }
    val dpu_intr, vdec_intr = out Bool()
    val top_intr = out Bool()
  }

  val busif = BusInterface(io.apb, (0x000, 4 KiB), 0, regPre = "AP")

  val VERSION = busif.newReg("Core version ")
  val major = VERSION.field(Bits(32 bit), AccessType.ROV, 0x20240f01, doc = "Major version")

  val CRG0 = busif.newRegSC("crg ")
  val sys0_cgen = CRG0.field(Bool(), 1, doc = "sys0 clock gate enable")
  val sys1_cgen = CRG0.field(Bool(), 1, doc = "sys0 clock gate enable")
  val sys2_cgen = CRG0.field(Bool(), 1, doc = "sys0 clock gate enable")
  val npu_cgen  = CRG0.fieldAt(16, Bits(4 bit), 0x5, doc = "sys0 clock gate enable")

  val CRG2 = busif.newRegSCR("crg ")
  val dpu_cgen  = CRG2.field(Bits(3 bit), 0x7, doc = "dup system clock gate enables")
  val dec_cgen  = CRG2.fieldAt(16, Bits(4 bit), 0x5, doc = "dec cgen clock gate enable")

  val DPU_INTR = busif.newIntrMMS3(0x100, "DPU sys level Interrupt merge")
  val ip0 = DPU_INTR.field(io.int_level.a, maskRstVal = 0x1, doc = "ip0 interrupt")
  val ip1 = DPU_INTR.field(io.int_level.b, maskRstVal = 0x1, doc = "ip1 interrupt")
  val ip2 = DPU_INTR.field(io.int_level.c, maskRstVal = 0x1, doc = "ip2 interrupt")
  val ip3 = DPU_INTR.field(io.int_level.d, maskRstVal = 0x1, doc = "ip3 interrupt")
  val ip8 = DPU_INTR.fieldAt(16, io.int_level.e, maskRstVal = 0x1, doc = "ip4 interrupt")
  io.dpu_intr :=  DPU_INTR.intr()

  val DEC_INTR = busif.newIntrMS2("DEC sys Level Interrupt merge")
  val dec_ip0 = DEC_INTR.field(io.int_level.a, maskRstVal = 0x1, doc = "ip3 interrupt")
  val dec_ip2 = DEC_INTR.fieldAt(16, io.int_level.b, maskRstVal = 0x1, doc = "ip4 interrupt")
  io.vdec_intr :=  DEC_INTR.intr()

  val GPS_INTR = busif.newIntrRFMS4("gps")
  val gps_ip0 = GPS_INTR.field(io.int_event.a, maskRstVal = 0x1, doc = "gps interrupt")
  val gps_ip1 = GPS_INTR.fieldAt(4, io.int_event.b, maskRstVal = 0x1, doc = "gps interrupt")
  val gps_ip2 = GPS_INTR.field(io.int_event.c, maskRstVal = 0x1, doc = "gps interrupt")
  val gps_ip3 = GPS_INTR.field(io.int_event.mebit, maskRstVal = 0x5, doc = "mebit gps interrupt")
  val gps_intr: Bool = GPS_INTR.intr()

  val BD_INTR = busif.newIntrRFMMS5("gps")
  val bd_ip0 = BD_INTR.field(io.int_event.e, maskRstVal = 0x1, doc = "bd interrupt")
  val bd_ip1 = BD_INTR.fieldAt(4, io.int_event.d, maskRstVal = 0x2, doc = "bd interrupt")
  val bd_ip2 = BD_INTR.field(io.int_event.f, maskRstVal = 0x1, doc = "bd interrupt")

  val GPU_INTR = busif.newIntrOMMS4("gpu")
  val gpu_ip0 = GPU_INTR.field(io.int_level.int0, maskRstVal = 0x1, doc = "gpu interrupt")
  val gpu_ip1 = GPU_INTR.field(io.int_level.int1, maskRstVal = 0x1, doc = "gpu interrupt")
  val gpu_ip2 = GPU_INTR.field(io.int_level.int2, maskRstVal = 0x1, doc = "gpu interrupt")

  val CORE0_INTR = busif.newIntrOMS3("core0")
  val core0_ip0 = CORE0_INTR.field(io.int_level.int4, maskRstVal = 0x1, doc = "core0 interrupt")
  val core0_ip1 = CORE0_INTR.field(io.int_level.int5, maskRstVal = 0x1, doc = "core0 interrupt")
  val core0_ip2 = CORE0_INTR.field(io.int_level.int6, maskRstVal = 0x1, doc = "core0 interrupt")
  val core0_ip3 = CORE0_INTR.field(io.int_level.int7, maskRstVal = 0x1, doc = "core0 interrupt")

  val TOP_INTR = busif.newIntrS1("top intrrupt merge")
  TOP_INTR.field(gps_intr, doc = "gpu interrupt")
  TOP_INTR.field(CORE0_INTR.intr(), doc = "core0 interrupt")
  TOP_INTR.field(GPU_INTR.intr(), doc = "gpu interrupt")
  TOP_INTR.field(io.int_level.mbit, doc = "multibit")
  TOP_INTR.field(BD_INTR.intr(), doc = "gpu interrupt")
  io.top_intr := TOP_INTR.intr()

  busif.accept(DocHtml("regif"))
  busif.accept(DocJson("regif"))
  busif.accept(DocRalf("regif"))
  busif.accept(DocCHeader("regif", ""))
  busif.accept(DocSystemRdl("regif"))
}

object RegIfGrpTesterMain extends App{
  val sp = SpinalConfig()
    .copy(targetDirectory = "./out/regifGrp")
  sp.generateVerilog(new RegIfGrpTester)
}
