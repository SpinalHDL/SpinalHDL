package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.apb._
import spinal.lib.bus.regif.AccessType._

class RegIfReuseBlock extends Component {
  val io = new Bundle {
    val apb = slave(Apb4(Apb4Config(16, 32)))
  }

  val busif = BusInterface(io.apb, (0x000, 4 KiB), 0, regPre = "AP")

  (0 to 8).foreach{i =>
    val tagAddr = 0x20 + i *4*2
    busif.newBlockTagAt(tagAddr, s"r${i}")("Turbo")
    new Area {
      val Reg = busif.newRegAt(tagAddr, "reg0")(SymbolName(s"RegA"))
      val field0 = Reg.field(Bits(2 bits), RW, 0, doc = "inter Row number\n0:5,1:10,2:20,3:20other").asOutput()
      val field1 = Reg.field(Bits(2 bits), RW, 0, doc = "CP relation\n0: C=P-1\n1: C=p\n2: C=p+1").asOutput()
      val Reg2 = busif.newReg(doc = "Turbo CRC Poly")(SymbolName(s"RegB"))
      val crc_mode = Reg2.field(Bool(), RW, 1, doc = "0: CRC24; 1: CRC16").asOutput()
      val crc_poly = Reg2.field(Bits(24 bit), RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)").asOutput()
    }
    busif.resetBlockTag()
  }

  busif.accept(DocHtml("regif"))
  busif.accept(DocJson("regif"))
  busif.accept(DocRalf("regif"))
  busif.accept(DocCHeader("regif", ""))
  busif.accept(DocSVHeader("regif", ""))
  busif.accept(DocSystemRdl("regif"))
}

object RegIfReuseBlockTesterMain extends App{
  val sp = SpinalConfig()
    .copy(targetDirectory = "./out/regifreuse")
  sp.generateVerilog(new RegIfReuseBlock)
}
