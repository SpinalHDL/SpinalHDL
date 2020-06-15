package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.mmslavefactory._
import spinal.lib.bus.mmslavefactory.generators._

class MMSlaveFactoryExample extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16,32)))
  }

  val slavFac = Apb3MMSlaveFactory(io.apb,(0x000,1 KiB), 0)

  val regVersion = slavFac.creatReg("version", 0x0, "Version number")
  val versionMajor = regVersion.field(4 bits, AccessType.RO, 0x1, "Major version")
  val versionMinor = regVersion.field(8 bits, AccessType.RO, 0x23, "Minor version")

  versionMajor := 0x1l
  versionMinor := 0x23l
}

object MMSlaveFactory {
  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new MMSlaveFactoryExample).toplevel
    toplevel.slavFac.accept(CHeaderGenerator("test_hw.h", "test"))
    toplevel.slavFac.accept(HtmlGenerator("test.html", "test"))
    toplevel.slavFac.accept(JsonGenerator("test.json"))
  }
}