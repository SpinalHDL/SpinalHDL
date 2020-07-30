package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim._
import spinal.lib.bus.mmslavefactory._
import spinal.lib.bus.mmslavefactory.generators._

class MMSlaveFactoryExample extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16,32)))
  }

  val slavFac = Apb3MMSlaveFactory(io.apb,(0x000,1 KiB), 0)

  val regVersion = slavFac.createReadOnlyReg("version", 0x0, "Version number")
  val versionMajor = regVersion.newField(4 bits, 0x1, "Major version")
  val versionMinor = regVersion.newField(8 bits, 0x23, "Minor version")

  val regAddr = slavFac.createReg("address", 0x4, "Destination address")
  val address = regAddr.newField(32 bits, 0, "Address")

  val irqStatus = slavFac.createClearReg("IRQ status", 0x8, "IRQ status and clear")
  val status = irqStatus.newField(4 bits, 0x3, "IRQ status")

  val regSecret = slavFac.createWriteOnlyReg("Secret", 0xc, "Secret data")
  val secret1 = regSecret.newField(16 bits, 0x0, "Secret 1")
  val secretFlow = regSecret.newFlowField(8 bits, 0x0, "Secret Flow")

  val regValue = slavFac.createReadOnlyReg("value", 0x10, "Values")
  val value1 = regValue.addField(secret1, 0x0, "Value 1")
  val value2 = regValue.addField(secretFlow.payload, 0x0, "Value 2")
}

object MMSlaveFactory {
  def main(args: Array[String]) {
    val report = SpinalVhdl(new MMSlaveFactoryExample)
    val toplevel = report.toplevel

    report.printPruned()

    toplevel.slavFac.accept(CHeaderGenerator("test_hw.h", "test"))
    toplevel.slavFac.accept(HtmlGenerator("test.html", "test"))
    toplevel.slavFac.accept(JsonGenerator("test.json"))

    SimConfig.withWave.doSim(new MMSlaveFactoryExample){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val apb = Apb3Driver(dut.io.apb, dut.clockDomain)

      dut.clockDomain.waitSampling(10)
      
      apb.read(0x0)

      dut.clockDomain.waitSampling(10)

      apb.write(0x4, 0xdeadbeefl)
      apb.read(0x4)

      dut.clockDomain.waitSampling(10)

      apb.read(0x8)
      apb.write(0x8, 0x5l)

      dut.clockDomain.waitSampling(10)

      apb.read(0x8)

      dut.clockDomain.waitSampling(10)
      
      apb.write(0xc, 0xal)
      apb.read(0x10)

      dut.clockDomain.waitSampling(10)
    }
  }
}