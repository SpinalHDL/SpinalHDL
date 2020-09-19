package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim._
import spinal.lib.bus.mmslavefactory._
import spinal.lib.bus.mmslavefactory.generators._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver

class Apb3MMSlaveFactoryExample extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16,32)))
  }
  
  val slavFac = Apb3MMSlaveFactory(io.apb,(0x000,1 KiB), 0)
  
  val dummy = Reg(Bool) init(false)
  val irq = slavFac.createIrqRegs("irq", dummy)

  val regVersion = slavFac.createReadOnlyReg("version", "Version number")
  val versionMajor = regVersion.newField(4 bits, 0x1, "Major version")
  val versionMinor = regVersion.newField(8 bits, 0x23, "Minor version")

  val regAddr = slavFac.createReg("address", "Destination address")
  val address = regAddr.newField(32 bits, 0, "Address")

  val regSecret = slavFac.createWriteOnlyReg("Secret", "Secret data")
  val secret1 = regSecret.newField(16 bits, 0x0, "Secret 1")

  val regValue = slavFac.createReadOnlyReg("value", "Values")
  val value1 = regValue.addField(secret1, 0x0, "Value 1")
}

class AxiLite4MMSlaveFactoryExample extends Component {
  val io = new Bundle {
    val axilite = slave(AxiLite4(AxiLite4Config(16,32)))
  }
  
  val slavFac = AxiLite4MMSlaveFactory(io.axilite,(0x000,1 KiB), 0)

  val regVersion = slavFac.createReadOnlyReg("version", "Version number")
  val versionMajor = regVersion.newField(4 bits, 0x1, "Major version")
  val versionMinor = regVersion.newField(8 bits, 0x23, "Minor version")

  val regAddr = slavFac.createReg("address", "Destination address")
  val address = regAddr.newField(32 bits, 0, "Address")

  val regSecret = slavFac.createWriteStream("Secret", "Secret data")
  val secretFlow = regSecret.newStreamField(8 bits, 0x0, "Secret Flow")

  secretFlow.ready := True
}

object MMSlaveFactory {
  def main(args: Array[String]) {
    val report = SpinalVhdl(new Apb3MMSlaveFactoryExample)
    val toplevel = report.toplevel

    report.printPruned()

    toplevel.slavFac.accept(CHeaderGenerator("test_hw.h", "test"))
    toplevel.slavFac.accept(HtmlGenerator("test.html", "test"))
    toplevel.slavFac.accept(JsonGenerator("test.json"))

    //SimConfig.withWave.doSim(new Apb3MMSlaveFactoryExample){dut =>
    //  //Fork a process to generate the reset and the clock on the dut
    //  dut.clockDomain.forkStimulus(period = 10)
//
    //  val apb = Apb3Driver(dut.io.apb, dut.clockDomain)
//
    //  dut.clockDomain.waitSampling(10)
    //  
    //  apb.read(0x4)
//
    //  dut.clockDomain.waitSampling(10)
//
    //  apb.write(0x8, 0xdeadbeefl)
    //  apb.read(0x8)
//
    //  dut.clockDomain.waitSampling(10)
//
    //  apb.read(0xc)
    //  apb.write(0xc, 0x5l)
//
    //  dut.clockDomain.waitSampling(10)
//
    //  apb.read(0xc)
//
    //  dut.clockDomain.waitSampling(10)
    //  
    //  apb.write(0x10, 0xal)
    //  apb.read(0x14)
//
    //  dut.clockDomain.waitSampling(10)
    //}

    SimConfig.withWave.doSim(new AxiLite4MMSlaveFactoryExample){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val axi = AxiLite4Driver(dut.io.axilite, dut.clockDomain)
      
      axi.reset()

      dut.clockDomain.waitSampling(10)
      
      axi.read(0x0)

      dut.clockDomain.waitSampling(10)

      axi.write(0x4, 0xdead)

      dut.clockDomain.waitSampling(10)
      
      axi.read(0x4)

      dut.clockDomain.waitSampling(10)
    }
  }
}