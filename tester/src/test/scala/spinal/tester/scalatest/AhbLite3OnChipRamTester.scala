package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._



object AhbLite3OnChipRamTester{
  class AhbLite3OnChipRamTester extends Component {
    val ahbConfig = AhbLite3Config(addressWidth = 10,dataWidth = 32)
    val ahb = slave(AhbLite3(ahbConfig))
    val AhbRam = AhbLite3OnChipRam(ahbConfig, byteCount= 1 << ahbConfig.addressWidth)
    AhbRam.io.ahb <> ahb
  }
}

class AhbLite3OnChipRamTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "AhbLite3OnChipRamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/AhbLite3OnChipRamTester"
  override def createToplevel: Component = new AhbLite3OnChipRamTester.AhbLite3OnChipRamTester
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
  override def noVhdl = true
}