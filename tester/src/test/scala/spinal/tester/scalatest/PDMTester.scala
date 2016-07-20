package spinal.tester.scalatest 

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pdm._

class PDMTester extends Component {
  val io = new Bundle {
    val enable = in Bool
    val density = in UInt(9 bits)
    val output = out Bool
  }
  val PDMC = new PDMCore(8)
  PDMC.io.enable := io.enable
  PDMC.io.density := io.density
  io.output := PDMC.io.output 
}

class PDMTesterCocotb extends SpinalTesterCocotbBase {
  override def getName: String = "PDMTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/PDMTester"
  override def createToplevel: Component = new PDMTester
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}
