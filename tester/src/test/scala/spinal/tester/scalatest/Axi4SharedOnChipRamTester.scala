package spinal.tester.scalatest


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


class Axi4SharedOnChipRamTester extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4SharedOnChipRamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4SharedOnChipRamTester"
  override def createToplevel: Component = Axi4SharedOnChipRam(Axi4Config(12,32,4,useLock = false),4096).setDefinitionName(getName)
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
}