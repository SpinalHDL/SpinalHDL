package spinal.lib.bus.amba4.axi

import spinal.tester.SpinalTesterCocotbBase

class Axi4SharedOnChipRamTester extends SpinalTesterCocotbBase {
  override def getName = "Axi4SharedOnChipRamTester"
  override def pythonTestLocation = "tester/src/test/python/spinal/Axi4SharedOnChipRamTester"
  override def createToplevel = Axi4SharedOnChipRam(32, 4096, 4).setDefinitionName(getName)
  override def noVhdl = true
}
