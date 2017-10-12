package spinal.tester.scalatest


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


class Axi4SharedOnChipRamTester extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4SharedOnChipRamTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4SharedOnChipRamTester"
  override def createToplevel: Component = Axi4SharedOnChipRam(32,4096,4).setDefinitionName(getName)
  override def noVhdl = true
}