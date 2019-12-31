package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr.{Axi4SharedSdramCtrl, MT48LC16M16A2}


class Axi4SharedSdramCtrlTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "Axi4SharedSdramCtrlTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/Axi4SharedSdramCtrlTester"
  override def createToplevel: Component = {
    val device = MT48LC16M16A2
    Axi4SharedSdramCtrl(32,2,device.layout,device.timingGrade7.copy(tPOW = 5 us),CAS = 3).setDefinitionName(getName)
  }
  override def backendConfig(config: SpinalConfig): SpinalConfig = config.copy(defaultClockDomainFrequency = FixedFrequency(133 MHz))//.copy(dumpWave = null)
  override def noVhdl = true
}