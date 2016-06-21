package spinal.tester.scalatest

import spinal.core._
import spinal.lib.cpu.riscv.impl.bench.CoreUut
import spinal.lib.cpu.riscv.impl.bench.CoreUut.TopLevel
import language.postfixOps

object RiscvTester {



}


class RiscvTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "RiscvTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/RiscvTester"
  override def createToplevel: Component = new CoreUut.TopLevel().setDefinitionName("RiscvTester")
}