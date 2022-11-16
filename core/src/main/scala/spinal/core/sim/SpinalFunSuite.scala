package spinal.core.sim

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._

abstract class SpinalFunSuiteTransform[Dut <: Component](dut: => Dut) extends AnyFunSuite {

  /** The type of the software abstraction of the Dut */
  type SoftDut

  val config: SpinalSimConfig = SimConfig

  lazy val bench: SimCompiled[Dut] = config.compile(dut)

  /** Run once at the beginning of each test to provide an software abstraction on your Dut
    *
    * Useful for instance to:
    *
    * - define buses with protocols
    * - define simulation timeout
    * - initialize inputs
    * - fork clock domain stumulus
    * - add other prehooks
    *
    * Example:
    *
    * {{{
    * def preSimTransform(bareDut: Dut): SoftDut = {
    *   // Build software abstraction to manage bus protocols
    *   val dut = IdentityDut(bareDut)
    *   // Define a simulation timeout for all tests
    *   SimTimeout(1000)
    *   // Initialize inputs
    *   dut.init()
    *   // Start actions after the first clock sampling
    *   dut.cd.waitSampling()
    *   // Return the abstracted component
    *   dut
    * }
    * }}}
    *
    * @param dut the Component
    * @return transformed component
    */
  def preSimTransform(dut: Dut): SoftDut

  final def test(name: String)(body: SoftDut => Unit) = super.test(name) {
    bench.doSim(name) { dut =>
      val softDut = preSimTransform(dut)
      body(softDut)
    }
  }
}

abstract class SpinalFunSuite[Dut <: Component](dut: => Dut) extends SpinalFunSuiteTransform(dut) {

  def preSimHook(dut: Dut): Unit = {}

  type SoftDut = Dut

  final def preSimTransform(dut: Dut): SoftDut = {
    preSimHook(dut)
    dut
  }
}
