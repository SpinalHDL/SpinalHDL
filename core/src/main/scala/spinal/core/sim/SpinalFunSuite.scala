package spinal.core.sim

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import org.scalatest.flatspec.AnyFlatSpec

abstract class SpinalFlatSpec[Dut <: Component] extends AnyFlatSpec with spinal.idslplugin.ValCallback {

  override def valCallback[T](ref: T, name: String): T = {
    ref match {
      case nameable: Nameable => nameable.setName(name)
      case _                  =>
    }
    ref
  }

  val config: SpinalSimConfig = SimConfig
  val caching: Boolean = true

  def bench(
      dut: => Dut,
      preSimHook: Dut => Unit = { _ => },
      config: SpinalSimConfig = config,
      caching: Boolean = caching
  ): Bench[Dut] = {
    benchTransform(dut, { dut => preSimHook(dut); dut }, config, caching)
  }

  def benchTransform[SoftDut](
      dut: => Dut,
      preSimTransform: Dut => SoftDut,
      config: SpinalSimConfig = config,
      caching: Boolean = caching
  ): Bench[SoftDut] = new Bench[SoftDut](dut, config, caching, preSimTransform)

  class Bench[SoftDut](
      dut: => Dut,
      config: SpinalSimConfig = SimConfig,
      caching: Boolean,
      preSimTransform: Dut => SoftDut
  ) extends Nameable {
    def buildBench: SimCompiled[Dut] = config.compile(dut)

    private var triedCompile: Boolean = false
    private var cachedBench: Option[SimCompiled[Dut]] = None

    def prepareCachedBench(): Unit = {
      if (caching && !triedCompile) {
        triedCompile = true
        getName should "compile" in {
          cachedBench = Some(buildBench)
        }
      }
    }

    case class should(doWhat: String) {
      def in(body: SoftDut => Unit): Unit = {
        prepareCachedBench()
        getName should doWhat in {
          val bench =
            if (caching) cachedBench getOrElse cancel
            else buildBench
          bench.doSim(s"$getName should $doWhat") { dut =>
            val softDut = preSimTransform(dut)
            body(softDut)
          }
        }
      }
    }
  }
}

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
