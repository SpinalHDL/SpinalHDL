package spinal.core.sim

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.freespec.AnyFreeSpec

import spinal.core._

abstract class SpinalFreeSpec[Dut <: Component] extends AnyFreeSpec with spinal.idslplugin.ValCallback {

  override def valCallback[T](ref: T, name: String): T = {
    ref match {
      case nameable: Nameable => nameable.setName(name)
      case _                  =>
    }
    ref
  }

  val config: SpinalSimConfig = SimConfig
  val caching: Boolean = true

  case class Protocol[SoftDut](tests: Bench[SoftDut] => Unit) extends Nameable {
    def runOn(benches: Bench[SoftDut]*): Unit = {
      getName - {
        for (bench <- benches) {
          bench.getName - {
            tests(bench)
          }
        }
      }
    }

    def runOnAll(namedBenches: Seq[(Bench[SoftDut], String)]): Unit = {
      getName - {
        for ((bench, name) <- namedBenches) {
          name - {
            tests(bench)
          }
        }
      }
    }
  }

  case class ProtocolCfg[Cfg, SoftDut](benchBuilder: Cfg => Bench[SoftDut])(tests: (Cfg, Bench[SoftDut]) => Unit)
      extends Nameable {

    /** The goal of this function is to be overriden */
    def cfgToString(cfg: Cfg): String = cfg.toString()

    def runWith(cfgs: Cfg*): Unit = runWithAll(cfgs)

    def runWithAll(cfgs: Seq[Cfg]): Unit = {
      getName - {
        for (cfg <- cfgs) {
          cfgToString(cfg) - {
            tests(cfg, benchBuilder(cfg))
          }
        }
      }
    }
  }

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

  class Compile
  object compile extends Compile

  class Bench[SoftDut](
      dut: => Dut,
      config: SpinalSimConfig = SimConfig,
      caching: Boolean,
      preSimTransform: Dut => SoftDut
  ) extends Nameable {

    type Body = SoftDut => Unit

    def buildBench: SimCompiled[Dut] = config.compile(dut)

    private var triedCompile: Boolean = false
    private var cachedBench: Option[SimCompiled[Dut]] = None

    private object txt {
      def itCompiles: String = s"$getName compiles"
      def notCompiles: String = s"$getName does not compile"
      def itShould(doWhat: String): String = s"$getName should $doWhat"
      def test(doWhat: String): String = s"$getName test: $doWhat"
    }

    def prepareCachedBench(): Unit = {
      if (caching && !triedCompile) {
        triedCompile = true
        txt.itCompiles in {
          cachedBench = Some(buildBench)
        }
      }
    }

    protected def sim(name: String, f: SimCompiled[Dut] => String => (Dut => Unit) => Unit = _.doSim)(
        body: Body
    ): Unit = {
      prepareCachedBench()
      name in {
        val bench =
          if (caching) cachedBench getOrElse cancel
          else buildBench
        f(bench)(name) { dut =>
          val softDut = preSimTransform(dut)
          body(softDut)
        }
      }
    }

    class Should(doWhat: String) {
      def in(body: SoftDut => Unit): Unit = sim(txt.itShould(doWhat))(body)
      def untilVoid(body: SoftDut => Unit): Unit = sim(txt.itShould(doWhat), _.doSimUntilVoid)(body)
    }

    def should(doWhat: String): Should = new Should(doWhat)

    def should(c: Compile): Unit = {
      if (caching) prepareCachedBench()
      else txt.itCompiles in { buildBench }
    }

    def shouldNot(c: Compile): Unit = {
      txt.notCompiles in {
        if (caching && triedCompile) {
          assert(cachedBench.isEmpty)
        } else {
          var failed = false
          try { buildBench }
          catch { case _: Throwable => failed = true }
          assert(failed)
        }
      }
    }

    def test(doWhat: String)(body: SoftDut => Unit): Unit =
      sim(txt.test(doWhat))(body)
    def testUntilVoid(doWhat: String)(body: SoftDut => Unit): Unit =
      sim(txt.test(doWhat), _.doSimUntilVoid)(body)

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
