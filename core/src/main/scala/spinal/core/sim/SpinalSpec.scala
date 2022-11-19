package spinal.core.sim

import org.scalatest.freespec.AnyFreeSpec
import spinal.core.{Component, Nameable}

/** Define unit / regression tests for a component */
abstract class SpinalSpec[Dut <: Component] extends AnyFreeSpec with spinal.idslplugin.ValCallback {

  override def valCallback[T](ref: T, name: String): T = {
    ref match {
      case nameable: Nameable => nameable.setName(name)
      case _                  =>
    }
    ref
  }

  /** Override it to define the default SpinalSimConfig (else defaults to SimConfig) */
  val config: SpinalSimConfig = SimConfig

  /** `override val caching = false` to disable caching by default */
  val caching: Boolean = true

  /** `override val seed = Some(value)` to choose a fixed seed */
  var seed: Option[Int] = None

  /** Define a protocol that can be run on several benches */
  case class Protocol[SoftDut](tests: Bench[SoftDut] => Unit) extends Nameable {

    /** Run the protocol on (a) named (in a single `val`) benche(s) */
    def runOn(benches: Bench[SoftDut]*): Unit = {
      getName - {
        for (bench <- benches) {
          bench.getName - {
            tests(bench)
          }
        }
      }
    }

    /** Run the protocol on a sequence of benches */
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

  /** Define a protocol that can be run with several component configurations
    *
    * {{{
    * val myProtocol =
    *  ProtocolCfg { (cfg: MyComponentConfig) =>
    *    bench(new MyComponent(cfg))
    *  } { (cfg, it) =>
    *    it should "do nothing" in { dut => }
    *  }
    * }}}
    *
    * @param benchBuilder the function to create a bench from the component configuration
    * @param tests tests to run, which can be conditional over the component configuration
    */
  case class ProtocolCfg[Cfg, SoftDut](benchBuilder: Cfg => Bench[SoftDut])(tests: (Cfg, Bench[SoftDut]) => Unit)
      extends Nameable {

    /** Override this function to define how configurations should appear in test results */
    def cfgToString(cfg: Cfg): String = cfg.toString()

    /** Run the protocol with (a) component configuration(s) */
    def runWith(cfgs: Cfg*): Unit = runWithAll(cfgs)

    /** Run the protocol with a sequnece of component configurations */
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

  /** Create a test-bench
    *
    * @param dut The device to test
    * @param preSimHook An action to do on `dut` before each test (defaults to nothing)
    * @param config To override `SpinalSpec.config`
    * @param seed To override `SpinalSpec.seed`
    * @param caching To override `SpinalSpec.caching`
    * @return
    */
  def bench(
      dut: => Dut,
      preSimHook: Dut => Unit = { _ => },
      config: SpinalSimConfig = config,
      seed: Option[Int] = seed,
      caching: Boolean = caching
  ): Bench[Dut] = {
    benchTransform(dut, { dut => preSimHook(dut); dut }, config, seed, caching)
  }

  /** Create a test-bench with an abstraction of the `dut`
    *
    * @param dut The device to test
    * @param preSimTransform Function taking the `dut` and returning your abstracted dut
    * @param config To override `SpinalSpec.config`
    * @param seed To override `SpinalSpec.seed`
    * @param caching To override `SpinalSpec.caching`
    * @return
    */
  def benchTransform[SoftDut](
      dut: => Dut,
      preSimTransform: Dut => SoftDut,
      config: SpinalSimConfig = config,
      seed: Option[Int] = seed,
      caching: Boolean = caching
  ): Bench[SoftDut] = new Bench[SoftDut](dut, preSimTransform, config, seed, caching)

  /** The type of `compile` */
  class Compile

  /** For `it should compile` or `it shouldNot compile` */
  object compile extends Compile

  /** A test bench
    *
    * @see [[bench]]
    * @see [[benchTransform]]
    */
  class Bench[SoftDut](
      dut: => Dut,
      preSimTransform: Dut => SoftDut,
      config: SpinalSimConfig = SimConfig,
      seed: Option[Int],
      caching: Boolean
  ) extends Nameable {

    /** A function doing actions on the (possibly transformed) `dut` */
    type Body = SoftDut => Unit

    /** Get a new compiled `dut` */
    def buildBench: SimCompiled[Dut] = config.compile(dut)

    /** Has this testbench already been compiled for caching? */
    private var triedCompile: Boolean = false

    /** The cached bench. Can be `None` if `!triedCompile` or if compile failed */
    private var cachedBench: Option[SimCompiled[Dut]] = None

    /** Set of functions to build text for test results */
    private object txt {
      def itCompiles: String = s"$getName compiles"
      def notCompiles: String = s"$getName does not compile"
      def itShould(doWhat: String): String = s"$getName should $doWhat"
      def test(doWhat: String): String = s"$getName test: $doWhat"
    }

    /** Compile `cachedBench` if caching is enabled and not compiled yet
      *
      * Use this function only before tests, NEVER in a test
      */
    protected def prepareCachedBench(): Unit = {
      if (caching && !triedCompile) {
        triedCompile = true
        txt.itCompiles in {
          cachedBench = Some(buildBench)
        }
      }
    }

    /** Function used to run a test/simulation
      *
      * @param name The name of the test/simulation
      * @param f doSim function, eg: `_.doSimUntilVoid(name)` or `dut => dut.doSim(name, seed)`
      * @param body the code to run on the (possibly abstracted) `dut` during the simulation
      */
    protected def sim(name: String, f: SimCompiled[Dut] => (Dut => Unit) => Unit)(
        body: Body
    ): Unit = {
      prepareCachedBench()
      name in {
        val bench =
          if (caching) cachedBench getOrElse cancel
          else buildBench
        f(bench) { dut =>
          val softDut = preSimTransform(dut)
          body(softDut)
        }
      }
    }

    /** Helper to call sim with optional seed */
    final protected def sim(name: String, seed: Option[Int]): Body => Unit = seed match {
      case Some(s) => sim(name, _.doSim(name, s))
      case None    => sim(name, _.doSim(name))
    }

    /** Helper to call sim with doSimUntilVoid and optional seed */
    final protected def simUntilVoid(name: String, seed: Option[Int]): Body => Unit = seed match {
      case Some(s) => sim(name, _.doSimUntilVoid(name, s))
      case None    => sim(name, _.doSimUntilVoid(name))
    }

    /** Context for `it should "do something"` */
    class Should(doWhat: String, seed: Option[Int] = None) {

      /** Specify a seed for this test */
      def withSeed(seed: Int): Should = new Should(doWhat, Some(seed))

      /** Specify a seed for this test, or to not fix one */
      def withSeed(seed: Option[Int]): Should = new Should(doWhat, seed)

      /** Run a simulation to check that it does the thing it should
        *
        * {{{
        * it should "do something" in { dut =>
        *   // code to check that it does the thing it should
        * }
        * }}}
        */
      def in: Body => Unit = sim(txt.itShould(doWhat), seed)

      /** Run a simulation to check that it does the thing it should
        *
        * {{{
        * it should "do something" untilVoid { dut =>
        *   // code to check that it does the thing it should
        * }
        * }}}
        */
      def untilVoid: Body => Unit = simUntilVoid(txt.itShould(doWhat), seed)
    }

    /** To build a [[Should]] context */
    def should(doWhat: String): Should = new Should(doWhat)

    /** `it should compile` runs a test to check that `it` compiles */
    def should(c: Compile): Unit = {
      if (caching) {
        if (triedCompile)
          txt.itCompiles in { assert(cachedBench.isDefined) }
        else
          prepareCachedBench()
      } else {
        txt.itCompiles in { buildBench }
      }
    }

    /** `it should compile` runs a test to check that `it` does not compile */
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

    /** Run a test using this testbench, see spinal.core.sim.SimCompiled.doSim */
    def test(doWhat: String, seed: Option[Int] = seed): Body => Unit =
      sim(txt.test(doWhat), seed)

    /** Run a test using this testbench, see spinal.core.sim.SimCompiled.doSim */
    def testUntilVoid(doWhat: String, seed: Option[Int] = seed): Body => Unit =
      simUntilVoid(txt.test(doWhat), seed)
  }
}

/** Take parts of this for RTD:
  *
  * Run once at the beginning of each test to provide an software abstraction on your Dut
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
