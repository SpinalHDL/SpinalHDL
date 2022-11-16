package spinal.core.sim

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.{BeforeAndAfterAllConfigMap, ConfigMap}
import spinal.idslplugin.ValCallback

import spinal.core.{Component, Nameable}

/** Something with the name from another thing */
trait InheritedNameable {
  def getName(): String
  def setWeakName(name: String): this.type
}

/** Define unit / regression tests for a component */
abstract class SpinalTestBench extends AnyFreeSpec with ValCallback with BeforeAndAfterAllConfigMap {

  override def valCallback[T](ref: T, name: String): T = {
    ref match {
      case n: Nameable if n.isUnnamed => n.setName(name)
      case n: InheritedNameable       => n.setWeakName(name)
      case _                          =>
    }
    ref
  }

  /** Override it to define the default seed for tests (else seed seed is not fixed)
    *
    * It can be overriden from sbt command: `testOnly Myspec -- -Dseed=11`.
    */
  val defaultSeed: Option[Int] = None

  // Private else user could modify it in a test, leading to strange behavior.
  private var seed: Option[Int] = None

  /** Seed can be set with `testOnly MyComponentSpec -- -Dseed=123456789` */
  final override def beforeAll(configMap: ConfigMap): Unit = {
    seed = configMap
      .get("seed")
      .map(_.asInstanceOf[String].toInt)
      .orElse(defaultSeed)
  }

  /** Override it to define the default SpinalSimConfig (else defaults to SimConfig) */
  def defaultConfig: SpinalSimConfig = SimConfig

  /** Override it to false to disable caching of dut compilation */
  val defaultCaching: Boolean = true

  /** For `it should compile` or `it shouldNot compile` */
  object compile extends SpinalTestBench.Compile

  object Dut {
    def apply[Device <: Component](
        device: => Device,
        finalConfig: SpinalSimConfig = defaultConfig,
        finalCaching: Boolean = defaultCaching
    ): Dut[Device] =
      new Dut[Device] {
        def dut = device
        val config = finalConfig
        val caching = finalCaching
      }
  }

  /** Device under test with context to make it testable */
  abstract class Dut[Device <: Component] extends Nameable with SpinalTestBench.Testable[Device] {
    def dut: Device
    val config: SpinalSimConfig
    val caching: Boolean

    /** Get a new compiled `dut` */
    protected def compiled: SimCompiled[Device] =
      config.workspaceName(getName.map(c => if (c.isLetterOrDigit) c else '_')).compile(dut)

    /** Has this dut already been compiled for caching? */
    private var triedCompile: Boolean = false

    /** The cached dut. Can be `None` if `!triedCompile` or if compile failed */
    private var cachedDut: Option[SimCompiled[Device]] = None

    /** Set of functions to build text for test results */
    private object txt {
      def itCompiles: String = s"$getName compiles"
      def notCompiles: String = s"$getName does not compile"
      def itShould(doWhat: String): String = s"$getName should $doWhat"
      def test(doWhat: String): String = s"$getName test: $doWhat"
    }

    /** Compile `cachedDut` if caching is enabled and not compiled yet
      *
      * Use this function only before tests, NEVER in a test.
      */
    protected def prepareCachedDut(force: Boolean = false): Unit = {
      if ((caching && !triedCompile) || force) {
        triedCompile = true
        txt.itCompiles in {
          cachedDut = Some(compiled)
        }
      }
    }

    /** Function used to run a test/simulation
      *
      * @param name The name of the test/simulation
      * @param f doSim function, eg: `_.doSimUntilVoid(name)` or `dut => dut.doSim(name, seed)`
      * @param body the code to run on the (possibly abstracted) `dut` during the simulation
      */
    protected def sim(name: String, f: SimCompiled[Device] => (Device => Unit) => Unit)(
        body: Body
    ): Unit = {
      prepareCachedDut()
      name in {
        val dut =
          if (caching) cachedDut getOrElse cancel
          else compiled
        f(dut)(body)
      }
    }

    /** Helper to call sim with optional seed */
    final protected def sim(name: String): Body => Unit = seed match {
      case Some(s) => sim(name, _.doSim(name, s))
      case None    => sim(name, _.doSim(name))
    }

    /** Helper to call sim with doSimUntilVoid and optional seed */
    final protected def simUntilVoid(name: String): Body => Unit = seed match {
      case Some(s) => sim(name, _.doSimUntilVoid(name, s))
      case None    => sim(name, _.doSimUntilVoid(name))
    }

    /** `it should compile` runs a test to check that `it` compiles */
    override def should(c: SpinalTestBench.Compile): Unit = prepareCachedDut(force = true)

    /** `it should compile` runs a test to check that `it` does not compile */
    override def shouldNot(c: SpinalTestBench.Compile): Unit = {
      txt.notCompiles in {
        if (caching && triedCompile) {
          assert(cachedDut.isEmpty, "Dut has already compiled")
        } else {
          var failed = false
          try { compiled }
          catch { case _: Throwable => failed = true }
          assert(failed, "Dut has compiled")
        }
      }
    }

    override protected def shouldInSim(doWhat: String)(body: Body): Unit =
      sim(txt.itShould(doWhat))(body)

    override protected def shouldInSimUntilVoid(doWhat: String)(body: Body): Unit =
      simUntilVoid(txt.itShould(doWhat))(body)

    override def test(testName: String)(body: Body): Unit =
      sim(txt.test(testName))(body)

    override def testUntilVoid(testName: String)(body: Body): Unit =
      simUntilVoid(txt.test(testName))(body)
  }

  class TestSuite[Cfg, Device](
      builder: DutBuilder[Cfg, Device],
      val tests: (Cfg, SpinalTestBench.Testable[Device]) => Unit
  ) extends Nameable {

    /** Run with configs/duts from a sequence */
    final def runWithAll(cfgs: Seq[Cfg]): Unit = {
      getName - {
        for (cfg <- cfgs) {
          val it = builder(cfg)
          it.getName - {
            tests(cfg, it)
          }
        }
      }
    }

    /** Run with one or more configs/duts */
    final def runWith(cfgs: Cfg*): Unit = runWithAll(cfgs)

    /** Get the same TestSuite accepting different kind of config (keeps name) */
    final def from[A](cfgBuilder: A => Cfg) = {
      val ts = new TestSuite[A, Device](builder.from[A](cfgBuilder), (a, it) => tests(cfgBuilder(a), it))
      if (isNamed) ts.setName(getName) else ts
    }
  }

  object TestSuite {
    import SpinalTestBench.Testable

    def apply[Device](tests: Testable[Device] => Unit) =
      new TestSuite[Testable[Device], Device](DutBuilder.fromTestable(dut => dut), (cfg, it) => tests(it))

    def apply[Cfg, Device](builder: DutBuilder[Cfg, Device])(tests: (Cfg, Testable[Device]) => Unit) =
      new TestSuite[Cfg, Device](builder, tests)
  }

  object DutBuilder {

    /** Create a DutBuilder from a function creating a Testable (~ calling `Dut`) */
    def fromTestable[Cfg, Device](builder: Cfg => SpinalTestBench.Testable[Device]) =
      new DutBuilder[Cfg, Device](cfg => builder(cfg), _.toString())

    /** Create a DutBuilder from a function creating a Device (~ not calling `Dut`) */
    def fromDevice[Cfg, Device <: Component](builder: Cfg => Device) =
      fromTestable[Cfg, Device](cfg => Dut(builder(cfg)))
  }

  /** Functor to create a testable device */
  class DutBuilder[Cfg, Device](builder: Cfg => SpinalTestBench.Testable[Device], nameBuilder: Cfg => String) {
    def apply(cfg: Cfg): SpinalTestBench.Testable[Device] = builder(cfg).setWeakName(nameBuilder(cfg))

    /** Get a new functor putting devices in an environment */
    final def withEnv[InEnv](env: Device => InEnv) =
      new DutBuilder[Cfg, InEnv](cfg => apply(cfg) withEnv env, nameBuilder)

    /** Get a new functor giving a custom weak name to created Dut's */
    final def withNameBuilder(nb: Cfg => String) =
      new DutBuilder[Cfg, Device](builder, nb)

    /** Get a new functon which caches the Dut's */
    final def withCache = new DutCache[Cfg, Device](apply, nameBuilder)

    /** Get a new function which takes another type of parameter */
    final def from[A](f: A => Cfg) = new DutBuilder[A, Device](a => builder(f(a)), a => nameBuilder(f(a)))
  }

  /** Functor to create a testable device with caching over configurations */
  class DutCache[Cfg, Device](builder: Cfg => SpinalTestBench.Testable[Device], nameBuilder: Cfg => String)
      extends DutBuilder[Cfg, Device](builder, nameBuilder) {
    import scala.collection.mutable.Map

    private var cache: Map[Cfg, SpinalTestBench.Testable[Device]] = Map()

    override def apply(cfg: Cfg): SpinalTestBench.Testable[Device] =
      cache.getOrElseUpdate(cfg, super.apply(cfg))
  }
}

object SpinalTestBench {

  /** The type of `compile` */
  class Compile

  /** Something which can be tested */
  trait Testable[Device] extends InheritedNameable {

    /** A function doing actions on the (possibly transformed) `dut` */
    final type Body = Device => Unit

    /** Context for `it should "do something"` */
    final class Should(doWhat: String) {

      /** Run a simulation to check that it does the thing it should
        *
        * {{{
        * it should "do something" inSim { dut =>
        *   // code to check that it does the thing it should
        * }
        * }}}
        */
      final def inSim: Body => Unit = shouldInSim(doWhat)

      /** Run a simulation to check that it does the thing it should
        *
        * {{{
        * it should "do something" inSimUntilVoid { dut =>
        *   // code to check that it does the thing it should
        * }
        * }}}
        */
      final def inSimUntilVoid: Body => Unit = shouldInSimUntilVoid(doWhat)
    }

    /** To build a [[Should]] context */
    final def should(doWhat: String): Should = new Should(doWhat)

    /** Create a new testable putting dut in provided environment */
    final def withEnv[InEnv](e: Device => InEnv): Testable[InEnv] = transformTestable(this, e)

    /** Run several tests on it */
    final def perform(tests: this.type => Unit): Unit = tests(this)

    /** Called by [[Should.inSim]] */
    protected def shouldInSim(doWhat: String)(body: Body): Unit

    /** Called by [[Should.inSimUntilVoid]] */
    protected def shouldInSimUntilVoid(doWhat: String)(body: Body): Unit

    /** Run a test using this dut */
    def test(testName: String)(body: Body): Unit

    /** Run a test using this dut */
    def testUntilVoid(testName: String)(body: Body): Unit

    /** `it should compile` runs a test to check that `it` compiles */
    def should(c: Compile): Unit

    /** `it should compile` runs a test to check that `it` does not compile */
    def shouldNot(c: Compile): Unit
  }

  private def transformTestable[Device, InEnv](it: Testable[Device], transform: Device => InEnv) =
    new Testable[InEnv] {
      override def getName(): String = it.getName

      override def setWeakName(name: String): this.type = { it.setWeakName(name); this }

      override protected def shouldInSim(doWhat: String)(body: Body): Unit =
        it should doWhat inSim (dut => body(transform(dut)))

      override protected def shouldInSimUntilVoid(doWhat: String)(body: Body): Unit =
        it should doWhat inSimUntilVoid (dut => body(transform(dut)))

      override def test(testName: String)(body: Body): Unit =
        it.test(testName)(dut => body(transform(dut)))

      override def testUntilVoid(testName: String)(body: Body): Unit =
        it.testUntilVoid(testName)(dut => body(transform(dut)))

      override def should(c: Compile): Unit = it should c

      override def shouldNot(c: Compile): Unit = it shouldNot c
    }
}
