package spinal.core.sim

import spinal.core._

case class IdentityConfig(size: Int)

class Identity(cfg: IdentityConfig = IdentityConfig(8)) extends Component {
  val io = new Bundle {
    val input = in port Bits(cfg.size bits)
    val output = out port Bits(cfg.size bits)
  }
  io.output := io.input
}

class BadIdentity(cfg: IdentityConfig = IdentityConfig(8)) extends Identity(cfg) {
  io.output := io.output
}

class IdentityFreeSpec extends SpinalFreeSpec[Identity] {

  override val config = SimConfig.withWave

  val asIs = bench(new Identity, caching = false)
  val transformed = benchTransform(new Identity, dut => SoftIdentity(dut))
  val failing = bench(new BadIdentity)
  val failingNoCache = bench(new BadIdentity, caching = false)

  failing shouldNot compile
  failing should compile

  failingNoCache shouldNot compile
  failingNoCache should compile

  failing.test("Figure 1: empty test") { dut => }

  // New feature: protocols (Nameable) with conditional tests depending on the config
  val asIsProtCfg = ProtocolCfg { (cfg: IdentityConfig) =>
    bench(new Identity(cfg))
  } { (cfg, it) =>
    it should "do nothing" in { dut => }

    it should "pass random tests" in { dut =>
      for (i <- 0 to 99) {
        val v = dut.io.input.randomize()
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }

    if (cfg.size >= 8) {
      it should "pass all tests" in { dut =>
        for (v <- 0 to 255) {
          dut.io.input #= v
          sleep(1)
          assert(dut.io.output.toInt == v)
        }
      }
    }
  }

  // Works with software abstractions as well
  val tranProtCfg =
    ProtocolCfg { (cfg: IdentityConfig) =>
      benchTransform(new Identity(cfg), dut => SoftIdentity(dut))
    } { (cfg, it) =>
      it should "do nothing" in { dut => }

      it should "pass random tests" in { dut =>
        for (i <- 0 to 99) {
          val v = dut.dut.io.input.randomizedInt()
          assert(dut(v) == v)
        }
      }

      if (cfg.size >= 8) {
        it should "pass all tests" in { dut =>
          for (v <- 0 to 255)
            assert(dut(v) == v)
        }
      }
    }

  // The main benefit is that they can be run on generated configs:
  val cfgs = for (n <- 5 to 10) yield IdentityConfig(n)
  asIsProtCfg.runWithAll(cfgs)
  asIsProtCfg.runWith(IdentityConfig(16))
  tranProtCfg.runWithAll(cfgs)
  tranProtCfg.runWith(IdentityConfig(16))
  /* Filtered with `grep -ve '^\['`
IdentityFreeSpec:
asIsProtCfg
  IdentityConfig(5)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(6)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(7)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(8)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  IdentityConfig(9)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  IdentityConfig(10)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
asIsProtCfg
  IdentityConfig(16)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
tranProtCfg
  IdentityConfig(5)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(6)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(7)
  -  compiles
  -  should do nothing
  -  should pass random tests
  IdentityConfig(8)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  IdentityConfig(9)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  IdentityConfig(10)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
tranProtCfg
  IdentityConfig(16)
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
   */

  // It is also possible to have a protocol to be used directly on benches:
  val asIsProt = Protocol[Identity] { it =>
    it should "do nothing" in { dut => }

    it should "pass random tests" in { dut =>
      for (i <- 0 to 99) {
        val v = dut.io.input.randomize()
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }

    it should "pass all tests" in { dut =>
      for (v <- 0 to 255) {
        dut.io.input #= v
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }
  }

  val tranProt = Protocol[SoftIdentity] { it =>
    it should "do nothing" in { dut => }

    it should "pass random tests" in { dut =>
      for (i <- 0 to 99) {
        val v = dut.dut.io.input.randomizedInt()
        assert(dut(v) == v)
      }
    }

    it should "pass all tests" in { dut =>
      for (v <- 0 to 255)
        assert(dut(v) == v)
    }
  }

  asIsProt.runOn(asIs, failing, failingNoCache)
  asIsProt.runOnAll(
    Seq(
      bench(new Identity, caching = false) -> "asIs renamed",
      bench(new Identity, dut => SoftIdentity(dut)) -> "failing renamed",
      bench(new Identity, dut => SoftIdentity(dut)) -> "failingNoCache renamed"
    )
  )
  tranProt.runOnAll(
    Seq(
      transformed -> "transformed renamed"
    )
  )
  /* Filtered with `grep -e '^\s*-' -e '^\w' -e '^  \w' | grep -ve '^\[' -e "^Design's errors arelisted above.$" -e "^SpinalHDL compiler exit stack :$"`
asIsProt
  asIs
  - asIs should do nothing
  - asIs should pass random tests
  - asIs should pass all tests
  failing
  - failing compiles *** FAILED ***
ASSIGNMENT OVERLAP completely the previous one of (toplevel/io_output : out Bits[8 bits])
  - failing should do nothing !!! CANCELED !!!
  - failing should pass random tests !!! CANCELED !!!
  - failing should pass all tests !!! CANCELED !!!
  failingNoCache
  - failingNoCache should do nothing *** FAILED ***
ASSIGNMENT OVERLAP completely the previous one of (toplevel/io_output : out Bits[8 bits])
  - failingNoCache should pass random tests *** FAILED ***
ASSIGNMENT OVERLAP completely the previous one of (toplevel/io_output : out Bits[8 bits])
  - failingNoCache should pass all tests *** FAILED ***
ASSIGNMENT OVERLAP completely the previous one of (toplevel/io_output : out Bits[8 bits])
asIsProt
  asIs renamed
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  failing renamed
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
  failingNoCache renamed
  -  compiles
  -  should do nothing
  -  should pass random tests
  -  should pass all tests
tranProt
  transformed renamed
  - transformed compiles
  - transformed should do nothing
  - transformed should pass random tests
  - transformed should pass all tests
   */

  failing should "do nothing" in { dut => }

  failingNoCache should "do nothing" in { dut => }

  asIs should "pass random tests" in { dut =>
    for (i <- 0 to 99) {
      val v = dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  asIs should "pass all tests" in { dut =>
    for (v <- 0 to 255) {
      dut.io.input #= v
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  transformed should "pass random tests" in { dut =>
    for (i <- 0 to 99) {
      val v = dut.dut.io.input.randomizedInt()
      assert(dut(v) == v)
    }
  }

  transformed should "pass all tests" in { dut =>
    for (v <- 0 to 255)
      assert(dut(v) == v)
  }
}

class SimpleIdentityTester extends SpinalFunSuite(new Identity) {
  override val config = SimConfig.withWave

  test("random tests") { dut =>
    for (i <- 0 to 99) {
      val v = dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  test("all tests") { dut =>
    for (v <- 0 to 255) {
      dut.io.input #= v
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }
}

class AbstractIdentityTester extends SpinalFunSuiteTransform(new Identity) {
  type SoftDut = SoftIdentity

  override val config = SimConfig.withWave

  def preSimTransform(dut: Identity): SoftIdentity = SoftIdentity(dut)

  test("random tests") { dut =>
    for (i <- 0 to 99) {
      val v = dut.dut.io.input.randomizedInt()
      assert(dut(v) == v)
    }
  }

  test("all tests") { dut =>
    for (v <- 0 to 255)
      assert(dut(v) == v)
  }
}

case class SoftIdentity(dut: Identity) {
  import dut._

  def write(v: Int): Unit = io.input #= v
  def read(): Int = io.output.toInt
  def apply(v: Int): Int = {
    write(v)
    sleep(1)
    read()
  }
}
