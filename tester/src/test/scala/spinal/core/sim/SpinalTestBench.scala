package spinal.core.sim

import spinal.core._

case class IdentityConfig(size: Int)

case class Identity(cfg: IdentityConfig) extends Component {
  require(cfg.size > 0)

  val io = new Bundle {
    val input = in port Bits(cfg.size bits)
    val output = out port Bits(cfg.size bits)
  }

  io.output := io.input
}

case class IdentityEnv(dut: Identity) {
  def write(v: Int): Unit = dut.io.input #= v
  def read(): Int = dut.io.output.toInt
  def run(v: Int): Int = { write(v); sleep(1); read }
}

class IdentityTestBench extends SpinalTestBench {

  // Create a testable device
  val cfg = IdentityConfig(8)
  val identity8 = Dut(Identity(cfg))

  // Test things (a test for compilation is prepended automatically)

  identity8 should "pass random tests" inSim { dut =>
    for (i <- 0 to 99) {
      val v = dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  identity8 should "pass all tests" inSim { dut =>
    for (v <- 0 to 255) {
      dut.io.input #= v
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  // It is possible to not use "should" syntax
  identity8.test("empty test") { dut => }

  // Test that it compiles / doesn't compile

  val identity10 = Dut(Identity(IdentityConfig(10)))
  identity10 should compile

  val identity0 = Dut(Identity(IdentityConfig(0)))
  identity0 shouldNot compile

  val compileTests = TestSuite(DutBuilder.fromDevice(Identity)) { (cfg, dut) =>
    if (cfg.size > 0) dut should compile
    else dut shouldNot compile
  }.from[Int](IdentityConfig)
  compileTests.runWithAll(-1 to 2)

  // It is possible to define a group of tests (flat, not nested)

  val testSuite1 = TestSuite[Identity] { it =>
    it should "pass random tests" inSim { dut =>
      for (i <- 0 to 99) {
        val v = dut.io.input.randomize()
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }

    it should "pass all tests" inSim { dut =>
      for (v <- 0 to 255) {
        dut.io.input #= v
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }
  }

  // Run it
  testSuite1.runWith(identity8, identity10)
  testSuite1.runWithAll(Seq(Dut(Identity(cfg)).setName("custom name")))

  // It is useful if there is the equivalent of "1 entity, >=2 architectures"
  // But most of the time we just want to iterate over configurations

  // Let's cache testable devices
  val dutCache = DutBuilder.fromDevice(Identity).withCache

  val testSuite2 = TestSuite(dutCache) { (cfg, it) =>
    it should "pass random tests" inSim { dut =>
      for (i <- 0 to 99) {
        val v = dut.io.input.randomize()
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }

    // Tests can be enabled conditionally over the current configuration
    if (cfg.size >= 8) {
      it should "pass all tests" inSim { dut =>
        for (v <- 0 to 255) {
          dut.io.input #= v
          sleep(1)
          assert(dut.io.output.toInt == v)
        }
      }
    }
  }

  // And use it over generated configurations
  val cfgs = for (n <- 5 to 7) yield IdentityConfig(n)
  testSuite2.runWithAll(cfgs)
  testSuite2.runWith(cfg)

  // It is still possible to work with configuration even if the TestSuite doesn't use it
  // Let's use the same cache as above
  testSuite1.from[Int](n => dutCache(IdentityConfig(n))).runWithAll(11 to 12)

  // It is possible to put the dut in a testing environment to ease test writing.

  // For a single test
  identity8 withEnv IdentityEnv should "pass random tests (single soft)" inSim { env =>
    for (i <- 0 to 99) {
      val v = env.dut.io.input.randomizedInt()
      assert(env.run(v) == v)
    }
  }

  // For several tests in a block
  identity8 withEnv IdentityEnv perform { it =>
    it.test("empty test (global soft)") { env => }

    it should "pass all tests (global soft)" inSim { env =>
      for (v <- 0 to 255)
        assert(env.run(v) == v)
    }
  }

  // For several tests not in a block
  val identityInEnv = identity8 withEnv IdentityEnv
  identityInEnv should "work for 5" inSim { env =>
    assert(env.run(5) == 5)
  }

  // Using a fresh new `Dut` (name is `identityInEnv2` and is weak)
  val identityInEnv2 = Dut(Identity(cfg)) withEnv IdentityEnv
  identityInEnv2 should "work for 5" inSim { env =>
    assert(env.run(5) == 5)
  }

  // For a TestSuite
  val testSuite3 = TestSuite[IdentityEnv] { it =>
    it should "pass random tests (soft group)" inSim { env =>
      for (i <- 0 to 99) {
        val v = env.dut.io.input.randomizedInt()
        assert(env.run(v) == v)
      }
    }

    it should "pass all tests (soft group)" inSim { env =>
      for (v <- 0 to 255)
        assert(env.run(v) == v)
    }
  }

  testSuite3.runWith(identity8 withEnv IdentityEnv)

  // For a test suite working on the device environment
  val testSuite4 = TestSuite(dutCache withEnv IdentityEnv) { (cfg, it) =>
    it should "pass random tests" inSim { env =>
      for (i <- 0 to 99) {
        val v = env.dut.io.input.randomizedInt()
        assert(env.run(v) == v)
      }
    }

    if (cfg.size >= 8) {
      it should "pass all tests" inSim { env =>
        for (v <- 0 to 255)
          assert(env.run(v) == v)
      }
    }
  }

  testSuite4.runWith(cfg)
  testSuite4.runWithAll(cfgs)
}

// The user can add new words in the vocabulary
object MustTestBench {
  implicit class MustTestable[Device](val t: SpinalTestBench.Testable[Device]) {
    def must(doWhat: String) = new t.Should(doWhat, "must")
  }
}

class MustTestBench extends SpinalTestBench {
  val t = Dut(Identity(IdentityConfig(8)))
  t must "do nothing" inSim { dut => }
}
