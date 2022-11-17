package spinal.core.sim

import spinal.core._

class Identity extends Component {
  val io = new Bundle {
    val input = in port Bits(8 bits)
    val output = out port Bits(8 bits)
  }
  io.output := io.input
}

class BadIdentity extends Identity {
  io.output := io.output
}


// Drawback: I have to provide a base type as I do not take arguments anymore
class IdentityFlatSpec extends SpinalFlatSpec[Identity] {

  // Global parameters
  override val config = SimConfig.withWave
  // override val caching = false

  // Parameters can be erased by local parameter when building benches
  val asIs = bench(new Identity, caching = false)
  // I can transform, and the "software abstraction" type is inferred
  val transformed = benchTransform(new Identity, dut => SoftIdentity(dut))
  // These ones will not compile
  val failing = bench(new BadIdentity)
  val failingNoCache = bench(new BadIdentity, caching = false)

  // Before my first test, it tests if it compiles, and cancels test if it doesn't
  // Here I provide an empty test and I get:
  //   failing should compile *** FAILED ***
  //   failing should do nothing !!! CANCELED !!!
  failing should "do nothing" in { dut => }

  //    failingNoCache should do nothing *** FAILED ***
  failingNoCache should "do nothing" in { dut => }

  // Notice that benches are `Nameable` (but I did not implement recursive names)

  // Here I have no caching so no "compiles" test, I get:
  //   asIs should pass random test
  asIs should "pass random tests" in { dut =>
    for (i <- 0 to 99) {
      val v = dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  //   asIs should pass all tests
  asIs should "pass all tests" in { dut =>
    for (v <- 0 to 255) {
      dut.io.input #= v
      sleep(1)
      assert(dut.io.output.toInt == v)
    }
  }

  //   transformed should compile
  //   transformed should pass random tests
  transformed should "pass random tests" in { dut =>
    for (i <- 0 to 99) {
      val v = dut.dut.io.input.randomizedInt()
      assert(dut(v) == v)
    }
  }

  //   transformed should pass all tests
  transformed should "pass all tests" in { dut =>
    for (v <- 0 to 255)
      assert(dut(v) == v)
  }
}


class IdentityFlatSpecForLoop extends SpinalFlatSpec[Identity] {
  override val config = SimConfig.withWave

  val asIs = bench(new Identity, caching = false)
  val failing = bench(new BadIdentity)
  val failingNoCache = bench(new BadIdentity, caching = false)

  for (b <- Seq(asIs, failing, failingNoCache)) {
    b should "pass random tests" in { dut =>
      for (i <- 0 to 99) {
        val v = dut.io.input.randomize()
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }

    b should "pass all tests" in { dut =>
      for (v <- 0 to 255) {
        dut.io.input #= v
        sleep(1)
        assert(dut.io.output.toInt == v)
      }
    }
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
