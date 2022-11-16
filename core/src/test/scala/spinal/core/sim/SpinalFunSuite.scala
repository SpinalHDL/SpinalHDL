package spinal.core.sim

import spinal.core._

class Identity extends Component {
  val io = new Bundle {
    val input = in port Bits(8 bits)
    val output = out port Bits(8 bits)
  }
  io.output := io.input
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

  override def preSimTransform(dut: Identity): SoftIdentity = SoftIdentity(dut)

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
