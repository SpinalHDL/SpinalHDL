package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4SToStreamRich
import spinal.lib.bus.amba4.axis._

case class Axi4StreamEndianFixture[T <: Data](config: Axi4StreamConfig, outType: HardType[T], endian: Endianness = LITTLE) extends Component {
  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_data = master(Stream(outType))
    val s_data = slave(Stream(outType))
    val m_axis = master(Axi4Stream(config))
  }

  io.m_data << io.s_axis.toStream(outType, endian).stage()
  io.m_axis << io.s_data.toAxi4Stream(endian)
}

case class Axi4StreamFragmentFixture[T <: Data](config: Axi4StreamConfig, outType: HardType[T], endian: Endianness = LITTLE) extends Component {
  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_data = master(Stream(Fragment(outType)))
    val s_data = slave(Stream(Fragment(outType)))
    val m_axis = master(Axi4Stream(config))
  }

  io.m_data << io.s_axis.toStreamFragment(outType, endian).stage()
  io.m_axis << io.s_data.toAxi4Stream(endian)
}

class Axi4StreamTester extends AnyFunSuite {

  def duplexTest(dut: Axi4StreamEndianFixture[Bits]): Unit = {
    dut.clockDomain.forkStimulus(10)

    for (_ <- 0 until 100) {
      dut.io.s_axis.data.randomize()
      dut.io.s_axis.valid #= true
      dut.io.m_data.ready #= true

      dut.clockDomain.waitSampling(2)

      val extractedAxisValue = dut.endian match {
        case LITTLE =>
          dut.io.s_axis.data.toBigInt & ((BigInt(1) << dut.io.m_data.payload.getWidth)-1)
        case BIG =>
          dut.io.s_axis.data.toBigInt >> ((dut.io.s_axis.config.dataWidth*8)-dut.io.m_data.payload.getWidth)
      }
      assert(extractedAxisValue == dut.io.m_data.payload.toBigInt)
    }

    for (_ <- 0 until 100) {
      dut.io.s_data.payload.randomize()
      dut.io.s_data.valid #= true
      dut.io.m_axis.ready #= true

      dut.clockDomain.waitSampling(2)
      val extractedAxisValue = dut.endian match {
        case LITTLE =>
          dut.io.m_axis.data.toBigInt & ((BigInt(1) << dut.io.s_data.payload.getWidth)-1)
        case BIG =>
          dut.io.m_axis.data.toBigInt >> ((dut.io.m_axis.config.dataWidth*8)-dut.io.s_data.payload.getWidth)
      }
      assert(dut.io.s_data.payload.toBigInt == extractedAxisValue)
    }

    simSuccess()
  }

  def duplexFragmentTest(dut: Axi4StreamFragmentFixture[Bits]): Unit = {
    dut.clockDomain.forkStimulus(10)

    for (_ <- 0 until 100) {
      dut.io.s_axis.data.randomize()
      dut.io.s_axis.valid #= true
      dut.io.s_axis.last.randomize()
      dut.io.m_data.ready #= true

      dut.clockDomain.waitSampling(2)

      val extractedAxisValue = dut.endian match {
        case LITTLE =>
          dut.io.s_axis.data.toBigInt & ((BigInt(1) << dut.io.m_data.payload.getWidth)-1)
        case BIG =>
          dut.io.s_axis.data.toBigInt >> ((dut.io.s_axis.config.dataWidth*8)-dut.io.m_data.payload.getWidth)
      }
      assert(extractedAxisValue == dut.io.m_data.fragment.toBigInt)
      assert(dut.io.s_axis.last.toBoolean == dut.io.m_data.last.toBoolean)
    }

    for (_ <- 0 until 100) {
      dut.io.s_data.fragment.randomize()
      dut.io.s_data.valid #= true
      dut.io.s_data.last.randomize()
      dut.io.m_axis.ready #= true

      dut.clockDomain.waitSampling(2)

      val extractedAxisValue = dut.endian match {
        case LITTLE =>
          dut.io.m_axis.data.toBigInt & ((BigInt(1) << dut.io.s_data.payload.getWidth)-1)
        case BIG =>
          dut.io.m_axis.data.toBigInt >> ((dut.io.m_axis.config.dataWidth*8)-dut.io.s_data.payload.getWidth)
      }
      assert(dut.io.s_data.fragment.toBigInt == extractedAxisValue)
      assert(dut.io.s_data.last.toBoolean == dut.io.m_axis.last.toBoolean)
    }

    simSuccess()
  }

  test("endianness_byte") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(32 bit)))
      .doSim("test")(duplexTest)
  }

  test("endianness_lsb") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(30 bit), LITTLE))
      .doSim("test")(duplexTest)
  }

  test("endianness_msb") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(30 bit), BIG))
      .doSim("test")(duplexTest)
  }

  test("fragment") {
    SimConfig.compile(Axi4StreamFragmentFixture(Axi4StreamConfig(dataWidth = 4, useLast = true), Bits(32 bit)))
      .doSim("test")(duplexFragmentTest)
  }
}
