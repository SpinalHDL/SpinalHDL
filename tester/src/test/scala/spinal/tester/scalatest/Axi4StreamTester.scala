package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}

case class Axi4StreamEndianFixture[T <: Data](config: Axi4StreamConfig, outType: HardType[T], endian: Endianness = LITTLE) extends Component {
  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_data = master(Stream(outType))
  }

  io.m_data << io.s_axis.toStream(outType, endian).stage()
}

class Axi4StreamTester extends AnyFunSuite {

  test("endianness_byte") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(32 bit)))
      .doSim("test")(dut => {
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until 100) {
          dut.io.s_axis.data.randomize()
          dut.io.s_axis.valid #= true
          dut.io.m_data.ready #= true

          dut.clockDomain.waitSampling(2)

          assert(dut.io.s_axis.data.toBigInt == dut.io.m_data.payload.toBigInt)
        }

        simSuccess()
      })
  }

  test("endianness_lsb") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(30 bit), LITTLE))
      .doSim("test")(dut => {
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until 100) {
          dut.io.s_axis.data.randomize()
          dut.io.s_axis.valid #= true
          dut.io.m_data.ready #= true

          dut.clockDomain.waitSampling(2)

          assert((dut.io.s_axis.data.toBigInt & 0x3FFFFFFF) == dut.io.m_data.payload.toBigInt)
        }

        simSuccess()
      })
  }

  test("endianness_msb") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(30 bit), BIG))
      .doSim("test")(dut => {
        dut.clockDomain.forkStimulus(10)

        for (_ <- 0 until 100) {
          dut.io.s_axis.data.randomize()
          dut.io.s_axis.valid #= true
          dut.io.m_data.ready #= true

          dut.clockDomain.waitSampling(2)

          assert((dut.io.s_axis.data.toBigInt >> 2) == dut.io.m_data.payload.toBigInt)
        }

        simSuccess()
      })
  }
}
