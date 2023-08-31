package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

case class Axi4StreamEndianFixture[T <: Data](config: Axi4StreamConfig, outType: HardType[T]) extends Component {
  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_data = master(Stream(outType))
    val s_data = slave(Stream(outType))
    val m_axis = master(Axi4Stream(config.copy(useKeep = true)))
  }

  io.m_data << io.s_axis.toBitStream().map(_.toDataType(outType())).stage()
  io.m_axis << Axi4Stream(io.s_data.stage())
}

case class Axi4StreamFragmentFixture[T <: Data](config: Axi4StreamConfig, outType: HardType[T]) extends Component {
  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_data = master(Stream(Fragment(outType)))
    val s_data = slave(Stream(Fragment(outType)))
    val m_axis = master(Axi4Stream(config.copy(useKeep = true)))
  }

  io.m_data << io.s_axis.toBitStreamFragment().map(f => {
    val newF = Fragment(outType())
    newF.last := f.last
    newF.fragment := f.fragment.toDataType(outType())
    newF
  })
  io.m_axis << Axi4Stream(io.s_data.stage())
}

class Axi4StreamTester extends SpinalAnyFunSuite {

  def duplexTest(dut: Axi4StreamEndianFixture[Bits]): Unit = {
    dut.clockDomain.forkStimulus(10)

    for (_ <- 0 until 100) {
      dut.io.s_axis.data.randomize()
      dut.io.s_axis.valid #= true
      dut.io.m_data.ready #= true

      dut.clockDomain.waitSampling(2)

      assert(dut.io.s_axis.data.toBigInt == dut.io.m_data.payload.toBigInt)
    }

    for (_ <- 0 until 100) {
      dut.io.s_data.payload.randomize()
      dut.io.s_data.valid #= true
      dut.io.m_axis.ready #= true

      dut.clockDomain.waitSampling(2)

      assert(dut.io.s_data.payload.toBigInt == dut.io.m_axis.data.toBigInt)
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

      assert(dut.io.s_axis.data.toBigInt == dut.io.m_data.fragment.toBigInt)
      assert(dut.io.s_axis.last.toBoolean == dut.io.m_data.last.toBoolean)
    }

    for (_ <- 0 until 100) {
      dut.io.s_data.fragment.randomize()
      dut.io.s_data.valid #= true
      dut.io.s_data.last.randomize()
      dut.io.m_axis.ready #= true

      dut.clockDomain.waitSampling(2)

      assert(dut.io.s_data.fragment.toBigInt == dut.io.m_axis.data.toBigInt)
      assert(dut.io.s_data.last.toBoolean == dut.io.m_axis.last.toBoolean)
    }

    simSuccess()
  }

  test("stream") {
    SimConfig.compile(Axi4StreamEndianFixture(Axi4StreamConfig(dataWidth = 4), Bits(32 bit)))
      .doSim("test")(duplexTest)
  }

  test("fragment") {
    SimConfig.compile(Axi4StreamFragmentFixture(Axi4StreamConfig(dataWidth = 4, useLast = true), Bits(32 bit)))
      .doSim("test")(duplexFragmentTest)
  }
}
