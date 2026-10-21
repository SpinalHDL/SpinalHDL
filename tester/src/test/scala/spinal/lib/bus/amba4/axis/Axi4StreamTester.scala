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

  test("user width association") {
    assert(Axi4StreamConfig(dataWidth = 4, useUser = true, userWidth = 2).userBitsWidth == 8)
    assert(Axi4StreamConfig(dataWidth = 4, useUser = true, beatUserWidth = 3).userBitsWidth == 3)

    val legacyPositionalConfig = Axi4StreamConfig(4, 5, 6, 2, true, true, true, true, true, true)
    assert(legacyPositionalConfig.userBitsWidth == 8)

    SpinalVerilog(new Component {
      val laneSource = slave(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useUser = true, userWidth = 2)))
      val laneSink = master(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useUser = true, userWidth = 2)))
      val beatSource = slave(Axi4Stream(Axi4StreamConfig(dataWidth = 2, useUser = true, beatUserWidth = 2)))
      val beatSink = master(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useUser = true, beatUserWidth = 3)))
      val compactSource = slave(Axi4Stream(Axi4StreamConfig(dataWidth = 4, useKeep = true, useUser = true, beatUserWidth = 3)))
      val compactSink = master(Axi4Stream(compactSource.config))

      laneSink << laneSource
      beatSink << beatSource
      compactSink << Axi4StreamSparseCompactor(compactSource)

      assert(laneSource.user.getBitsWidth == 8)
      assert(beatSource.user.getBitsWidth == 2)
      assert(beatSink.user.getBitsWidth == 3)
    })
  }

  test("user width configuration validation") {
    assertThrows[IllegalArgumentException](
      Axi4StreamConfig(dataWidth = 4, userWidth = 1, beatUserWidth = 1)
    )
    assertThrows[IllegalArgumentException](
      Axi4StreamConfig(dataWidth = 4, beatUserWidth = 0)
    )
    assertThrows[IllegalArgumentException](
      Axi4StreamConfig(dataWidth = 4, beatUserWidth = -2)
    )
  }

  test("beat user width adapter support") {
    val beatConfig = Axi4StreamConfig(dataWidth = 4, useUser = true, beatUserWidth = 3)

    SpinalVerilog(new Axi4StreamSimpleWidthAdapter(beatConfig, outWidth = 4))
    SpinalVerilog(new Axi4StreamSimpleWidthAdapter(beatConfig, outWidth = 2))
    SpinalVerilog(new Axi4StreamSimpleWidthAdapter(beatConfig, outWidth = 8))
    SpinalVerilog(new Axi4StreamWidthAdapter(beatConfig, beatConfig.copy(dataWidth = 2)))
    SpinalVerilog(new Axi4StreamWidthAdapter(beatConfig, beatConfig.copy(dataWidth = 8)))
    SpinalVerilog(new Axi4StreamWidthAdapter(
      beatConfig.copy(useKeep = true, useLast = true),
      beatConfig.copy(dataWidth = 8, useKeep = true, useLast = true),
      compact = true
    ))

    val laneConfig = Axi4StreamConfig(dataWidth = 4, useUser = true, userWidth = 2)
    SpinalVerilog(new Axi4StreamSimpleWidthAdapter(laneConfig, outWidth = 2))
    SpinalVerilog(new Axi4StreamWidthAdapter(laneConfig, laneConfig.copy(dataWidth = 8)))

    assertThrows[IllegalArgumentException](
      SpinalVerilog(new Axi4StreamWidthAdapter(
        Axi4StreamConfig(dataWidth = 4, useUser = true, userWidth = 1),
        Axi4StreamConfig(dataWidth = 2, useUser = true, beatUserWidth = 3)
      ))
    )
  }

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
