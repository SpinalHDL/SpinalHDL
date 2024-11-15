package spinal.tester.scalatest

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.formal._

class FormalDeMuxTester extends SpinalFormalFunSuite {
  def formaldemuxControl(selWithCtrl: Boolean = false, backend: FormalBackend) = {
    FormalConfig
      .withBackend(backend)
      .withBMC(20)
      .withProve(20)
      .withCover(20)
      // .withDebug
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))

        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val demuxSelect = anyseq(UInt(log2Up(portCount) bit))
        val demuxInput = slave(Stream(dataType))
        val demuxOutputs = Vec(master(Stream(dataType)), portCount)

        dut.io.select := demuxSelect
        demuxInput >> dut.io.input

        when(reset || past(reset)) {
          assume(demuxInput.valid === False)
        }

        val selStableCond = if (selWithCtrl) past(demuxOutputs(demuxSelect).isStall) else null
        if (selWithCtrl) {
          cover(selStableCond)
          when(selStableCond) {
            assume(stable(demuxSelect))
          }
        }

        assumeInitial(demuxSelect < portCount)
        cover(demuxInput.fire)
        demuxInput.formalAssumesSlave()
        demuxInput.formalCovers(5)

        val inputFireStableSelChanged = past(demuxInput.fire) && demuxInput.fire && changed(demuxSelect)
        cover(inputFireStableSelChanged)

        for (i <- 0 until portCount) {
          demuxOutputs(i) << dut.io.outputs(i)
          demuxOutputs(i).formalAssertsMaster()
          demuxOutputs(i).formalCovers(5)
        }

        for (i <- 0 until portCount) {
          assert(demuxOutputs(i).payload === demuxInput.payload)
          when(i =/= demuxSelect) {
            assert(demuxOutputs(i).valid === False)
          }
        }
        when(demuxSelect < portCount) {
          assert(demuxOutputs(demuxSelect) === demuxInput)
        }
      })
  }
  test("demux_sel_with_control_symbiyosys") {
    formaldemuxControl(true, SymbiYosysFormalBackend)
  }
  test("demux_sel_without_control_symbiyosys") {
    shouldFail(formaldemuxControl(false, SymbiYosysFormalBackend))
  }
  test("demux_sel_with_control_ghdl") {
    formaldemuxControl(true, GhdlFormalBackend)
  }
  test("demux_sel_without_control_ghdl") {
    shouldFail(formaldemuxControl(false, GhdlFormalBackend))
  }

  def formaldemuxSync(backend: FormalBackend) = {
    FormalConfig
      .withBackend(backend)
      .withProve(20)
      .withCover(20)
      .doVerify(new Component {

        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new Component {
          val io = new Bundle {
            val input = slave(Stream(dataType))
            val selector = slave(Stream(UInt(log2Up(portCount) bit)))
            val outputs = Vec(master(Stream(dataType)), portCount)
          }
          (io.outputs, StreamDemux.joinSel(io.input, io.selector, portCount)).zipped.foreach(_ << _)
        })

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val demuxSelector = slave(Stream(UInt(log2Up(portCount) bit)))
        demuxSelector >> dut.io.selector
        val demuxInput = slave(Stream(dataType))
        demuxInput >> dut.io.input
        val demuxOutputs = Vec(master(Stream(dataType)), portCount)

        for (i <- 0 until portCount) {
          demuxOutputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(demuxInput.valid === False)
          assume(demuxSelector.valid === False)
        }
        demuxSelector.formalAssumesSlave()
        demuxSelector.formalCovers(5)
        when(demuxSelector.valid) {
          assume(demuxSelector.payload < portCount)
        }
        demuxInput.formalAssumesSlave()
        demuxInput.formalCovers(5)

        val inputFireStableSelChanged = past(demuxInput.fire) && demuxInput.fire && changed(dut.io.selector.payload)
        cover(inputFireStableSelChanged)

        for (i <- 0 until portCount) {
          demuxOutputs(i).formalAssertsMaster()
          demuxOutputs(i).formalCovers(5)
        }

        for (i <- 0 until portCount) {
          when(i =/= dut.io.selector.payload) {
            assert(demuxOutputs(i).valid === False)
          }
        }
        when(dut.io.input.valid && dut.io.selector.valid) {
          assert(dut.io.input === dut.io.outputs(dut.io.selector.payload))
        }
        setDefinitionName("demux_sync_sel")
      })
  }
  test("demux_sync_sel_symbiyosys") {
    formaldemuxSync(SymbiYosysFormalBackend)
  }
  test("demux_sync_sel_ghdl") {
    formaldemuxSync(GhdlFormalBackend)
  }

  def formaldemuxSelector(backend: FormalBackend) = {
    FormalConfig
      .withBackend(backend)
      .withProve(20)
      .withCover(20)
      .doVerify(new Component {
        val portCount = 5
        val dataType = Bits(8 bits)
        val dut = FormalDut(new StreamDemux(dataType, portCount))
        val selector = dut.io.createStreamRegSelect()

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val demuxSelector = slave(Stream(UInt(log2Up(portCount) bit)))
        demuxSelector >> selector
        val demuxInput = slave(Stream(dataType))
        demuxInput >> dut.io.input
        val demuxOutputs = Vec(master(Stream(dataType)), portCount)
        for (i <- 0 until portCount) {
          demuxOutputs(i) << dut.io.outputs(i)
        }

        when(reset || past(reset)) {
          assume(demuxInput.valid === False)
          assume(demuxSelector.valid === False)
        }

        assumeInitial(demuxSelector.payload < portCount)
        demuxSelector.formalAssumesSlave()
        demuxInput.formalAssumesSlave()
        demuxInput.formalCovers(5)

        val inputFireStableSelChanged = past(demuxInput.fire) && demuxInput.fire && changed(dut.io.select)
        cover(inputFireStableSelChanged)

        for (i <- 0 until portCount) {
          demuxOutputs(i).formalAssertsMaster()
          demuxOutputs(i).formalCovers(5)
        }

        for (i <- 0 until portCount) {
          when(i =/= dut.io.select) {
            assert(demuxOutputs(i).valid === False)
          }
        }
        when(dut.io.select < portCount) {
          assert(demuxOutputs(dut.io.select) === demuxInput)
        }
      })
  }
  test("demux_with_selector_symbiyosys") {
    formaldemuxSelector(SymbiYosysFormalBackend)
  }
  test("demux_with_selector_ghdl") {
    formaldemuxSelector(GhdlFormalBackend)
  }
}
