package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.HistoryModifyable

object FormalAxi4Record {
  def apply(config: Axi4Config, maxStbs: Int) = {
    new FormalAxi4Record(config, maxStbs)
  }
}

class FormalAxi4Record(val config: Axi4Config, maxStbs: Int) extends Bundle {
  val addr = UInt(7 bits)
  val id = if (config.useId) UInt(config.idWidth bits) else null
  val len = if (config.useLen) UInt(8 bits) else null
  val size = if (config.useSize) UInt(3 bits) else null
  val burst = if (config.useBurst) Bits(2 bits) else null
  val isLockExclusive = if (config.useLock) Bool() else null
  val awDone = Bool()

  val strbs = if (config.useStrb) Vec(Bits(config.bytePerWord bits), maxStbs) else null
  val count = UInt(9 bits)
  val seenLast = if (config.useLast) Bool() else null

  val bResp = Bool()

  def assignFromAx(ax: Stream[Axi4Ax]) {
    when(ax.valid) {
      addr := ax.addr.resized
      isLockExclusive := ax.lock === Axi4.lock.EXCLUSIVE
      // burst := ax.burst
      len := ax.len
      size := ax.size
      // id := ax.id
      awDone := ax.ready
    }
  }

  def assignFromW(w: Stream[Axi4W], selectCount: UInt) {
    when(w.valid) {
      seenLast := w.last & w.ready
      strbs(selectCount.resized) := w.strb
      when(w.ready) { count := selectCount + 1 }
    }
  }

  def assignFromB(b: Stream[Axi4B]) {
    when(b.valid) {
      bResp := b.ready
    }
  }
}

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def outputAsserter(axi: Axi4WriteOnly, maxBursts: Int = 16, maxStbs: Int = 256) {
    val histInput = Flow(FormalAxi4Record(axi.config, maxStbs))
    histInput.payload.assignFromBits(B(0, histInput.payload.getBitsWidth bits))
    histInput.valid := False
    val hist = HistoryModifyable(histInput, maxBursts)

    val oRecord = cloneOf(histInput.payload)
    oRecord.assignFromBits(B(0, oRecord.getBitsWidth bits))

    val (awExist, awId) = hist.io.outStreams.sFindFirst(x => x.valid && !x.awDone)
    val awRecord = CombInit(oRecord)
    when(axi.aw.valid) {
      when(awExist) { awRecord := hist.io.outStreams(awId) }
    }
    awRecord.assignFromAx(axi.aw.asInstanceOf[Stream[Axi4Ax]])

    val (wExist, wId) = hist.io.outStreams.sFindFirst(x => x.valid && !x.seenLast)
    val wRecord = CombInit(oRecord)
    when(axi.w.valid) {
      when(wExist) { wRecord := hist.io.outStreams(wId) }
      wRecord.assignFromW(axi.w, hist.io.outStreams(wId).count)
    }

    val (bExist, bId) = hist.io.outStreams.sFindFirst(x => x.valid /*&& axi.b.id === x.id*/ && !x.bResp)
    when(axi.b.valid) {
      val inRecord = histInput.payload
      when(bExist) { inRecord := hist.io.inStreams(bId) }
      inRecord.assignFromB(axi.b)

    }

  }

  def tester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      .withBMC(10)
      // .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new Axi4WriteOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Axi4WriteOnly(inConfig))
        dut.io.input << input

        val output = master(Axi4WriteOnly(outConfig))
        dut.io.output >> output

        val maxStall = 16
        val inputAssumes = input.withAssumes(maxStall)
        val outputAsserts = output.withAsserts(maxStall)

        outputAsserter(output, 4, 4)

        output.withCovers()
        input.withCovers()
      })
  }

  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false)
  test("64_32") {
    tester(inConfig, outConfig)
  }
}
