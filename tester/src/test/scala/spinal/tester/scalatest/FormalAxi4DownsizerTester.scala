package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.HistoryModifyable

case class FormalAxi4Record(val config: Axi4Config, maxStrbs: Int) extends Bundle {
  val addr = UInt(7 bits)
  val id = if (config.useId) UInt(config.idWidth bits) else null
  val len = if (config.useLen) UInt(8 bits) else null
  val size = if (config.useSize) UInt(3 bits) else null
  val burst = if (config.useBurst) Bits(2 bits) else null
  val isLockExclusive = if (config.useLock) Bool() else null
  val awDone = Bool()

  val strbs = if (config.useStrb) Vec(Bits(config.bytePerWord bits), maxStrbs) else null
  val count = UInt(9 bits)
  val seenLast = if (config.useLast) Bool() else null

  val bResp = Bool()

  def assignFromAx(ax: Stream[Axi4Ax]) {
    addr := ax.addr.resized
    isLockExclusive := ax.lock === Axi4.lock.EXCLUSIVE
    // burst := ax.burst
    len := ax.len
    size := ax.size
    // id := ax.id
    awDone := ax.ready
  }

  def assignFromW(w: Stream[Axi4W], selectCount: UInt) {
    seenLast := w.last & w.ready
    strbs(selectCount.resized) := w.strb
    when(w.ready) { count := selectCount + 1 }.otherwise{ count := selectCount }
  }

  def assignFromB(b: Stream[Axi4B]) {
    bResp := b.ready
  }

  def checkStrbs(cond: Bool) = new Area{
    val strbMaxMask = U((1 << config.bytePerWord) - 1, config.bytePerWord bits)
    val addrStrbMaxMask = (U(config.bytePerWord) - 1).resize(addr.getBitsWidth)
    val strbError = Bool()
    strbError := False
    when(cond) {
      val bytes = (len +^ 1) << size      
      val sizeMask = ((U(1) << (U(1) << size)) - 1).resize(config.bytePerWord bits)
      val addrSizeMask = ((U(1) << size) - 1).resize(addr.getBitsWidth)
      val strbsErrors = Vec(Bool(), maxStrbs)
      strbsErrors.map(_:=False)
      for(i <- 0 until maxStrbs) {
        when( i < count ) {
          val targetAddress = (addr + i << size).resize(addr.getBitsWidth)
          val offset = targetAddress & addrStrbMaxMask & ~addrSizeMask
          val byteLaneMask = (sizeMask << offset).resize(config.bytePerWord bits)
          strbsErrors(i) := (strbs(i) & ~byteLaneMask.asBits).orR
        }
      }
      strbError := strbsErrors.reduce(_ | _)
    }
  }
}

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def outputAsserter(axi: Axi4WriteOnly, maxBursts: Int = 16, maxStrbs: Int = 256) = new Area {
    val oRecord = FormalAxi4Record(axi.config, maxStrbs)
    oRecord.assignFromBits(B(0, oRecord.getBitsWidth bits))

    val histInput = Flow(cloneOf(oRecord))
    histInput.payload := oRecord
    histInput.valid := False
    val hist = HistoryModifyable(histInput, maxBursts)
    hist.io.inStreams.map(_.valid := False)
    hist.io.inStreams.map(_.payload := oRecord)
    hist.io.outStreams.map(_.ready := False)

    val (awExist, awFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid && !x.awDone)
    val (wExist, wFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid && !x.seenLast)
    val (bExist, bFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid /*&& axi.b.id === x.id*/ && !x.bResp)
    val awId = maxBursts - 1 - awFoundId
    val wId = maxBursts - 1 - wFoundId
    val bId = maxBursts - 1 - bFoundId

    val dataErrors = Vec(Bool(), 5)
    dataErrors.map(_ := False)

    val awRecord = CombInit(oRecord)
    val awValid = False
    val awSelect = CombInit(oRecord)
    when(axi.aw.valid) {
      val ax = axi.aw.asInstanceOf[Stream[Axi4Ax]]
      when(awExist) { 
        awRecord := hist.io.outStreams(awId)
        awRecord.allowOverride
        awRecord.assignFromAx(ax)
        awValid := True
        awSelect := hist.io.outStreams(awId)

        val realLen = axi.aw.len +^ 1
        dataErrors(0) := awSelect.seenLast & realLen > awSelect.count
        dataErrors(1) := !awSelect.seenLast & realLen === awSelect.count
        dataErrors(2) := realLen < awSelect.count
      }
      .otherwise { histInput.assignFromAx(ax) }
    }
    when(awValid) {
      hist.io.inStreams(awId).payload := awRecord
      hist.io.inStreams(awId).valid := awValid
    }

    val wRecord = CombInit(oRecord)
    val wValid = False
    val wSelect = CombInit(oRecord)
    when(axi.w.valid) {
      when(wExist) {
        wRecord := hist.io.outStreams(wId)
        wSelect := hist.io.outStreams(wId)
        when(awValid && wId === awId) {
          awRecord.assignFromW(axi.w, wSelect.count)
        }.otherwise { wRecord.assignFromW(axi.w, wSelect.count); wValid := True }
      }.otherwise { histInput.assignFromW(axi.w, U(0, 9 bits)) }
      dataErrors(3) := wSelect.awDone & (axi.w.last & (wSelect.count =/= wSelect.len))
      dataErrors(4) := wSelect.awDone & (!axi.w.last & (wSelect.count === wSelect.len))
    }
    when(wValid) {      
      hist.io.inStreams(wId).payload := wRecord
      hist.io.inStreams(wId).valid := wValid
    }

    val respErrors = Vec(Bool(), 4)
    respErrors.map(_ := False)

    val bRecord = CombInit(oRecord)
    val bValid = False
    val bSelect = CombInit(oRecord)
    when(axi.b.valid) {
      when(bExist) {  
        bRecord := hist.io.outStreams(bId)
        bSelect := hist.io.outStreams(bId)
        when(awValid && bId === awId) {
          awRecord.assignFromB(axi.b)
        }.elsewhen(wValid && bId === wId) {
          wRecord.assignFromB(axi.b)
        }.otherwise { bRecord.assignFromB(axi.b); bValid := True }

        hist.io.outStreams(bId).ready := axi.b.ready & bRecord.awDone & bRecord.seenLast

        respErrors(0) := !bSelect.awDone
        respErrors(1) := bSelect.awDone & !bSelect.seenLast
        respErrors(2) := bSelect.awDone & bSelect.isLockExclusive & axi.b.resp === Axi4.resp.EXOKAY
        // respErrors(3) := axi.b.ready & bSelect.awDone & bSelect.seenLast & checkStrb()
      }.otherwise {
        respErrors(0) := True
      }
      // when(axi.b.ready) { assert(bExist) }
    }
    when(bValid) {      
      hist.io.inStreams(bId).payload := bRecord
      hist.io.inStreams(bId).valid := bValid
    }    
    val strbsChecker = bSelect.checkStrbs(axi.b.valid & bExist)
    val strbError = strbsChecker.strbError

    when((axi.aw.valid & !awExist) | (axi.w.valid & !wExist)) {
      histInput.valid := True
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

        val outChecker = outputAsserter(output, 4, 4)
        outChecker.dataErrors.map(x => assert(!x))
        outChecker.respErrors.map(x => assume(!x))
        assert(!outChecker.strbError)
        
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
