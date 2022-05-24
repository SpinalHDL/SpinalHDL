package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._


case class Axi4WriteOnly(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{

  val aw = Stream(Axi4Aw(config))
  val w = Stream(Axi4W(config))
  val b = Stream(Axi4B(config))


  def writeCmd = aw
  def writeData = w
  def writeRsp = b

  def <<(that : Axi4) : Unit = that >> this
  def >> (that : Axi4) : Unit = {
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
  }

  def <<(that : Axi4WriteOnly) : Unit = that >> this
  def >> (that : Axi4WriteOnly) : Unit = {
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
  }

  def awValidPipe() : Axi4WriteOnly = {
    val sink = Axi4WriteOnly(config)
    sink.aw << this.aw.validPipe()
    sink.w  << this.w
    sink.b  >> this.b
    sink
  }

  def setIdle(): this.type = {
    this.writeCmd.setIdle()
    this.writeData.setIdle()
    this.writeRsp.setBlocked()
    this
  }

  def setBlocked(): this.type = {
    this.writeCmd.setBlocked()
    this.writeData.setBlocked()
    this.writeRsp.setIdle()
    this
  }

  def toAxi4(): Axi4 = {
    val ret = Axi4(config)
    this >> ret
  
    ret.readCmd.setIdle()
    ret.readRsp.setBlocked()

    ret
  }

  def toFullConfig(): Axi4WriteOnly = {
    val ret = Axi4WriteOnly(config.toFullConfig())
    ret << this
    ret
  }

  def pipelined(
    aw: StreamPipe = StreamPipe.NONE,
    w: StreamPipe = StreamPipe.NONE,
    b: StreamPipe = StreamPipe.NONE
  ): Axi4WriteOnly = {
    val ret = cloneOf(this)
    ret.aw << this.aw.pipelined(aw)
    ret.w << this.w.pipelined(w)
    ret.b.pipelined(b) >> this.b
    ret
  }

  override def asMaster(): Unit = {
    master(aw, w)
    slave(b)
  }

  def formalContext(maxBursts: Int = 16, maxStrbs: Int = 256) = new Area {
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

      def assignFromW(w: Stream[Axi4W], selected: FormalAxi4Record) = new Area {
        seenLast := w.last & w.ready
        for (i <- 0 until maxStrbs) {
          when(selected.count === i) {
            strbs(i) := w.strb
          }.otherwise {
            strbs(i) := selected.strbs(i)
          }
        }
        when(w.ready) { count := selected.count + 1 }.otherwise { count := selected.count }
      }

      def assignFromB(b: Stream[Axi4B]) {
        bResp := b.ready
      }

      def checkStrbs(cond: Bool) = new Area {
        // val strbMaxMask = U((1 << config.bytePerWord) - 1, config.bytePerWord bits)
        val addrStrbMaxMask = (U(config.bytePerWord) - 1).resize(addr.getBitsWidth)
        val strbError = CombInit(False)
        when(cond) {
          // val bytes = (len +^ 1) << size
          val sizeMask = ((U(1) << (U(1) << size)) - 1).resize(config.bytePerWord bits)
          val addrSizeMask = ((U(1) << size) - 1).resize(addr.getBitsWidth)
          val strbsErrors = Vec(Bool(), maxStrbs)
          strbsErrors.map(_ := False)
          for (i <- 0 until maxStrbs) {
            when(i < count) {
              val targetAddress = (addr + (i << size)).resize(addr.getBitsWidth)
              val offset = targetAddress & addrStrbMaxMask & ~addrSizeMask
              val byteLaneMask = (sizeMask << offset).resize(config.bytePerWord bits)
              strbsErrors(i) := (strbs(i) & ~byteLaneMask.asBits).orR
            }
          }
          strbError := strbsErrors.reduce(_ | _)
        }
      }
    }

    import spinal.core.formal._

    val reset = ClockDomain.current.isResetActive
    val errorValidWhileReset = (reset | past(reset)) & (aw.valid === True | w.valid === True)
    val errorRespWhileReset = (reset | past(reset)) & (b.valid === True)

    val oRecord = FormalAxi4Record(config, maxStrbs)
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
    val (bExist, bFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid /*&& b.id === x.id*/ && !x.bResp)
    val awId = maxBursts - 1 - awFoundId
    val wId = maxBursts - 1 - wFoundId
    val bId = maxBursts - 1 - bFoundId

    val dataErrors = Vec(Bool(), 6)
    dataErrors.map(_ := False)

    dataErrors(5).allowOverride
    dataErrors(5) := hist.io.outStreams
      .map(x => x.valid & x.awDone & x.seenLast & (x.len +^ 1) =/= x.count)
      .reduce(_ | _)

    val awRecord = CombInit(oRecord)
    val awValid = False
    val addressLogic = new Area {
      val selected = CombInit(oRecord)
      when(aw.valid) {
        val ax = aw.asInstanceOf[Stream[Axi4Ax]]
        when(awExist) {
          awRecord := hist.io.outStreams(awId)
          awRecord.allowOverride
          awRecord.assignFromAx(ax)
          awValid := True
          selected := hist.io.outStreams(awId)

          val realLen = aw.len +^ 1
          dataErrors(0) := selected.seenLast & realLen > selected.count
          dataErrors(1) := !selected.seenLast & realLen === selected.count
          dataErrors(2) := realLen < selected.count
        }
          .otherwise { histInput.assignFromAx(ax) }
      }
      when(awValid) {
        hist.io.inStreams(awId).strbs.zip(awRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(awId).payload := awRecord
        hist.io.inStreams(awId).valid := awValid
      }
    }

    val wRecord = CombInit(oRecord)
    val wValid = False
    val dataLogic = new Area {
      val selected = CombInit(oRecord)
      when(w.valid) {
        when(wExist) {
          wRecord := hist.io.outStreams(wId)
          selected := hist.io.outStreams(wId)
          when(awValid && wId === awId) {
            awRecord.assignFromW(w, selected)
          }.otherwise { wRecord.assignFromW(w, selected); wValid := True }
        }.otherwise { histInput.assignFromW(w, oRecord) }
        dataErrors(3) := selected.awDone & (w.last & (selected.count =/= selected.len))
        dataErrors(4) := selected.awDone & (!w.last & (selected.count === selected.len))
      }
      when(wValid) {
        hist.io.inStreams(wId).strbs.zip(wRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(wId).payload := wRecord
        hist.io.inStreams(wId).valid := wValid
      }
    }

    val respErrors = Vec(Bool(), 4)
    respErrors.map(_ := False)

    val bRecord = CombInit(oRecord)
    val bValid = False
    val responseLogic = new Area {
      val selected = CombInit(oRecord)
      when(b.valid) {
        when(bExist) {
          bRecord := hist.io.outStreams(bId)
          selected := hist.io.outStreams(bId)
          when(awValid && bId === awId) {
            awRecord.assignFromB(b)
          }.elsewhen(wValid && bId === wId) {
            wRecord.assignFromB(b)
          }.otherwise { bRecord.assignFromB(b); bValid := True }

          hist.io.outStreams(bId).ready := b.ready & bRecord.awDone & bRecord.seenLast

          respErrors(0) := !selected.awDone
          respErrors(1) := selected.awDone & !selected.seenLast
          respErrors(2) := selected.awDone & b.resp === Axi4.resp.EXOKAY & !selected.isLockExclusive
          // respErrors(3) := b.ready & selected.awDone & selected.seenLast & checkStrb()
        }.otherwise {
          respErrors(0) := True
        }
      }
      when(bValid) {
        hist.io.inStreams(bId).strbs.zip(bRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(bId).payload := bRecord
        hist.io.inStreams(bId).valid := bValid
      }
      val strbsChecker = selected.checkStrbs(b.valid & bExist & b.ready & selected.awDone & selected.seenLast)
    }
    val strbError = responseLogic.strbsChecker.strbError

    when((aw.valid & !awExist) | (w.valid & !wExist)) {
      histInput.valid := True
    }

    def withAsserts(maxStallCycles: Int = 0) = new Area {
      aw.withAsserts()
      aw.withTimeoutAssumes(maxStallCycles)
      w.withAsserts()
      w.withTimeoutAssumes(maxStallCycles)
      b.withAssumes()
      b.withTimeoutAsserts(maxStallCycles)

      when(aw.valid) {
        aw.payload.withAsserts()
      }

      dataErrors.map(x => assert(!x))
      respErrors.map(x => assume(!x))
      assert(!strbError)
      assert(!errorValidWhileReset)
      assume(!errorRespWhileReset)
    }

    def withAssumes(maxStallCycles: Int = 0) = new Area {
      aw.withAssumes()
      aw.withTimeoutAsserts(maxStallCycles)
      w.withAssumes()
      w.withTimeoutAsserts(maxStallCycles)
      b.withAsserts()
      b.withTimeoutAssumes(maxStallCycles)

      when(aw.valid) {
        aw.payload.withAssumes()
      }

      dataErrors.map(x => assume(!x))
      respErrors.map(x => assume(!x))
      assume(!strbError)
      assume(!errorValidWhileReset)
      assert(!errorRespWhileReset)
    }

    def withCovers() = {
      aw.withCovers(2)
      when(aw.fire) {
        aw.payload.withCovers()
      }
      w.withCovers(2)
      when(w.fire) {
        w.payload.withCovers()
      }
      b.withCovers(2)
      when(b.fire) {
        b.payload.withCovers()
      }
    }
  }
}
