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
      val len = UInt(8 bits)
      val size = UInt(3 bits)
      val burst = if (config.useBurst) Bits(2 bits) else null
      val isLockExclusive = if (config.useLock) Bool() else null
      val awDone = Bool()

      val strbs = if (config.useStrb) Vec(Bits(config.bytePerWord bits), maxStrbs) else null
      val count = UInt(9 bits)
      val seenLast = Bool()

      val bResp = Bool()

      def init():FormalAxi4Record = {
        val oRecord = FormalAxi4Record(config, maxStrbs)
        oRecord.assignFromBits(B(0, oRecord.getBitsWidth bits))
        size := U(log2Up(config.bytePerWord), 3 bits)
        if(config.useBurst) burst := B(Axi4.burst.INCR)
        this.assignUnassignedByName(oRecord)
        this
      }

      def assignFromAx(ax: Stream[Axi4Ax]) {
        addr := ax.addr.resized
        isLockExclusive := ax.lock === Axi4.lock.EXCLUSIVE
        if (config.useBurst) burst := ax.burst
        if (config.useLen) len := ax.len
        if (config.useSize) size := ax.size
        if (config.useId) id := ax.id
        awDone := ax.ready
      }

      def assignFromW(w: Stream[Axi4W], selected: FormalAxi4Record) = new Area {
        seenLast := w.last & w.ready
        when(w.ready) { count := selected.count + 1 }.otherwise { count := selected.count }
        if (config.useStrb) {
          for (i <- 0 until maxStrbs) {
            when(selected.count === i) {
              strbs(i) := w.strb
            }.otherwise {
              strbs(i) := selected.strbs(i)
            }
          }
        }
      }

      def assignFromB(b: Stream[Axi4B]) {
        bResp := b.ready
      }

      def checkStrbs(cond: Bool) = new Area {
        val addrStrbMaxMask = (U(config.bytePerWord) - 1).resize(addr.getBitsWidth)
        val strbError = CombInit(False)
        when(cond) {
          val sizeMask = ((U(1) << (U(1) << size)) - 1).resize(config.bytePerWord bits)
          val addrSizeMask = ((U(1) << size) - 1).resize(addr.getBitsWidth)
          val strbsErrors = Vec(Bool(), maxStrbs)
          strbsErrors.map(_ := False)
          for (i <- 0 until maxStrbs) {
            when(i < count) {
              val targetAddress = (addr + (i << size)).resize(addr.getBitsWidth)
              if (config.useBurst) when(burst === Axi4.burst.FIXED) { targetAddress := addr }
              val offset = targetAddress & addrStrbMaxMask & ~addrSizeMask
              val byteLaneMask = (sizeMask << offset).resize(config.bytePerWord bits)
              strbsErrors(i) := (strbs(i) & ~byteLaneMask.asBits).orR
            }
          }
          strbError := strbsErrors.reduce(_ | _)
        }
      }

      def checkLen(): Bool = {
        val realLen = len +^ 1
        val transDoneWithWrongLen = seenLast & realLen =/= count
        val getLimitLenWhileTransfer = !seenLast & realLen === count
        val wrongLen = realLen < count
        awDone & (transDoneWithWrongLen | getLimitLenWhileTransfer | wrongLen)
      }
    }

    import spinal.core.formal._

    val oRecord = FormalAxi4Record(config, maxStrbs).init()

    val histInput = Flow(cloneOf(oRecord))
    histInput.payload := oRecord
    histInput.valid := False
    val hist = HistoryModifyable(histInput, maxBursts)
    hist.io.inStreams.map(_.valid := False)
    hist.io.inStreams.map(_.payload := oRecord)
    hist.io.outStreams.map(_.ready := False)

    val (awExist, awFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid && !x.awDone)
    val (wExist, wFoundId) = hist.io.outStreams.reverse.sFindFirst(x => x.valid && !x.seenLast)
    val (bExist, bFoundId) =
      hist.io.outStreams.reverse.sFindFirst(x => x.valid && !x.bResp && { if (config.useId) b.id === x.id else True })
    val awId = maxBursts - 1 - awFoundId
    val wId = maxBursts - 1 - wFoundId
    val bId = maxBursts - 1 - bFoundId

    val dataErrors = Vec(Bool(), 3)
    dataErrors.map(_ := False)

    when(histInput.valid) { dataErrors(0) := histInput.checkLen() }

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
        }
          .otherwise { histInput.assignFromAx(ax) }
      }
      when(awValid) {
        if (config.useStrb) hist.io.inStreams(awId).strbs.zip(awRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(awId).payload := awRecord
        hist.io.inStreams(awId).valid := awValid
        dataErrors(1) := awRecord.checkLen()
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
      }
      when(wValid) {
        if (config.useStrb) hist.io.inStreams(wId).strbs.zip(wRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(wId).payload := wRecord
        hist.io.inStreams(wId).valid := wValid
        dataErrors(2) := wRecord.checkLen()
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
          if (config.useResp && config.useLock)
            respErrors(2) := selected.awDone & b.resp === Axi4.resp.EXOKAY & !selected.isLockExclusive
        }.otherwise {
          respErrors(0) := True
        }
      }
      when(bValid) {
        if (config.useStrb) hist.io.inStreams(bId).strbs.zip(bRecord.strbs).map { case (x, y) => x := y }
        hist.io.inStreams(bId).payload := bRecord
        hist.io.inStreams(bId).valid := bValid
      }
      val strbsChecker =
        if (config.useStrb) selected.checkStrbs(b.valid & bExist & b.ready & selected.awDone & selected.seenLast)
        else null
    }

    when((aw.valid & !awExist) | (w.valid & !wExist)) {
      histInput.valid := True
    }

    val errors = new Area {
      val reset = ClockDomain.current.isResetActive
      val ValidWhileReset = (reset | past(reset)) & (aw.valid === True | w.valid === True)
      val RespWhileReset = (reset | past(reset)) & (b.valid === True)
      val WrongStrb = if (config.useStrb) responseLogic.strbsChecker.strbError else False
      val WrongResponse = respErrors.reduce(_ | _)
      val DataNumberDonotFitLen = dataErrors.reduce(_ | _)
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

      assert(!errors.DataNumberDonotFitLen)
      assume(!errors.WrongResponse)
      assert(!errors.WrongStrb)
      assert(!errors.ValidWhileReset)
      assume(!errors.RespWhileReset)
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

      assume(!errors.DataNumberDonotFitLen)
      assert(!errors.WrongResponse)
      assume(!errors.WrongStrb)
      assume(!errors.ValidWhileReset)
      assert(!errors.RespWhileReset)
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
