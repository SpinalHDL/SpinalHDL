package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

case class Axi4ReadOnly(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{
  val ar = Stream(Axi4Ar(config))
  val r = Stream(Axi4R(config))

  def readCmd = ar
  def readRsp = r


  def <<(that : Axi4) : Unit = that >> this
  def >> (that : Axi4) : Unit = {
    this.readCmd drive that.readCmd
    that.readRsp drive this.readRsp
  }

  def <<(that : Axi4ReadOnly) : Unit = that >> this
  def >> (that : Axi4ReadOnly) : Unit = {
    this.readCmd drive that.readCmd
    that.readRsp drive this.readRsp
  }

  def arValidPipe() : Axi4ReadOnly = {
    val sink = Axi4ReadOnly(config)
    sink.ar << this.ar.validPipe()
    sink.r  >> this.r
    sink
  }

  def setIdle(): this.type = {
    this.readCmd.setIdle()
    this.readRsp.setBlocked()
    this
  }

  def setBlocked(): this.type = {
    this.readCmd.setBlocked()
    this.readRsp.setIdle()
    this
  }

  def toAxi4(): Axi4 = {
    val ret = Axi4(config)
    this >> ret
  
    ret.writeCmd.setIdle()
    ret.writeData.setIdle()
    ret.writeRsp.setBlocked()

    ret
  }

  def toFullConfig(): Axi4ReadOnly = {
    val ret = Axi4ReadOnly(config.toFullConfig())
    ret << this
    ret
  }

  def pipelined(
    ar: StreamPipe = StreamPipe.NONE,
    r: StreamPipe = StreamPipe.NONE
  ): Axi4ReadOnly = {
    val ret = cloneOf(this)
    ret.ar << this.ar.pipelined(ar)
    ret.r.pipelined(r) >> this.r
    ret
  }

  override def asMaster(): Unit = {
    master(ar)
    slave(r)
  }

  def formalContext(maxBursts: Int = 16) = new Composite(this, "formal") {
    import spinal.core.formal._
    val addrChecker = ar.payload.formalContext()

    val oRecord = FormalAxi4Record(config, 0).init()

    val histInput = Flow(cloneOf(oRecord))
    histInput.payload := oRecord
    histInput.valid := False
    val hist = HistoryModifyable(histInput, maxBursts)
    hist.io.inStreams.map(_.valid := False)
    hist.io.inStreams.map(_.payload := oRecord)
    hist.io.outStreams.map(_.ready := False)

    val (arExist, arId) = hist.findFirst(x => x.valid && !x.axDone)
    val (rExist, rId) =
      hist.findFirst(x => x.valid && !x.seenLast && { if (config.useId) r.id === x.id else True })
    hist.io.outStreams.zipWithIndex.foreach { case (x, i) =>
      when (arExist & x.valid & i < arId) { assert(!x.axDone) }
      when (rExist & x.valid & { if (config.useId) r.id === x.id else True } & i < rId) { assert(x.count === 0 & !x.seenLast ) }
      when (x.valid & x.seenLast) { assert(x.axDone) }
      when (x.valid) {
        assert(x.size <= log2Up(config.bytePerWord))
        assert(!x.payload.checkLen())
      }
    }

    val (rmExist, rmId) =
      hist.findFirst(x => x.valid && x.seenLast && x.axDone)
    when(rmExist) { hist.io.outStreams(rmId).ready := True }
    
    val (undoneExist, undoneId) = hist.findFirst(x => x.valid & !x.axDone)
    val undoneInput = hist.io.outStreams(undoneId)
    val undoneCount = hist.io.outStreams.sCount(x => x.valid & !x.axDone)    
    assert(undoneCount <= 1)
    when(undoneExist) {
      assert(ar.valid & undoneInput.equalToAx(ar.asInstanceOf[Stream[Axi4Ax]]))
    }

    val errors = new Area {
      val reset = ClockDomain.current.isResetActive
      val ValidWhileReset = (reset | past(reset)) & (ar.valid === True)
      val RespWhileReset = (reset | past(reset)) & (r.valid === True)
      val DataNumberDonotFitLen = CombInit(False)
      val NoAddrRequest = CombInit(False)
      val WrongResponseForExAccesss = CombInit(False)
    }
    when(rExist) {
      errors.NoAddrRequest := !hist.io.outStreams(rId).axDone
    }

    val arRecord = CombInit(oRecord)
    val arValid = False
    val addressLogic = new Area {
      when(ar.valid) {
        val ax = ar.asInstanceOf[Stream[Axi4Ax]]
        when(arExist) {
          arRecord := hist.io.outStreams(arId)
          arRecord.allowOverride
          arRecord.assignFromAx(ax)
          arValid := True
        }
          .otherwise { histInput.assignFromAx(ax) }
      }
      when(arValid) {
        hist.io.inStreams(arId).payload := arRecord
        hist.io.inStreams(arId).valid := arValid
      }
    }

    val rRecord = CombInit(oRecord)
    val dataLogic = new Area {
      val selected = CombInit(oRecord)
      when(r.valid) {
        when(rExist) {
          rRecord := hist.io.outStreams(rId)
          selected := hist.io.outStreams(rId)

          rRecord.allowOverride
          rRecord.assignFromR(r, selected)
          hist.io.inStreams(rId).payload := rRecord
          hist.io.inStreams(rId).valid := True

          if (config.useResp && config.useLock)
            errors.WrongResponseForExAccesss := selected.axDone & r.resp === Axi4.resp.EXOKAY & !selected.isLockExclusive
          errors.DataNumberDonotFitLen := rRecord.checkLen()

        }.otherwise { errors.NoAddrRequest := True }
      }
    }
    
    when(ar.valid & !arExist) {
      histInput.valid := True
    }

    def formalAssertsMaster(maxStallCycles: Int = 0) = new Area {
      ar.formalAssertsMaster()
      r.formalAssertsTimeout(maxStallCycles)

      when(ar.valid) {
        addrChecker.formalAsserts()
      }
      assert(!errors.ValidWhileReset)
    }

    def formalAssumesMaster(maxStallCycles: Int = 0) = new Area {
      ar.formalAssumesTimeout(maxStallCycles)
      r.formalAssumesSlave()

      assume(!errors.DataNumberDonotFitLen)
      assume(!errors.NoAddrRequest)
      assume(!errors.WrongResponseForExAccesss)      
      assume(!errors.RespWhileReset)
    }

    def formalAssertsSlave(maxStallCycles: Int = 0) = new Area {
      ar.formalAssertsTimeout(maxStallCycles)
      r.formalAssertsMaster()

      assert(!errors.DataNumberDonotFitLen)
      assert(!errors.NoAddrRequest)
      assert(!errors.WrongResponseForExAccesss)
      assert(!errors.RespWhileReset)
    }

    def formalAssumesSlave(maxStallCycles: Int = 0) = new Area {
      ar.formalAssumesSlave()
      r.formalAssumesTimeout(maxStallCycles)

      when(ar.valid) {
        addrChecker.formalAssumes()
      }
      assume(!errors.ValidWhileReset)
    }

    def formalCovers() = new Area {
      ar.formalCovers(2)
      when(ar.fire) {
        addrChecker.formalCovers()
      }
      r.formalCovers(2)
      when(r.fire) {
        r.payload.formalCovers()
      }
    }
  }
}
