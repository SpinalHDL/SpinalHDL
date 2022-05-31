package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._



/**
 * Definition of the Write/Read address channel
 * @param config Axi4 configuration class
 */
class Axi4Ax(val config: Axi4Config,val userWidth : Int) extends Bundle {
  val addr   = UInt(config.addressWidth bits)
  val id     = if(config.useId)     UInt(config.idWidth bits)   else null
  val region = if(config.useRegion) Bits(4 bits)                else null
  val len    = if(config.useLen)    UInt(8 bits)                else null
  val size   = if(config.useSize)   UInt(3 bits)                else null
  val burst  = if(config.useBurst)  Bits(2 bits)                else null
  val lock   = if(config.useLock)   Bits(1 bits)                else null
  val cache  = if(config.useCache)  Bits(4 bits)                else null
  val qos    = if(config.useQos)    Bits(4 bits)                else null
  val user   = if(userWidth >= 0)   Bits(userWidth bits)        else null
  val prot   = if(config.useProt)   Bits(3 bits)                else null

  import Axi4.burst._

  def setBurstFIXED(): Unit = {assert(config.useBurst); burst := FIXED}
  def setBurstWRAP() : Unit = {assert(config.useBurst); burst := WRAP}
  def setBurstINCR() : Unit = {assert(config.useBurst); burst := INCR}

  def isINCR() = if(config.useBurst) burst === INCR else True
  def isFIXED() = if(config.useBurst) burst === FIXED else False

  def setSize(sizeBurst :UInt) : Unit = if(config.useBurst) size := sizeBurst
  def setLock(lockType :Bits) : Unit = if(config.useLock) lock := lockType
  def setCache(cacheType : Bits) : Unit = if (config.useCache ) cache := cacheType

  override def clone: this.type = new Axi4Ax(config,userWidth).asInstanceOf[this.type]


  def formalContext() = new Area {
    import spinal.core.formal._

    val maxSize = log2Up(config.bytePerWord)
    val formalLen = if (config.useLen) len else U(0, 8 bits)
    val formalSize = if (config.useSize) size else U(maxSize, 3 bits)
    val endAddr = addr + ((formalLen +^ 1) << formalSize) - 1
    val in4KBoundary = endAddr(config.addressWidth - 1 downto 12) === addr(config.addressWidth - 1 downto 12)

    val addrAlignMask = ((U(1) << formalSize) - 1).resize(7 bits)
    val addrAlignedToSize = (addr(6 downto 0) & addrAlignMask) === 0
    val validWrapLen = Seq(2, 4, 8, 16).map(formalLen === _).reduce(_ || _)
    val validCache = if (config.useCache) !Seq(4, 5, 8, 9, 0xc, 0xd).map(cache === _).reduce(_ || _) else null

    val errors = new Area {
      val UseReservedBurst = CombInit(False)
      val AccessOutOf4KBound = Bool()
      val WrapAddressNotAligned = CombInit(False)
      val WrapInvalidLen = CombInit(False)
      val FixedInvalidLen = CombInit(False)
      val SizeOutOfRange = CombInit(formalSize > maxSize)
      val CacheInvalid = Bool()
      val ExclusiveInvalidLen = CombInit(False)
      val ExclusiveInvalidCache = CombInit(False)
    }

    if (config.useBurst) {
      errors.AccessOutOf4KBound := False
      errors.UseReservedBurst := burst === RESERVED
      switch(burst) {
        is(INCR) {
          if (config.addressWidth > 12) errors.AccessOutOf4KBound := !in4KBoundary
        }
        is(WRAP) {
          if (config.useSize) errors.WrapAddressNotAligned := !addrAlignedToSize
          if (config.useLen) errors.WrapInvalidLen := !validWrapLen
        }
        is(FIXED) {
          if (config.useLen) errors.FixedInvalidLen := len(7 downto 4) =/= 0 // len <= 16
        }
      }
    } else {
      errors.AccessOutOf4KBound := !in4KBoundary
    }

    if (config.useCache) errors.CacheInvalid := !validCache else errors.CacheInvalid := False
    if (config.useLock) {
      if (config.useLen) when(lock === Axi4.lock.EXCLUSIVE) { errors.ExclusiveInvalidLen := len(7 downto 4) =/= 0 }
      if (config.useCache) when(lock === Axi4.lock.EXCLUSIVE) {
        errors.ExclusiveInvalidCache := cache(3 downto 2) =/= 0
      }
    }

    def withAsserts() = {
      errors.foreachReflectableNameables(x => x match { case y: Bool => assert(!y); case _ => })
    }

    def withAssumes() = {
      errors.foreachReflectableNameables(x => x match { case y: Bool => assume(!y); case _ => })
    }

    def withCovers() = {
      // Unaligned burst access.
      if (config.useSize && config.useBurst) {
        cover(size === U(Axi4.size.BYTE_2) && addr(0) === True && burst === FIXED)
        cover(size === U(Axi4.size.BYTE_2) && addr(1) === True && burst === INCR)
      }

      if (config.useBurst) {
        cover(burst === FIXED)
        cover(burst === INCR)
        cover(burst === WRAP)
      }

      if (config.useLock) {
        cover(lock === Axi4.lock.NORMAL)
        cover(lock === Axi4.lock.EXCLUSIVE)
      }

      if (config.useCache) {
        Seq(0, 1, 2, 3, 6, 7, 0xa, 0xb, 0xe, 0xf).map(x => cover(cache === x))
        if (config.useLock) cover(lock === Axi4.lock.EXCLUSIVE && cache === M"--0-")
      }

      if (config.useProt) {
        cover(prot === M"--0")
        cover(prot === M"--1")
        cover(prot === M"-0-")
        cover(prot === M"-1-")
        cover(prot === M"0--")
        cover(prot === M"1--")
      }
    }
  }
}


class Axi4Aw(config: Axi4Config) extends Axi4Ax(config, config.awUserWidth){
  override def clone: this.type = new Axi4Aw(config).asInstanceOf[this.type]
}
class Axi4Ar(config: Axi4Config) extends Axi4Ax(config, config.arUserWidth){
  override def clone: this.type = new Axi4Ar(config).asInstanceOf[this.type]
}
class Axi4Arw(config: Axi4Config) extends Axi4Ax(config, config.arwUserWidth){
  val write = Bool()
  override def clone: this.type = new Axi4Arw(config).asInstanceOf[this.type]
}


/**
 * Definition of the Write data channel
 * @param config Axi4 configuration class
 */
case class Axi4W(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val strb = if(config.useStrb)  Bits(config.bytePerWord bits) else null
  val user = if(config.useWUser) Bits(config.wUserWidth bits)  else null
  val last = if(config.useLast)  Bool()                        else null

  def setStrb() : Unit = if(config.useStrb) strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = if(config.useStrb) strb := bytesLane

  def withCovers() = {
    if(config.useLast) cover(last === True)
    if(config.useStrb) {
      val fullStrb = (1 << config.bytePerWord) - 1
      cover(strb === fullStrb)
      cover(strb =/= fullStrb)
    }
  }
}


/**
 * Definition of the Write response channel
 * @param config Axi4 configuration class
 */
case class Axi4B(config: Axi4Config) extends Bundle {
  val id   = if(config.useId)    UInt(config.idWidth bits)    else null
  val resp = if(config.useResp)  Bits(2 bits)                 else null
  val user = if(config.useBUser) Bits(config.bUserWidth bits) else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Bool = resp === OKAY
  def isEXOKAY() : Bool = resp === EXOKAY
  def isSLVERR() : Bool = resp === SLVERR
  def isDECERR() : Bool = resp === DECERR

  def withCovers() = {
    if(config.useResp) {
      Seq(OKAY, SLVERR, DECERR, EXOKAY).map(x => cover(resp === x))
    }
  }
}


/**
 * Definition of the Read Data channel
 * @param config Axi4 configuration class
 */
case class Axi4R(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val id   = if(config.useId)    UInt(config.idWidth bits)    else null
  val resp = if(config.useResp)  Bits(2 bits)                 else null
  val last = if(config.useLast)  Bool()                       else null
  val user = if(config.useRUser) Bits(config.rUserWidth bits) else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Bool = resp === OKAY
  def isEXOKAY() : Bool = resp === EXOKAY
  def isSLVERR() : Bool = resp === SLVERR
  def isDECERR() : Bool = resp === DECERR

  def withCovers() = {
    if(config.useResp) {
      Seq(OKAY, SLVERR, DECERR, EXOKAY).map(x => cover(resp === x))
    }
  }
}






class Axi4AxUnburstified(val config : Axi4Config, userWidth : Int) extends Bundle {
  val addr   = UInt(config.addressWidth bits)
  val id     = if(config.useId)     UInt(config.idWidth bits) else null
  val region = if(config.useRegion) Bits(4 bits)              else null
  val size   = if(config.useSize)   UInt(3 bits)              else null
  val burst  = if(config.useBurst)  Bits(2 bits)              else null
  val lock   = if(config.useLock)   Bits(1 bits)              else null
  val cache  = if(config.useCache)  Bits(4 bits)              else null
  val qos    = if(config.useQos)    Bits(4 bits)              else null
  val user   = if(userWidth >= 0)   Bits(userWidth bits)      else null
  val prot   = if(config.useProt)   Bits(3 bits)              else null
}

object Axi4AxUnburstified{
  def unburstify[X <: Axi4Ax,Y <: Axi4AxUnburstified](stream : Stream[X], outPayloadType : Y) : Stream[Fragment[Y]] = {
    case class State() extends Bundle{
      val busy = Bool()
      val len = UInt(8 bits)
      val beat = UInt(8 bits)
      val transaction = cloneOf(outPayloadType)

      override def clone: State.this.type = new State().asInstanceOf[this.type]
    }
    val area = new Area {
      val result = Stream Fragment (outPayloadType)
      val doResult = Bool()
      val addrIncrRange = (Math.min(11, stream.payload.config.addressWidth - 1) downto 0)

      val buffer = new Area{
        val valid       = RegInit(False)
        val len         = Reg(UInt(8 bits))
        val beat        = Reg(UInt(8 bits))
        val transaction = Reg(cloneOf(outPayloadType))
        val last        = beat === 1
        val address     = Axi4.incr(
          address = transaction.addr,
          burst   = if(stream.config.useBurst) transaction.burst else Axi4.burst.INCR,
          len     = len,
          size    = if(stream.config.useSize) transaction.size else U(log2Up(stream.config.bytePerWord)),
          bytePerWord = stream.config.bytePerWord
        )

        when(result.ready) {
          beat := beat - 1
          transaction.addr(addrIncrRange) := address(addrIncrRange)
          when(last){
            valid := False
          }
        }
      }

      stream.ready := False
      when(buffer.valid){
        result.valid    := True
        result.last     := buffer.last
        result.fragment := buffer.transaction
        result.addr.removeAssignments()
        result.addr     := buffer.address
      }otherwise{
        stream.ready    := result.ready
        result.valid    := stream.valid
        result.fragment.assignSomeByName(stream.payload)
        result.last := True
        if(stream.config.useLen) {
          when(stream.len =/= 0) {
            result.last := False
            when(result.ready){
              buffer.valid := stream.valid
              buffer.transaction.assignSomeByName(stream.payload)
              buffer.beat := stream.len
              buffer.len := stream.len
            }
          }
        }
      }
    }.setWeakName("unburstify")
    area.result
  }
}

class Axi4ArUnburstified(axiConfig : Axi4Config) extends Axi4AxUnburstified(axiConfig, axiConfig.arUserWidth){
  override def clone: this.type = new Axi4ArUnburstified(axiConfig).asInstanceOf[this.type]
}
class Axi4AwUnburstified(axiConfig : Axi4Config) extends Axi4AxUnburstified(axiConfig, axiConfig.awUserWidth){
  override def clone: this.type = new Axi4AwUnburstified(axiConfig).asInstanceOf[this.type]
}
class Axi4ArwUnburstified(axiConfig : Axi4Config) extends Axi4AxUnburstified(axiConfig, axiConfig.arwUserWidth){
  val write = Bool()
  override def clone: this.type = new Axi4ArwUnburstified(axiConfig).asInstanceOf[this.type]
}

object Axi4ArUnburstified{
  def apply(axiConfig : Axi4Config) = new Axi4ArUnburstified(axiConfig)
}
object Axi4AwUnburstified{
  def apply(axiConfig : Axi4Config) = new Axi4AwUnburstified(axiConfig)
}
object Axi4ArwUnburstified{
  def apply(axiConfig : Axi4Config) = new Axi4ArwUnburstified(axiConfig)
}


object Axi4Priv{

  def driveWeak[T <: Data](source : Bundle,sink : Bundle, by : T,to : T,defaultValue : () => T,allowResize : Boolean,allowDrop : Boolean) : Unit = {
    (to != null,by != null) match {
      case (false,false) =>
      case (true,false) => if(defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because this first doesn't has the corresponding pin")
      case (false,true) => if(!allowDrop) LocatedPendingError(s"$by can't drive $sink because this last one doesn't has the corresponding pin")
      case (true,true) => to := (if(allowResize) by.resized else by)
    }
  }

  def driveAx[T <: Axi4Ax](stream: Stream[T],sink: Stream[T]): Unit = {
    sink.arbitrationFrom(stream)
    assert(stream.config.idWidth <= sink.config.idWidth, s"Expect $stream idWidth=${stream.config.idWidth} <= $sink idWidth=${sink.config.idWidth}")
    assert(stream.config.addressWidth >= sink.config.addressWidth, s"Expect $stream addressWidth=${stream.config.addressWidth} >= $sink addressWidth=${sink.config.addressWidth}")

    sink.addr := stream.addr.resized
    driveWeak(stream,sink,stream.id,sink.id,() => U(sink.id.range -> false),true,false)
    driveWeak(stream,sink,stream.region,sink.region,() => B(sink.region.range -> false),false,true)
    driveWeak(stream,sink,stream.len,sink.len,() => U(sink.len.range -> false),false,false)
    driveWeak(stream,sink,stream.size,sink.size,() => U(log2Up(sink.config.dataWidth/8)),false,false)
    driveWeak(stream,sink,stream.burst,sink.burst,() => Axi4.burst.INCR,false,false)
    driveWeak(stream,sink,stream.lock,sink.lock,() => Axi4.lock.NORMAL,false,true)
    driveWeak(stream,sink,stream.cache,sink.cache,() => B"0000",false,true)
    driveWeak(stream,sink,stream.qos,sink.qos,() => B"0000",false,true)
    driveWeak(stream,sink,stream.user,sink.user,() => B(sink.user.range -> false),true,true)
    driveWeak(stream,sink,stream.prot,sink.prot,() => B"010",false,true)
  }

}


object Axi4Aw{
  def apply(config: Axi4Config) = new Axi4Aw(config)

  implicit class StreamPimper(stream : Stream[Axi4Aw]) {
    def unburstify : Stream[Fragment[Axi4AwUnburstified]] = {
      Axi4AxUnburstified.unburstify(stream, Axi4AwUnburstified(stream.config))
    }

    def drive(sink: Stream[Axi4Aw]): Unit = Axi4Priv.driveAx(stream,sink)
  }
}



object Axi4Ar{
  def apply(config: Axi4Config) = new Axi4Ar(config)
  implicit class StreamPimper(stream : Stream[Axi4Ar]){
    def unburstify : Stream[Fragment[Axi4ArUnburstified]] = {
      Axi4AxUnburstified.unburstify(stream, Axi4ArUnburstified(stream.config))
    }

    def drive(sink : Stream[Axi4Ar]): Unit = Axi4Priv.driveAx(stream,sink)
  }
}


object Axi4Arw{
  def apply(config: Axi4Config) = new Axi4Arw(config)

  implicit class StreamPimper(stream : Stream[Axi4Arw]) {
    def unburstify : Stream[Fragment[Axi4ArwUnburstified]] = {
      Axi4AxUnburstified.unburstify(stream,Axi4ArwUnburstified(stream.config))
    }

    def drive(sink : Stream[Axi4Arw]): Unit ={
      Axi4Priv.driveAx(stream,sink)
      sink.write := stream.write
    }
  }
}


object Axi4W{
  implicit class StreamPimper(stream : Stream[Axi4W]) {
    def drive(sink: Stream[Axi4W]): Unit = {
      sink.arbitrationFrom(stream)
      sink.data := stream.data
      Axi4Priv.driveWeak(stream,sink,stream.strb,sink.strb,() => B(sink.strb.range -> true),false,false)
      Axi4Priv.driveWeak(stream,sink,stream.user,sink.user,() => B(sink.user.range -> false),false,true)
      Axi4Priv.driveWeak(stream,sink,stream.last,sink.last,null,false,true)
    }
  }
}


object Axi4B{
  implicit class StreamPimper(stream : Stream[Axi4B]) {
    def drive(sink: Stream[Axi4B]): Unit = {
      assert(stream.config.idWidth >= sink.config.idWidth, s"Expect $stream idWidth=${stream.config.idWidth} >= $sink idWidth=${sink.config.idWidth}")
      sink.arbitrationFrom(stream)

      Axi4Priv.driveWeak(stream,sink,stream.id,sink.id,null,true,true)
      Axi4Priv.driveWeak(stream,sink,stream.resp,sink.resp,() => Axi4.resp.OKAY,false,true)
      Axi4Priv.driveWeak(stream,sink,stream.user,sink.user,() => B(sink.user.range -> false),false,true)
    }
  }
}

object Axi4R{
  implicit class StreamPimper(stream : Stream[Axi4R]) {
    def drive(sink: Stream[Axi4R]): Unit = {
      assert(stream.config.idWidth >= sink.config.idWidth, s"Expect $stream idWidth=${stream.config.idWidth} >= $sink idWidth=${sink.config.idWidth}")

      sink.arbitrationFrom(stream)
      sink.data := stream.data
      Axi4Priv.driveWeak(stream,sink,stream.last,sink.last,null,false,true)
      Axi4Priv.driveWeak(stream,sink,stream.id,sink.id,null,true,true)
      Axi4Priv.driveWeak(stream,sink,stream.resp,sink.resp,() => Axi4.resp.OKAY,false,true)
      Axi4Priv.driveWeak(stream,sink,stream.user,sink.user,() => B(sink.user.range -> false),false,true)
    }
  }
}


case class FormalAxi4Record(val config: Axi4Config, maxStrbs: Int) extends Bundle {
  val addr = UInt(7 bits)
  val id = if (config.useId) UInt(config.idWidth bits) else null
  val len = UInt(8 bits)
  val size = UInt(3 bits)
  val burst = if (config.useBurst) Bits(2 bits) else null
  val isLockExclusive = if (config.useLock) Bool() else null
  val axDone = Bool()

  val strbs = if (config.useStrb) Vec(Bits(config.bytePerWord bits), maxStrbs) else null
  val count = UInt(9 bits)
  val seenLast = Bool()

  val responsed = Bool()

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
    axDone := ax.ready
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

  def assignFromR(r: Stream[Axi4R], selected: FormalAxi4Record) = new Area {
    seenLast := r.last & r.ready
    when(r.ready) { count := selected.count + 1 }.otherwise { count := selected.count }
    responsed := r.ready
  }

  def assignFromB(b: Stream[Axi4B]) {
    responsed := b.ready
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
    axDone & (transDoneWithWrongLen | getLimitLenWhileTransfer | wrongLen)
  }
}
