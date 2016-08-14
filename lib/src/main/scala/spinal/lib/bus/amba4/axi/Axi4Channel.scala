package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import scala.Predef.assert


/**
 * Definition of the Write/Read address channel
 * @param config Axi4 configuration class
 */
class Axi4Ax(config: Axi4Config) extends Bundle {
  val addr   = UInt(config.addressWidth bits)
  val id     = if(config.useId)     UInt(config.idWidth bits)   else null
  val region = if(config.useRegion) Bits(4 bits)                else null
  val len    = if(config.useLen)    UInt(8 bits)  else null
  val size   = if(config.useSize)   UInt(3 bits)                else null
  val burst  = if(config.useBurst)  Bits(2 bits)                else null
  val lock   = if(config.useLock)   Bits(1 bits)                else null
  val cache  = if(config.useCache)  Bits(4 bits)                else null
  val qos    = if(config.useQos)    Bits(4 bits)                else null
  val user   = if(config.useUser)   Bits(config.userWidth bits) else null
  val prot   = Bits(3 bits)

  import Axi4.burst._

  def setBurstFIXED(): Unit = {assert(config.useBurst); burst := FIXED}
  def setBurstWRAP() : Unit = {assert(config.useBurst); burst := WRAP}
  def setBurstINCR() : Unit = {assert(config.useBurst); burst := INCR}

  def setSize(sizeBurst :UInt) : Unit = if(config.useBurst) size := sizeBurst
  def setLock(lockType :Bits) : Unit = if(config.useLock) lock := lockType
  def setCache(cacheType : Bits) : Unit = if (config.useCache ) cache := cacheType

}


case class Axi4Aw(config: Axi4Config) extends Axi4Ax(config)
case class Axi4Ar(config: Axi4Config) extends Axi4Ax(config)
case class Axi4Arw(config: Axi4Config) extends Axi4Ax(config){
  val wr = Bool
}


/**
 * Definition of the Write data channel
 * @param config Axi4 configuration class
 */
case class Axi4W(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val strb = if(config.useStrb) Bits(config.bytePerWord bits) else null
  val user = if(config.useUser) Bits(config.userWidth bits)     else null
  val last = if(config.useLen)  Bool                            else null

  def setStrb() : Unit = if(config.useStrb) strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = if(config.useStrb) strb := bytesLane
}


/**
 * Definition of the Write response channel
 * @param config Axi4 configuration class
 */
case class Axi4B(config: Axi4Config) extends Bundle {
  val id   = if(config.useId)   UInt(config.idWidth bits)   else null
  val resp = if(config.useResp) Bits(2 bits)                else null
  val user = if(config.useUser) UInt(config.userWidth bits) else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Unit = resp === OKAY
  def isEXOKAY() : Unit = resp === EXOKAY
  def isSLVERR() : Unit = resp === SLVERR
  def isDECERR() : Unit = resp === DECERR
}


/**
 * Definition of the Read Data channel
 * @param config Axi4 configuration class
 */
case class Axi4R(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val id   = if(config.useId)     UInt(config.idWidth bits)   else null
  val resp = if(config.useResp) Bits(2 bits)               else null
  val last = if(config.useLen)  Bool                       else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Unit = resp === OKAY
  def isEXOKAY() : Unit = resp === EXOKAY
  def isSLVERR() : Unit = resp === SLVERR
  def isDECERR() : Unit = resp === DECERR
}









object Axi4Aw{
  implicit class StreamPimper(stream : Stream[Axi4Aw]) {
    def drive(sink: Stream[Axi4Aw]): Unit = {
      stream >> sink
      assert(stream.config.idWidth <= sink.config.idWidth, s"$this idWidth > $sink idWidth")

      sink.id.removeAssignements()
      sink.id := stream.id.resized
    }
  }
}



object Axi4Ar{
  implicit class StreamPimper(stream : Stream[Axi4Ar]){
    def drive(sink : Stream[Axi4Ar]): Unit ={
      stream >> sink
      assert(stream.config.idWidth <= sink.config.idWidth,s"$this idWidth > $sink idWidth")

      sink.id.removeAssignements()
      sink.id := stream.id.resized
    }


    case class Axi4ArUnburstified(axiConfig : Axi4Config) extends Bundle {
      val addr   = UInt(axiConfig.addressWidth bits)
      val id     = if(axiConfig.useId)     UInt(axiConfig.idWidth bits)   else null
      val region = if(axiConfig.useRegion) Bits(4 bits)                else null
      val size   = if(axiConfig.useSize)   UInt(3 bits)                else null
      val burst  = if(axiConfig.useBurst)  Bits(2 bits)                else null
      val lock   = if(axiConfig.useLock)   Bits(1 bits)                else null
      val cache  = if(axiConfig.useCache)  Bits(4 bits)                else null
      val qos    = if(axiConfig.useQos)    Bits(4 bits)                else null
      val user   = if(axiConfig.useUser)   Bits(axiConfig.userWidth bits) else null
      val prot   = Bits(3 bits)
    }

    def unburstify : Stream[Fragment[Axi4ArUnburstified]] = {
      case class State() extends Bundle{
        val busy = Bool
        val len = UInt(8 bits)
        val beat = UInt(8 bits)
        val transaction = Axi4ArUnburstified(stream.config)
      }
      val result = Stream Fragment(Axi4ArUnburstified(stream.config))
      val stateNext = State()
      val state = RegNext(stateNext)
      val doResult = Bool

      stateNext := state
      doResult := state.busy

      val addrIncrRange = (Math.min(11,stream.config.addressWidth-1) downto 0)
      stateNext.transaction.addr(addrIncrRange) := Axi4.incr(
        address = state.transaction.addr(addrIncrRange),
        burst = state.transaction.burst,
        len = state.len,
        size = state.transaction.size,
        bytePerWord = stream.config.bytePerWord
      )

      when(result.ready){
        stateNext.beat := state.beat - 1
      }

      when(stream.fire){
        stateNext.busy := True
        stateNext.beat := stream.len
        stateNext.len  := stream.len
        doResult := True
        stateNext.transaction.assignSomeByName(stream.payload)
      }

      when(stateNext.beat === 0){
        stateNext.busy := False
      }

      stream.ready := !state.busy & result.ready

      result.valid := doResult
      result.last := stateNext.beat === 0
      result.fragment := stateNext.transaction
      result
    }
  }
}



object Axi4W{
  implicit class StreamPimper(stream : Stream[Axi4W]) {
    def drive(sink: Stream[Axi4W]): Unit = {
      stream >> sink
    }
  }
}


object Axi4B{
  implicit class StreamPimper(stream : Stream[Axi4B]) {
    def drive(sink: Stream[Axi4B]): Unit = {
      stream >> sink
      assert(sink.config.idWidth <= stream.config.idWidth,s"$sink idWidth > $stream idWidth")

      sink.id.removeAssignements()
      sink.id := stream.id.resized
    }
  }
}

object Axi4R{
  implicit class StreamPimper(stream : Stream[Axi4R]) {
    def drive(sink: Stream[Axi4R]): Unit = {
      stream >> sink
      assert(sink.config.idWidth <= stream.config.idWidth,s"$sink idWidth > $stream idWidth")

      sink.id.removeAssignements()
      sink.id := stream.id.resized
    }
  }
}
