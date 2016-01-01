package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._

class Apb3Config(val addressWidth: Int, val dataWidth: Int)

class Apb3Slave(val p: Apb3Config) extends Bundle with IMasterSlave {
  val PADDR    = UInt(p.addressWidth bit)
  val PSEL    = Bool
  val PENABLE = Bool
  val PREADY  = Bool
  val PWRITE  = Bool
  val PWDATA  = Bits(p.dataWidth bit)
  val PRDATA  = Bits(p.dataWidth bit)

  override def asMaster(): Apb3Slave.this.type = {
    out(PADDR)
    out(PSEL)
    out(PENABLE)
    in(PREADY)
    out(PWRITE)
    out(PWDATA)
    in(PRDATA)
    this
  }

  def fire : Bool = PSEL && PENABLE && PREADY

  override def asSlave(): Apb3Slave.this.type = asMaster().flip
}


class Apb3SlaveController(bus: Apb3Slave) {

  bus.PREADY := True
  bus.PRDATA := 0

  private val writeAccess = bus.PSEL && bus.PENABLE && bus.PWRITE
  private val readAccess = bus.PSEL && bus.PENABLE && !bus.PWRITE
  private val addressAccess = scala.collection.mutable.Map[BigInt,Bool]()

  def isAccessing(address : BigInt): Bool = addressAccess.getOrElseUpdate(address,bus.PADDR === address)

  def read[T <: Data](that: T, baseAddress: BigInt): Unit = {
    val wordCount = (widthOf(that) - 1) / bus.p.dataWidth + 1
    val valueBits = that.toBits.resize(wordCount*bus.p.dataWidth)
    val words = (0 until wordCount).map(id => valueBits(id * bus.p.dataWidth , bus.p.dataWidth bit))

    for (wordId <- (0 until wordCount)) {
      when(isAccessing(baseAddress + wordId*bus.p.dataWidth/8)) {
        bus.PRDATA  := words(wordId).resized
      }
    }
  }

  def write[T <: Data](that: T, baseAddress: BigInt): Unit = {
    assert(that.isReg,"reg argument must be a Reg")
    val wordCount = (widthOf(that) - 1) / bus.p.dataWidth + 1
    for (wordId <- (0 until wordCount)) {
      when(isAccessing(baseAddress + wordId * bus.p.dataWidth / 8)) {
        when(writeAccess) {
          that.assignFromBits(bus.PWDATA.resized,wordId * bus.p.dataWidth, Math.min(bus.p.dataWidth, widthOf(that) - wordId * bus.p.dataWidth) bit)
        }
      }
    }
  }

//  def writeOnlyReg[T <: Data](that: T, baseAddress: BigInt): Unit = {
//    val reg = Reg(Bits(widthOf(that) bit))
//    write(reg,baseAddress)
//    that.assignFromBits(reg)
//  }
  def writeOnlyRegOf[T <: Data](dataType: T, baseAddress: BigInt): T = {
    val ret = Reg(dataType)
    write(ret,baseAddress)
    ret
  }

  def writeReadReg[T <: Data](that: T, baseAddress: BigInt): T = {
    val reg = Reg(that)
    write(reg,baseAddress)
    read(reg,baseAddress)
    reg
  }



  def writeStream(baseAddress: BigInt): Stream[Bits] = {
    val ret = Stream(bus.PWDATA)
    ret.valid := False
    ret.data := bus.PWDATA

    when(isAccessing(baseAddress)) {
      when(writeAccess) {
        ret.valid := True
        bus.PREADY := ret.ready
      }
    }
    return ret;
  }



  def writeStreamOf[T <: Data](dataType: T, baseAddress: BigInt): Stream[T] = {
    val wordCount = (widthOf(dataType) - 1) / bus.p.dataWidth + 1
    val stream = writeStream(baseAddress)
    val streamFragment = stream.addFragmentLast(Counter(wordCount,stream.fire) === wordCount-1)
    return streamFragment.toStreamOf(dataType)
  }

  def writeStream[T <: Data](that: Stream[T], baseAddress: BigInt) : Unit = {
    that << writeStreamOf(that.dataType,baseAddress)
  }

  def readStream[T <: Data](transactionStream : Stream[T],baseAddress: BigInt): Unit = {
    val fragmentStream = transactionStream.fragmentTransaction(bus.p.dataWidth)
    fragmentStream.ready := False

    when(isAccessing(baseAddress)) {
      bus.PRDATA := fragmentStream.fragment
      when(readAccess) {
        fragmentStream.ready := True
        bus.PREADY := fragmentStream.valid
      }
    }
  }

  def readStreamOf[T <: Data](dataType : T,baseAddress: BigInt): Stream[T] = {
    val stream = Stream(dataType)
    readStream(stream,baseAddress)
    return stream
  }
}