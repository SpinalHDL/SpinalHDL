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

  def readSignal[T <: Data](value: T, baseAddress: BigInt): Unit = {
    val wordCount = (widthOf(value) - 1) / bus.p.dataWidth + 1
    val valueBits = value.toBits.resize(wordCount*bus.p.dataWidth)
    val words = (0 until wordCount).map(id => valueBits(id * bus.p.dataWidth , bus.p.dataWidth bit))

    for (wordId <- (0 until wordCount)) {
      when(bus.PADDR === baseAddress + wordId*bus.p.dataWidth/8) {
        bus.PRDATA  := words(wordId).autoResize()
      }
    }
  }

  def writeRegister(reg: Bits, baseAddress: BigInt): Unit = {
    assert(reg.isReg,"reg argument must be a Reg")
    val wordCount = (widthOf(reg) - 1) / bus.p.dataWidth + 1
    for (wordId <- (0 until wordCount)) {
      when(bus.PADDR === baseAddress + wordId * bus.p.dataWidth / 8) {
        when(bus.PSEL && bus.PENABLE && bus.PWRITE) {
          reg(wordId * bus.p.dataWidth, Math.min(bus.p.dataWidth, widthOf(reg) - wordId * bus.p.dataWidth) bit) := bus.PWDATA.autoResize()
        }
      }
    }
  }

  def writeOnlyReg[T <: Data](that: T, baseAddress: BigInt): Unit = {
    val reg = Reg(Bits(widthOf(that) bit))
    writeRegister(reg,baseAddress)
    that.assignFromBits(reg)
  }
  def writeOnlyRegOf[T <: Data](dataType: T, baseAddress: BigInt): T = {
    val ret = dataType.clone()
    writeOnlyReg(ret,baseAddress)
    ret
  }

  def writeReadReg[T <: Data](that: T, baseAddress: BigInt): Unit = {
    val reg = Reg(Bits(widthOf(that) bit))
    writeRegister(reg,baseAddress)
    readSignal(reg,baseAddress)
    return that.assignFromBits(reg)
  }



  def writeStream(baseAddress: BigInt): Stream[Bits] = {
    val ret = Stream(bus.PWDATA)
    ret.valid := False
    ret.data := bus.PWDATA

    when(bus.PADDR === baseAddress) {
      when(bus.PSEL && bus.PENABLE && bus.PWRITE) {
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

    when(bus.PADDR === baseAddress) {
      bus.PRDATA := fragmentStream.fragment
      when(bus.PSEL && bus.PENABLE && ! bus.PWRITE) {
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