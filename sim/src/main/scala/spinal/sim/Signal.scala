package spinal.sim
import scala.collection.Seq

abstract class DataType(){
  var isMem = false
  def setMem() : this.type = { isMem = true; this }
  def width : Int
  def longToRaw64(that : Long, signal : Signal) : Long
  def raw64ToLong(that : Long, signal : Signal) : Long
  def raw64ToInt(that : Long, signal : Signal) : Int
  def checkIs64(that : Long, signal : Signal) : Unit
  def rangeError(that : Any, signal : Signal): Unit =
    SimError(f"ASSIGNMENT ERROR : ${that} is outside the range of $signal")
  def readIntError(signal : Signal): Unit =
    SimError(f"READ ERROR : $signal is too big to be read with an Int")
  def readLongError(signal : Signal): Unit =
    SimError(f"READ ERROR : $signal is too big to be read with an Long")
  def checkBigIntRange(value : BigInt, signal : Signal): Unit
  def checkLongRange(value : Long, signal : Signal) : Unit
  def checkIntRange(value : Int, signal : Signal) : Unit
}

class BoolDataType extends DataType{
  override def width = 1

  override def longToRaw64(that: Long, signal : Signal) = {
    if((that & ~1l) != 0) rangeError(that, signal)
    that
  }

  override def raw64ToLong(that: Long, signal : Signal) = {
    that
  }

  override def raw64ToInt(that: Long, signal : Signal) = {
    that.toInt
  }

  override def checkIs64(that: Long, signal : Signal) = assert(false)

  override def checkBigIntRange(value: BigInt, signal: Signal): Unit = {
    if(value < 0 || value > 1) rangeError(value, signal)
  }

  override def checkLongRange(that: Long, signal: Signal): Unit = if((that & ~1l) != 0) rangeError(that, signal)
  override def checkIntRange(that: Int, signal: Signal): Unit = if((that & ~1) != 0) rangeError(that, signal)

  override def toString = "Bool"
}

abstract class BitVectorDataType(override val width : Int) extends DataType{
  override def checkIs64(that: Long, signal : Signal) = assert(width >= 64)
}

class BitsDataType(width : Int) extends BitVectorDataType(width) {
  val maxLongValue = if (width >= 63) Long.MaxValue else ((1l << width) - 1)
  val maxIntValue = if (width >= 31) Long.MaxValue else ((1l << width) - 1)

  override def longToRaw64(that: Long, signal : Signal) = {
    if(that < 0 || that > maxLongValue) rangeError(that, signal)
    that
  }

  override def raw64ToLong(that: Long, signal : Signal) = {
    if(width > 64) readLongError(signal)
    that
  }

  override def raw64ToInt(that: Long, signal : Signal) = {
    if(width > 32) readIntError(signal)
    that.toInt
  }

  override def checkBigIntRange(value: BigInt, signal: Signal): Unit = {
    if(value.signum == -1 || value.bitCount > width) rangeError(value, signal)
  }

  override def checkLongRange(that: Long, signal: Signal): Unit =  if(that < 0 || that > maxLongValue) rangeError(that, signal)
  override def checkIntRange(that: Int, signal: Signal): Unit =  if(that < 0 || that > maxIntValue) rangeError(that, signal)


  override def toString = s"Bits[$width bits]"
}

class UIntDataType(width : Int) extends BitVectorDataType(width){
  val maxLongValue = if(width >= 63) Long.MaxValue else ((1l << width)-1)
  val maxIntValue = if(width >= 31) Long.MaxValue else ((1l << width)-1)
  override def longToRaw64(that: Long, signal : Signal) = {
    if(that < 0 || that > maxLongValue) rangeError(that, signal)
    that
  }

  override def raw64ToLong(that: Long, signal : Signal) = {
    if(width > 63) readLongError(signal)
    that
  }

  override def raw64ToInt(that: Long, signal : Signal) = {
    if(width > 31) readIntError(signal)
    that.toInt
  }

  override def checkBigIntRange(value: BigInt, signal: Signal): Unit = {
    if(value.signum == -1 || value.bitCount > width) rangeError(value, signal)
  }
  override def checkLongRange(that: Long, signal: Signal): Unit =  if(that < 0 || that > maxLongValue) rangeError(that, signal)
  override def checkIntRange(that: Int, signal: Signal): Unit =  if(that < 0 || that > maxIntValue) rangeError(that, signal)

  override def toString = s"UInt[$width bits]"
}

class SIntDataType(width : Int) extends BitVectorDataType(width){
  val maxLongValue = if(width >= 63) Long.MaxValue else ((1l << width-1)-1)
  val maxIntValue = if(width >= 31) Long.MaxValue else ((1l << width-1)-1)

  override def longToRaw64(that: Long, signal : Signal) = {
    if(that < -maxLongValue-1 || that > maxLongValue) rangeError(that, signal)
    that & ((maxLongValue << 1) + 1)
  }

  override def raw64ToLong(that: Long, signal : Signal) = {
    if(width > 64) readLongError(signal)
    (that << 64-width) >> 64-width
  }

  override def raw64ToInt(that: Long, signal : Signal) = {
    if(width > 32) readIntError(signal)
    ((that << 64-width) >> 64-width).toInt
  }

  override def checkBigIntRange(value: BigInt, signal: Signal): Unit = {
    if(value.bitLength + (if(value.signum == -1) 1 else 0) > width) rangeError(value, signal)
  }
  override def checkLongRange(that: Long, signal: Signal): Unit =  if(that < -maxLongValue-1 || that > maxLongValue) rangeError(that, signal)
  override def checkIntRange(that: Int, signal: Signal): Unit =  if(that < -maxIntValue-1 || that > maxIntValue) rangeError(that, signal)
  override def toString = s"SInt[$width bits]"
}

class Signal(val path : Seq[String],val dataType : DataType) {
  var id : Int = -1
  override def toString = s"${path.mkString("/")} : $dataType"
  def toVpiAddress = path.mkString(".")
  def toXsiAddress = path.last
  def width = dataType.width
  val hash = toVpiAddress.hashCode()
}


