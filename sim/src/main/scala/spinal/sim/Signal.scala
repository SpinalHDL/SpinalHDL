package spinal.sim

abstract class DataType(){
  def width : Int
  def longToRaw64(that : Long) : Long
  def raw64ToLong(that : Long) : Long
  def raw64ToInt(that : Long) : Int
}

class BoolDataType extends DataType{
  override def width = 1

  override def longToRaw64(that: Long) = {
    assert(that < 2)
    that
  }

  override def raw64ToLong(that: Long) = {
    that
  }

  override def raw64ToInt(that: Long) = {
    that.toInt
  }
}

abstract class BitVectorDataType(override val width : Int) extends DataType

class BitsDataType(width : Int) extends BitVectorDataType(width) {
  val maxLongValue = if (width >= 63) Long.MaxValue else ((1l << width) - 1)

  override def longToRaw64(that: Long) = {
    assert(that >= 0 && that <= maxLongValue)
    that
  }

  override def raw64ToLong(that: Long) = {
    assert(width <= 63)
    that
  }

  override def raw64ToInt(that: Long) = {
    assert(width <= 31)
    that.toInt
  }
}

class UIntDataType(width : Int) extends BitVectorDataType(width){
  val maxLongValue = if(width >= 63) Long.MaxValue else ((1l << width)-1)
  override def longToRaw64(that: Long) = {
    assert(that >= 0 && that <= maxLongValue)
    that
  }

  override def raw64ToLong(that: Long) = {
    assert(width <= 63)
    that
  }

  override def raw64ToInt(that: Long) = {
    assert(width <= 31)
    that.toInt
  }
}

class SIntDataType(width : Int) extends BitVectorDataType(width){
  val maxLongValue = if(width >= 63) Long.MaxValue else ((1l << width-1)-1)
  override def longToRaw64(that: Long) = {
    assert(that >= -maxLongValue-1 && that <= maxLongValue)
    that
  }

  override def raw64ToLong(that: Long) = {
    assert(width <= 64)
    (that << 64-width) >> 64-width
  }

  override def raw64ToInt(that: Long) = {
    assert(width <= 32)
    ((that << 64-width) >> 64-width).toInt
  }

}
class Signal(val path : Seq[String],val dataType : DataType) {
  var id = -1
}
