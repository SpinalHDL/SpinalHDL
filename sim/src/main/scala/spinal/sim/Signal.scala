package spinal.sim

abstract class DataType(){
  def width : Int
  def longToRaw64(that : Long) : Long
  def raw64ToLong(that : Long) : Long
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
}

class BitVectorDataType(override val width : Int) extends BoolDataType

class BitsDataType(width : Int) extends BitVectorDataType(width){
  override def longToRaw64(that: Long) = {
    assert(width == 64 || that < (1 << width))
    that
  }

  override def raw64ToLong(that: Long) = {
    that
  }
}

class UIntDataType(width : Int) extends BitVectorDataType(width){
  override def longToRaw64(that: Long) = {
    assert(width == 64 || that < (1 << width))
    that
  }

  override def raw64ToLong(that: Long) = {
    that
  }
}

class SIntDataType(width : Int) extends BitVectorDataType(width){
  ???
  override def longToRaw64(that: Long) = ???
  override def raw64ToLong(that: Long) = ???
}
class Signal(val path : Seq[String],val dataType : DataType) {
  var id = -1
}
