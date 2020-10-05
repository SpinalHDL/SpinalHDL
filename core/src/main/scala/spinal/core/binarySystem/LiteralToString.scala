package spinal.core.binarySystem

object LiteralToString {
  def HexString(value: BigInt, bitSize: Int, align: Boolean = false): String = {
    if(align){
      val hexCount = scala.math.ceil(bitSize / 4.0).toInt
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << bitSize) + value)
      s"%${hexCount}s".format(unsignedValue.toString(16)).replace(' ', '0')
    }else{
      value.toString(16)
    }
  }

  def OctString(value: BigInt, bitSize: Int, align: Boolean = false): String = {
    if(align){
      val octCount = scala.math.ceil(bitSize / 3.0).toInt
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << bitSize) + value)
      s"%${octCount}s".format(unsignedValue.toString(8)).replace(' ', '0')
    }else{
      value.toString(8)
    }
  }

  def BinString(value: BigInt, bitSize: Int, align: Boolean = false): String = {
    if(align){
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << bitSize) + value)
      s"%${bitSize}s".format(unsignedValue.toString(8)).replace(' ', '0')
    }else{
      value.toString(2)
    }
  }
}
