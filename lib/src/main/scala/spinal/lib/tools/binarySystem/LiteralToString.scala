package spinal.lib.tools.binarySystem

object LiteralToString {
  def HexString(value: BigInt, alignBits: Int = 0): String = {
    if(alignBits > 0){
      val hexCount = scala.math.ceil(alignBits / 4.0).toInt
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << alignBits) + value)
      s"%${hexCount}s".format(unsignedValue.toString(16)).replace(' ', '0')
    }else{
      value.toString(16)
    }
  }

  def OctString(value: BigInt, alignBits: Int = 0): String = {
    if(alignBits > 0){
      val octCount = scala.math.ceil(alignBits / 3.0).toInt
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << alignBits) + value)
      s"%${octCount}s".format(unsignedValue.toString(8)).replace(' ', '0')
    }else{
      value.toString(8)
    }
  }

  def BinString(value: BigInt, alignBits: Int = 0): String = {
    if(alignBits > 0){
      val unsignedValue = if (value >= 0) value else ((BigInt(1) << alignBits) + value)
      s"%${alignBits}s".format(unsignedValue.toString(2)).replace(' ', '0')
    }else{
      value.toString(2)
    }
  }
}
