package spinal.lib.tools.binarySystem

object LiteralToBinInts {
  def listPad(x: List[Int], size: Int): List[Int] = {
    if(size <= x.size){
      x
    } else {
      List.fill(size - x.size)(0) ++ x
    }
  }

  def BigIntToDecInts(value: BigInt, alignNum: Int = 0): List[Int] = {
    val ret = value.toString(radix = 10).map(_.asDigit).toList
    listPad(ret, alignNum).reverse
  }

  def BigIntToOctInts(value: BigInt, alignNum: Int = 0): List[Int] = {
    LiteralToString.OctString(value, alignNum).map(_.asDigit).toList.reverse
  }

  def BigIntToBinInts(value: BigInt, alignNum: Int = 0): List[Int] = {
    LiteralToString.BinString(value, alignNum).map(_.asDigit).toList.reverse
  }
}
