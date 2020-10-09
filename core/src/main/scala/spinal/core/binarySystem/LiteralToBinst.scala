package spinal.core.binarySystem

object LiteralToBinst {
  def listPad(x: List[Int], size: Int): List[Int] = {
    if(size <= x.size){
      x.drop(x.size - size)
    } else {
      List.fill(size - x.size)(0) ++ x
    }
  }

  def BigIntToDecst(value: BigInt, alignNum: Int = 0): List[Int] = {
    val ret = value.toString(radix = 10).map(_.asDigit).toList
    listPad(ret, alignNum).reverse
  }

  def BigIntToOctst(value: BigInt, alignNum: Int = 0): List[Int] = {
    LiteralToString.OctString(value, alignNum).map(_.asDigit).toList.reverse
  }

  def BigIntToBinst(value: BigInt, alignNum: Int = 0): List[Int] = {
    LiteralToString.BinString(value, alignNum).map(_.asDigit).toList.reverse
  }
}
