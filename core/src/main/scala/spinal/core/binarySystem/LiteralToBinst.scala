package spinal.core.binarySystem

object LiteralToBinst {
  def listPad(x: List[Int], size: Int): List[Int] = {
    if(size <= x.size){
      x.drop(x.size - size)
    } else {
      List.fill(size - x.size)(0) ++ x
    }
  }

  def BigIntToBinst(value: BigInt, size: Int = 0, align: Boolean = false): List[Int] = {
    val ret = value.toString(2)
      .map(b => Integer.parseInt(b.toString,2))
      .toList
    if (align) listPad(ret, size) else ret
  }

  def BigIntToDecst(value: BigInt, size: Int = 0 , align: Boolean = false): List[Int] = {
    val ret = value.toString(10)
      .map(b => Integer.parseInt(b.toString,10))
      .toList
    if (align) listPad(ret, size) else ret
  }

  def BigIntToOctst(value: BigInt, size: Int = 0, align: Boolean = false): List[Int] = {
    val ret = value.toString(8)
      .map(b => Integer.parseInt(b.toString,8))
      .toList
    if (align) listPad(ret, size) else ret
  }
}
