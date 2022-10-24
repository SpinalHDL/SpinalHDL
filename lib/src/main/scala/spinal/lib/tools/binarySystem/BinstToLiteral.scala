package spinal.lib.tools.binarySystem

object BinIntsToLiteral {
  def binIntsToHexString(li: List[Int], alignHigh: Boolean = false): String = {
    val hexCount = scala.math.ceil(li.size/4.0).toInt
    val newli = if(alignHigh) List.fill(hexCount * 4 - li.size)(0) ++ li else li
    newli.grouped(4)
      .map(_.reverse.mkString(""))
      .map(Integer.parseInt(_,2).toHexString)
      .mkString("").reverse
  }

  def binIntsToOctString(li: List[Int], alignHigh: Boolean = false): String = {
    val hexCount = scala.math.ceil(li.size/3.0).toInt
    val newli = if(alignHigh) List.fill(hexCount * 3 - li.size)(0) ++ li else li
    newli.grouped(3)
      .map(_.reverse.mkString(""))
      .map(Integer.parseInt(_,2).toOctalString)
      .mkString("").reverse
  }

  def binIntsToBigInt(li: List[Int]): BigInt = {
    BigInt(binIntsToHexString(li), 16)
  }
}
