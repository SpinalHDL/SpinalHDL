package spinal.lib.tools

import spinal.core._


/**
  * Convert a BigInt into a List of Boolean
  * @example {{{
  *
  *    BigInt2ListBoolean(14, 4 bits)
  *    // List(false, true, true, true)
  * }}}
  */
object BigInt2ListBoolean{

  def apply(value: BigInt, size: BitCount): List[Boolean] = {
    def bigInt2ListBool(that: BigInt): List[Boolean] = {
      if(that == 0)  Nil
      else List(that.testBit(0)) ::: bigInt2ListBool(that >> 1)
    }

    castListBool(bigInt2ListBool(value), size.value)
  }

  private def castListBool(l: List[Boolean], size: Int): List[Boolean] = {
    if (l.length == size)     l
    else if (l.length > size) l.drop( l.length - size)
    else                      l ::: List.fill(size - l.length)(false)
  }
}
