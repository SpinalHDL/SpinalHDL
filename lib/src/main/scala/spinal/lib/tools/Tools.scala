package spinal.lib.tools

import spinal.core._


/**
  * Convert a BigInt into a List of Boolean
  * @example {{{
  *
  *    BigIntToListBoolean(14, 4 bits)
  *    // List(false, true, true, true)
  * }}}
  */
object BigIntToListBoolean{

  def apply(value: BigInt, size: BitCount): List[Boolean] = {

    assert(size.value >= 0, "The size must be bigger than 0")

    def bigInt2ListBool(that: BigInt): List[Boolean] = {
      if(that == 0)  Nil
      else List(that.testBit(0)) ::: bigInt2ListBool(that >> 1)
    }

    var listBoolean = List[Boolean]()

    if(value < 0){
      listBoolean = bigInt2ListBool((value.abs ^ ((BigInt(1) << size.value) - 1)) + 1)
    }else{
      listBoolean = bigInt2ListBool(value)
    }

    castListBool(listBoolean, size.value)
  }

  private def castListBool(l: List[Boolean], size: Int): List[Boolean] = {
    if (l.length == size)     l
    else if (l.length > size) l.drop( l.length - size)
    else                      l ::: List.fill(size - l.length)(false)
  }
}

