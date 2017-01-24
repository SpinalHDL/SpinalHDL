package spinal.lib.crypto.symmetric

import spinal.core._
import spinal.lib._


/******************************************************************************
  * Interface use by a symmetric block
  */

case class SymmetricCryptoBlockGeneric(keyWidth    : BitCount,
                                       blockWidth  : BitCount,
                                       useEncDec   : Boolean = true){}


case class SymmetricCryptoBlockCmd(g : SymmetricCryptoBlockGeneric) extends Bundle{
  val key    = Bits(g.keyWidth)
  val block  = Bits(g.blockWidth)
  val enc    = if(g.useEncDec) Bool else null
}


case class SymmetricCryptoBlockRsp(g : SymmetricCryptoBlockGeneric) extends Bundle{
  val block = Bits(g.blockWidth)
}


case class SymmetricCryptoBlockIO(g : SymmetricCryptoBlockGeneric) extends Bundle{
  val cmd  = slave  Stream(SymmetricCryptoBlockCmd(g))
  val rsp  = master Flow(SymmetricCryptoBlockRsp(g))
}