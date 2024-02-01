package spinal.lib.tools.binarySystem

import spinal.core._

object LiteralToBytes {
  def bigIntToBytes(value: BigInt, outputEndian: Endianness = LITTLE) = {
    val ba = value.toByteArray
    (outputEndian match {
      case LITTLE => ba.reverse
      case BIG => ba
    }).padTo(8, 0.toByte).toList
  }
}
