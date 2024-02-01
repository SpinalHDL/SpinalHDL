package spinal.lib.tools.binarySystem

import spinal.core._

object BytesToLiteral {
  def bytesToHexString(bytes: List[Byte]): String = {
    bytes.map(v => f"$v%02x").mkString
  }
  def bytesToBigInt(bytes: List[Byte], inputEndian: Endianness = LITTLE): BigInt = {
    BigInt((inputEndian match {
      case BIG => bytes
      case LITTLE => bytes.reverse
    }).toArray)
  }
}
