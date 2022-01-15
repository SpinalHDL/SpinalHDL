package spinal.lib.tools.binarySystem

object StringToLiteral {
  def hex(s: String): BigInt = {
    BigInt(s, 16)
  }

  def dec(s: String): BigInt = {
    BigInt(s, 10)
  }

  def oct(s: String): BigInt = {
    BigInt(s, 8)
  }

  def bin(s: String): BigInt = {
    BigInt(s, 2)
  }
}
