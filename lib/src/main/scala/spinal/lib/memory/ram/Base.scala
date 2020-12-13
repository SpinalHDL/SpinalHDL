package spinal.lib.memory.ram

import spinal.core._

trait BWEType{
  def width: Int

  def maskInfo: String = width match {
    case 16 => "wordmask"
    case 8  => "bytemask"
    case 1  => "bitmask"
    case 0  => "nomask"
    case _  => SpinalError("Undefiend BWE type")
  }

  def nonbwe: Boolean = width == 0

}

object BWEByte extends BWEType{
  def width: Int = 8
}

object BWEWord extends BWEType{
  def width: Int = 16
}

object BWEBit extends BWEType{
  def width: Int = 1
}

object BWENon extends BWEType{
  def width: Int = 0
}

case class MemConfig(dw: Int,
                     depth: Int,
                     maskBitWidth: Int = 0,
                     vendor: Vendor = UMC,
                     withScan: Boolean = false,
                     withBist: Boolean = false,
                     withRepaire: Boolean = false){
  def bistaw: Int = 14
  def aw: Int = log2Up(depth)
  def maskName: String = maskBitWidth match {
    case 16 => "wordmask"
    case 8  => "bytemask"
    case 1  => "bitmask"
    case 0  => "nomask"
    case _  => SpinalError("Undefiend BWE type")
  }
  def bweWidth = if(maskBitWidth == 0) 0 else math.ceil(dw.toDouble/maskBitWidth).toInt
  def nonbwe: Boolean = (maskBitWidth == 0)
  def needBWE: Boolean = !nonbwe
  def genBWE(): Bits = in Bits(bweWidth bits)
}

trait MemWrap

trait MemBlackBoxTrait {
  def Build[T <: MemWrap](wrap: T): Unit
}

abstract class MemBlackBox extends BlackBox {
   val mem: Mem[Bits]
   def Build(): BlackBox
}

