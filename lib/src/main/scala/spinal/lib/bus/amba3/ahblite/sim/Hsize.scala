package spinal.lib.bus.amba3.ahblite.sim

import scala.language.implicitConversions

/** Possible values of HSIZE
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  */
object Hsize {
  // Table 3-2 Transfer size encoding in section 3.4

  val Byte = 0
  val Halfword = 1
  val Word = 2
  val Doubleword = 3
  val FourWord = 4
  val EightWord = 5

  val byte = Hsize(Byte)
  val halfword = Hsize(Halfword)
  val word = Hsize(Word)
  val doubleword = Hsize(Doubleword)
  val fourWord = Hsize(FourWord)
  val eightWord = Hsize(EightWord)

  /** Create Hsize from number of bits */
  def fromSize(sizeInBits: Int): Hsize = {
    sizeInBits match {
      case 8   => byte
      case 16  => halfword
      case 32  => word
      case 64  => doubleword
      case 128 => fourWord
      case 256 => eightWord
      // Unnamed cases
      case 512  => Hsize(6)
      case 1024 => Hsize(7)
    }
  }

  /** Number of bytes in a transfer */
  def toBytes(hsize: Int): Int = 1 << hsize

  /** Hsize can be implicitly converted to Int */
  implicit def hsize2int(hsize: Hsize): Int = hsize.value
}

case class Hsize(value: Int)
