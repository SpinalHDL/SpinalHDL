package spinal.lib.bus.amba3.ahblite.sim

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

  /** Create Hsize from number of bits */
  def fromSize(sizeInBits: Int): Int = {
    sizeInBits match {
      case 8   => Byte
      case 16  => Halfword
      case 32  => Word
      case 64  => Doubleword
      case 128 => FourWord
      case 256 => EightWord
      // Unnamed cases
      case 512  => 6
      case 1024 => 7
    }
  }

  /** Number of bytes in a transfer */
  def toBytes(hsize: Int): Int = 1 << hsize
}
