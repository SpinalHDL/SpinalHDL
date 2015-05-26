package spinal.demo.mandelbrot

import spinal.core._


case class UInt2D(bitCount: BitCount) extends Bundle {
  val x = UInt(bitCount)
  val y = UInt(bitCount)
}


object SFix2D {
  def apply(exp: Int, bitCount: BitCount): SFix2D = new SFix2D(exp, bitCount.value)
  def apply(copy: SFix): SFix2D = SFix2D(copy.exp, copy.bitCount bit)

}

@valClone
class SFix2D(val exp: Int, val bitCount: Int) extends Bundle {
  val x = SFix(exp, bitCount bit)
  val y = SFix(exp, bitCount bit)
}


case class MandelbrotCoreParameters(iterationLimit: Int, unitCount: Int, screenResX: Int, screenResY: Int, fixExp: Int, fixWidth: Int) {
  def fix = SFix(fixExp, fixWidth bit)
  def iterationWidth = log2Up(iterationLimit + 1)
}


case class FrameTask(p: MandelbrotCoreParameters) extends Bundle {
  val start = SFix2D(p.fixExp, p.fixWidth bit)
  val inc = SFix2D(p.fixExp - 4, p.fixWidth + 8 bit)

  def fullRangeSFix = SFix(p.fixExp, p.fixWidth + 12 bit)
}

case class PixelTask(p: MandelbrotCoreParameters) extends Bundle {
  val mandelbrotPosition = SFix2D(p.fix)
}
case class PixelResult(p: MandelbrotCoreParameters) extends Bundle {
  val iteration = UInt(p.iterationWidth bit)
}