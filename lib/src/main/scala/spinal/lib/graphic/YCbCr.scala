package spinal.lib.graphic

import spinal.core._

case class YcbcrConfig(yWidth : Int,cbWidth : Int,crWidth : Int){
  def getWidth = yWidth + cbWidth + crWidth
}

object Ycbcr{
  def apply(yWidth : Int,cbWidth : Int,crWidth : Int) : Rgb = Rgb(RgbConfig(yWidth,cbWidth,crWidth))
}
case class Ycbcr(c: YcbcrConfig) extends Bundle{
  val y = UInt(c.yWidth bits)
  val cb = UInt(c.cbWidth bits)
  val cr = UInt(c.crWidth bits)
}

case class YcbcrPix2(c: YcbcrConfig) extends Bundle{
  assert(c.cbWidth == c.crWidth)
  val y = UInt(c.yWidth bits)
  val cx = UInt(c.cbWidth bits)
}