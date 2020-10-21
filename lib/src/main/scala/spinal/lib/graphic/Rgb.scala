package spinal.lib.graphic

import spinal.core._

case class RgbConfig(rWidth : Int,gWidth : Int,bWidth : Int){
  def getWidth = rWidth + gWidth + bWidth
}

object Rgb{
  def apply(rWidth : Int,gWidth : Int,bWidth : Int) : Rgb = Rgb(RgbConfig(rWidth,gWidth,bWidth))
}
case class Rgb(c: RgbConfig) extends Bundle{
  val r = UInt(c.rWidth bits)
  val g = UInt(c.gWidth bits)
  val b = UInt(c.bWidth bits)

  def clear(): Unit ={
    r := 0
    g := 0
    b := 0
  }
}