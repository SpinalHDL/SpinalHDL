package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._



case class Vga (rgbType : Rgb) extends Bundle with IMasterSlave{
  val vSync = Bool
  val hSync = Bool

  val color = cloneOf(rgbType)

  override def asMaster = asOutput
  override def asSlave = asInput
}


case class Rgb(rWidth : Int,gWidth : Int,bWidth : Int) extends Bundle{
  val r = UInt(rWidth bit)
  val g = UInt(gWidth bit)
  val b = UInt(bWidth bit)
}