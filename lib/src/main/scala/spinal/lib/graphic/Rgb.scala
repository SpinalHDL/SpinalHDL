package spinal.lib.graphic

import spinal.core._

case class Rgb(rWidth : Int,gWidth : Int,bWidth : Int) extends Bundle{
  val r = UInt(rWidth bit)
  val g = UInt(gWidth bit)
  val b = UInt(bWidth bit)
}