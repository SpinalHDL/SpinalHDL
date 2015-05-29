package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.Rgb


case class Vga (rgbType : Rgb) extends Bundle with IMasterSlave{
  val vSync = Bool
  val hSync = Bool

  val color = cloneOf(rgbType)

  override def asMaster = asOutput
  override def asSlave = asInput
}


