package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.{RgbConfig, Rgb}


case class Vga (rgbConfig: RgbConfig, withColorEn : Boolean = true) extends Bundle with IMasterSlave{
  val vSync = Bool
  val hSync = Bool

  val colorEn = withColorEn generate Bool
  val color = Rgb(rgbConfig)

  override def asMaster() = this.asOutput()
  override def asSlave() = this.asInput()


  def <<(m : Vga): Unit ={
    this.vSync := m.vSync
    this.hSync := m.hSync
    this.colorEn := m.colorEn

    def adjust(from : UInt, width : Int) = width-widthOf(from) match {
      case 0 => from
      case v if v > 0 => from << v
      case v if v < 0 => from >> v
    }
    this.color.r := adjust(m.color.r, widthOf(this.color.r))
    this.color.g := adjust(m.color.g, widthOf(this.color.g))
    this.color.b := adjust(m.color.b, widthOf(this.color.b))
  }
}


