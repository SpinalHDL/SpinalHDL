package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._

case class VgaTimingsHV(timingsWidth : Int) extends Bundle{
  val colorStart = UInt(timingsWidth bit)
  val colorEnd = UInt(timingsWidth bit)
  val syncStart = UInt(timingsWidth bit)
  val syncEnd  = UInt(timingsWidth bit)
}

case class VgaTimings(timingsWidth : Int) extends Bundle{
  val h = VgaTimingsHV(timingsWidth)
  val v = VgaTimingsHV(timingsWidth)
}


class VgaCtrl(rgbType : Rgb,timingsWidth : Int = 12) extends Component{
  val io = new Bundle{
    val timings = in (VgaTimings(timingsWidth))
    val softReset = in Bool
    val colorLink = slave Handshake(rgbType)
    val frameStart = out Bool
    val vga = master(Vga(rgbType))
  }

  case class HVArea(timingsHV: VgaTimingsHV,enable : Bool) extends Area{
    val counter = Reg(UInt(timingsWidth bit))

    val syncStart = counter === timingsHV.syncStart
    val syncEnd = counter === timingsHV.syncEnd
    val colorStart = counter === timingsHV.colorStart
    val colorEnd = counter === timingsHV.colorEnd

    when(enable){
      counter := counter + 1
      when(colorEnd){
        counter := 0
      }
    }

    val sync = BoolReg(syncStart,syncEnd)
    val colorEn = BoolReg(colorStart,colorEnd)

    when(io.softReset){
      counter := 0
      sync := False
      colorEn := False
    }
  }

  val h = HVArea(io.timings.h,True)
  val v = HVArea(io.timings.v,h.syncEnd)
  val colorEn = h.colorEn && v.colorEn

  io.frameStart := v.syncEnd

  io.vga.hSync := h.sync
  io.vga.vSync := v.sync
  io.vga.color := (io.colorLink & colorEn).toFlow.data
}




object VgaCtrl{
  def main(args: Array[String]) {
    SpinalVhdl(new VgaCtrl(Rgb(8,8,8)))
  }
}