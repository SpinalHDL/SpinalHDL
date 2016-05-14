package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.bus.neutral.NeutralStreamDma
import spinal.lib.graphic.{RgbConfig, Rgb}
import spinal.lib.tool.QSysify


case class VgaTimingsHV(timingsWidth: Int) extends Bundle {
  val colorStart = UInt(timingsWidth bit)
  val colorEnd = UInt(timingsWidth bit)
  val syncStart = UInt(timingsWidth bit)
  val syncEnd = UInt(timingsWidth bit)
}

case class VgaTimings(timingsWidth: Int) extends Bundle {
  val h = VgaTimingsHV(timingsWidth)
  val v = VgaTimingsHV(timingsWidth)

  def setAs_h640_v480_r60: Unit = {
    h.syncStart := 96 - 1
    h.syncEnd := 800 - 1
    h.colorStart := 96 + 16 - 1
    h.colorEnd := 800 - 48 - 1
    v.syncStart := 2 - 1
    v.syncEnd := 525 - 1
    v.colorStart := 2 + 10 - 1
    v.colorEnd := 525 - 33 - 1
  }
  def setAs_h64_v64_r60: Unit = {
    h.syncStart := 96 - 1
    h.syncEnd := 800 - 1
    h.colorStart := 96 + 16 - 1 + 288
    h.colorEnd := 800 - 48 - 1 - 288
    v.syncStart := 2 - 1
    v.syncEnd := 525 - 1
    v.colorStart := 2 + 10 - 1 + 208
    v.colorEnd := 525 - 33 - 1 - 208
  }
}


class VgaCtrl(rgbConfig: RgbConfig, timingsWidth: Int = 12) extends Component {
  val io = new Bundle {
    val softReset = in Bool() default(False)
    val timings = in(VgaTimings(timingsWidth))

    val frameStart = out Bool
    val pixels = slave Stream (Rgb(rgbConfig))
    val vga = master(Vga(rgbConfig))

    val error = out Bool
  }

  case class HVArea(timingsHV: VgaTimingsHV, enable: Bool) extends Area {
    val counter = Reg(UInt(timingsWidth bit)) init(0)

    val syncStart = counter === timingsHV.syncStart
    val syncEnd = counter === timingsHV.syncEnd
    val colorStart = counter === timingsHV.colorStart
    val colorEnd = counter === timingsHV.colorEnd

    when(enable) {
      counter := counter + 1
      when(syncEnd) {
        counter := 0
      }
    }

    val sync    = RegInit(False) setWhen(syncStart) clearWhen(syncEnd)
    val colorEn = RegInit(False) setWhen(colorStart) clearWhen(colorEnd)

    when(io.softReset) {
      counter := 0
      sync := False
      colorEn := False
    }
  }

  val h = HVArea(io.timings.h, True)
  val v = HVArea(io.timings.v, h.syncEnd)
  val colorEn = h.colorEn && v.colorEn
  io.pixels.ready := colorEn
  io.error := colorEn && ! io.pixels.valid

  io.frameStart := v.syncEnd

  io.vga.hSync := h.sync
  io.vga.vSync := v.sync
  io.vga.colorEn := colorEn
  io.vga.color := io.pixels.payload


  //Can be called by parent component to make the VgaCtrl autonom by using a Stream of fragment to feed it.
  def feedWith(that : Stream[Fragment[Rgb]]): Unit ={
    io.pixels << that.toStreamOfFragment

    val error = RegInit(False)
    when(io.error){
      error := True
    }
    when(that.isLast){
      error := False
    }

    io.softReset := error
    when(error){
      that.ready := True
    }
  }
}


object VgaCtrl {
  def main(args: Array[String]) {
    SpinalVhdl(new VgaCtrl(RgbConfig(8, 8, 8)))
  }
}


class QsysVgaCtrl(rgbConfig: RgbConfig) extends Component{
  val io = new Bundle{
    val pixel = slave Stream Fragment(Bits(rgbConfig.getWidth bits))
    val vga = master(Vga(rgbConfig))
  }

  val pixel = Stream Fragment(Rgb(rgbConfig))
  pixel.arbitrationFrom(io.pixel)
  pixel.last := io.pixel.last
  pixel.fragment.assignFromBits(io.pixel.fragment)

  val burstSize = 8;
//  val dma = new NeutralBasicReadDma(burstSize)
//  dma.io.
//
//  val ctrl = new VgaCtrl(rgbConfig)
//  ctrl.feedWith(pixel)


}

object QsysVgaCtrl {
  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new QsysVgaCtrl(RgbConfig(8, 8, 8))).toplevel
    toplevel.io.pixel.addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)
  }
}