package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.{RgbConfig, RgbToYcbcr, Ycbcr, YcbcrConfig, YcbcrPix2}
import spinal.lib.misc.pipeline.{Payload, StagePipeline}

class VgaYcbcrPix2(ycbcrConfig: YcbcrConfig) extends Component{
  val io = new Bundle{
    val up = slave(new VgaBus(Ycbcr(ycbcrConfig)))
    val down = master(new VgaBus(YcbcrPix2(ycbcrConfig)))
  }

  val state = RegInit(False)
  state := !state && io.up.colorEn

  val cr = RegNext(io.up.color.cr)
  io.down.vSync := io.up.vSync
  io.down.hSync := io.up.hSync
  io.down.colorEn := io.up.colorEn
  io.down.color.y := io.up.color.y
  io.down.color.cx := state.mux(cr, io.up.color.cb)
}
