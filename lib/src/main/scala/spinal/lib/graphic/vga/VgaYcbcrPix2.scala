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

  val s0 = RegNext(io.up)
  val cb = (io.up.color.cb +^ s0.color.cb) >> 1
  val cr = (io.up.color.cr +^ s0.color.cr) >> 1
  val crLast = RegNext(cr)

  val state = RegInit(False)
  state := !state && s0.colorEn
  io.down.vSync   := s0.vSync
  io.down.hSync   := s0.hSync
  io.down.colorEn := s0.colorEn
  io.down.color.y := s0.color.y
  io.down.color.cx := state.mux(crLast, cb)
}
