package spinal.lib.graphic.vga

import spinal.core._
import spinal.lib._
import spinal.lib.graphic.{RgbConfig, RgbToYcbcr, Ycbcr, YcbcrConfig}
import spinal.lib.misc.pipeline.{Payload, StagePipeline}

class VgaRgbToYcbcr(rgbConfig : RgbConfig,
                    ycbcrConfig: YcbcrConfig) extends Component{
  val io = new Bundle{
    val up = slave(new Vga(rgbConfig))
    val down = master(new VgaBus(Ycbcr(ycbcrConfig)))
  }

  val pip = new StagePipeline

  val onInsert = new pip.Area(0){
    val VSYNC = insert(io.up.vSync)
    val HSYNC = insert(io.up.hSync)
    val COLOREN = insert(io.up.colorEn)
    val RGB = insert(io.up.color)
  }

  val YCBCR = Payload(Ycbcr(ycbcrConfig))

  val logic = new RgbToYcbcr(
    rgbConfig = rgbConfig,
    ycbcrConfig = ycbcrConfig,
    RGB = onInsert.RGB,
    YCBCR = YCBCR,
    read0 = pip(0),
    read1 = pip(1),
    add = pip(2)
  )

  val extract = new pip.Area(3) {
    io.down.vSync := onInsert.VSYNC
    io.down.hSync := onInsert.HSYNC
    io.down.colorEn := onInsert.COLOREN
    io.down.color := YCBCR
  }

  pip.build()
}
