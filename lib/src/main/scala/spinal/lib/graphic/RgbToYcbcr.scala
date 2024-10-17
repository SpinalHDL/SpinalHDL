package spinal.lib.graphic

import spinal.core._
import spinal.lib.misc.pipeline._

class RgbToYcbcr(rgbConfig : RgbConfig,
                 ycbcrConfig: YcbcrConfig,
                 RGB: Payload[Rgb],
                 YCBCR: Payload[Ycbcr],
                 read0 : Node,
                 read1: Node,
                 add: Node) extends Area{

  import rgbConfig._
  import ycbcrConfig._

  val rom = new Area {
    def craft(iWidth: Int, factors : List[Double]) = {
      val iMax = (1 << iWidth) - 1
      val widths = factors.map(f => log2Up((f * iMax).floor.toInt))
      val values = for (i <- 0 to iMax) yield {
        var value = BigInt(0)
        var ptr = 0
        for((f, w) <- (factors, widths).zipped){
          value += ((i * f).floor.toLong << ptr)
          ptr += w
        }
        value
      }
      Mem.fill(iMax + 1)(Vec.tabulate(factors.size)(i => UInt(widths(i) bits))) initBigInt (values.toSeq)
    }
    val r = craft(rgbConfig.rWidth, List(0.299, 0.168736/*, 0.5*/))
    val g = craft(rgbConfig.gWidth, List(0.587, 0.331264, 0.418688))
    val b = craft(rgbConfig.bWidth, List(0.114, /*0.5, */ 0.081312))
  }

  val mul = new Area {
    val R = read1 insert rom.r.readSync(read0(RGB).r, read0.isFiring)
    val G = read1 insert rom.g.readSync(read0(RGB).g, read0.isFiring)
    val B = read1 insert rom.b.readSync(read0(RGB).b, read0.isFiring)
  }


  val onAdd = new Area{
    import add._
    def s(iw : Int, ow : Int, that : UInt) = ((that << ow) >> iw).resize(ow)
    YCBCR.y  :=                                                             s(rWidth,  yWidth, mul.R(0))     + s(gWidth,  yWidth, mul.G(0)) + s(bWidth, yWidth, mul.B(0))
    YCBCR.cb := U(1 << (ycbcrConfig.cbWidth-1), ycbcrConfig.cbWidth bits) - s(rWidth, cbWidth, mul.R(1))     - s(gWidth, cbWidth, mul.G(1)) + s(bWidth, cbWidth, (RGB.b >> 1))
    YCBCR.cr := U(1 << (ycbcrConfig.crWidth-1), ycbcrConfig.crWidth bits) + s(rWidth, crWidth, (RGB.r >> 1)) - s(gWidth, crWidth, mul.G(2)) - s(bWidth, crWidth, mul.B(1))
  }

//  val y = (0.299 * r + 0.587 * g + 0.114 * b).round.toInt
//  val cb = (128 - 0.168736 * r - 0.331264 * g + 0.5 * b).round.toInt
//  val cr = (128 + 0.5 * r - 0.418688 * g - 0.081312 * b).round.toInt
}
