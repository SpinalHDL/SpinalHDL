// =======================================================================
//  ____         _                ____
// | __ )  _ __ (_)  __ _  _ __  / ___|  _   _  _ __    ___
// |  _ \ | '__|| | / _` || '_ \ \___ \ | | | || '_ \  / _ \
// | |_) || |   | || (_| || | | | ___) || |_| || | | ||  __/
// |____/ |_|   |_| \__,_||_| |_||____/  \__,_||_| |_| \___|
//
// =======================================================================
// Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Trial Version
//
// Latency 4 ticks or 1 tick
//
// Real FPGA fitting shows good t_su and t_h @ > 175MHz
//
// Errors are kept @ log2(bits/pixel-elements)
// 8bits <= +/-2 LSb ... < 2%
//
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: VideoColorSpace.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.lib._
import spinal.core._

object VideoColorSpace {
  def apply(
      vcsp: VideoSpaceParameter,
      SPACE_IN: Bits = null,
      IE: Bits = Bits(3 bits),
      OE: Bits = Bits(3 bits)
  ): VideoColorSpace = {
    val ret = VideoColorSpace(vcsp)
    ret.io.IE := IE
    ret.io.OE := OE
    ret.io.SPACE_IN := SPACE_IN
    ret
  }
}

case class VideoSpaceParameter(
    bitsPerContent: Int = 8,
    useRGB2YUV: Boolean = true,
    useYUV2RGB: Boolean = false,
    stdBT470: Boolean = true,
    stdBT601Full: Boolean = false,
    stdBT601TV: Boolean = false
) {
  require(bitsPerContent > 0, "bitsPerContent must be greater than 0")
  require(
    (useRGB2YUV ^ useYUV2RGB) | (!(useRGB2YUV & useYUV2RGB)),
    "Cannot use both RGB2YUV and YUV2RGB at the same time, please select one!"
  )

  require(
    (List(
      stdBT470,
      stdBT601Full,
      stdBT601TV
    ).count(_ == true) == 1 & (useRGB2YUV ^ useYUV2RGB)) |
      (!(useRGB2YUV & useYUV2RGB)),
    "You must select ONE standard only!"
  )
}

case class VideoColorSpace(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val IE = in Bits (3 bit)
    val OE = in Bits (3 bit)
    val SPACE_IN = in Bits ((vcsp.bitsPerContent * 3) bits)
    val SPACE_OUT = out UInt ((vcsp.bitsPerContent * 3) bits)
  }

  var latency: Int = 1

  if (vcsp.useRGB2YUV) {
    val rgb2yuv = VideoRGB2YUV(vcsp)

    rgb2yuv.io.R := RegNextWhen(
      io.SPACE_IN(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent),
      io.IE(2)
    )
    rgb2yuv.io.G := RegNextWhen(
      io.SPACE_IN(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent),
      io.IE(1)
    )
    rgb2yuv.io.B := RegNextWhen(
      io.SPACE_IN(1 * vcsp.bitsPerContent - 1 downto 0),
      io.IE(0)
    )

    val oe = Delay(io.OE, 3)
    io.SPACE_OUT(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent) :=
      RegNextWhen(rgb2yuv.io.Y, oe(2))
    io.SPACE_OUT(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent) :=
      RegNextWhen(rgb2yuv.io.U, oe(1))
    io.SPACE_OUT(1 * vcsp.bitsPerContent - 1 downto 0) :=
      RegNextWhen(rgb2yuv.io.V, oe(0))
    latency = 4
  } else if (vcsp.useYUV2RGB) {
    val rgb2yuv = VideoYUV2RGB(vcsp)

    rgb2yuv.io.Y := RegNextWhen(
      io.SPACE_IN(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent),
      io.IE(2)
    )
    rgb2yuv.io.U := RegNextWhen(
      io.SPACE_IN(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent),
      io.IE(1)
    )
    rgb2yuv.io.V := RegNextWhen(
      io.SPACE_IN(1 * vcsp.bitsPerContent - 1 downto 0),
      io.IE(0)
    )

    val oe = Delay(io.OE, 3)
    io.SPACE_OUT(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent) :=
      RegNextWhen(rgb2yuv.io.R, oe(2))
    io.SPACE_OUT(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent) :=
      RegNextWhen(rgb2yuv.io.G, oe(1))
    io.SPACE_OUT(1 * vcsp.bitsPerContent - 1 downto 0) :=
      RegNextWhen(rgb2yuv.io.B, oe(0))
    latency = 4
  } else {
    io.SPACE_OUT(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent) := RegNextWhen(
      io.SPACE_IN(3 * vcsp.bitsPerContent - 1 downto 2 * vcsp.bitsPerContent),
      io.IE(2) & io.OE(2)
    ).asUInt
    io.SPACE_OUT(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent) := RegNextWhen(
      io.SPACE_IN(2 * vcsp.bitsPerContent - 1 downto vcsp.bitsPerContent),
      io.IE(1) & io.OE(1)
    ).asUInt
    io.SPACE_OUT(1 * vcsp.bitsPerContent - 1 downto 0) := RegNextWhen(
      io.SPACE_IN(1 * vcsp.bitsPerContent - 1 downto 0),
      io.IE(0) & io.OE(0)
    ).asUInt
    latency = 1
  }
}

case class VideoYUV2RGB(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val Y = in Bits (vcsp.bitsPerContent bits)
    val U = in Bits (vcsp.bitsPerContent bits)
    val V = in Bits (vcsp.bitsPerContent bits)
    val R = out UInt (vcsp.bitsPerContent bits)
    val G = out UInt (vcsp.bitsPerContent bits)
    val B = out UInt (vcsp.bitsPerContent bits)
  }

  def zp(value: Bits, factor: Int) = {
    val paddingWidth = factor + (vcsp.bitsPerContent - 8)
    (U(0, 1 bits) ## value ## U(0, paddingWidth bits)).asUInt
  }

  def sumSfts(input: Bits, formula: List[Int]): UInt = {
    val head = zp(input, formula.head)
    formula.tail.foldLeft(head) { (acc, shift) =>
      val term = zp(input, shift.abs)
      if (shift >= 0) acc +^ term
      else acc -^ term
    }
  }

  def clip(value: SInt): UInt = {
    val result = UInt(vcsp.bitsPerContent bits)
    when(value > ((1 << vcsp.bitsPerContent)) - 1) {
      result := (1 << vcsp.bitsPerContent) - 1
    } elsewhen (value < 0) {
      result := 0
    } otherwise {
      result := value.asUInt.resize(vcsp.bitsPerContent bits)
    }
    result
  }

  // Y∈ [0,1] U,V∈[-0.5,0.5]
  // BT.470
  val BT_470 = List(
    // R:
    // 1 * Y + 0 * U + 1.13983 * V
    List(List(vcsp.bitsPerContent), List(), List(vcsp.bitsPerContent, 5, 2)),
    // G:
    // 1 * Y - 0.39465 * U - 0.5806 * V
    List(List(vcsp.bitsPerContent), List(6, 5, 2, 0), List(7, 4, 2)),
    // V:
    // 1 * Y + 2.03211 * U + 0 * V
    List(List(vcsp.bitsPerContent), List(vcsp.bitsPerContent + 1, 3), List())
  )

  // BT.601_FULL
  val BT_601_FULL = List(
    // R:
    // 1 * Y + 0 * U + 1.4075 * V
    List(List(vcsp.bitsPerContent), List(), List(vcsp.bitsPerContent, 6, 5, 3)),
    // G:
    // 1 * Y - 0.3455 * U - 0.7169 * V
    List(List(vcsp.bitsPerContent), List(6, 4, 3), List(7, 6, -3)),
    // V:
    // 1 * Y + 1.779 * U + 0 * V
    List(List(vcsp.bitsPerContent), List(vcsp.bitsPerContent, 7, 6, 3), List())
  )

  // BT.601_TV
  val BT_601_TV = List(
    // R:
    // 1.164 * Y + 0 * U + 1.4075 * V
    List(List(vcsp.bitsPerContent, 5, 3, 1), List(), List(vcsp.bitsPerContent, 7, 5, -3)),
    // G:
    // 1.164 * Y - 0.3455 * U - 0.7169 * V
    List(List(vcsp.bitsPerContent, 5, 3, 1), List(6, 5, 3), List(7, 6, 4)),
    // V:
    // 1.164 * Y + 1.779 * U + 0 * V
    List(List(vcsp.bitsPerContent, 5, 3, 1), List(vcsp.bitsPerContent + 1, 2), List())
  )

  val BT_standard = vcsp match {
    case v if v.stdBT601Full => BT_601_FULL
    case v if v.stdBT601TV   => BT_601_TV
    case _                   => BT_470
  }

  val r_offset = ((vcsp match {
    case v if v.stdBT601Full => 1.4075
    case v if v.stdBT601TV   => List(1.164 / 8, 1.596).sum
    case _                   => 1.13983
  }) * (1 << (vcsp.bitsPerContent - 1))).toInt

  val g_offset = ((vcsp match {
    case v if v.stdBT601Full => List(0.3455, 0.7169).sum
    case v if v.stdBT601TV   => List(-1.164 / 8, 0.392, 0.812).sum
    case _                   => List(0.39465, 0.5806).sum
  }) * (1 << (vcsp.bitsPerContent - 1))).toInt

  val b_offset = ((vcsp match {
    case v if v.stdBT601Full => 1.779
    case v if v.stdBT601TV   => List(1.164 / 8, 2.016).sum
    case _                   => 2.03211
  }) * (1 << (vcsp.bitsPerContent - 1))).toInt

  val r_y = RegNext(sumSfts(io.Y, BT_standard(0)(0)))
  val r_v = RegNext(sumSfts(io.V, BT_standard(0)(2)))
  val r_sum = RegNext((r_y + r_v) >> vcsp.bitsPerContent)
  val r = clip(r_sum.asSInt - S(r_offset))

  val g_y = RegNext(sumSfts(io.Y, BT_standard(1)(0)))
  val g_u = RegNext(sumSfts(io.U, BT_standard(1)(1)))
  val g_v = RegNext(sumSfts(io.V, BT_standard(1)(2)))
  val g_sum = RegNext((g_y.asSInt - g_u.asSInt - g_v.asSInt) >> vcsp.bitsPerContent)
  val g = clip(g_sum + S(g_offset))

  val b_y = RegNext(sumSfts(io.Y, BT_standard(2)(0)))
  val b_u = RegNext(sumSfts(io.U, BT_standard(2)(1)))
  val b_sum = RegNext((b_y + b_u) >> vcsp.bitsPerContent)
  val b = clip(b_sum.asSInt - S(b_offset))

  io.R := r
  io.G := g
  io.B := b
}

case class VideoRGB2YUV(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val R = in Bits (vcsp.bitsPerContent bits)
    val G = in Bits (vcsp.bitsPerContent bits)
    val B = in Bits (vcsp.bitsPerContent bits)
    val Y = out UInt (vcsp.bitsPerContent bits)
    val U = out UInt (vcsp.bitsPerContent bits)
    val V = out UInt (vcsp.bitsPerContent bits)
  }

  def zp(value: Bits, factor: Int) = {
    val paddingWidth = factor + (vcsp.bitsPerContent - 8)
    (U(0, 1 bits) ## value ## U(0, paddingWidth bits)).asUInt
  }

  def sumSfts(input: Bits, formula: List[Int]): UInt = {
    val head = zp(input, formula.head)
    formula.tail.foldLeft(head) { (acc, shift) =>
      val term = zp(input, shift.abs)
      if (shift >= 0) acc +^ term
      else acc -^ term
    }
  }

  def clip(value: SInt): UInt = {
    val result = UInt(vcsp.bitsPerContent bits)
    val mid = (U(1) << (vcsp.bitsPerContent - 1)).resize(vcsp.bitsPerContent + 1 bits).asSInt
    when(value > ((1 << (vcsp.bitsPerContent - 1)) - 1)) {
      result := (1 << vcsp.bitsPerContent) - 1
    } elsewhen (value < -(1 << (vcsp.bitsPerContent - 1))) {
      result := 0
    } otherwise {
      result := (mid + value).asUInt.resize(vcsp.bitsPerContent bits)
    }
    result
  }

  // Y∈ [0,1] U,V∈[-0.5,0.5]
  // BT.470: https://www.itu.int/rec/R-REC-BT.470/en
  val BT_470 = List(
    // Y:
    // 0.299 * R + 0.587 * G + 0.114 * B
    List(List(6, 3, 2), List(7, 4, 2, 1), List(4, 3, 2, 0)),
    // U:
    // -0.14713 * R - 0.28886 * G + 0.436 * B + (mid point)
    List(List(5, 2, 0), List(6, 3, 1), List(6, 5, 4)),
    // V:
    // 0.615 * R - 0.51499 * G - 0.10001 * B + (mid point)
    List(List(7, 5, -1), List(7, 2), List(4, 3, 1))
  )

  // Y∈ [0,1] U,V∈[-0.5,0.5]
  // BT.601.full / [analog  + mid-point]
  val BT_601_FULL = List(
    // Y:
    // 0.299 * R + 0.587 * G + 0.114 * B
    List(List(6, 3, 2), List(7, 4, 2, 1), List(4, 3, 2, 0)),
    // U:
    // -0.169 * R - 0.331 * G + 0.5 * B + (mid point)
    List(List(5, 3, 1, 0), List(6, 4, 2), List(7)),
    // V:
    // 0.5 * R - 0.419 * G - 0.081 * B + (mid point)
    List(List(7), List(7, -4, -2), List(4, 2, 0))
  )

  // Y∈[16,235] Cb∈[16-240] Cr∈[16-240]
  // BT.601.tv
  val BT_601_TV = List(
    // Y:
    // 0.257 * R + 0.504 * G + 0.098 * B + (full / 16)
    List(List(6, 1), List(7, 0), List(4, 3, 0)),
    // U/Cb:
    // -0.148 * R - 0.291 * G + 0.439 * B + (mid point)
    List(List(5, 2, 1), List(6, 3, 1), List(7, -4)),
    // V/Cr:
    // 0.439 * R - 0.368 * G - 0.071 * B + (mid point)
    List(List(7, -4), List(6, 5, -1), List(4, 1))
  )

  val BT_standard = vcsp match {
    case v if v.stdBT601Full => BT_601_FULL
    case v if v.stdBT601TV   => BT_601_TV
    case _                   => BT_470
  }

  val y_r = RegNext(sumSfts(io.R, BT_standard(0)(0)))
  val y_g = RegNext(sumSfts(io.G, BT_standard(0)(1)))
  val y_b = RegNext(sumSfts(io.B, BT_standard(0)(2)))

  val u_r = RegNext(sumSfts(io.R, BT_standard(1)(0)))
  val u_g = RegNext(sumSfts(io.G, BT_standard(1)(1)))
  val u_b = RegNext(sumSfts(io.B, BT_standard(1)(2)))

  val v_r = RegNext(sumSfts(io.R, BT_standard(2)(0)))
  val v_g = RegNext(sumSfts(io.G, BT_standard(2)(1)))
  val v_b = RegNext(sumSfts(io.B, BT_standard(2)(2)))

  val y_offset = U(if (vcsp.stdBT601TV) (1 << (vcsp.bitsPerContent - 4)) else 0)
  val y_sum = RegNext((y_r + y_g + y_b) >> vcsp.bitsPerContent)
  val y = (y_sum + y_offset).resize(vcsp.bitsPerContent bits)

  val u_sum = RegNext((u_b.asSInt - u_r.asSInt - u_g.asSInt) >> vcsp.bitsPerContent)
  val u = clip(u_sum)

  val v_sum = RegNext((v_r.asSInt - v_g.asSInt - v_b.asSInt) >> vcsp.bitsPerContent)
  val v = clip(v_sum)

  io.Y := y
  io.U := u
  io.V := v
}

object GenerateVideoColorSpace {
  def main(args: Array[String]): Unit = {

    val report = SpinalVerilog(
      new VideoColorSpace(
        VideoSpaceParameter(
          bitsPerContent = 8,
          useRGB2YUV = false,
          useYUV2RGB = true,
          // stdBT470 = true
          // stdBT601Full = true
          stdBT601TV = true
        )
      )
    )

    report.printPruned()
  }
}
