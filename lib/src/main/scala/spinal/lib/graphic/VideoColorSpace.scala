// =======================================================================
// License: MIT (part of spinal.lib)
// =======================================================================
//  ____         _                ____
// | __ )  _ __ (_)  __ _  _ __  / ___|  _   _  _ __    ___
// |  _ \ | '__|| | / _` || '_ \ \___ \ | | | || '_ \  / _ \
// | |_) || |   | || (_| || | | | ___) || |_| || | | ||  __/
// |____/ |_|   |_| \__,_||_| |_||____/  \__,_||_| |_| \___|
//
// =======================================================================
// File Revision: 0.9.1
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
//
// Add in/out enable to gate the data flows
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
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
      SPACE_IN: UInt = null,
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
    useMixARGB: Boolean = false,
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

  val total_in_no = if (useMixARGB) 4 * 2 else 3
  val total_out_no = if (useMixARGB) 4 else 3
}

case class VideoColorSpace(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val IE = in Bits (vcsp.total_in_no bit)
    val OE = in Bits (vcsp.total_out_no bit)
    val SPACE_IN = in UInt (vcsp.total_in_no * vcsp.bitsPerContent bits)
    val SPACE_OUT = out UInt (vcsp.total_out_no * vcsp.bitsPerContent bits)
  }

  var latency: Int = 1
  val zeros = U(0, vcsp.bitsPerContent bits)

  if (vcsp.useMixARGB) {
    val mix_argb = VideoMixerRGB(vcsp)

    val in_ch = io.SPACE_IN.subdivideIn(vcsp.bitsPerContent bits)
    val in_chs = mix_argb.io.FORE_RGB.channels ++ Seq(mix_argb.io.FORE_A.A) ++
      mix_argb.io.BACK_RGB.channels ++ Seq(mix_argb.io.BACK_A.A)
    in_chs.zipWithIndex.foreach { case (ch, i) => ch := RegNextWhen(in_ch(i), io.IE(i), zeros) }

    val oe = Delay(io.OE, 3, init = B(0, io.OE.getBitsWidth bits))
    val out_chs = mix_argb.io.MIX_RGB.channels ++ Seq(mix_argb.io.MIX_A.A)
    io.SPACE_OUT.subdivideIn(vcsp.bitsPerContent bits).zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(out_chs(i), oe(i), zeros)
    }

    latency = 4
  } else if (vcsp.useRGB2YUV) {
    val rgb2yuv = VideoRGB2YUV(vcsp)

    val in_ch = io.SPACE_IN.subdivideIn(vcsp.bitsPerContent bits)
    rgb2yuv.io.RGB.channels.zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(in_ch(i), io.IE(i), zeros)
    }

    val oe = Delay(io.OE, 3, init = B(0, io.OE.getBitsWidth bits))
    io.SPACE_OUT.subdivideIn(vcsp.bitsPerContent bits).zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(rgb2yuv.io.YUV.channels(i), oe(i), zeros)
    }

    latency = 4
  } else if (vcsp.useYUV2RGB) {
    val rgb2yuv = VideoYUV2RGB(vcsp)

    val in_ch = io.SPACE_IN.subdivideIn(vcsp.bitsPerContent bits)
    rgb2yuv.io.YUV.channels.zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(in_ch(i), io.IE(i), zeros)
    }

    val oe = Delay(io.OE, 3, init = B(0, io.OE.getBitsWidth bits))
    io.SPACE_OUT.subdivideIn(vcsp.bitsPerContent bits).zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(rgb2yuv.io.RGB.channels(i), oe(i), zeros)
    }

    latency = 4
  } else {
    io.SPACE_OUT.subdivideIn(vcsp.bitsPerContent bits).zipWithIndex.foreach { case (ch, i) =>
      ch := RegNextWhen(
        io.SPACE_IN.subdivideIn(vcsp.bitsPerContent bits)(i),
        io.IE(i) & io.OE(i),
        zeros
      )
    }
    latency = 1
  }
}

// Must use DSP multiplier
// Reference: https://en.wikipedia.org/wiki/Alpha_compositing
// Premultiplied Alpha
case class VideoMixerRGB(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val FORE_RGB = in(new VideoColorRgb(vcsp.bitsPerContent))
    val FORE_A = in(new VideoColorAlpha(vcsp.bitsPerContent))
    val BACK_RGB = in(new VideoColorRgb(vcsp.bitsPerContent))
    val BACK_A = in(new VideoColorAlpha(vcsp.bitsPerContent))
    val MIX_RGB = out(new VideoColorRgb(vcsp.bitsPerContent))
    val MIX_A = out(new VideoColorAlpha(vcsp.bitsPerContent))
  }

  val maxCh = U((1 << vcsp.bitsPerContent) - 1, vcsp.bitsPerContent bits)

  def clip(value: UInt): UInt = {
    Mux(value > maxCh, maxCh, value.resize(vcsp.bitsPerContent bits))
  }

  val fa = io.FORE_A.A
  val ba = io.BACK_A.A
  val fa_dly = RegNext(fa)
  val bsa_a = RegNext((ba * (maxCh - fa)) >> vcsp.bitsPerContent)
  val oa = fa_dly +^ bsa_a
  val oa_r = RegNext(oa)
  io.MIX_A.A := clip(oa_r)

  val f_next = io.FORE_RGB.channels.map(ch => RegNext(ch))
  val b_next = io.BACK_RGB.channels.map(ch => RegNext((ch * (maxCh - fa)) >> vcsp.bitsPerContent))
  val o = Vec(f_next.zip(b_next).map { case (f, b) => f +^ b })
  val o_r = o.map(ch => RegNext(ch))
  io.MIX_RGB.channels.zip(o_r).foreach { case (res, src) => res := clip(src) }
}

case class VideoYUV2RGB(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val YUV = in(new VideoColorYuv(vcsp.bitsPerContent))
    val RGB = out(new VideoColorRgb(vcsp.bitsPerContent))
  }

  def zp(value: UInt, factor: Int) = {
    val paddingWidth = factor + (vcsp.bitsPerContent - 8)
    (U(0, 1 bits) ## value ## U(0, paddingWidth bits)).asUInt
  }

  def sumSfts(input: UInt, formula: List[Int]): UInt = {
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

  val r_y = RegNext(sumSfts(io.YUV.Y, BT_standard(0)(0)))
  val r_v = RegNext(sumSfts(io.YUV.V, BT_standard(0)(2)))
  val r_sum = RegNext((r_y + r_v) >> vcsp.bitsPerContent)
  val r = clip(r_sum.asSInt - S(r_offset))

  val g_y = RegNext(sumSfts(io.YUV.Y, BT_standard(1)(0)))
  val g_u = RegNext(sumSfts(io.YUV.U, BT_standard(1)(1)))
  val g_v = RegNext(sumSfts(io.YUV.V, BT_standard(1)(2)))
  val g_sum = RegNext((g_y.asSInt - g_u.asSInt - g_v.asSInt) >> vcsp.bitsPerContent)
  val g = clip(g_sum + S(g_offset))

  val b_y = RegNext(sumSfts(io.YUV.Y, BT_standard(2)(0)))
  val b_u = RegNext(sumSfts(io.YUV.U, BT_standard(2)(1)))
  val b_sum = RegNext((b_y + b_u) >> vcsp.bitsPerContent)
  val b = clip(b_sum.asSInt - S(b_offset))

  io.RGB.R := r
  io.RGB.G := g
  io.RGB.B := b
}

case class VideoRGB2YUV(
    vcsp: VideoSpaceParameter
) extends Component {
  val io = new Bundle {
    val RGB = in(new VideoColorRgb(vcsp.bitsPerContent))
    val YUV = out(new VideoColorYuv(vcsp.bitsPerContent))
  }

  def zp(value: UInt, factor: Int) = {
    val paddingWidth = factor + (vcsp.bitsPerContent - 8)
    (U(0, 1 bits) ## value ## U(0, paddingWidth bits)).asUInt
  }

  def sumSfts(input: UInt, formula: List[Int]): UInt = {
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

  val y_r = RegNext(sumSfts(io.RGB.R, BT_standard(0)(0)))
  val y_g = RegNext(sumSfts(io.RGB.G, BT_standard(0)(1)))
  val y_b = RegNext(sumSfts(io.RGB.B, BT_standard(0)(2)))

  val u_r = RegNext(sumSfts(io.RGB.R, BT_standard(1)(0)))
  val u_g = RegNext(sumSfts(io.RGB.G, BT_standard(1)(1)))
  val u_b = RegNext(sumSfts(io.RGB.B, BT_standard(1)(2)))

  val v_r = RegNext(sumSfts(io.RGB.R, BT_standard(2)(0)))
  val v_g = RegNext(sumSfts(io.RGB.G, BT_standard(2)(1)))
  val v_b = RegNext(sumSfts(io.RGB.B, BT_standard(2)(2)))

  val y_offset = U(if (vcsp.stdBT601TV) (1 << (vcsp.bitsPerContent - 4)) else 0)
  val y_sum = RegNext((y_r + y_g + y_b) >> vcsp.bitsPerContent)
  val y = (y_sum + y_offset).resize(vcsp.bitsPerContent bits)

  val u_sum = RegNext((u_b.asSInt - u_r.asSInt - u_g.asSInt) >> vcsp.bitsPerContent)
  val u = clip(u_sum)

  val v_sum = RegNext((v_r.asSInt - v_g.asSInt - v_b.asSInt) >> vcsp.bitsPerContent)
  val v = clip(v_sum)

  io.YUV.Y := y
  io.YUV.U := u
  io.YUV.V := v
}

object GenerateVideoColorSpace {
  def main(args: Array[String]): Unit = {

    // Define the list of standards you want to generate
    val standards = List("BT470", "BT601Full", "BT601TV")
    val useRGB2YUV_modes = List(true, false)

    val config = SpinalConfig(
      targetDirectory = "videoColorSpace_RTL"
    )

    for (
      std <- standards;
      useRGB2YUV <- useRGB2YUV_modes
    ) {
      val stdStr = std
      val modeStr = if (useRGB2YUV) "RGB2YUV" else "YUV2RGB"
      val fileName = s"VideoColorSpace_${modeStr}_${stdStr}"

      val report = config.generateVerilog(
        new VideoColorSpace(
          VideoSpaceParameter(
            bitsPerContent = 8,
            useRGB2YUV = useRGB2YUV,
            useYUV2RGB = !useRGB2YUV,
            stdBT470 = (std == "BT470"),
            stdBT601Full = (std == "BT601Full"),
            stdBT601TV = (std == "BT601TV")
          )
        ).setDefinitionName(fileName)
      )

      report.printPruned()
    }

    val report = config.generateVerilog(
      new VideoColorSpace(
        VideoSpaceParameter(
          bitsPerContent = 8,
          useRGB2YUV = false,
          useYUV2RGB = false
        )
      ).setDefinitionName("VideoColorSpace_Through")
    )

    report.printPruned()

    val report2 = config.generateVerilog(
      new VideoColorSpace(
        VideoSpaceParameter(
          bitsPerContent = 8,
          useMixARGB = true,
          useRGB2YUV = false,
          useYUV2RGB = false
        )
      ).setDefinitionName("VideoColorSpace_RGB_Mixer")
    )

    report2.printPruned()
  }
}
