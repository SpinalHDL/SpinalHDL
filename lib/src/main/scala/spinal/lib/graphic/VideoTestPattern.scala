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
// File File Revision: 0.9.1
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
//
// R-Cyan alternations
// G-Magenta alternations
// B-Yellow alternations
//
// Output enable - YUV 422 411 subsampling control
//
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
//
// Color Bars
// Fill [Red, Green, Blue]
// Color Checker - CIE data for Illuminant C from Poynton
// Gray Bars
//
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: VideoTestPattern.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.lib._
import spinal.core.sim._

object VideoTestPattern {
  def apply(
      vtpp: VideoPatternParameter,
      vtcp: VideoTimingParameter,
      OE: Bool
  ): VideoTestPattern = {
    val ret = VideoTestPattern(vtpp, vtcp)
    ret.io.videoIF.OE := OE
    ret
  }
}

case class VideoPatternParameter(
    // Must select one or more
    withColorBar: Boolean = true,
    withFillRed: Boolean = true,
    withFillGreen: Boolean = true,
    withFillBlue: Boolean = true,
    withGrayScale: Boolean = true,
    // Very handy in Camera and Video industry
    // Reference:
    // https://en.wikipedia.org/wiki/File:Color_Checker.pdf
    withColorChecker: Boolean = true,
    // Color Strip to test subsample UV channels leak
    // V 0 vs max
    withColorStripRCy: Boolean = false,
    // UV 0 vs max
    withColorStripBY: Boolean = false,
    // U 0 vs max
    withColorStripGM: Boolean = false,
    // Alpha channel, for ARGB or AYUV output
    outputWithAlpha: Boolean = false,
    // Must select one or more
    outputTypeRGB: Boolean = true,
    // Reference:
    // https://learn.microsoft.com/en-us/windows/win32/medfound/10-bit-and-16-bit-yuv-video-formats#420-formats
    bitsPerContent: Int = 8,
    // Reference:
    // https://en.wikipedia.org/wiki/Chroma_subsampling#/media/File:Common_chroma_subsampling_ratios_YCbCr_CORRECTED.svg
    // usually 10bits / 16bits
    outputTypeYUV444: Boolean = false,
    outputTypeYUV422: Boolean = false,
    outputTypeYUV420: Boolean = false,
    outputTypeYUV411: Boolean = false,
    withCounterOutput: Boolean = false
) {

  val maxCh = (1 << bitsPerContent) - 1
  val minCh = 0

  val haveYUV = List(
    outputTypeYUV444,
    outputTypeYUV422,
    outputTypeYUV420,
    outputTypeYUV411
  ).count(_ == true) > 0

  val selectionCount = List(
    withColorBar,
    withFillRed,
    withFillGreen,
    withFillBlue,
    withColorChecker,
    withGrayScale,
    withColorStripRCy,
    withColorStripGM,
    withColorStripBY
  ).count(_ == true)

  require(
    List(
      withColorBar,
      withFillRed,
      withFillGreen,
      withFillBlue,
      withColorChecker,
      withGrayScale,
      withColorStripRCy,
      withColorStripGM,
      withColorStripBY
    ).count(_ == true) >= 1,
    "You must select one or more pattern!"
  )

  require(
    List(
      outputTypeRGB,
      outputTypeYUV444,
      outputTypeYUV422,
      outputTypeYUV420,
      outputTypeYUV411
    ).count(_ == true) == 1,
    "You must select one output type!"
  )
}

case class VideoPatternColorBar(
    val bitsPerContent: Int = 8,
    val hactive_bw: Int = 10
) extends Component {

  val io = new Bundle {
    val OE = in Bool ()
    val DE = in Bool ()
    val HACTIVE = in UInt (hactive_bw bits)
    val COLOR_BAR = out Bits ((bitsPerContent * 3) bits)
  }

  // h-active / 8, 8 colors in BAR mode
  val barWidth = io.HACTIVE(io.HACTIVE.high downto 3)
  val barCounter = Reg(UInt(barWidth.getBitsWidth bits)) init (0)
  val barIndex = Reg(UInt(3 bits)) init (0)
  when(io.OE & io.DE) {
    when(barCounter >= (barWidth - 1)) {
      barCounter := 0
      barIndex := barIndex + 1
    } otherwise {
      barCounter := barCounter + 1
    }
  } otherwise {
    barCounter := 0
    barIndex := 0
  }

  val maxCh = B((1 << bitsPerContent) - 1, bitsPerContent bits)
  val minCh = B(0, bitsPerContent bits)

  val io_bar = Reg(Bits(bitsPerContent * 3 bits)) init (0)
  io_bar := 0

  switch(barIndex) {
    is(0) { io_bar := maxCh ## minCh ## minCh } // Red
    is(1) { io_bar := minCh ## maxCh ## minCh } // Green
    is(2) { io_bar := minCh ## minCh ## maxCh } // Blue
    is(3) { io_bar := maxCh ## maxCh ## minCh } // Yellow
    is(4) { io_bar := minCh ## maxCh ## maxCh } // Cyan
    is(5) { io_bar := maxCh ## minCh ## maxCh } // Magenta
    is(6) { io_bar := minCh #* 3 } // Black
    is(7) { io_bar := maxCh #* 3 } // White
  }

  io.COLOR_BAR := RegNext(io_bar, init = B(0))
  var latency = 2
}

case class VideoPatternGrayBar(
    val bitsPerContent: Int = 8,
    val hactive_bw: Int = 10
) extends Component {

  val io = new Bundle {
    val OE = in Bool ()
    val DE = in Bool ()
    val HACTIVE = in UInt (hactive_bw bits)
    val GRAY_BAR = out Bits ((bitsPerContent * 3) bits)
  }

  val barWidth = io.HACTIVE(io.HACTIVE.high downto 3)
  val barCounter = Reg(UInt(barWidth.getBitsWidth bits)) init (0)
  val barIndex = Reg(UInt(3 bits)) init (0)
  when(io.OE & io.DE) {
    when(barCounter >= (barWidth - 1)) {
      barCounter := 0
      barIndex := barIndex + 1
    } otherwise {
      barCounter := barCounter + 1
    }
  } otherwise {
    barCounter := 0
    barIndex := 0
  }

  val maxCh = (1 << bitsPerContent) - 1
  val step = ((1 << bitsPerContent) / 7).toInt

  val io_gray = Reg(Bits(bitsPerContent * 3 bits)) init (0)
  io_gray := 0

  for (i <- 0 until bitsPerContent) {
    when(barIndex === i) {
      val gray_level = (maxCh - (step * i)).toInt
      io_gray := B(gray_level, bitsPerContent bits) #* 3
    }
  }

  io.GRAY_BAR := RegNext(io_gray, init = B(0))
  var latency = 2
}

case class VideoPatternColorChecker(
    val bitsPerContent: Int,
    val hactive_bw: Int,
    val vactive_bw: Int,
    val hcount_bw: Int,
    val vcount_bw: Int
) extends Component {

  val io = new Bundle {
    val OE = in Bool ()
    val DE = in Bool ()
    val HACTIVE = in UInt (hactive_bw bits)
    val VACTIVE = in UInt (vactive_bw bits)
    val HCOUNT = in UInt (hcount_bw bits)
    val VCOUNT = in UInt (vcount_bw bits)
    val CHECKER = out Bits ((bitsPerContent * 3) bits)
  }

  // These latency is not critical once it is setup
  // It will not eventually static
  val box_gap_sh = RegNext(io.HACTIVE(io.HACTIVE.high downto 5))
  val box_gap_lh = RegNext(io.HACTIVE(io.HACTIVE.high downto 4))
  val box_gap_sth = RegNext(io.HACTIVE(io.HACTIVE.high downto 3) +^ box_gap_sh)
  val box_gap_lth = RegNext(io.HACTIVE(io.HACTIVE.high downto 3))
  val box_th = RegNext(io.HACTIVE - box_gap_sth - box_gap_lth)

  val box_gap_sv = RegNext(io.VACTIVE(io.VACTIVE.high downto 5))
  val box_gap_lv = RegNext(io.VACTIVE(io.VACTIVE.high downto 4))
  val box_gap_stv = RegNext(io.VACTIVE(io.VACTIVE.high downto 4) +^ box_gap_sv)
  val box_gap_ltv = RegNext(io.VACTIVE(io.VACTIVE.high downto 3))
  val box_tv = RegNext(io.VACTIVE - box_gap_stv - box_gap_ltv)

  val box_h = RegNext(((box_th << 5) +^ (box_th << 3) +^ (box_th << 2)) >> 8)
  val box_v = RegNext(box_tv >> 2)

  box_gap_lh.simPublic().setName("box_gap_lh")
  box_gap_sh.simPublic().setName("box_gap_sh")
  box_gap_sth.simPublic().setName("box_gap_sth")
  box_gap_lth.simPublic().setName("box_gap_lth")

  box_gap_lv.simPublic().setName("box_gap_lv")
  box_gap_sv.simPublic().setName("box_gap_sv")
  box_gap_stv.simPublic().setName("box_gap_stv")
  box_gap_ltv.simPublic().setName("box_gap_ltv")

  box_h.simPublic().setName("box_h")
  box_v.simPublic().setName("box_v")

  def resize(coeff: Int): Bits = {
    B((1 << bitsPerContent) * coeff / 256).resize(bitsPerContent)
  }

  def processPatch(values: List[Int]): Bits = {
    values.map(c => resize(c)).reduce(_ ## _)
  }

  val colorGrid = List(
    List(
      List(115, 82, 68),
      List(194, 150, 130),
      List(98, 122, 157),
      List(87, 108, 67),
      List(133, 128, 177),
      List(103, 189, 170)
    ).map(processPatch), // Row 0
    List(
      List(214, 126, 44),
      List(80, 91, 166),
      List(193, 90, 99),
      List(94, 60, 108),
      List(157, 188, 64),
      List(224, 163, 46)
    ).map(processPatch), // Row 1
    List(
      List(56, 61, 150),
      List(70, 148, 73),
      List(175, 54, 60),
      List(231, 199, 31),
      List(187, 86, 149),
      List(8, 133, 161)
    ).map(processPatch), // Row 2
    List(
      List(243, 243, 242),
      List(200, 200, 200),
      List(160, 160, 160),
      List(122, 122, 122),
      List(85, 85, 85),
      List(52, 52, 52)
    ).map(processPatch) // Row 3
  )

  val colStarts = Vec(Reg(UInt(io.HCOUNT.getWidth bits).allowUnsetRegToAvoidLatch), 6)
  val colEnds = Vec(Reg(UInt(io.HCOUNT.getWidth bits).allowUnsetRegToAvoidLatch), 6)

  val rowStarts = Vec(Reg(UInt(io.VCOUNT.getWidth bits).allowUnsetRegToAvoidLatch), 4)
  val rowEnds = Vec(Reg(UInt(io.VCOUNT.getWidth bits).allowUnsetRegToAvoidLatch), 4)

  colStarts(0) := box_gap_lh.resize(io.HCOUNT.getWidth bits)
  colEnds(0) := colStarts(0) + box_h
  for (i <- 1 until 6) {
    colStarts(i) := colEnds(i - 1) + box_gap_sh
    colEnds(i) := colStarts(i) + box_h
  }

  rowStarts(0) := box_gap_lv.resize(io.VCOUNT.getWidth bits)
  rowEnds(0) := rowStarts(0) + box_v
  for (i <- 1 until 4) {
    rowStarts(i) := rowEnds(i - 1) + box_gap_sv
    rowEnds(i) := rowStarts(i) + box_v
  }

  val io_checker = Bits(bitsPerContent * 3 bits)
  io_checker := 0

  for (r <- 0 until 4) {
    for (c <- 0 until 6) {
      val inRange = RegNext(
        (io.VCOUNT >= rowStarts(r) && io.VCOUNT < rowEnds(r)) &&
          (io.HCOUNT >= colStarts(c) && io.HCOUNT < colEnds(c))
      )

      when(inRange & io.OE & io.DE) {
        io_checker := colorGrid(r)(c)
      }
    }
  }

  io.CHECKER := RegNext(io_checker, init = B(0))
  var latency = 2
}

case class VideoTestPattern(
    val vtpp: VideoPatternParameter,
    val vtcp: VideoTimingParameter
) extends Component {

  val vtcpi = vtcp.copy(withCounterOutput = true)
  val vtc = new VideoTimingCtrl(vtcpi)

  val io = new Bundle {

    val videoIF = new VideoIOs(vtcpi.copy(withCounterOutput = vtpp.withCounterOutput))
    val videoH = in(new VideoTimingIOs(vtcpi))
    val videoV = in(new VideoTimingIOs(vtcpi))

    val PATTERN_SEL =
      if (vtpp.selectionCount > 1) Some(in UInt (U(vtpp.selectionCount).getBitsWidth bits))
      else None

    val ALPHA =
      if (vtpp.outputWithAlpha) Some(out(new VideoColorAlpha(vtpp.bitsPerContent)))
      else None

    val RGB =
      if (vtpp.outputTypeRGB) Some(out(new VideoColorRgb(vtpp.bitsPerContent)))
      else None

    val YUV =
      if (vtpp.haveYUV) Some(out(new VideoColorYuv(vtpp.bitsPerContent)))
      else None
  }

  val alphaCounter =
    if (vtpp.outputWithAlpha)
      Some(Reg(UInt(vtpp.bitsPerContent bits)) init (vtpp.maxCh))
    else
      None

  if (vtpp.outputWithAlpha) {
    val alphaWidth = vtc.io.videoIF.VACTIVE.get(vtc.io.videoIF.VACTIVE.get.high downto 1)
    when(io.videoIF.OE & (vtc.io.videoIF.VCOUNT.get >= alphaWidth)) {
      alphaCounter.get := vtpp.maxCh - (
        vtc.io.videoIF.VCOUNT.get - alphaWidth
      ).resize(vtpp.bitsPerContent)
    } otherwise {
      alphaCounter.get := vtpp.maxCh
    }
  }

  val colorBar =
    if (vtpp.withColorBar) Some(Bits(3 * vtpp.bitsPerContent bits))
    else None
  if (vtpp.withColorBar) {
    val color_bar = new VideoPatternColorBar(
      vtpp.bitsPerContent,
      vtc.io.videoIF.HACTIVE.get.getWidth
    )
    color_bar.io.DE := vtc.io.videoIF.DE
    color_bar.io.OE := io.videoIF.OE
    color_bar.io.HACTIVE := vtc.io.videoIF.HACTIVE.get
    colorBar.get := color_bar.io.COLOR_BAR
  }

  val grayBar =
    if (vtpp.withGrayScale) Some(Bits(3 * vtpp.bitsPerContent bits))
    else None
  if (vtpp.withGrayScale) {
    val gray_bar = new VideoPatternGrayBar(
      vtpp.bitsPerContent,
      vtc.io.videoIF.HACTIVE.get.getWidth
    )
    gray_bar.io.DE := vtc.io.videoIF.DE
    gray_bar.io.OE := io.videoIF.OE
    gray_bar.io.HACTIVE := vtc.io.videoIF.HACTIVE.get
    grayBar.get := gray_bar.io.GRAY_BAR
  }

  val colorChecker =
    if (vtpp.withColorChecker) Some(Bits(3 * vtpp.bitsPerContent bits))
    else None
  if (vtpp.withColorChecker) {
    val color_checker = new VideoPatternColorChecker(
      vtpp.bitsPerContent,
      vtc.io.videoIF.HACTIVE.get.getWidth,
      vtc.io.videoIF.VACTIVE.get.getWidth,
      vtc.io.videoIF.HCOUNT.get.getWidth,
      vtc.io.videoIF.VCOUNT.get.getWidth
    )
    color_checker.io.DE := vtc.io.videoIF.DE
    color_checker.io.OE := io.videoIF.OE
    color_checker.io.HACTIVE := vtc.io.videoIF.HACTIVE.get
    color_checker.io.VACTIVE := vtc.io.videoIF.VACTIVE.get
    color_checker.io.HCOUNT := vtc.io.videoIF.HCOUNT.get
    color_checker.io.VCOUNT := vtc.io.videoIF.VCOUNT.get
    colorChecker.get := color_checker.io.CHECKER
  }

  var latency = 2
  val maxCh = B(vtpp.maxCh, vtpp.bitsPerContent bits)
  val minCh = B(0, vtpp.bitsPerContent bits)

  val flip_strip = if (vtpp.withColorStripRCy | vtpp.withColorStripGM | vtpp.withColorStripBY) {
    Some(Delay(vtc.io.videoIF.HCOUNT.get(vtpp.outputTypeYUV411.toInt), latency) #* 8)
  } else None

  val availablePatterns = List(
    if (vtpp.withColorBar) Some(colorBar.get) else None,
    if (vtpp.withFillRed) Some(maxCh ## minCh ## minCh) else None,
    if (vtpp.withFillGreen) Some(minCh ## maxCh ## minCh) else None,
    if (vtpp.withFillBlue) Some(minCh ## minCh ## maxCh) else None,
    if (vtpp.withGrayScale) Some(grayBar.get) else None,
    if (vtpp.withColorChecker) Some(colorChecker.get) else None,
    if (vtpp.withColorStripRCy) Some((~flip_strip.get) ## (flip_strip.get #* 2)) else None,
    if (vtpp.withColorStripGM) Some(flip_strip.get ## (~flip_strip.get) ## flip_strip.get) else None,
    if (vtpp.withColorStripBY) Some((flip_strip.get #* 2) ## (~flip_strip.get)) else None
  ).flatten

  val selectedPattern =
    if (availablePatterns.size == 1)
      availablePatterns.head
    else {
      val patternsVec = Vec(availablePatterns)
      // gate index, there is no auto default to last / first
      val safeIndex = io.PATTERN_SEL.get.min(patternsVec.length - 1)
      patternsVec(safeIndex)
    }

  if (vtpp.outputTypeRGB) {
    val vcsp = VideoSpaceParameter(
      bitsPerContent = vtpp.bitsPerContent,
      useRGB2YUV = false,
      useYUV2RGB = false
    )
    val colorSpace = VideoColorSpace(vcsp)
    colorSpace.io.SPACE_IN := selectedPattern.asUInt
    colorSpace.io.IE := B(1) #* 3
    colorSpace.io.OE := B(1) #* 3

    // Total Latency for Video signal align
    latency += colorSpace.latency

    if (vtpp.outputWithAlpha) {
      val syncAlpha = Delay(alphaCounter.get, latency)
      io.ALPHA.foreach(_.A := syncAlpha)
    }

    val rgb_value = colorSpace.io.SPACE_OUT
    io.RGB.foreach { rgb =>
      rgb.R := rgb_value(vtpp.bitsPerContent * 3 - 1 downto vtpp.bitsPerContent * 2)
      rgb.G := rgb_value(vtpp.bitsPerContent * 2 - 1 downto vtpp.bitsPerContent * 1)
      rgb.B := rgb_value(vtpp.bitsPerContent - 1 downto 0)
    }
  }

  if (vtpp.haveYUV) {

    val vcsp = VideoSpaceParameter(
      bitsPerContent = vtpp.bitsPerContent,
      useRGB2YUV = true,
      useYUV2RGB = false
    )
    val colorSpace = VideoColorSpace(vcsp)
    colorSpace.io.SPACE_IN := selectedPattern.asUInt

    val oe = Bool()
    if (vtpp.outputTypeYUV422) {
      oe := Delay(!vtc.io.videoIF.HCOUNT.get(0), latency)
    } else if (vtpp.outputTypeYUV411) {
      oe := Delay(
        (vtc.io.videoIF.HCOUNT.get(1 downto 0) === 0),
        latency
      )
    } else {
      oe := True
    }
    colorSpace.io.IE := True #* 3
    colorSpace.io.OE(2) := True
    colorSpace.io.OE(1 downto 0) := oe #* 2

    latency += colorSpace.latency

    if (vtpp.outputWithAlpha) {
      val syncAlpha = Delay(alphaCounter.get, latency)
      io.ALPHA.foreach(_.A := syncAlpha)
    }

    val yuv_value = colorSpace.io.SPACE_OUT
    io.YUV.foreach { yuv =>
      yuv.Y := yuv_value(vtpp.bitsPerContent * 3 - 1 downto vtpp.bitsPerContent * 2)
      yuv.U := yuv_value(vtpp.bitsPerContent * 2 - 1 downto vtpp.bitsPerContent * 1)
      yuv.V := yuv_value(vtpp.bitsPerContent - 1 downto 0)
    }
  }

  vtc.io.videoIF.OE := io.videoIF.OE

  io.videoIF.DE := Delay(vtc.io.videoIF.DE, init = False, cycleCount = latency)
  io.videoIF.HSYNC := Delay(vtc.io.videoIF.HSYNC, cycleCount = latency)
  io.videoIF.VSYNC := Delay(vtc.io.videoIF.VSYNC, cycleCount = latency)
  io.videoIF.HBLANK := Delay(vtc.io.videoIF.HBLANK, cycleCount = latency)
  io.videoIF.VBLANK := Delay(vtc.io.videoIF.VBLANK, cycleCount = latency)

  io.videoIF.HACTIVE.foreach(_ := vtc.io.videoIF.HACTIVE.get)
  io.videoIF.VACTIVE.foreach(_ := vtc.io.videoIF.VACTIVE.get)

  if (vtcpi.withDynamicSetup) {
    vtc.io.videoH := io.videoH
    vtc.io.videoV := io.videoV
  }

  if (vtpp.withCounterOutput) {
    // too lazy to optimization this, should do counter offset
    // LUT vs Reg # usually not too balance so it should be okay
    io.videoIF.VCOUNT.foreach(_ := Delay(vtc.io.videoIF.VCOUNT.get, cycleCount = latency))
    io.videoIF.HCOUNT.foreach(_ := Delay(vtc.io.videoIF.HCOUNT.get, cycleCount = latency))
  }

  println(s"[\u001B[32mInfo\u001B[0m] Hardware Total Latency: $latency")
}

object GenerateVideoTestPattern {
  def main(args: Array[String]): Unit = {

    for (
      out_type <- List(true, false);
      with_alpha <- List(true, false);
      with_counter <- List(true, false);
      with_dynamic <- List(true, false)
    ) {

      val typeStr = if (out_type) "RGB" else "YUV"
      val alphaStr = if (with_alpha) "Alpha" else "NoAlpha"
      val countStr = if (with_counter) "Count" else "NoCount"
      val dynamicStr = if (with_dynamic) "Dynamic" else "Static"
      val fileName = s"VideoTestPattern_${typeStr}_${alphaStr}_${countStr}_${dynamicStr}"

      val config = SpinalConfig(
        targetDirectory = "videoTestPattern_RTL",
        defaultConfigForClockDomains = ClockDomainConfig()
      )

      val report = config.generateVerilog(
        new VideoTestPattern(
          VideoPatternParameter(
            outputWithAlpha = with_alpha,
            outputTypeRGB = out_type,
            outputTypeYUV444 = !out_type,
            withCounterOutput = with_counter
          ),
          VideoResolutions
            .timingLibrary("h1920_v1080_r60")
            .copy(
              withDynamicSetup = with_dynamic
            )
        ).setDefinitionName(fileName)
      )
      report.printPruned()
    }
  }
}
