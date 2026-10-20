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
// change reset state to 0 to prevent latch loop
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: VideoTimingCtrl.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.lib._

object VideoTimingCtrl {
  def apply(p: VideoTimingParameter, OE: Bool): VideoTimingCtrl = {
    val ret = VideoTimingCtrl(p)
    ret.io.videoIF.OE := OE
    ret
  }
}

// Useful calculator:
// https://tomverbeure.github.io/video_timings_calculator
// Default VGA, CVT
case class VideoTimingParameter(
    // H: For APB3 or other controller setup
    // L: Fix value, simple video mode
    withDynamicSetup: Boolean = false,
    // Export counter out sync with Video Pulse
    withCounterOutput: Boolean = false,
    // Windowing
    withWindow: Boolean = false,
    // Horizontal
    hActive: Int = 640,
    hFrontPorch: Int = 160,
    hSync: Int = 64,
    hBackPorch: Int = 160,
    // 1: active H, 0: active L
    hSyncPolarity: Boolean = true,
    hBlankPolarity: Boolean = true,
    // Vertical
    vActive: Int = 480,
    vFrontPorch: Int = 3,
    vSync: Int = 4,
    vBackPorch: Int = 13,
    // 1: active H, 0: active L
    vSyncPolarity: Boolean = true,
    vBlankPolarity: Boolean = true
)

case class VideoTimingCtrl(val p: VideoTimingParameter) extends Component {

  val h_total_fix = p.hActive + p.hFrontPorch + p.hSync + p.hBackPorch
  val v_total_fix = p.vActive + p.vFrontPorch + p.vSync + p.vBackPorch

  val h_bw = if (p.withDynamicSetup) 16 else U(h_total_fix).getBitsWidth
  val v_bw = if (p.withDynamicSetup) 16 else U(v_total_fix).getBitsWidth

  val io = new Bundle {
    val videoIF = new VideoIOs(p)
    val videoH = in(new VideoTimingIOs(p))
    val videoV = in(new VideoTimingIOs(p))
  }

  def getDynamicParam(ioPort: Option[UInt], defaultValue: Int, target: UInt): UInt = {
    ioPort
      .map(p => RegNext(p, init = U(0, p.getBitsWidth bits)))
      .getOrElse(U(defaultValue, 16 bits))
      .resize(target.getBitsWidth)
  }

  def getDynamicBoolParam(ioPort: Option[Bool], defaultValue: Boolean): Bool = {
    ioPort.map(p => RegNext(p, init = True)).getOrElse(Bool(defaultValue))
  }

  val v_count = Reg(UInt(v_bw bits)) init (0)
  val h_count = Reg(UInt(h_bw bits)) init (0)

  val vActive = getDynamicParam(io.videoV.active, p.vActive, v_count)
  val hActive = getDynamicParam(io.videoH.active, p.hActive, h_count)

  val vSync = getDynamicParam(io.videoV.sync, p.vSync, v_count)
  val hSync = getDynamicParam(io.videoH.sync, p.hSync, h_count)

  val vFrontPorch = getDynamicParam(io.videoV.frontPorch, p.vFrontPorch, v_count)
  val hFrontPorch = getDynamicParam(io.videoH.frontPorch, p.hFrontPorch, h_count)

  val vBackPorch = getDynamicParam(io.videoV.backPorch, p.vBackPorch, v_count)
  val hBackPorch = getDynamicParam(io.videoH.backPorch, p.hBackPorch, h_count)

  val vSyncPolarity = getDynamicBoolParam(io.videoV.syncPolarity, p.vSyncPolarity)
  val hSyncPolarity = getDynamicBoolParam(io.videoH.syncPolarity, p.hSyncPolarity)

  val vBlankPolarity = getDynamicBoolParam(io.videoV.blankPolarity, p.vBlankPolarity)
  val hBlankPolarity = getDynamicBoolParam(io.videoH.blankPolarity, p.hBlankPolarity)

  val hS_Bp =
    if (p.withDynamicSetup) RegNext(hSync + hBackPorch, init = U(0, 16 bits))
    else (hSync + hBackPorch)

  val vS_Bp =
    if (p.withDynamicSetup) RegNext(vSync + vBackPorch, init = U(0, 16 bits))
    else (vSync + vBackPorch)

  val hS_Bp_Ac =
    if (p.withDynamicSetup) RegNext(hS_Bp + hActive, init = U(0, 16 bits))
    else (hS_Bp + hActive)
  val vS_Bp_Ac =
    if (p.withDynamicSetup) RegNext(vS_Bp + vActive, init = U(0, 16 bits))
    else (vS_Bp + vActive)

  val hcount_total =
    if (p.withDynamicSetup) RegNext(hS_Bp_Ac + hFrontPorch, init = U(0, 16 bits))
    else (hS_Bp_Ac + hFrontPorch)
  val vcount_total =
    if (p.withDynamicSetup) RegNext(vS_Bp_Ac + vFrontPorch, init = U(0, 16 bits))
    else (vS_Bp_Ac + vFrontPorch)

  val counter_gate = if (p.withDynamicSetup) (hcount_total > 0) && (vcount_total > 0) else True
  val output_enable = RegNext(io.videoIF.OE, init = False)

  when(output_enable & counter_gate) {
    h_count := h_count + 1
    when(h_count > hcount_total) {
      h_count := 0
      v_count := v_count + 1
      when(v_count > vcount_total) {
        v_count := 0
      }
    }
  } otherwise {
    h_count := 0
    v_count := 0
  }

  val h_visible = (h_count >= hS_Bp && h_count < hS_Bp_Ac)
  val v_visible = (v_count >= vS_Bp && v_count < vS_Bp_Ac)

  val de = Reg(Bool()) init (false)
  de := (v_visible & h_visible)
  io.videoIF.DE := de

  val h_blank = Reg(Bool()) init (false)
  h_blank := (h_count < hS_Bp || h_count >= hS_Bp_Ac) ^ !hBlankPolarity
  io.videoIF.HBLANK := h_blank

  val v_blank = Reg(Bool()) init (false)
  v_blank := (v_count < vS_Bp || v_count >= vS_Bp_Ac) ^ !vBlankPolarity
  io.videoIF.VBLANK := v_blank

  val hsync = Reg(Bool()) init (false)
  hsync := (h_count < hSync) ^ !hSyncPolarity
  io.videoIF.HSYNC := hsync

  val vsync = Reg(Bool()) init (false)
  vsync := (v_count < vSync) ^ !vSyncPolarity
  io.videoIF.VSYNC := vsync

  if (p.withCounterOutput) {
    val hCountReg = Reg(UInt(h_count.getBitsWidth bits)) init (0)
    val vCountReg = Reg(UInt(v_count.getBitsWidth bits)) init (0)

    hCountReg := Mux(v_visible & h_visible, h_count - hS_Bp, U(0))
    vCountReg := Mux(v_visible, v_count - vS_Bp, U(0))

    io.videoIF.HCOUNT.foreach(_ := hCountReg)
    io.videoIF.VCOUNT.foreach(_ := vCountReg)

    io.videoIF.VACTIVE.foreach(_ := vActive.resize(v_bw bits))
    io.videoIF.HACTIVE.foreach(_ := hActive.resize(h_bw bits))
  }
}

object GenerateVideoTimingCtrl {
  def main(args: Array[String]): Unit = {

    for (
      with_counter <- List(true, false);
      with_dynamic <- List(true, false)
    ) {

      val countStr = if (with_counter) "Count" else "NoCount"
      val dynamicStr = if (with_dynamic) "Dynamic" else "Static"
      val fileName = s"VideoTimingCtrl_${countStr}_${dynamicStr}"

      printf(s"[Info] Generated: $fileName\n")

      val config = SpinalConfig(
        targetDirectory = "videoTimingCtrl_RTL",
        defaultConfigForClockDomains = ClockDomainConfig()
      )

      val report = config.generateVerilog(
        new VideoTimingCtrl(
          VideoTimingParameter(
            withCounterOutput = with_counter,
            withDynamicSetup = with_dynamic
          )
        ).setDefinitionName(fileName)
      )

      report.printPruned()
    }
  }
}
