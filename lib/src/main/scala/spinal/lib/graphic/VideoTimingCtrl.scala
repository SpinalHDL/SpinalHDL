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

  val maxVal = (1 << 16) - 1
  val h_total_fix = p.hActive + p.hFrontPorch + p.hSync + p.hBackPorch
  val v_total_fix = p.vActive + p.vFrontPorch + p.vSync + p.vBackPorch

  val h_total = if (p.withDynamicSetup) maxVal else h_total_fix
  val v_total = if (p.withDynamicSetup) maxVal else v_total_fix

  val io = new Bundle {
    val videoIF = new VideoIOs(p)
    val videoCfg = new VideoTimingIOs(p)
  }

  def getDynamicParam(ioPort: Option[UInt], defaultValue: Int, target: UInt): UInt = {
    ioPort.getOrElse(U(defaultValue, 16 bits)).resize(target.getWidth)
  }

  val v_count = Reg(UInt(log2Up(v_total) bits)) init (0)
  val h_count = Reg(UInt(log2Up(h_total) bits)) init (0)

  val vActive = getDynamicParam(io.videoCfg.v_active, p.vActive, v_count)
  val hActive = getDynamicParam(io.videoCfg.h_active, p.hActive, h_count)

  val vSync = getDynamicParam(io.videoCfg.v_sync, p.vSync, v_count)
  val hSync = getDynamicParam(io.videoCfg.h_sync, p.hSync, h_count)

  val vFrontPorch = getDynamicParam(io.videoCfg.v_front_porch, p.vFrontPorch, v_count)
  val hFrontPorch = getDynamicParam(io.videoCfg.h_front_porch, p.hFrontPorch, h_count)

  val vBackPorch = getDynamicParam(io.videoCfg.v_back_porch, p.vBackPorch, v_count)
  val hBackPorch = getDynamicParam(io.videoCfg.h_back_porch, p.hBackPorch, h_count)

  val vSyncPolarity = ~io.videoCfg.v_sync_polarity.getOrElse(Bool(p.vSyncPolarity))
  val hSyncPolarity = ~io.videoCfg.h_sync_polarity.getOrElse(Bool(p.hSyncPolarity))

  val vBlankPolarity = ~io.videoCfg.v_blank_polarity.getOrElse(Bool(p.vBlankPolarity))
  val hBlankPolarity = ~io.videoCfg.h_blank_polarity.getOrElse(Bool(p.hBlankPolarity))

  val hcount_total =
    if (p.withDynamicSetup)
      RegNext(hActive, init = U(0)) +
        RegNext(hFrontPorch, init = U(0)) +
        RegNext(hSync, init = U(0)) +
        RegNext(hBackPorch, init = U(0))
    else
      U(h_total_fix)
  val vcount_total =
    if (p.withDynamicSetup)
      RegNext(vActive, init = U(0)) +
        RegNext(vFrontPorch, init = U(0)) +
        RegNext(vSync, init = U(0)) +
        RegNext(vBackPorch, init = U(0))
    else
      U(v_total_fix)

  val counter_gate = if (p.withDynamicSetup) (hcount_total > 0) && (vcount_total > 0) else True
  val output_enable = RegNext(io.videoIF.OE, init = False)

  when(output_enable & counter_gate) {
    h_count := h_count + 1
    when(h_count >= (hcount_total - 1)) {
      h_count := 0
      v_count := v_count + 1
      when(v_count >= (vcount_total - 1)) {
        v_count := 0
      }
    }
  } otherwise {
    h_count := 0
    v_count := 0
  }

  val v_visible = (v_count >= (vSync + vBackPorch) && v_count < (vSync + vBackPorch + vActive))
  val h_visible = (h_count >= (hSync + hBackPorch) && h_count < (hSync + hBackPorch + hActive))

  val de = Reg(Bool()) init (False)
  de := (v_visible & h_visible)
  io.videoIF.DE := de

  val h_blank = Reg(Bool()) init (hBlankPolarity)
  h_blank := (
    h_count < (hSync + hBackPorch) || h_count >= (hSync + hBackPorch + hActive)
  ) ^ hBlankPolarity
  io.videoIF.HBLANK := h_blank

  val v_blank = Reg(Bool()) init (vBlankPolarity)
  v_blank := (
    v_count < (vSync + vBackPorch) || v_count >= (vSync + vBackPorch + vActive)
  ) ^ vBlankPolarity
  io.videoIF.VBLANK := v_blank

  val hsync = Reg(Bool()) init (hSyncPolarity)
  hsync := (h_count < hSync) ^ hSyncPolarity
  io.videoIF.HSYNC := hsync

  val vsync = Reg(Bool()) init (vSyncPolarity)
  vsync := (v_count < vSync) ^ vSyncPolarity
  io.videoIF.VSYNC := vsync

  val hCountReg = Reg(UInt(h_count.getWidth bits)) init (0)
  val vCountReg = Reg(UInt(v_count.getWidth bits)) init (0)

  hCountReg := Mux(v_visible & h_visible, h_count - (hSync + hBackPorch), U(0))
  vCountReg := Mux(v_visible, v_count - (vSync + vBackPorch), U(0))

  io.videoIF.HCOUNT.foreach(_ := hCountReg)
  io.videoIF.VCOUNT.foreach(_ := vCountReg)

  io.videoIF.VACTIVE := vActive.resize(log2Up(v_total) bits)
  io.videoIF.HACTIVE := hActive.resize(log2Up(h_total) bits)
}

object GenerateVideoTimingCtrl {
  def main(args: Array[String]): Unit = {

    val report = SpinalVerilog(
      new VideoTimingCtrl(
        VideoTimingParameter(
          withCounterOutput = true
          // withDynamicSetup = true
        )
      )
    )

    report.printPruned()
  }
}
