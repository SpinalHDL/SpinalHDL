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
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Verification Pending Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: VideoTimingInterfaces.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.lib._
import spinal.core._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.misc.BusSlaveFactoryAddressWrapper

object VideoTimingCtrlBusMapping {
  def driveFrom(
      io: Bundle {
        val videoH: VideoTimingIOs
        val videoV: VideoTimingIOs
      },
      busCtrl: BusSlaveFactory,
      config: VideoTimingParameter,
      baseAddress: Int = 0
  ) = new Area {
    require(busCtrl.busDataWidth == 32)
    require(config.withDynamicSetup)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val cfgH_regs = Reg(new VideoTimingIOs(config))
    val cfgV_regs = Reg(new VideoTimingIOs(config))

    io.videoH := cfgH_regs
    io.videoV := cfgV_regs

    busCtrlWrapped.readAndWrite(cfgV_regs.active.get, 0x00, 16, "v active")
    busCtrlWrapped.readAndWrite(cfgH_regs.active.get, 0x00, 0, "h active")

    busCtrlWrapped.readAndWrite(cfgH_regs.blankPolarity.get, 0x04, 31, "h blank polarity")
    busCtrlWrapped.readAndWrite(cfgH_regs.syncPolarity.get, 0x04, 30, "h sync polarity")
    busCtrlWrapped.readAndWrite(cfgH_regs.frontPorch.get, 0x04, 20, "h front porach")
    busCtrlWrapped.readAndWrite(cfgH_regs.sync.get, 0x04, 10, "h sync")
    busCtrlWrapped.readAndWrite(cfgH_regs.backPorch.get, 0x04, 0, "h back porach")

    busCtrlWrapped.readAndWrite(cfgV_regs.blankPolarity.get, 0x08, 31, "v blank polarity")
    busCtrlWrapped.readAndWrite(cfgV_regs.syncPolarity.get, 0x08, 30, "v sync polarity")
    busCtrlWrapped.readAndWrite(cfgV_regs.frontPorch.get, 0x08, 20, "v front porach")
    busCtrlWrapped.readAndWrite(cfgV_regs.sync.get, 0x08, 10, "v sync")
    busCtrlWrapped.readAndWrite(cfgV_regs.backPorch.get, 0x08, 0, "v back porach")
  }

  def driveFrom32(
      io: Bundle {
        val videoH: VideoTimingIOs
        val videoV: VideoTimingIOs
      },
      busCtrl: BusSlaveFactory,
      config: VideoTimingParameter,
      baseAddress: Int = 0
  ) = {
    require(busCtrl.busDataWidth == 32)
    driveFrom(io, busCtrl, config, baseAddress)
  }
}

case class VideoColorRgb(bitsPerContent: Int = 8) extends Bundle {
  val R = UInt(bitsPerContent bits)
  val G = UInt(bitsPerContent bits)
  val B = UInt(bitsPerContent bits)
  // index 0 will be LS(channel)
  def channels = Seq(B, G, R)
}

case class VideoColorYuv(bitsPerContent: Int = 8) extends Bundle {
  val Y = UInt(bitsPerContent bits)
  val U = UInt(bitsPerContent bits)
  val V = UInt(bitsPerContent bits)
  // index 0 will be LS(channel)
  def channels = Seq(V, U, Y)
}

case class VideoColorAlpha(bitsPerContent: Int = 8) extends Bundle {
  val A = UInt(bitsPerContent bits)
}

case class VideoIOs(p: VideoTimingParameter) extends Bundle {
  // output enable must keep active
  // any deactivation will reset
  val OE = in Bool ()
  val VSYNC = out Bool ()
  val HSYNC = out Bool ()
  val VBLANK = out Bool ()
  val HBLANK = out Bool ()
  val DE = out Bool ()

  val h_total_fix = p.hActive + p.hFrontPorch + p.hSync + p.hBackPorch
  val v_total_fix = p.vActive + p.vFrontPorch + p.vSync + p.vBackPorch

  // 16 bit is defined by 16k resolution < 20,000
  val h_bw = if (p.withDynamicSetup) 16 else U(h_total_fix).getBitsWidth
  val v_bw = if (p.withDynamicSetup) 16 else U(v_total_fix).getBitsWidth

  val VCOUNT = if (p.withCounterOutput) Some(out UInt (v_bw bits)) else None
  val HCOUNT = if (p.withCounterOutput) Some(out UInt (h_bw bits)) else None

  val VACTIVE = if (p.withCounterOutput) Some(out UInt (v_bw bits)) else None
  val HACTIVE = if (p.withCounterOutput) Some(out UInt (h_bw bits)) else None
}

case class VideoTimingIOs(p: VideoTimingParameter) extends Bundle {

  def opt[T <: Data](gen: => T): Option[T] =
    if (p.withDynamicSetup) Some(gen) else None

  val active = opt(UInt(16 bits))
  val frontPorch = opt(UInt(10 bits))
  val sync = opt(UInt(10 bits))
  val backPorch = opt(UInt(10 bits))
  val syncPolarity = opt(Bool())
  val blankPolarity = opt(Bool())
}
