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

trait VideoTimingInterface {
  def h_active: Option[UInt]
  def h_front_porch: Option[UInt]
  def h_sync: Option[UInt]
  def h_back_porch: Option[UInt]
  def h_sync_polarity: Option[Bool]
  def h_blank_polarity: Option[Bool]

  def v_active: Option[UInt]
  def v_front_porch: Option[UInt]
  def v_sync: Option[UInt]
  def v_back_porch: Option[UInt]
  def v_sync_polarity: Option[Bool]
  def v_blank_polarity: Option[Bool]
}

object VideoTimingCtrlBusMapping {
  def driveFrom(
      io: Bundle { val videoCfg: VideoTimingIOs },
      busCtrl: BusSlaveFactory,
      config: VideoTimingParameter,
      baseAddress: Int = 0
  ) = new Area {
    require(busCtrl.busDataWidth == 32)
    require(config.withDynamicSetup)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    val cfg_regs = Reg(new VideoTimingIOs(config))

    io.videoCfg := cfg_regs

    busCtrlWrapped.readAndWrite(cfg_regs.v_active.get, 0x00, 16, "v active")
    busCtrlWrapped.readAndWrite(cfg_regs.h_active.get, 0x00, 0, "h active")

    busCtrlWrapped.readAndWrite(cfg_regs.h_blank_polarity.get, 0x04, 31, "h blank polarity")
    busCtrlWrapped.readAndWrite(cfg_regs.h_sync_polarity.get, 0x04, 30, "h sync polarity")
    busCtrlWrapped.readAndWrite(cfg_regs.h_front_porch.get, 0x04, 20, "h front porach")
    busCtrlWrapped.readAndWrite(cfg_regs.h_sync.get, 0x04, 10, "h sync")
    busCtrlWrapped.readAndWrite(cfg_regs.h_back_porch.get, 0x04, 0, "h back porach")

    busCtrlWrapped.readAndWrite(cfg_regs.v_blank_polarity.get, 0x08, 31, "v blank polarity")
    busCtrlWrapped.readAndWrite(cfg_regs.v_sync_polarity.get, 0x08, 30, "v sync polarity")
    busCtrlWrapped.readAndWrite(cfg_regs.v_front_porch.get, 0x08, 20, "v front porach")
    busCtrlWrapped.readAndWrite(cfg_regs.v_sync.get, 0x08, 10, "v sync")
    busCtrlWrapped.readAndWrite(cfg_regs.v_back_porch.get, 0x08, 0, "v back porach")
  }

  def driveFrom32(
      io: Bundle { val videoCfg: VideoTimingIOs },
      busCtrl: BusSlaveFactory,
      config: VideoTimingParameter,
      baseAddress: Int = 0
  ) = {
    require(busCtrl.busDataWidth == 32)
    driveFrom(io, busCtrl, config, baseAddress)
  }
}

case class VideoColorRgb(bitsPerContent: Int = 8) extends Bundle {
  val R = UInt (bitsPerContent bits)
  val G = UInt (bitsPerContent bits)
  val B = UInt (bitsPerContent bits)
  // index 0 will be LS(channel)
  def channels = Seq(B, G, R)
}

case class VideoColorYuv(bitsPerContent: Int = 8) extends Bundle {
  val Y = UInt (bitsPerContent bits)
  val U = UInt (bitsPerContent bits)
  val V = UInt (bitsPerContent bits)
  // index 0 will be LS(channel)
  def channels = Seq(V, U, Y)
}

case class VideoColorAlpha(bitsPerContent: Int = 8) extends Bundle {
  val A = UInt (bitsPerContent bits)
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

  // 16 bit is defined by 16k resolution < 20,000
  val maxVal = (1 << 16) - 1
  val h_total_fix = p.hActive + p.hFrontPorch + p.hSync + p.hBackPorch
  val v_total_fix = p.vActive + p.vFrontPorch + p.vSync + p.vBackPorch

  val h_total = if (p.withDynamicSetup) maxVal else h_total_fix
  val v_total = if (p.withDynamicSetup) maxVal else v_total_fix

  val VCOUNT = if (p.withCounterOutput) Some(out UInt (U(v_total).getWidth bits)) else None
  val HCOUNT = if (p.withCounterOutput) Some(out UInt (U(h_total).getWidth bits)) else None

  val VACTIVE = if (p.withCounterOutput) Some(out UInt (U(v_total).getWidth bits)) else None
  val HACTIVE = if (p.withCounterOutput) Some(out UInt (U(h_total).getWidth bits)) else None
}

case class VideoTimingIOs(p: VideoTimingParameter) extends Bundle with VideoTimingInterface {

  def optIn[T <: Data](gen: => T): Option[T] =
    if (p.withDynamicSetup) Some(in(gen)) else None

  val v_front_porch = optIn(UInt(10 bits))
  val h_front_porch = optIn(UInt(10 bits))
  val v_back_porch = optIn(UInt(10 bits))
  val h_back_porch = optIn(UInt(10 bits))
  val v_sync = optIn(UInt(10 bits))
  val h_sync = optIn(UInt(10 bits))
  val v_active = optIn(UInt(16 bits))
  val h_active = optIn(UInt(16 bits))
  val v_sync_polarity = optIn(Bool())
  val v_blank_polarity = optIn(Bool())
  val h_sync_polarity = optIn(Bool())
  val h_blank_polarity = optIn(Bool())
}
