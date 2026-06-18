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
// File: VideoTimingInterfaces.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.lib._
import spinal.core._
import spinal.lib.graphic.VideoTimingCtrl._

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

class VideoIOs(p: VideoTimingParameter) extends Bundle {
  // output enable must keep active
  // any deactivation will reset
  val OE = in Bool ()
  val VSYNC = out Bool ()
  val HSYNC = out Bool ()
  val VBLANK = out Bool ()
  val HBLANK = out Bool ()
  val DE = out Bool ()

  val maxVal = (1 << 16) - 1
  val h_total_fix = p.hActive + p.hFrontPorch + p.hSync + p.hBackPorch
  val v_total_fix = p.vActive + p.vFrontPorch + p.vSync + p.vBackPorch

  val h_total = if (p.withDynamicSetup) maxVal else h_total_fix
  val v_total = if (p.withDynamicSetup) maxVal else v_total_fix

  val VCOUNT = if (p.withCounterOutput) Some(out UInt (log2Up(v_total) bits)) else None
  val HCOUNT = if (p.withCounterOutput) Some(out UInt (log2Up(h_total) bits)) else None

  val VACTIVE = out UInt (log2Up(v_total) bits)
  val HACTIVE = out UInt (log2Up(h_total) bits)
}

class VideoTimingIOs(p: VideoTimingParameter) extends Bundle with VideoTimingInterface {
  
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
