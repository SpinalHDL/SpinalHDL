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
// Add any necessary resolutions for your needs.
// Trial Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: VideoResolutions.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.core.sim._

// Referenced from:
// https://tomverbeure.github.io/video_timings_calculator

/** Standardized Video Timing Parameters.
  * Reference: CEA-861 / VESA DMT
  * Polarity: true = Active High, false = Active Low
  */
object VideoResolutions {

  // Handy for simulation
  implicit class VideoTimingPimper(val p: VideoTimingParameter) {
    def applyTo(io: VideoTimingInterface): Unit = {
      io.h_active.foreach(port => port #= p.hActive)
      io.h_front_porch.foreach(port => port #= p.hFrontPorch)
      io.h_sync.foreach(port => port #= p.hSync)
      io.h_back_porch.foreach(port => port #= p.hBackPorch)
      io.h_sync_polarity.foreach(port => port #= p.hSyncPolarity)
      io.h_blank_polarity.foreach(port => port #= p.hBlankPolarity)

      io.v_active.foreach(port => port #= p.vActive)
      io.v_front_porch.foreach(port => port #= p.vFrontPorch)
      io.v_sync.foreach(port => port #= p.vSync)
      io.v_back_porch.foreach(port => port #= p.vBackPorch)
      io.v_sync_polarity.foreach(port => port #= p.vSyncPolarity)
      io.v_blank_polarity.foreach(port => port #= p.vBlankPolarity)
    }
  }

  val timingLibrary = Map(
    // 640x480 @ 60Hz (DMT)
    "h640_v480_r60" -> VideoTimingParameter(
      hActive = 640,
      hFrontPorch = 16,
      hSync = 96,
      hBackPorch = 48,
      hSyncPolarity = false,
      vActive = 480,
      vFrontPorch = 10,
      vSync = 2,
      vBackPorch = 33,
      vSyncPolarity = false
    ),

    // 800x600 @ 60Hz (DMT)
    "h800_v600_r60" -> VideoTimingParameter(
      hActive = 800,
      hFrontPorch = 40,
      hSync = 128,
      hBackPorch = 88,
      hSyncPolarity = true,
      vActive = 600,
      vFrontPorch = 1,
      vSync = 4,
      vBackPorch = 23,
      vSyncPolarity = true
    ),

    // 1024x768 @ 60Hz (DMT)
    "h1024_v768_r60" -> VideoTimingParameter(
      hActive = 1024,
      hFrontPorch = 24,
      hSync = 136,
      hBackPorch = 160,
      hSyncPolarity = false,
      vActive = 768,
      vFrontPorch = 3,
      vSync = 6,
      vBackPorch = 29,
      vSyncPolarity = false
    ),

    // 1280x720 @ 60Hz (CEA-861, VIC 4)
    "h1280_v720_r60" -> VideoTimingParameter(
      hActive = 1280,
      hFrontPorch = 110,
      hSync = 40,
      hBackPorch = 220,
      hSyncPolarity = true,
      vActive = 720,
      vFrontPorch = 5,
      vSync = 5,
      vBackPorch = 20,
      vSyncPolarity = true
    ),

    // 1280x1024 @ 60Hz (DMT)
    "h1280_v1024_r60" -> VideoTimingParameter(
      hActive = 1280,
      hFrontPorch = 48,
      hSync = 112,
      hBackPorch = 248,
      hSyncPolarity = true,
      vActive = 1024,
      vFrontPorch = 1,
      vSync = 3,
      vBackPorch = 38,
      vSyncPolarity = true
    ),

    // 1920x1080 @ 60Hz (CEA-861, VIC 16)
    "h1920_v1080_r60" -> VideoTimingParameter(
      hActive = 1920,
      hFrontPorch = 88,
      hSync = 44,
      hBackPorch = 148,
      hSyncPolarity = true,
      vActive = 1080,
      vFrontPorch = 4,
      vSync = 5,
      vBackPorch = 36,
      vSyncPolarity = true
    ),

    // 2560x1440 @ 60Hz (VESA CVT)
    "h2560_v1440_r60" -> VideoTimingParameter(
      hActive = 2560,
      hFrontPorch = 48,
      hSync = 32,
      hBackPorch = 80,
      hSyncPolarity = true,
      vActive = 1440,
      vFrontPorch = 3,
      vSync = 5,
      vBackPorch = 33,
      vSyncPolarity = true
    ),

    // 3840x2160 @ 60Hz (CEA-861, VIC 93)
    "h3840_v2160_r60" -> VideoTimingParameter(
      hActive = 3840,
      hFrontPorch = 176,
      hSync = 88,
      hBackPorch = 296,
      hSyncPolarity = true,
      vActive = 2160,
      vFrontPorch = 8,
      vSync = 10,
      vBackPorch = 72,
      vSyncPolarity = true
    ),

    // 4096x2160 @ 60Hz (CEA-861, VIC 98)
    "h4096_v2160_r60" -> VideoTimingParameter(
      hActive = 4096,
      hFrontPorch = 176,
      hSync = 88,
      hBackPorch = 296,
      hSyncPolarity = true,
      vActive = 2160,
      vFrontPorch = 8,
      vSync = 10,
      vBackPorch = 72,
      vSyncPolarity = true
    )
  )
}
