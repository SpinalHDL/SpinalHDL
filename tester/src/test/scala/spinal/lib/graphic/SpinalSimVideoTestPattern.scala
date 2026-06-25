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
// Trial Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: SpinalSimVideoTestPattern.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}
import spinal.lib.graphic.VideoResolutions.VideoTimingPimper


class SpinalSimVideoTestPattern extends SpinalAnyFunSuite {
  def runVideoSim(
    vtpp: VideoPatternParameter,
    vtcp: VideoTimingParameter, 
    name: String
  ): Unit = {
    // 1. Compile the design
    val compiled = SimConfig.withWave.withConfig(SpinalConfig(verbose = true)).compile(
      VideoTestPattern(vtpp, vtcp)
    )

    // 2. Run the simulation
    compiled.doSim(name) { dut =>
      dut.clockDomain.forkStimulus(period = 10) // 100MHz clock

      val timingConfig = VideoResolutions.timingLibrary("h1920_v1080_r60")

      if (vtcp.withDynamicSetup) {
        timingConfig.applyH(dut.io.videoH)
        timingConfig.applyV(dut.io.videoV)
      }

      var i = 0

      dut.io.videoIF.OE #= true
      dut.clockDomain.waitSampling()

      dut.io.PATTERN_SEL.foreach(_ #= i)
      dut.clockDomain.waitSampling(10)

      fork{
        var lastVBlank = false
        
        while (true) {
          val currentVBlank = dut.io.videoIF.VBLANK.toBoolean
          
          if (currentVBlank && !lastVBlank) {
            if(i >= vtpp.selectionCount){
              simSuccess()
            }
            dut.io.PATTERN_SEL.foreach(_ #= (i % vtpp.selectionCount))
            i += 1
          }
          
          lastVBlank = currentVBlank
          dut.clockDomain.waitSampling()
        }
      }.join()
    }
  }

  test("VideoTestPattern") {

    // runVideoSim(
    //   VideoPatternParameter(outputWithAlpha = true),
    //   VideoResolutions.timingLibrary("h1024_v768_r60"),
    //   "VideoTestPattern-ARGB"
    // )

    // runVideoSim(
    //   VideoPatternParameter(
    //     outputTypeRGB = false,
    //     outputTypeYUV444 = true,
    //     withFillRed = false,
    //     withFillGreen = false,
    //     withFillBlue = false,
    //     bitsPerContent = 10,
    //     withCounterOutput = true
    //   ),
    //   VideoResolutions.timingLibrary("h640_v480_r60"),
    //   "VideoTestPattern-YUV444"
    // )

    runVideoSim(
      VideoPatternParameter(
        withCounterOutput = true
      ),
      VideoTimingParameter(
        withDynamicSetup = true
      ),
      "VideoTestPattern-RGB-Dynamic"
    )
  }
}