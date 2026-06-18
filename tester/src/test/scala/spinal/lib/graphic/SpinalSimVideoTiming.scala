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
// File: SpinalSimVideoTiming.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}
import spinal.lib.graphic.VideoResolutions.VideoTimingPimper


class SpinalSimVideoTiming extends SpinalAnyFunSuite {
  def runVideoSim(p: VideoTimingParameter, name: String): Unit = {
    // 1. Compile the design
    val compiled = SimConfig.withWave.withConfig(SpinalConfig(verbose = true)).compile(
      VideoTimingCtrl(p)
    )

    // 2. Run the simulation
    compiled.doSim(name) { dut =>
      dut.clockDomain.forkStimulus(period = 10) // 100MHz clock

      val timingConfig = VideoResolutions.timingLibrary("h1920_v1080_r60")

      if (p.withDynamicSetup) {
        timingConfig.applyTo(dut.io.videoCfg)
      }

      var i = 0

      dut.io.videoIF.OE #= true
      dut.clockDomain.waitSampling(10)

      fork{
        var lastVBlank = false
        
        while (true) {
          val currentVBlank = dut.io.videoIF.VBLANK.toBoolean
          
          if (currentVBlank && !lastVBlank) {
            // we do two frames timing.
            if(i >= 2){
              simSuccess()
            }
            i += 1
          }
          
          lastVBlank = currentVBlank
          dut.clockDomain.waitSampling()
        }
      }.join()
    }
  }

  test("Configs") {
    runVideoSim(
      VideoTimingParameter(),
      "Static"
    )

    runVideoSim(
      VideoResolutions.timingLibrary("h1024_v768_r60").copy(withCounterOutput = true),
      "Static-User"
    )

    runVideoSim(
      VideoTimingParameter(
        withCounterOutput = true,
        withDynamicSetup = true
      ),
      "Dynamic"
    )
  }
}