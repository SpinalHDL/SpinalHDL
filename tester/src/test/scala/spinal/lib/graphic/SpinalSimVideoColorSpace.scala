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
// Revision: 0.9.0
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


class SpinalSimVideoColorSpace extends SpinalAnyFunSuite {
  def runVideoSim(
    vcsp: VideoSpaceParameter,
    name: String
  ): Unit = {
    // 1. Compile the design
    val compiled = SimConfig
    // .withWave
    .withConfig(SpinalConfig(verbose = true)).compile(
      VideoColorSpace(vcsp)
    )

    val full = (1 << (vcsp.bitsPerContent)) - 1
    val half = (1 << (vcsp.bitsPerContent - 1))
    val hex = (1 << (vcsp.bitsPerContent - 4))

    def clamp(i: Int): Int = Math.max(0, Math.min(full, i))

    def bt470(r: Int, g: Int, b: Int): (Int, Int, Int) = {
      val y = (0.299 * r + 0.587 * g + 0.114 * b).toInt
      val u = (0.436 * b - 0.14713 * r - 0.28886 * g).toInt
      val v = (0.615 * r - 0.51499 * g - 0.10001 * b).toInt
      (y, clamp(u + half), clamp(v + half))
    }

    def bt470_rev(y: Int, u: Int, v: Int): (Int, Int, Int) = {
      val r = (y + 1.13983 * (v-half)).toInt
      val g = (y -0.39465 * (u-half) - 0.5806 * (v-half)).toInt
      val b = (y + 2.03211 * (u-half)).toInt
      (clamp(r), clamp(g), clamp(b))
    }

    def bt601_full(r: Int, g: Int, b: Int): (Int, Int, Int) = {
      val y = (0.299 * r + 0.587 * g + 0.114 * b).toInt
      val u = (0.5 * b - 0.169 * r - 0.331 * g).toInt
      val v = (0.5 * r - 0.419 * g - 0.081 * b).toInt
      (y, clamp(u + half), clamp(v + half))
    }

    def bt601_full_rev(y: Int, u: Int, v: Int): (Int, Int, Int) = {
      val r = (y + 1.4075 * (v-half)).toInt
      val g = (y -0.3455 * (u-half) - 0.7169 * (v-half)).toInt
      val b = (y + 1.799 * (u-half)).toInt
      (clamp(r), clamp(g), clamp(b))
    }

    def bt601_tv(r: Int, g: Int, b: Int): (Int, Int, Int) = {
      val y = (0.257 * r + 0.504 * g + 0.098 * b).toInt
      val u = (0.439 * b - 0.148 * r - 0.291 * g).toInt
      val v = (0.439 * r - 0.368 * g - 0.071 * b).toInt
      (y + hex, clamp(u + half), clamp(v + half))
    }

    def bt601_tv_rev(y: Int, u: Int, v: Int): (Int, Int, Int) = {
      val r = (1.164 * (y - hex) + 1.596 * (v-half)).toInt
      val g = (1.164 * (y - hex) - 0.392 * (u-half) - 0.812 * (v-half)).toInt
      val b = (1.164 * (y - hex) + 2.016 * (u-half)).toInt
      (clamp(r), clamp(g), clamp(b))
    }

    // 2. Run the simulation
    compiled.doSim(name) { dut =>
      dut.clockDomain.forkStimulus(period = 10) // 100MHz clock

      val inputQueue = scala.collection.mutable.Queue[(Int, Int, Int)]()
      
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()
      
      def check(d0: Int, d1: Int, d2: Int): Unit = {
        val in0 = d0 & full
        val in1 = d1 & full
        val in2 = d2 & full
        inputQueue.enqueue((in0, in1, in2))
        val in_cat = (in0 << (2 * vcsp.bitsPerContent)) | (in1 << (vcsp.bitsPerContent)) | in2
        
        dut.io.SPACE_IN #= in_cat
        dut.clockDomain.waitSampling()
        val sim_o = dut.io.SPACE_OUT.toBigInt

        if (inputQueue.size > 4) {
          val (c0, c1, c2) = inputQueue.dequeue()

          val a0 = (sim_o >> (2 * vcsp.bitsPerContent) & full);
          val a1 = (sim_o >> (vcsp.bitsPerContent) & full);
          val a2 = (sim_o & full);
          
          println(s"Input R:$c0 G:$c1 B:$c2")
          val (e0, e1, e2) = vcsp match {
            case v if v.stdBT601Full & v.useRGB2YUV => bt601_full(c0, c1, c2)
            case v if v.stdBT601TV & v.useRGB2YUV   => bt601_tv(c0, c1, c2)
            case v if v.stdBT470 & v.useRGB2YUV     => bt470(c0, c1, c2)
            case v if v.stdBT470 & v.useYUV2RGB     => bt470_rev(c0, c1, c2)
            case v if v.stdBT601Full & v.useYUV2RGB => bt601_full_rev(c0, c1, c2)
            case v if v.stdBT601TV & v.useYUV2RGB   => bt601_tv_rev(c0, c1, c2)
            case _                                  => (c0, c1, c2)
          }

          println(s"Expected $e0\t|\tGot $a0")
          println(s"Expected $e1\t|\tGot $a1")
          println(s"Expected $e2\t|\tGot $a2")
          assert((a0 - e0).abs <= log2Up(vcsp.bitsPerContent), "Delta too much!")
          assert((a1 - e1).abs <= log2Up(vcsp.bitsPerContent), "Delta too much!")
          assert((a2 - e2).abs <= log2Up(vcsp.bitsPerContent), "Delta too much!")
        }
      }

      if(vcsp.useRGB2YUV){
        List(
          (full, 0, 0),
          (0, full, 0),
          (0, 0, full),
          (full, 0, full),
          (full, full, 0),
          (0, full, full),
          (0, 0, 0),
          (full, full, full),
          (full, full, full),
          (full, full, full),
          (full, full, full),
          (full, full, full),
        ).foreach((check _).tupled)
      }else if(vcsp.useYUV2RGB){
        List(
          (0x4C, 0x55, 0xFF), // Red
          (0x96, 0x2C, 0x15), // Green
          (0x1D, 0xFF, 0x6B), // Blue
          (0x69, 0xD4, 0xEA), // Magenta
          (0xE2, 0x01, 0x95), // Yellow
          (0xB3, 0xB0, 0x10), // Cyan
          (0x00, 0x80, 0x80), // Black
          (0xFF, 0x80, 0x80), // White
          (0xFF, 0x80, 0x80),
          (0xFF, 0x80, 0x80),
          (0xFF, 0x80, 0x80),
          (0xFF, 0x80, 0x80)
        ).foreach((check _).tupled)
      }
    }
  }

  test("VideoColorSpace") {

    runVideoSim(
      VideoSpaceParameter(
        // bitsPerContent = 10,
        useRGB2YUV = true,
        useYUV2RGB = false,
        stdBT470 = true,
        stdBT601Full = false,
        stdBT601TV = false
      ),
      "Standard BT.470 YUV2RGB"
    )

    runVideoSim(
      VideoSpaceParameter(
        // bitsPerContent = 10,
        useRGB2YUV = true,
        useYUV2RGB = false,
        stdBT470 = false,
        stdBT601Full = true,
        stdBT601TV = false
      ),
      "Standard BT.601-Full YUV2RGB"
    )

    runVideoSim(
      VideoSpaceParameter(
        // bitsPerContent = 10,
        useRGB2YUV = true,
        useYUV2RGB = false,
        stdBT470 = false,
        stdBT601Full = false,
        stdBT601TV = true
      ),
      "Standard BT.601-TV YUV2RGB"
    )
  }
}