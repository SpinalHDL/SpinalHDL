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
// Add in/out enable and map to 1 for testing
// Change loop operation for different test case
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File Revision: 0.9.0
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Trial Version
// Date: 2026/06
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// File: SpinalSimVideoColorSpace.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.core.sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}
import scala.collection.mutable.Queue


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

    def rgb_mixer(fa: Int, frgb: Int, ba: Int, brgb: Int, select_op: Int): (Int, Int) = {

      var a_c = select_op match{
        case 0 => {List(1.0, (255 - fa).toDouble / 256.0)}
        case 1 => {List(ba.toDouble / 256.0, 0.0)}
        case 2 => {List((255 - ba).toDouble / 256.0, 0.0)}
        case 3 => {List(0.0, 1.0)}
        case 4 => {List((255 - ba).toDouble / 256.0, (255 - fa).toDouble / 256.0)}
      }

      val ao = (fa * a_c(0) + ba * a_c(1)).toInt

      a_c = select_op match{
        case 0 => {List(1.0, (255 - fa).toDouble / 256.0)}
        case 1 => {List(ba.toDouble / 256.0, 0.0)}
        case 2 => {List((255 - ba).toDouble / 256.0, 0.0)}
        case 3 => {List(ba.toDouble / 256.0, (255 - fa).toDouble / 256.0)}
        case 4 => {List((255 - ba).toDouble / 256.0, (255 - fa).toDouble / 256.0)}
      }

      val bo = ((frgb & 0xff) * a_c(0) + (brgb & 0xff) * a_c(1)).toInt
      val go = (((frgb >> 8) & 0xff) * a_c(0) + ((brgb >> 8) & 0xff) * a_c(1)).toInt
      val ro = (((frgb >> 16) & 0xff) * a_c(0) + ((brgb >> 16) & 0xff) * a_c(1)).toInt
      (ao, (ro << 16) | (go << 8) | bo)
    }

    // 2. Run the simulation
    compiled.doSim(name) { dut =>
      dut.clockDomain.forkStimulus(period = 10) // 100MHz clock
      
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      def rgbMix_check (
        fRgb: Int, fA: Int, bRgb: Int, bA: Int,
        inputQueue: Queue[(Int, Int, Int, Int)],
        select_op: Int
      ) = {

        inputQueue.enqueue((bA, bRgb, fA, fRgb))
        val v: BigInt = (BigInt(bA)   & 0xFF) << 56 | 
                (BigInt(bRgb) & 0xFFFFFF) << 32 | 
                (BigInt(fA)   & 0xFF) << 24 | 
                (BigInt(fRgb) & 0xFFFFFF)
        dut.io.SPACE_IN #= v
        
        dut.clockDomain.waitRisingEdge()
        
        if (inputQueue.size > 4) {
          val sim_o = dut.io.SPACE_OUT.toBigInt
          val outRgb = (sim_o & 0xffffff)
          val outA = (sim_o & 0xff000000) >> 24
          val (qbA, qbRgb, qfA, qfRgb) = inputQueue.dequeue()
          println(f"OP Type: $select_op, Input: F=$qfA $qfRgb%06X, B=$qbA $qbRgb%06X")
          val (ao, rgbo) = rgb_mixer(qfA, qfRgb, qbA, qbRgb, select_op)
          println(f"Expected Output: RGB=$rgbo%06X, A=$ao | DUT Output: RGB=$outRgb%06X, A=$outA")
          assert((ao - outA).abs  <= 1, "Alpha mismatch.")
          assert(rgbo == outRgb, "RGB mismatch.")
        }
      }
      
      def check(
        d0: Int, d1: Int, d2: Int, inputQueue: Queue[(Int, Int, Int)]
      ): Unit = {

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

      if(vcsp.useMixARGB){
        for(select_op <- 0 until 5){
          val inputQueue = Queue[(Int, Int, Int, Int)]()
          dut.io.MIXER_OP.get #= select_op
          dut.io.IE #= 255
          dut.io.OE #= 15
          List(
            (0xFFFFFF, 255, 0x000000, 0),
            (0xFF0000, 128, 0x0000FF, 128),
            (0x000000, 0,   0xFFFFFF, 255),
            (0x000000, 0,   0xFFFFFF, 255),
            (0x000000, 0,   0xFFFFFF, 255),
            (0x000000, 0,   0xFFFFFF, 255),
            (0x000000, 0,   0xFFFFFF, 255)
          ).foreach{ case (fRgb, fA, bRgb, bA) =>
            rgbMix_check(fRgb, fA, bRgb, bA, inputQueue, select_op)
          }
        }
      }else if(vcsp.useRGB2YUV){
        val inputQueue = Queue[(Int, Int, Int)]()
        dut.io.IE #= 7
        dut.io.OE #= 7
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
        ).foreach{ case (d0, d1, d2) =>
          check(d0, d1, d2, inputQueue)
        }
      }else if(vcsp.useYUV2RGB){
        val inputQueue = Queue[(Int, Int, Int)]()
        dut.io.IE #= 7
        dut.io.OE #= 7
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
        ).foreach{ case (d0, d1, d2) =>
          check(d0, d1, d2, inputQueue)
        }
      }
    }
  }

  val standards = List("470", "601F", "601TV")
  val convert = List("RGB2YUV", "YUV2RGB")

  test("VideoColorSpaceMixer") {
    runVideoSim(
      VideoSpaceParameter(
        bitsPerContent = 8,
        useMixARGB = true,
        useRGB2YUV = false,
        useYUV2RGB = false,
      ),
      s"Video Color Space RGB Mixer"
    )
  }

  // for (
  //   RGBYUV <- List(false, true); stdBTxx <- 0 to 2; bw <- List(8)) {
    
  //   val name = s"Standard BT-${standards(stdBTxx)} ${convert(RGBYUV.toInt)} BitWidth-$bw"

  //   test(name) {
  //     runVideoSim(
  //       VideoSpaceParameter(
  //         bitsPerContent = bw,
  //         useRGB2YUV = RGBYUV,
  //         useYUV2RGB = !RGBYUV,
  //         stdBT470 = (stdBTxx == 0),
  //         stdBT601Full = (stdBTxx == 1),
  //         stdBT601TV = (stdBTxx == 2)
  //       ),
  //       name
  //     )
  //   }
  // }
}