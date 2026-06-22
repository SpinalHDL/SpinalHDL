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
// File: AxiLite4VideoTestPattern.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axilite.AxiLite4SlaveFactory

object AxiLite4VideoTestPattern {
  def getAxi4LiteConfig = AxiLite4Config(
    addressWidth = 5,
    dataWidth = 32
  )
}

case class AxiLite4VideoTestPattern(
    vtpp: VideoPatternParameter,
    vtcp: VideoTimingParameter
) extends Component {
  val io = new Bundle {
    val axi4l = slave(AxiLite4(AxiLite4VideoTimingCtrl.getAxi4LiteConfig))
    val videoIF = new VideoIOs(vtcp.copy(withCounterOutput = vtpp.withCounterOutput))
    val RGB = if (vtpp.outputTypeRGB) Some(out(new VideoColorRgb())) else None
    val YUV = if (vtpp.haveYUV) Some(out(new VideoColorYuv())) else None
  }

  val vtp = new VideoTestPattern(vtpp, vtcp)

  val soft_enable = Reg(Bool()) init (False)

  io.videoIF <> vtp.io.videoIF
  io.videoIF.OE.removeStatement()
  vtp.io.videoIF.OE.removeAssignments(true)
  vtp.io.videoIF.OE := soft_enable
  if (vtpp.outputTypeRGB) { io.RGB.get <> vtp.io.RGB.get }
  if (vtpp.haveYUV) { io.YUV.get <> vtp.io.YUV.get }

  val busCtrl = AxiLite4SlaveFactory(io.axi4l)
  if (vtcp.withDynamicSetup) {
    val bridge = VideoTimingCtrlBusMapping.driveFrom32(vtp.io, busCtrl, vtcp)
  }

  if (vtpp.selectionCount > 1) {
    val pattern_sel = Reg(UInt(U(vtpp.selectionCount).getBitsWidth bits)) init (U(0))
    busCtrl.readAndWrite(pattern_sel, 0x10, 0, "Pattern Select")
    vtp.io.PATTERN_SEL.get := pattern_sel
  }

  busCtrl.readAndWrite(soft_enable, 0x0c, 0, "soft enable")
}

object GenerateAxiLite4VideoTestPattern {
  def main(args: Array[String]): Unit = {

    val report = SpinalVerilog(
      new AxiLite4VideoTestPattern(
        VideoPatternParameter(
          outputTypeRGB = false,
          outputTypeYUV444 = true
          // withColorBar = false,
          // withFillRed = false,
          // withFillGreen = false,
          // withFillBlue = false,
          // withGrayScale = false,
          // withColorChecker = false
          // withCounterOutput = true
          // withCounterOutput = false
        ),
        VideoTimingParameter(
          withDynamicSetup = true
        )
      )
    )

    report.printPruned()
  }
}
