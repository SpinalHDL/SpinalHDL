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
// File: AxiLite4VideoTimingCtrl.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axilite.AxiLite4SlaveFactory

object AxiLite4VideoTimingCtrl {
  def getAxi4LiteConfig = AxiLite4Config(
    addressWidth = 5,
    dataWidth = 32
  )
}

case class AxiLite4VideoTimingCtrl(config: VideoTimingParameter) extends Component {
  val io = new Bundle {
    val axi4l = slave(AxiLite4(AxiLite4VideoTimingCtrl.getAxi4LiteConfig))
    val videoIF = new VideoIOs(config)
  }

  val vtc = new VideoTimingCtrl(config)

  val soft_enable = Reg(Bool()) init (False)

  io.videoIF <> vtc.io.videoIF
  io.videoIF.OE.removeStatement()
  vtc.io.videoIF.OE.removeAssignments(true)
  vtc.io.videoIF.OE := soft_enable

  val busCtrl = AxiLite4SlaveFactory(io.axi4l)
  val bridge = VideoTimingCtrlBusMapping.driveFrom32(vtc.io, busCtrl, config)

  busCtrl.readAndWrite(soft_enable, 0x0c, 0, "soft enable")
}

object GenerateAxiLite4VideoTimingCtrl {
  def main(args: Array[String]): Unit = {

    val report = SpinalVerilog(
      new AxiLite4VideoTimingCtrl(
        VideoTimingParameter(
          withDynamicSetup = true,
          withCounterOutput = true
        )
      )
    )

    report.printPruned()
  }
}
