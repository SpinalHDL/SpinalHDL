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
// File: VideoTimingCtrl.scala
// Designed By: BrianSune
// Contact: briansune@gmail.com
// =======================================================================

package spinal.lib.graphic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}

object Apb3VideoTimingCtrl {
  def getApb3Config = Apb3Config(
    addressWidth = 5,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )
}

case class Apb3VideoTimingCtrl(config: VideoTimingParameter) extends Component {

  val io = new Bundle {
    val apb = slave(Apb3(Apb3VideoTimingCtrl.getApb3Config))
    val videoIF = new VideoIOs(config)
  }

  val vtc = new VideoTimingCtrl(config)

  val soft_enable = Reg(Bool()) init (False)

  io.videoIF <> vtc.io.videoIF
  io.videoIF.OE.removeStatement()
  vtc.io.videoIF.OE.removeAssignments(true)
  vtc.io.videoIF.OE := soft_enable

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = vtc.driveFrom32(busCtrl, config)

  busCtrl.readAndWrite(soft_enable, 0x0c, 0, "soft enable")
}

object GenerateApb3VideoTimingCtrl {
  def main(args: Array[String]): Unit = {

    val report = SpinalVerilog(
      new Apb3VideoTimingCtrl(
        VideoTimingParameter(
          withDynamicSetup = true,
          withCounterOutput = true
        )
      )
    )

    report.printPruned()
  }
}
