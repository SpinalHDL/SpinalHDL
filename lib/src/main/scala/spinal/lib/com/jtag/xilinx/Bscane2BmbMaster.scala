package spinal.lib.com.jtag.xilinx

import spinal.core._
import spinal.lib.blackbox.xilinx.s7.BSCANE2
import spinal.lib.bus.bmb.{Bmb, BmbInterconnectGenerator}
import spinal.core.fiber._
import spinal.lib.master
import spinal.lib.system.debugger.{JtagBridgeNoTap, SystemDebugger, SystemDebuggerConfig}

case class Bscane2BmbMaster(usedId : Int) extends Component{
  val jtagConfig = SystemDebuggerConfig()

  val io = new Bundle{
    val bmb = master(Bmb(jtagConfig.getBmbParameter))
  }

  val bscane2 = BSCANE2(usedId)
  val jtagClockDomain = ClockDomain(bscane2.TCK)

  val jtagBridge = new JtagBridgeNoTap(jtagConfig, jtagClockDomain)
  jtagBridge.io.ctrl << bscane2.toJtagTapInstructionCtrl()

  val debugger = new SystemDebugger(jtagConfig)
  debugger.io.remote <> jtagBridge.io.remote

  io.bmb << debugger.io.mem.toBmb()
}

case class Bscane2BmbMasterGenerator(userId : Int)(implicit interconnect : BmbInterconnectGenerator) extends Area{
  val bmb = Handle(logic.io.bmb)
  val logic = Handle(Bscane2BmbMaster(userId))
  interconnect.addMaster(
    accessRequirements = SystemDebuggerConfig().getBmbParameter,
    bus = bmb
  )
}