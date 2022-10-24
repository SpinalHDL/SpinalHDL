package spinal.lib.com.jtag.altera

import spinal.core._
import spinal.lib.blackbox.altera.VJTAG
import spinal.lib.bus.bmb.{Bmb, BmbInterconnectGenerator}
import spinal.lib.generator._
import spinal.lib.master
import spinal.lib.system.debugger.{JtagBridgeNoTap, SystemDebugger, SystemDebuggerConfig}

case class VJtag2BmbMaster(ignoreWidth : Int) extends Component{
  val jtagConfig = SystemDebuggerConfig()

  val io = new Bundle{
    val bmb = master(Bmb(jtagConfig.getBmbParameter))
  }

  val vjtag = VJTAG()
  val jtagClockDomain = ClockDomain(vjtag.tck)

  val jtagBridge = new JtagBridgeNoTap(jtagConfig, jtagClockDomain, ignoreWidth)
  jtagBridge.io.ctrl << vjtag.toJtagTapInstructionCtrl()

  val debugger = new SystemDebugger(jtagConfig)
  debugger.io.remote <> jtagBridge.io.remote

  io.bmb << debugger.io.mem.toBmb()
}

case class VJtag2BmbMasterGenerator(ignoreWidth : Int)(implicit interconnect : BmbInterconnectGenerator) extends Generator{
  val bmb = produce(logic.io.bmb)
  val logic = add task VJtag2BmbMaster(ignoreWidth)
  interconnect.addMaster(
    accessRequirements = SystemDebuggerConfig().getBmbParameter,
    bus = bmb
  )
}