package spinal.lib.com.jtag

import spinal.core._
import spinal.lib.bus.bmb.BmbInterconnectGenerator
import spinal.core.fiber._
import spinal.lib.system.debugger._

case class JtagInstructionDebuggerGenerator()(implicit val interconnect : BmbInterconnectGenerator) extends Area{
  val jtagClockDomain = Handle[ClockDomain]
  val jtagInstruction = Handle(logic.jtagBridge.io.ctrl)
  val bmb = Handle(logic.mmMaster)
  val jtagConfig = SystemDebuggerConfig(
    memAddressWidth = 32,
    memDataWidth    = 32,
    remoteCmdWidth  = 1
  )

  val logic = Handle(new Area{
    val jtagBridge = new JtagBridgeNoTap(jtagConfig, jtagClockDomain)
    val debugger = new SystemDebugger(jtagConfig)
    debugger.io.remote <> jtagBridge.io.remote

    val mmMaster = debugger.io.mem.toBmb()
  })

  interconnect.addMaster(
    accessRequirements = jtagConfig.getBmbParameter,
    bus = bmb
  )
}


case class JtagTapDebuggerGenerator()(implicit val interconnect : BmbInterconnectGenerator) extends Area{
  val jtag = Handle(logic.jtagBridge.io.jtag.toIo)
  val bmb = Handle(logic.mmMaster)
  val jtagConfig = SystemDebuggerConfig(
    memAddressWidth = 32,
    memDataWidth    = 32,
    remoteCmdWidth  = 1
  )

  val logic =Handle(new Area{
    val jtagBridge = new JtagBridge(jtagConfig)
    val debugger = new SystemDebugger(jtagConfig)
    debugger.io.remote <> jtagBridge.io.remote

    val mmMaster = debugger.io.mem.toBmb()
  })

  interconnect.addMaster(
    accessRequirements = jtagConfig.getBmbParameter,
    bus = bmb
  )
}