package spinal.lib.com.jtag

import spinal.core._
import spinal.lib.bus.bmb.BmbSmpInterconnectGenerator
import spinal.lib.generator.Generator
import spinal.lib.system.debugger._

case class JtagInstructionDebuggerGenerator()(implicit val interconnect : BmbSmpInterconnectGenerator) extends Generator{
  val jtagClockDomain = createDependency[ClockDomain]
  val jtagInstruction = produce(logic.jtagBridge.io.ctrl)
  val bmb = produce(logic.mmMaster)
  val jtagConfig = SystemDebuggerConfig(
    memAddressWidth = 32,
    memDataWidth    = 32,
    remoteCmdWidth  = 1
  )

  val logic = add task new Area{
    val jtagBridge = new JtagBridgeNoTap(jtagConfig, jtagClockDomain)
    val debugger = new SystemDebugger(jtagConfig)
    debugger.io.remote <> jtagBridge.io.remote

    val mmMaster = debugger.io.mem.toBmb()
  }

  interconnect.addMaster(
    accessRequirements = jtagConfig.getBmbParameter,
    bus = bmb
  )
}


case class JtagTapDebuggerGenerator()(implicit val interconnect : BmbSmpInterconnectGenerator) extends Generator{
  val jtag = produceIo(logic.jtagBridge.io.jtag)
  val bmb = produce(logic.mmMaster)
  val jtagConfig = SystemDebuggerConfig(
    memAddressWidth = 32,
    memDataWidth    = 32,
    remoteCmdWidth  = 1
  )

  val logic = add task new Area{

    val jtagBridge = new JtagBridge(jtagConfig)
    val debugger = new SystemDebugger(jtagConfig)
    debugger.io.remote <> jtagBridge.io.remote

    val mmMaster = debugger.io.mem.toBmb()
  }

  interconnect.addMaster(
    accessRequirements = jtagConfig.getBmbParameter,
    bus = bmb
  )
}