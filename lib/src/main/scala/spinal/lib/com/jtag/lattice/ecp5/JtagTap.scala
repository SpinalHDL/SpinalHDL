package spinal.lib.com.jtag.lattice.ecp5

import spinal.core._
import spinal.lib._

import spinal.lib.blackbox.lattice.ecp5.{JtaggIo, JTAGG, JtaggGeneric}
import spinal.lib.com.jtag.{JtagTapFunctions, JtagTapFactory, JtagTapInstructionCtrl}
// import spinal.lib.com.jtag.{JtagInstructionWrapper}


import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

//═════════════════════════════════════════════════════════════════════════════//
// This JtagTap implements a version of the SpinalHDL spinal.lib.com.jtag.JtagTap
// controller specifically for the Lattice ECP5 JTAGG Hard-IP Block.
//
// This implementation has some limitations. We don't have fully control over the Hard-IP.
// But we can implement the same signature as the spinal.lib.com.jtag.JtagTap
// thus they're used the same, but the ECP5 JTAGG has only 2 Instruction-Codes to use
// The InstructionWith is always 8-bit long, but kept as a variable for compatibility
//
// Lattice supports two private JTAG instructions, ER1 (0x32) and ER2 (0x38).
// The JtagTap of the FPGA itself will then reroute the TDI, TDO Signals to dedicated
// signals of the JTAGG Hard-IP Block JTDI, JTDO1, JTDO2
//
// Also the Hard-IP decodes the JTAG Tap state machine:
// The State-Machine singals are : JTDO1, JTDO2, JCE1, JCE2, JRTI1, JRTI2
//
//═════════════════════════════════════════════════════════════════════════════//
//
//                  ╔═════════╗
//                  ║ JtagTap ║
//          JTDOx---║         ║---JTCK
//                  ║         ║---JTDI
//                  ║         ║
//                  ║         ║---JSHIFT
//                  ║         ║---JUPDATE
//                  ║         ║---JRSTN
//                  ║         ║---JRTIx
//                  ║         ║---JCEx
//                  ╚═════════╝
//
//════════════════╦═════╦═══════════════════════════════════════════════════════
// JSHIFT         ║ out ║ True when in Shift-DR state
// JUPDATE        ║ out ║ True when in Update-DR state
// JRSTN          ║ out ║ True when in Test-Logic-Reset state
// JRTIx          ║ out ║ True when in Run-Test/Idle for the specific ERx Command
// JCEx           ║ out ║ True when in Capture-DR or Shift-DR for the specific ERx Command
//════════════════╬═════╬═══════════════════════════════════════════════════════
// JTDI           ║ out ║ Internal TDI rerouted if one of the ERx Command is set
// JTDOx          ║ in  ║ Internal TDO rerouted if the specific ERx Command is set
//════════════════╬═════╬═══════════════════════════════════════════════════════
// JTCK           ║ out ║ Internal TCK signal -> will be used as ClockDomain
//════════════════╩═════╩═══════════════════════════════════════════════════════
//
// The CLK Signal is buffered and so are all status signals based on this clk
// when we receive a JTAG toggle, the internal state machine will update all
// the status signals accordingly. Thus the last pulse of the JTAG receive toggle
// will change the SHIFT_DRx signal to the next state. Thus we have to generate
// a "late shift" signal to enable this last receive pulse.

//══════════════════════════════════════════════════════════════════════════════
// define lattice.ecp5 specific enums
object JtagTapState extends SpinalEnum {
  val Reset, Idle, Update_DR, Capture_DR, Shift_DR = newElement()
}

//══════════════════════════════════════════════════════════════════════════════
// define lattice.ecp5 specific JTagTap
//
class JtagTap(io: JtaggIo, instructionWidth: Int=8) extends Area
                                                    with JtagTapFunctions{

  assert( instructionWidth == 8,
    """The ECP5 JTAGG implements only 8-Bit instruction width.\n
      |Only instructions 0x32 / 0x38 are for embedded jtag usage""".stripMargin)

  val instruction     = Bool // 0-> 0x32 / 1-> 0x38
  val tdo             = Bool(false)
  val state           = asJtagTapState(io)
  val lastInstruction = Reg(Bool) init(False)
  
  when(io.JCE1.rise){
    lastInstruction := False
  }

  when(io.JCE2.rise){
    lastInstruction := True
  }

  instruction := B(io.JCE1 ## io.JCE2 ## lastInstruction).mux(
    M"10-"  -> False,
    M"01-"  -> True,
    default -> lastInstruction
  )

  io.JTDO1 := tdo;
  io.JTDO2 := tdo;

  def asJtagTapState(io: JtaggIo): JtagTapState.C = {
    val JCE = (io.JCE1 || io.JCE2)
    B(io.JRSTN ## io.JSHIFT ## io.JUPDATE ## JCE).mux(
      M"0---"  -> JtagTapState.Reset,         // JtagState : Reset
      M"1-1-"  -> JtagTapState.Update_DR,     // JtagState : DR-Update for ER1 / ER2
      M"1101"  -> JtagTapState.Shift_DR,      // JtagState : DR-Shift for ER1 / ER2
      M"1001"  -> JtagTapState.Capture_DR,    // JtagState : DR-Capture for ER1 / ER2
      default  -> JtagTapState.Idle           // JtagState : every other state
    )
  }

  def map(ctrl : JtagTapInstructionCtrl, instructionId : Int): Unit ={
    // instructionId can be either 0x32 / 0x38
    // to have a small (synthesised) area footprint we convert the 0x32 / 0x38 to a Bool
    // 0x32 : False / 0x38 : True
    assert( instructionId == 0x32 || instructionId == 0x38,
    """The ECP5 JTAGG implements two user instructions: 0x32 / 0x38 for embedded jtag usage.\n
      |use 0x32 or 0x38 as instructionId""".stripMargin)

    val idshort = if(instructionId == 0x38) 1 else 0 

    ctrl.tdi     := io.JTDI
    ctrl.enable  := instruction.asBits === idshort
    ctrl.capture := state === JtagTapState.Capture_DR
    ctrl.shift   := state === JtagTapState.Shift_DR
    ctrl.update  := state === JtagTapState.Update_DR
    ctrl.reset   := state === JtagTapState.Reset
    when(ctrl.enable) { tdo := ctrl.tdo }
  }

  // implement traits of JtagTapFunctions
  override def idcode(value: Bits)(instructionId: Int) = 
    assert(false, """sorry a custom JTAG idcode is not supported by the embedded jtagg controller\n
                   |Idcode always 0xE0\n
                   |delete the code ... = tap.idcode(...) it's not necessary\n""".stripMargin)

  override def read[T <: Data](data: T, light : Boolean = false)(instructionId: Int) = {
    val area = new JtagTapInstructionRead(data, light = light)
    map(area.ctrl, instructionId)
    area
  }

  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) = {
    val area = new JtagTapInstructionWrite(data, cleanUpdate, readable)
    map(area.ctrl, instructionId)
    area
  }
  
  override def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) = {
    val area = new JtagTapInstructionFlowFragmentPush(sink, sinkClockDomain)
    map(area.ctrl, instructionId)
    area
  }

  // def instructionWrapper(headerWidth : Int) (instructionId: Int)  = {
  //   val area = new JtagInstructionWrapper(headerWidth)
  //   map(area.ctrl, instructionId)
  //   area
  // }

}

//══════════════════════════════════════════════════════════════════════════════
// define SimpleJtagTap
//
class SimpleJtagTap extends Component {
  val io = new Bundle {
    val clk     = in  Bool
    val rst     = in  Bool
    val switchs = in  Bits(8 bit)
    val leds    = out Bits(8 bit)
  }

  val jtagg = new JTAGG(JtaggGeneric().copy())
  val ctrl = new ClockingArea(ClockDomain(io.clk, io.rst)) {
    val tap = JtagTapFactory(jtagg, 8)
    val switchsArea = tap.read(io.switchs)(0x32)
    val ledsArea = tap.write(io.leds)(0x38)
  }
}

//══════════════════════════════════════════════════════════════════════════════
// define SimpleJtagTap object
//
object SimpleJtagTap {
  def main(args: Array[String]) {
    SpinalVhdl(new SimpleJtagTap())
  }
}
