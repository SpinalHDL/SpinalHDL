package spinal.lib.com.jtag.lattice.ecp5

import spinal.core._
import spinal.lib._

import spinal.lib.blackbox.lattice.ecp5.{JtaggIo, JTAGG, JtaggGeneric}
import spinal.lib.com.jtag.{JtagTapFunctions, JtagTapShifter, JtagTapFactory}

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
// define lattice.ecp5 specific traits
//
trait JtagTapInternalProtocol {
  def getTdi : Bool
  def setTdo(value : Bool) : Unit
  def getJTaggState : JtagTapState.C
}

//══════════════════════════════════════════════════════════════════════════════
// define lattice.ecp5 specific enums
//
object JtagTapState extends SpinalEnum {
  val Reset, Idle, Update_DR1, Capture_DR1, Shift_DR1, Shift_DR1_Last,
                   Update_DR2, Capture_DR2, Shift_DR2, Shift_DR2_Last = newElement()
}

//══════════════════════════════════════════════════════════════════════════════
// define lattice.ecp5 specific JTagTap
//
class JtagTap(io: JtaggIo, instructionWidth: Int=8) extends Area
                                                    with JtagTapFunctions
                                                    with JtagTapInternalProtocol{

  assert( instructionWidth == 8,
    """The ECP5 JTAGG implements only 8-Bit instruction width.\n
      |Only instructions 0x32 / 0x38 are for embedded jtag usage""".stripMargin)

  val lateShift1  = RegNext(io.JSHIFT && io.JCE1) init(False)
  val lateShift2  = RegNext(io.JSHIFT && io.JCE2) init(False)

  val trigger1    = Reg(Bool) init(False)
  val trigger2    = Reg(Bool) init(False)

  when(io.JCE1.rise){
    trigger1 := True
  }
  when(io.JCE2.rise){
    trigger2 := True
  }

  when(io.JUPDATE.fall){
    trigger1 := False
    trigger2 := False
  }

  val state       = JtagTapState()
  val tdo         = Bool

  tdo             := False // default
  state           := asJtagTapState(io, lateShift1, lateShift2, trigger1, trigger2)

  io.JTDO1 := tdo; //default
  io.JTDO2 := tdo; //default

  def asJtagTapState(io: JtaggIo, lateShift1 : Bool, lateShift2 : Bool, trigger1 : Bool, trigger2 : Bool): JtagTapState.C = {
    B(io.JRSTN ## io.JSHIFT ## io.JUPDATE ## io.JCE1 ## io.JCE2  ## lateShift1 ## lateShift2 ## trigger1 ## trigger2).mux(
      M"0--------"  -> JtagTapState.Reset,          // JtagState : Reset
      M"1-1----1-"  -> JtagTapState.Update_DR1,     // JtagState : DR-Update for ER1
      M"1-1-----1"  -> JtagTapState.Update_DR2,     // JtagState : DR-Update for ER2
      M"11-1-----"  -> JtagTapState.Shift_DR1,      // JtagState : DR-Shift for ER1
      M"11--1----"  -> JtagTapState.Shift_DR2,      // JtagState : DR-Shift for ER2
      M"10-1-----"  -> JtagTapState.Capture_DR1,    // JtagState : DR-Capture for ER1
      M"10--1----"  -> JtagTapState.Capture_DR2,    // JtagState : DR-Capture for ER2
      M"1----1---"  -> JtagTapState.Shift_DR1_Last, // JtagState : (late) DR-Shift for ER1 or ER2
      M"1-----1--"  -> JtagTapState.Shift_DR2_Last, // JtagState : (late) DR-Shift for ER1 or ER2
      default       -> JtagTapState.Idle            // JtagState : every other state
    )
  }

  // implement traits of JtagTapInternalProtocol
  override def getTdi : Bool = io.JTDI
  override def setTdo(value: Bool): Unit = tdo := value
  override def getJTaggState : JtagTapState.C = state

  // implement traits of JtagTapFunctions
  override def idcode(value: Bits)(instructionId: Int): Unit =
    assert(false, """sorry a custom JTAG idcode is not supported by the embedded jtagg controller\n
                  |Idcode always 0xE0\n
                  |delete the code ... = tap.idcode(...) it's not necessary\n""".stripMargin)
  override def read[T <: Data](data: T, light : Boolean = false)(instructionId: Int) =
    new JtagTapCommandRead(data)(this, instructionId)
  override def write[T <: Data](data: T, cleanUpdate: Boolean = true, readable: Boolean = true)(instructionId: Int) =
    new JtagTapCommandWrite[T](data, cleanUpdate, readable)(this, instructionId)
  override def flowFragmentPush[T <: Data](sink : Flow[Fragment[Bits]], sinkClockDomain : ClockDomain)(instructionId: Int) =
    new JtagTapCommandFlowFragmentPush(sink, sinkClockDomain)(this, instructionId)
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
