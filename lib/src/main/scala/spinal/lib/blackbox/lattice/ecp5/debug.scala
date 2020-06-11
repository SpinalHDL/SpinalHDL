package spinal.lib.blackbox.lattice.ecp5

import spinal.core._
import spinal.lib._

//═════════════════════════════════════════════════════════════════════════════//
// JTAGG
// JTAG (Joint Test Action Group) Controller
//═════════════════════════════════════════════════════════════════════════════//
// Lattice supports two private JTAG instructions, ER1 (0x32) and ER2 (0x38).
//
//                  ╔═══════╗
//                  ║ JTAGG ║
//                  ║       ║
//     Ctrl.JTDO1---║       ║---Ctrl.JTCK
//     Ctrl.JTDO2---║       ║---Ctrl.JTDI
//                  ║       ║
//                  ║       ║---Status.JSHIFT
//                  ║       ║---Status.JUPDATE
//                  ║       ║---Status.JRSTN
//                  ║       ║---Status.JRTI1
//                  ║       ║---Status.JRTI2
//                  ║       ║---Status.JCE1
//                  ║       ║---Status.JCE2
//                  ╚═══════╝
//
//════════════════╦════╦═══════════════════════════════════════════════════════//
// Status.JSHIFT  ║ out║ Signal goes high when TAP Controller State is Shift-DR
// Status.JUPDATE ║ out║ Signal goes high when TAP controller state is Update-DR.
// Status.JRSTN   ║ out║ Signal goes "low" when TAP controller state is Test-Logic-Reset.
// Status.JRTI1   ║ out║ If ER1 instruction is shifted into the JTAG instruction register, JRTI1 will go high when TAP controller is in Run-Test/Idle state.
// Status.JRTI2   ║ out║ If ER2 instruction is shifted into the JTAG instruction register, JRTI2 will go high when TAP controller is in Run-Test/Idle state.
// Status.JCE1    ║ out║ If ER1 instruction is shifted into the JTAG instruction register, JCE1 will go high when TAP controller is in Capture-DR or Shift-DR states.
// Status.JCE2    ║ out║ If ER2 instruction is shifted into the JTAG instruction register, JCE2 will go high when TAP controller is in Capture-DR or Shift-DR states
//════════════════╬════╬═══════════════════════════════════════════════════════
// Ctrl.JTCK      ║ out║ JTAG Test Clock (connects to TCK), for internal logic to control non-disruptive reconfiguration through JTAG port. Signal is coming from TCK input and going into the FPGA fabric.
// Ctrl.JTDI      ║ out║ JTAG Test Data Input, for internal logic to control non-disruptive re-configuration through JTAG port. Signal is coming from TDI input and going into the FPGA fabric.
// Ctrl.JTDO1     ║ in ║ If ER1 instruction is shifted into the JTAG instruction register, TDO output will come from JTDO1.
// Ctrl.JTDO2     ║ in ║ If ER2 instruction is shifted into the JTAG instruction register, TDO output will come from JTDO2.
//════════════════╩════╩═══════════════════════════════════════════════════════//

case class JtaggGeneric (
    var ER1 : String = "ENABLED",
    var ER2 : String = "ENABLED" ) extends Generic {}

case class JtaggIo() extends Bundle with IMasterSlave{
    val JSHIFT, JUPDATE, JRSTN, JRTI1, JRTI2, JCE1, JCE2 = Bool
    val JTCK, JTDI   = Bool
    val JTDO1, JTDO2 =  Bool

    override def asMaster(): Unit = {
      out(JSHIFT, JUPDATE, JRSTN, JRTI1, JRTI2, JCE1, JCE2)
      out(JTCK,JTDI)
      in(JTDO1, JTDO2)
    }

    override def asSlave(): Unit = {
      in(JSHIFT, JUPDATE, JRSTN, JRTI1, JRTI2, JCE1, JCE2)
      in(JTCK, JTDI)
      out(JTDO1, JTDO2)
    }
}

class JTAGG( gen : JtaggGeneric = JtaggGeneric().copy() ) extends BlackBox {
  val io = master(JtaggIo())
  val generic = gen

  noIoPrefix()
}
