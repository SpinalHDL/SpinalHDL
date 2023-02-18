package spinal.lib.blackbox.altera

import spinal.core._
import spinal.lib.com.jtag.JtagTapInstructionCtrl
import spinal.lib._
//define the VJTAG IO
case class VJTAG(instructionWidth : Int) extends Bundle with IMasterSlave {
    val tck = Bool()					// output	JTAG test clock  (shared among all instances)
	  val tdi = Bool()					// output	JTAG test data input (shared among all instances)
	  val ir_in = Bits(instructionWidth bits)				// output	Virtual IR

    val tdo = Bool()				      // input Virtual JTAG test data out
    //optional input port that is used to parallel load the VIR, not used here
    //val ir_out = in Bool()				// input	Virtual IR capture port	

    val virtual_state_cdr = Bool()		//in the virtual Capture_DR state
	  val virtual_state_sdr	= Bool()	  //in the virtual Shift_DR state
    val virtual_state_e1dr = Bool()		//in the virtual Exit1_DR state
    val virtual_state_pdr	= Bool()	  //in the virtual Pause_DR state
    val virtual_state_e2dr = Bool()		//in the virtual Exit2_DR state
    val virtual_state_udr	= Bool()	  //in the virtual Update_DR state
    val virtual_state_cir	= Bool()	  //in the virtual Capture_IR state
	  val virtual_state_uir	= Bool()	  //in the virtual Update_IR state

    override def asMaster(): Unit = {
      out(tck,tdi,ir_in)
      in(tdo)
      out(virtual_state_cdr,virtual_state_sdr,virtual_state_e1dr,
          virtual_state_pdr,virtual_state_e2dr,virtual_state_udr,
          virtual_state_cir,virtual_state_uir)
    }
  }

  //Altera VJTAG primitive
case class sld_virtual_jtag(ir_width: Int,auto_index: Boolean = true, index: Int = 0) extends BlackBox {
 addGeneric("sld_auto_instance_index", if(auto_index)  "YES" else "NO")
 addGeneric("sld_instance_index", index) 
 addGeneric("sld_ir_width", ir_width)

  val io = master(new VJTAG(ir_width))

  noIoPrefix()

  def toJtagTapInstructionCtrl() = {
    val i = JtagTapInstructionCtrl()
    i.enable := True
    i.tdi     <> io.tdi
    i.capture <> io.virtual_state_cdr
    i.shift   <> io.virtual_state_sdr
    i.update  <> io.virtual_state_udr
    i.tdo     <> io.tdo
    i.reset := False
    i
  }
}