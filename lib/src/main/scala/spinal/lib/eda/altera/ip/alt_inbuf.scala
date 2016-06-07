package spinal.lib.eda.altera.ip

import spinal.core._


/**
  * ALT_INBUF
  *
  * http://quartushelp.altera.com/14.0/mergedProjects/hdl/prim/prim_file_alt_inbuf.htm
  * @TODO add library altera.altera_primitives_components
  * @TODO chech input termination and location
  */
case class alt_inbuf(_io_standard           : IO_STRANDARD = STD_NONE,
                     _location              : String       = "None",
                     _enable_bus_hold       : BOOLEAN      = NONE,
                     _weak_pull_up_resistor : BOOLEAN      = NONE,
                     _termination           : String       = "None") extends BlackBox{

  val generic = new Generic {
    val io_standard           = _io_standard.value
    val location              = _location
    val enable_bus_hold       = _enable_bus_hold.value
    val weak_pull_up_resistor = _weak_pull_up_resistor.value
    val termination           = _termination
  }

  val io = new Bundle{
    val i = in Bool
    val o = out Bool
  }.setName("")

  def i : Bool = io.i
  def o : Bool = io.o
}

