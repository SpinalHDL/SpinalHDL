package spinal.lib.eda.altera.ip

import spinal.core._

/**
  * alt_inbuf_diff
  *
  * http://quartushelp.altera.com/14.0/mergedProjects/hdl/prim/prim_file_alt_inbuf_diff.htm
  */
case class alt_inbuf_diff(_io_standard           : IO_STRANDARD = STD_NONE,
                          _location              : String       = "None",
                          _enable_bus_hold       : BOOLEAN      = NONE,
                          _termination           : String       = "None") extends BlackBox{

  val generic = new Generic {
    val io_standard           = _io_standard.value
    val location              = _location
    val enable_bus_hold       = _enable_bus_hold.value
    val termination           = _termination
  }

  val io = new Bundle{
    val i    = in Bool
    val ibar = in Bool
    val o    = out Bool
  }.setName("")

  def i    : Bool = io.i
  def ibar : Bool = io.ibar
  def o    : Bool = io.o
}
