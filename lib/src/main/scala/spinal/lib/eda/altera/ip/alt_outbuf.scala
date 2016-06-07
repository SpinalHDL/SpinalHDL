package spinal.lib.eda.altera.ip

import spinal.core._

/**
  * alt_inbuf_diff
  *
  * http://quartushelp.altera.com/14.0/mergedProjects/hdl/prim/prim_file_alt_inbuf_diff.htm
  */
case class alt_inbuf_diff(_io_standard           : IO_STRANDARD = STD_NONE,
                          _current_strength      : String       = "None",
                          _slew_rate             : Int          = -1,
                          _slow_slew_rate        : String       = "None",
                          _location              : String       = "None",
                          _enable_bus_hold       : BOOLEAN      = NONE,
                          _weak_pull_up_resistor : BOOLEAN      = NONE,
                          _termination           : String       = "None") extends BlackBox{

  val generic = new Generic {
    val io_standard           = _io_standard.value
    val current_strength      = _current_strength
    val slow_slew_rate        = _slow_slew_rate
    val location              = _location
    val enable_bus_hold       = _enable_bus_hold.value
    val weak_pull_up_resistor = _weak_pull_up_resistor.value
    val termination           = _termination
  }

  val io = new Bundle{
    val i    = in Bool
    val o    = out Bool
  }.setName("")

  def i    : Bool = io.i
  def o    : Bool = io.o
}


