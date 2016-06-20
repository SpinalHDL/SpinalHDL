package spinal.lib.eda.altera.ip

import spinal.core._

/**
  * Generic parameters for the alt_outbuf_tri
  */
case class alt_outbuf_triGeneric(io_standard           : String       = "None",
                          current_strength      : String       = "None",
                          slew_rate             : Int          = -1,
                          slow_slew_rate        : String       = "None",
                          location              : String       = "None",
                          enable_bus_hold       : String       = "None",
                          weak_pull_up_resistor : String       = "None",
                          termination           : String       = "None") extends Generic




/**
  * alt_outbuf_tri
  */
case class alt_outbuf_tri(val generic : alt_outbuf_triGeneric = alt_outbuf_triGeneric()) extends BlackBox{

  val io = new Bundle{
    val i    = in  Bool
    val oe   = in  Bool
    val o    = out Bool
  }.setName("")

  def i    : Bool = io.i
  def oe   : Bool = io.oe
  def o    : Bool = io.o
}

