package spinal.lib.eda.altera.ip

import spinal.core._


/**
  * Generic parameters for the alt_inbuf_diff
  */
case class alt_inbuf_diffGeneric(io_standard           : String = "None",
                                 location              : String = "None",
                                 enable_bus_hold       : String = "None",
                                 weak_pull_up_resistor : String = "None",
                                 termination           : String = "None") extends Generic


/**
  * alt_inbuf_diff
  *
  */
case class alt_inbuf_diff(val generic : alt_inbuf_diffGeneric = alt_inbuf_diffGeneric()) extends BlackBox {

  val io = new Bundle{
    val i    = in  Bool
    val ibar = in  Bool
    val o    = out Bool
  }.setName("")

  def i    : Bool = io.i
  def ibar : Bool = io.ibar
  def o    : Bool = io.o
}
