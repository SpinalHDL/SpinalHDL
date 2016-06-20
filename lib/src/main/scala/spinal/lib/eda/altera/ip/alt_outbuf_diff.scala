package spinal.lib.eda.altera.ip

import spinal.core._

/**
  * Generic parameters for the alt_outbuf_diff
  */
case class alt_outbuf_diffGeneric(io_standard           : String       = "None",
                                  current_strength      : String       = "None",
                                  slew_rate             : Int          = -1,
                                  location              : String       = "None",
                                  enable_bus_hold       : String       = "None",
                                  weak_pull_up_resistor : String       = "None",
                                  termination           : String       = "None") extends Generic




/**
  * alt_outbuf_diff
  */
case class alt_outbuf_diff(val generic : alt_outbuf_diffGeneric = alt_outbuf_diffGeneric()) extends BlackBox{

  val io = new Bundle{
    val i    = in  Bool
    val o    = out Bool
    val obar = out Bool
  }.setName("")

  def i    : Bool = io.i
  def o    : Bool = io.o
  def obar : Bool = io.obar
}

