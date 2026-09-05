package spinal.lib.bus.amba3.ahblite.sim.slave

import spinal.lib.sim.protocolSim.slave._
import spinal.lib.bus.amba3.ahblite.sim._

import spinal.core.sim._

import spinal.core.ClockDomain
import spinal.lib.bus.amba3.ahblite.AhbLite3

/** Abstraction to help testing a master component in a system, with an AhbLite3 interface
  *
  * Written to be compliant with:
  * http://eecs.umich.edu/courses/eecs373/readings/ARM_IHI0033A_AMBA_AHB-Lite_SPEC.pdf
  *
  * @param bus
  *   the bus of the master to test
  * @param cd
  *   the clock domain related to the bus
  */
case class AhbSlaveSubstitute(bus: AhbLite3, cd: ClockDomain)
    extends AhbSlave
    with CanReactToReadsDelayed
    with CanReactToWritesDelayed {
  // Section 2.1 Global signals
  def HCLK = cd.clockSim.toBoolean
  def HRESETn = cd.resetSim.toBoolean
  // Section 2.4 Decoder signals
  def HSEL = bus.HSEL.toBoolean
  // Section 2.2 Master signals
  def HADDR = bus.HADDR.toBigInt
  def HBURST = bus.HBURST.toInt
  def HMASTLOCK = bus.HMASTLOCK.toBoolean
  def HPROT = bus.HPROT.toInt
  def HSIZE = bus.HSIZE.toInt
  def HTRANS = bus.HTRANS.toInt
  def HWDATA = bus.HWDATA.toBigInt
  def HWRITE = bus.HWRITE.toBoolean
  // Section 2.5 Multiplexor signals
  // HRDATA from slave selected by the decoder
  def HREADY = bus.HREADY.toBoolean
  // HRESP from slave selected by the decoder
  // Section 2.3 Slave signals
  def HRDATA = bus.HRDATA
  def HREADYOUT = bus.HREADYOUT
  def HRESP = bus.HRESP

  /** Assigns default output values for simulation startup */
  def init() = {
    HREADYOUT #= true
    HRESP #= Hresp.OKAY
    HRDATA #= 0
  }

  // Section 5.1.2 Transfer pending
  private var handler: Option[Handler] = None

  // Section 3.1 Basic transfers
  private def newTransaction: Req = AhbReq(
    HADDR,
    HWRITE,
    HSIZE,
    HBURST,
    HPROT,
    HTRANS,
    HMASTLOCK,
    () => HREADY,
    () => HWDATA
  )

  var builder: HandlerBuilder = req => () => ready

  def useRwBuilder(): Unit = builder = rwBuilder

  cd.onSamplings {
    // Section 3.1 Basic transfers
    if (HREADY) {
      handler.foreach(_.apply())
      // Section 3.2 Transfer types
      handler =
        if (HSEL && (HTRANS == Htrans.NONSEQ || HTRANS == Htrans.SEQ))
          Some(builder(newTransaction))
        else None
    }

    val resp = handler.map(_.apply()).getOrElse(ready)
    HRDATA #= resp.HRDATA
    HREADYOUT #= resp.HREADY
    HRESP #= resp.HRESP
  }

  override def fromWriter(
      writer: (Address, Data) => Unit
  ): Behavior = {
    var done = false;
    { req =>
      if (done) {
        super.fromWriter(writer)(req)
      } else {
        done = true
        ready
      }
    }
  }
}
