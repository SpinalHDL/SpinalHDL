package spinal.lib.bus.amba3.ahb

import spinal.core._
import spinal.lib._
/**
 * Created by PIC32F_USER on 07/08/2016.
 */


case class Ahb3Config(addressWidth: Int,
                      dataWidth: Int){
  def addressType = UInt(addressWidth bits)
  def dataType = Bits(dataWidth bits)
  def bytePerWord = dataWidth/8
  def symboleRange = log2Up(bytePerWord)-1 downto 0
  def wordRange    = addressWidth-1 downto log2Up(bytePerWord)
}

case class Ahb3Master(config: Ahb3Config) extends Bundle with IMasterSlave{
  //  Address and control
  val HADDR     = UInt(config.addressWidth bits)
  val HWRITE    = Bool
  val HSIZE     = Bits(3 bits)
  val HBURST    = Bits(3 bits)
  val HPROT     = Bits(4 bits)
  val HTRANS    = Bits(2 bits)
  val HMASTLOCK = Bool

  //  Data
  val HWDATA    = Bits(config.dataWidth bits)
  val HRDATA    = Bits(config.dataWidth bits)

  //  Transfer response
  val HREADY    = Bool
  val HRESP     = Bool

  override def asMaster(): Ahb3Master.this.type = {
    out(HADDR,HWRITE,HSIZE,HBURST,HPROT,HTRANS,HMASTLOCK,HWDATA)
    in(HREADY,HRESP,HRDATA)
    this
  }
}


case class Ahb3Slave(config: Ahb3Config) extends Bundle with IMasterSlave{
  //  Address and control
  val HADDR = UInt(config.addressWidth bits)
  val HSEL = Bool
  val HREADYIN = Bool
  val HWRITE = Bool
  val HSIZE = Bits(3 bits)
  val HBURST = Bits(3 bits)
  val HPROT = Bits(4 bits)
  val HTRANS = Bits(2 bits)
  val HMASTLOCK = Bool

  //  Data
  val HWDATA = Bits(config.dataWidth bits)
  val HRDATA = Bits(config.dataWidth bits)

  //  Transfer response
  val HREADYOUT = Bool
  val HRESP = Bool

  def setOKEY = HRESP := False
  def setERROR   = HRESP := True

  override def asMaster(): Ahb3Slave.this.type = {
    out(HADDR,HWRITE,HSIZE,HBURST,HPROT,HTRANS,HMASTLOCK,HWDATA,HREADYIN,HSEL)
    in(HREADYOUT,HRESP,HRDATA)
    this
  }

  def OKEY  = !HRESP
  def ERROR = HRESP

  //return true when the current transaction is the last one of the current burst
  def last() : Bool = {
    val beatCounter = Reg(UInt(4 bits))
    val beatCounterPlusOne = beatCounter + "00001"
    val result = (HBURST(2 downto 1).asUInt @@ U"00") === beatCounterPlusOne || (HREADYIN && ERROR)

    when(HSEL && HREADYIN){
      beatCounter := beatCounterPlusOne.resized
      when(result){
        beatCounter := 0
      }
    }

    result
  }

  def isLast() : Bool = last && HSEL

  def fire() : Bool = HSEL && HREADYOUT


  def writeMask() : Bits = {
//    val lowMask,highRang =
    ???
  }
}
