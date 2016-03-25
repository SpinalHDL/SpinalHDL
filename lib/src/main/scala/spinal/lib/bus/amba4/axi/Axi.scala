package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
/**
 * Created by PIC32F_USER on 21/03/2016.
 */

object AxiBurst extends SpinalEnum(sequancial){
  val FIXED,INCR,WRAP = newElement()
}
object AxiLock extends SpinalEnum(sequancial){
  val NORMAL,EXCLUSIVE,LOCKED = newElement()
}
object AxiResp extends SpinalEnum(sequancial){
  val OKAY,EXOKAY,SLVERR,DECERR = newElement()
}

case class AxiConfig( addressWidth: Int,
                      dataWidth: Int,
                      useId : Boolean = false,
                      useRegion : Boolean = false,
                      useBurst : Boolean = false,
                      useLock : Boolean = false,
                      useCache : Boolean = false,
                      useSize : Boolean = false,
                      useQos : Boolean = false,
                      useLen : Boolean = false,
                      useResp : Boolean = false,
                      useUser : Boolean = false,
                      useStrb : Boolean = false,
                      lenWidth : Int = -1 ,
                      idWidth : Int = -1,
                      userWidth : Int = -1 ) {
  def dataByteCount = dataWidth/8
  val sizeWidth = if(useSize) log2Up(log2Up(dataByteCount)) else 0
}

case class AxiAw(p: AxiConfig) extends Bundle {
  val id = if(p.useId) UInt(p.idWidth bit) else null
  val addr = UInt(p.addressWidth bit)
  val region = if(p.useRegion)Bits(4 bit) else null
  val len = if(p.useLen) UInt(p.lenWidth bit) else null
  val size = if(p.useSize) UInt(p.sizeWidth bit) else null
  val burst = if(p.useBurst) AxiBurst() else null
  val lock = if(p.useLock) AxiLock() else null
  val cache = if(p.useCache) Bits(4 bit) else null
  val qos = if(p.useQos) UInt(4 bit) else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
  val prot = Bits(3 bit)
}

case class AxiW(p: AxiConfig) extends Bundle {
  val data = Bits(p.addressWidth bit)
  val strb = if(p.useStrb) UInt(p.dataByteCount bit) else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
  val last = if(p.useLen) Bool else null
}


case class AxiAr(p: AxiConfig) extends Bundle {
  val id = if(p.useId) UInt(p.idWidth bit) else null
  val addr = UInt(p.addressWidth bit)
  val region = if(p.useRegion)Bits(4 bit) else null
  val len = if(p.useLen) UInt(p.lenWidth bit) else null
  val size = if(p.useSize) UInt(p.sizeWidth bit) else null
  val burst = if(p.useBurst) AxiBurst() else null
  val lock = if(p.useLock) AxiLock() else null
  val cache = if(p.useCache) Bits(4 bit) else null
  val qos = if(p.useQos) UInt(4 bit) else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
  val prot = Bits(3 bit)
}

case class AxiR(p: AxiConfig) extends Bundle {
  val id = if(p.useId) UInt(p.idWidth bit) else null
  val data = Bits(p.addressWidth bit)
  val resp = if(p.useResp) AxiResp() else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
  val last = if(p.useLen) Bool else null
}





case class AxiB(p: AxiConfig) extends Bundle {
  val id = if(p.useId) UInt(p.idWidth bit) else null
  val resp = if(p.useResp) AxiResp() else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
}




case class AxiBus(config: AxiConfig) extends Bundle with IMasterSlave {
  val aw = Stream(AxiAw(config))
  val w = Stream(AxiW(config))
  val b = Stream(AxiB(config))
  val ar = Stream(AxiAr(config))
  val r = Stream(AxiR(config))

  def writeCmd = aw
  def writeData = w
  def writeRsp = b
  def readCmd = ar
  def readRsp = r

  def >> (that : AxiBus) : Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that : AxiBus) : Unit = that >> this

  override def asMaster(): this.type = {
    aw.asMaster()
    w.asMaster()
    b.asSlave()
    this
  }

  override def asSlave(): this.type = asMaster().flip()
}

case class AxiWriteOnly(config: AxiConfig) extends Bundle with IMasterSlave {
  val aw = Stream(AxiAw(config))
  val w = Stream(AxiW(config))
  val b = Stream(AxiB(config))

  def writeCmd = aw
  def writeData = w
  def writeRsp = b

  def >> (that : AxiWriteOnly) : Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRsp << that.writeRsp
  }

  def <<(that : AxiWriteOnly) : Unit = that >> this

  override def asMaster(): this.type = {
    aw.asMaster()
    w.asMaster()
    b.asSlave()
    this
  }

  override def asSlave(): this.type = asMaster().flip()
}

case class AxiReadOnly(config: AxiConfig) extends Bundle with IMasterSlave {
  val ar = Stream(AxiAr(config))
  val r = Stream(AxiR(config))

  def readCmd = ar
  def readRsp = r

  def >> (that : AxiReadOnly) : Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readRsp << that.readRsp
  }

  def <<(that : AxiReadOnly) : Unit = that >> this

  override def asMaster(): this.type = {
    ar.asMaster()
    r.asSlave()
    this
  }

  override def asSlave(): this.type = asMaster().flip()
}