package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
/**
 * Created by PIC32F_USER on 21/03/2016.
 */
case class AxiReadConfig(
  addressWidth: Int,
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
  lenWidth : Int = -1 ,
  idWidth : Int = -1,
  userWidth : Int = -1 ){
  def dataByteCount = dataWidth/8
  val sizeWidth = if(useSize) log2Up(log2Up(dataByteCount)) else 0
}

object AxiBurst extends SpinalEnum(sequancial){
  val FIXED,INCR,WRAP = newElement()
}
object AxiLock extends SpinalEnum(sequancial){
  val NORMAL,EXCLUSIVE,LOCKED = newElement()
}
object AxiResp extends SpinalEnum(sequancial){
  val OKAY,EXOKAY,SLVERR,DECERR = newElement()
}
case class AxiAr(p: AxiReadConfig) extends Bundle {
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

case class AxiR(p: AxiReadConfig) extends Bundle {
  val id = if(p.useId) UInt(p.idWidth bit) else null
  val data = Bits(p.addressWidth bit)
  val resp = if(p.useResp) AxiResp() else null
  val user = if(p.useUser) UInt(p.userWidth bit) else null
  val last = if(p.useLen) Bool else null
}

case class AxiReadOnly(config: AxiReadConfig) extends Bundle with IMasterSlave {
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


//case class AxiLiteAw(config: AxiLiteConfig) extends Bundle {
//  val addr = UInt(config.addressWidth bit)
//  val prot = Bits(3 bit)
//
//  def setUnprivileged : Unit = prot := 0
//}
//
//case class AxiLiteW(config: AxiLiteConfig) extends Bundle {
//  val data = Bits(config.dataWidth bit)
//  val strb = Bits(config.dataWidth / 8 bit)
//
//  def setStrb : Unit = strb := (1 << widthOf(strb))-1
//}
//
//case class AxiLiteB(config: AxiLiteConfig) extends Bundle {
//  val resp = Bits(2 bit)
//}
