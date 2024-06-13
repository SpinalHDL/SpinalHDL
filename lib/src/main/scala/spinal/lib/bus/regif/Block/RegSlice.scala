package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.localbus._

import scala.collection.mutable.ListBuffer

/*
*  RegSlice  -->  RegBase  -->  RegInst
*            \--------------->  RamInst
*             \-------------->  FifoInst
* */
abstract class RegSlice(val name: String, val addr: BigInt, val doc: String, val size: BigInt, sec: Secure = null, val grp: GrpTag = null)(val bi: BusIf){
  protected var _name  = name
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

  /*
  * 1: access pass
  * 0: access block
  * */
  private val blockSignals = ListBuffer[Bool]()
  def addBlockSignal(sig: Bool) = blockSignals.append(sig)
//
//  def setSecure(x: Secure) = { sec = x; this }
//  def setS() = { sec = Secure.S; this }
//  def setNS() = { sec = Secure.NS; this }
//  def setCS(reg_ns: Bool) = { sec = Secure.CS(reg_ns); this }

  lazy val secureLogic: (Option[Bool], Option[Bool]) = {
    val ret = if(bi.withSecFireWall){
      Option(sec) match {
        case Some(Secure.MS(x, y)) => {
          val ret = bi.getOrCreateSecLogic(bi.bus_nsbit, !bi.bus_nsbit ).setName(s"ns_sec_pass", weak = true)
          (x, y) match {
            case (true,  true ) => (ret,  ret )
            case (false, false) => (null, null)
            case (true,  false) => (ret,  null)
            case (false, true ) => (null, ret )
          }
        }
        case Some(Secure.CS(wrns,  rdns)) => {
          if(wrns == rdns) {
            val ret = bi.getOrCreateSecLogic(wrns, !(bi.bus_nsbit & !wrns)).setName(s"${wrns.getPartialName()}_sec_pass", weak = true)
            (ret, ret)
          } else {
            val ret1 = bi.getOrCreateSecLogic(wrns, !(bi.bus_nsbit & !wrns)).setName(s"${wrns.getPartialName()}_sec_pass", weak = true)
            val ret2 = bi.getOrCreateSecLogic(rdns, !(bi.bus_nsbit & !rdns)).setName(s"${rdns.getPartialName()}_sec_pass", weak = true)
            (ret1, ret2)
          }
        }
        case None => (null, null)
      }
    } else (null, null)
    (Option(ret._1), Option(ret._2))
  }

//  lazy val blockPassLogic = if(blockSignals.isEmpty) None else Option(blockSignals.reduceLeft(_ & _))

  def wrSecurePassage(access: Bool) = {
    secureLogic._1 match {
      case Some(nspass: Bool) => access & nspass
      case None               => access
    }
  }

  def wrSecurePassage(wrbits: Bits) = {
    secureLogic._1 match {
      case Some(nspass: Bool) => Mux(nspass, wrbits, B(0, bi.busDataWidth bit))
      case None               => wrbits
    }
  }

  def rdSecurePassage(access: Bool) = {
    secureLogic._2 match {
      case Some(nspass: Bool) => access & nspass
      case None               => access
    }
  }

  def rdSecurePassage(rdbits: Bits) = {
    secureLogic._2 match {
      case Some(nspass: Bool) => Mux(nspass, rdbits, bi.defualtReadBits)
      case None               => rdbits
    }
  }

  def getGrp = Option(grp).getOrElse(GrpTag(0, ""))

  def endaddr = addr + size - bi.wordAddressInc
  def getfieldPtr = fieldPtr

  def getDoc(): String  = doc
  def getName(): String = _name
  def getAddr(): BigInt = addr
  def getSize(): BigInt = size
  val regType: String = "REG"

  val hitDoRead: Bool
  val hitDoWrite: Bool
//  def getFieldDescrs() : List[FieldDescr] = fields.toList
  def getFields() : List[Field] = fields.toList
  def readValid(): Bool = RegNext(hitDoRead, init = False)
  def readBits() : Bits
  val reuseTag: ReuseTag = bi.getCurrentBlockTag

  override def toString: String = s"${regType}($name, 0x${addr.hexString()}, 0x${size.hexString()})"

  // users to rewrite rdata to replace the original register read back signal
  var updateReadBits: Bits = null
  protected def rdata(): Bits = Option(updateReadBits).getOrElse(readBits())

  def fieldNA(pos: Int, bit: Int): Unit = {
    val section: Range = pos + bit -1 downto pos
    fields   += Field("--", B(0, bit bits), section, AccessType.NA, 0, Rerror, "reserved")
    fieldPtr += bit
  }

  def fieldNA(bit: Int): Unit = fieldNA(fieldPtr, bit)

  def checkLast = {
    val spareNumbers = if (fields.isEmpty) bi.busDataWidth else bi.busDataWidth - 1 - fields.last.tailBitPos
    spareNumbers match {
      case x if x > 0 => fieldNA(fieldPtr, x)
      case x if x < 0 => SpinalError(s"Range ${Section(fields.last.section)} exceed Bus width ${bi.busDataWidth}")
      case _ =>
    }
  }

  def allIsNA: Boolean = {
    checkLast
    fields.map(_.accType == AccessType.NA).foldLeft(true)(_ && _)
  }

  def readGenerator(): Unit
}
