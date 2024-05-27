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
abstract class RegSlice(val name: String, val addr: BigInt, val doc: String, val size: BigInt, val grp: GrpTag = null)(val bi: BusIf){
  protected var _name  = name
  protected val fields = ListBuffer[Field]()
  protected var fieldPtr: Int = 0
  protected var Rerror: Boolean = false

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
