package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.Stream

abstract class FifoInst(name: String, addr: BigInt, doc: String, sec: Secure = null, grp: GrpTag = null)(bi: BusIf) extends RegSlice(name, addr, doc, size = bi.wordAddressInc, sec = sec, grp = grp)(bi){
  override val regType: String = "FIFO"

  val hitDoRead: Bool
  val hitDoWrite : Bool

  def field(bit: Int, doc: String = "")(name: String) = {
    val section: Range = fieldPtr + bit -1 downto fieldPtr
    fields   += Field(name, Bits(bit bits), section, AccessType.WO, 0, Rerror, doc)
    fieldPtr += bit
  }

  def NA(bit: Int) = {
    val section: Range = fieldPtr + bit -1 downto fieldPtr
    fields   += Field("reserved", Bits(bit bits), section, AccessType.NA, 0, Rerror, "N/A")
    fieldPtr += bit
  }

  def fieldAt(pos: Int, bit: Int, doc: String = "")(name: String) = {
    val sectionNext: Section = pos + bit-1 downto pos
    val sectionExists: Section = fieldPtr downto 0
    val ret = pos match {
      case x if x < fieldPtr                       => SpinalError(s"next field section ${sectionNext} overlap to allocated Section ${sectionExists}")
      case _ if sectionNext.max >= bi.busDataWidth => SpinalError(s"Range ${sectionNext} exceed Bus width ${bi.busDataWidth}")
      case x if (x == fieldPtr) => field(bit, doc)(name)
      case _ => {
        NA(pos - fieldPtr)
        field(bit, doc)(name)
      }
    }
    fieldPtr = pos + bit
  }
}
