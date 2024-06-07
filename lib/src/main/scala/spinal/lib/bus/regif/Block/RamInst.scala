package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.localbus.{MemBus, MemBusConfig}
import spinal.lib.bus.misc.SizeMapping

class RamInst(name: String, addr: BigInt, size: BigInt, doc: String, sec: Secure,  grp: GrpTag = null)(bi: BusIf) extends RegSlice(name, addr, doc, size, sec, grp)(bi) {
  require(size >= bi.wordAddressInc, "byte Size must be >= busWidth Byte")
  override val regType: String = "RAM"

  val hitDoRead  = rdSecurePassage((bi.readAddress <= U(endaddr)) && (bi.readAddress >= U(addr)) && bi.doRead)
  hitDoRead.setName(f"ram_read_hit_0x${endaddr}%04x_0x${addr}%04x", weak = true)
  val hitDoWrite = wrSecurePassage((bi.readAddress <= U(endaddr)) && (bi.readAddress >= U(addr)) && bi.doWrite)
  hitDoWrite.setName(f"ram_write_hit_0x${endaddr}%04x_0x${addr}%04x", weak = true)

  val bus = MemBus(MemBusConfig(aw = log2Up(size/8), dw = bi.busDataWidth))
  bus.ce   :=  hitDoRead || hitDoWrite
  bus.wr   :=  hitDoWrite
  bus.addr := (bi.readAddress - U(addr)).resized
  bus.wdat :=  bi.writeData
  bus.setName(s"${name}_mbus")

  val ram_rdvalid = RegNext(hitDoRead) init False
  ram_rdvalid.setName(s"${name}_ram_rdvalid", weak = true)

  override def readBits: Bits = bus.rdat

  def field(bit: Int, doc: String = "")(name: String) = {
    val section: Range = fieldPtr + bit -1 downto fieldPtr
    fields   += Field(name, Bits(bit bits), section, AccessType.RW, 0, Rerror, doc)
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
        fieldNA(pos - fieldPtr)
        field(bit, doc)(name)
      }
    }
    fieldPtr = pos + bit
  }
  override def readGenerator(): Unit = ???  //RAM read couldn't implement here but at BusIf
}