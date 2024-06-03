package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.Stream

class RdFifoInst(name: String, addr: BigInt, doc: String, grp: GrpTag = null)(bi: BusIf) extends FifoInst(name, addr, doc, grp)(bi){
  override val regType: String = "rFIFO"

  val bus = Stream(Bits(bi.busDataWidth bit))

  val hitDoRead = bi.writeAddress === U(addr) && bi.doRead
  val hitDoWrite: Bool = False
  hitDoRead.setName(f"read_hit_0x${addr}%04x", weak = true)

  bus.setName(s"${name}_rdfifo")
  bus.ready  := hitDoRead

  override def readBits: Bits = bus.payload

  override def readGenerator() = {
    is(addr) {
      bi.reg_rdata := this.rdata()
      bi.reg_rderr := !bus.valid
    }
  }
}
