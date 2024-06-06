package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.{Flow, Stream}

class WrFifoInst(name: String, addr: BigInt, doc: String, grp: GrpTag = null)(bi: BusIf) extends FifoInst(name, addr, doc, grp)(bi){
  override val regType: String = "wFIFO"

  val hitDoRead: Bool = False
  val hitDoWrite = bi.writeAddress === U(addr) && bi.doWrite
  hitDoWrite.setName(f"write_hit_0x${addr}%04x", weak = true)

  val bus = Flow(Bits(bi.busDataWidth bit))

  bus.setName(s"${name}_wrfifo")

  bus.valid   := hitDoWrite
  bus.payload := bi.writeData

  override def readBits: Bits = bi.defualtReadBits

  override def readGenerator() = {
    is(addr) {
      bi.reg_rdata := this.rdata()
      bi.reg_rderr := True
    }
  }
}
