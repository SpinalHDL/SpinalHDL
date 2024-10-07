package spinal.lib.bus.regif

import spinal.core._

class VirtualRegInst(name: String, addr: BigInt, doc: String, busif: BusIf) extends RegInst(name, addr, doc, busif) {
  // RegInst implementation
  override def readBits: Bits = busif.defaultReadBits
  override protected def creatWriteLogic[T <: BaseType](reg: T, acc: AccessType, section: Range): Unit = {}
}
