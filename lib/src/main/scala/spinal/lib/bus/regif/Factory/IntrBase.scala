package spinal.lib.bus.regif

import spinal.core._

import scala.collection.mutable.ListBuffer

trait IntrBase {
  val name: String
  protected val statusbuf: ListBuffer[BaseType] = ListBuffer[BaseType]()

  def getStatus = statusbuf.toList

  def fieldAt[T <: BaseType](pos: Int, signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T

  def field[T <: BaseType](signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T

  def intr()(implicit symbol: SymbolName): Bool = {
    val nm = if (symbol.name.startsWith("<local")) name else symbol.name
    statusbuf.map {
      case x: Bool => x
      case x: Bits => x.orR
      case _ => SpinalError("Interrupt Signals only accept Bool/Bits")
    }.reduceLeft(_ || _).setName(s"${nm}_intr".toLowerCase(), weak = true)
  }

  def levelLogic[T <: BaseType](signal: T, mask: T, status: T): Unit = {
    (signal, mask, status) match {
      case (sig: Bool, msk: Bool, stt: Bool) => stt := sig & (~msk)
      case (sig: Bits, msk: Bits, stt: Bits) => stt := sig & (~msk)
      case _ => SpinalError("Interrupt Only support Bool/Bits")
    }
  }

  def eventLogic[T <: BaseType](signal: T, raw: T, mask: T, status: T): Unit = {
    (signal, raw, mask, status) match {
      case (sig: Bool, raww: Bool, msk: Bool, stt: Bool) => {
        raww.setWhen(sig)
        stt := raww & (~msk)
      }
      case (sig: Bits, raww: Bits, msk: Bits, stt: Bits) => {
        val size = sig.getBitsWidth
        for (i <- 0 until size) {
          raww(i).setWhen(sig(i))
        }
        stt := raww & (~msk)
      }
      case _ => SpinalError("Interrupt Only support Bool/Bits")
    }
  }
}
