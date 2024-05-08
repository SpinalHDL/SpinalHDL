package spinal.lib.bus.regif

import spinal.core._

import scala.collection.mutable.ListBuffer

trait IntrBase {
  val name: String
  protected val statusbuf = ListBuffer[Bool]()

  def fieldAt(pos: Int, signal: Bool,  maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): Bool
  def field(signal: Bool,  maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): Bool
  def intr()(implicit symbol: SymbolName): Bool = {
    val nm = if (symbol.name.startsWith("<local")) name else symbol.name
    statusbuf.reduceLeft(_ || _).setName(s"${nm}_intr".toLowerCase(), weak = true)
  }
}
