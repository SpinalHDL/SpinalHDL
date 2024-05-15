package spinal.lib.bus.regif

import spinal.core._

import scala.collection.mutable.ListBuffer

/* S1(STATUS) 1 Interrupt Register Group used for 2nd interrupt signal merge
  * 1. STATUS: status register, status = raw && (!mask)
  * ```verilog demo
  * always @(*) begin
  *   case(addr) begin
  *     `xxx_STATUS: bus_rdata = {28'b0, signal_3....signal_0};
  *     ....
  *   endcase
  * end
  * assign  xxx_int = status_3 || status_2 || status_1 || status_0 ;
  * ```
  */
class IntrS1(val name: String, offset: BigInt, doc: String, bi: BusIf, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 1*bi.bw, doc, grp)(bi) with IntrBase {
  val STATUS = this.newReg(s"${doc} MS2-status Register\n status = raw && (!mask)")(SymbolName(s"${name}_INT_STATUS"))

  @deprecated("IntrS1 without mask, sou maskRstVal is invalid, use fieldAt(pos: Int, signal: Bool, doc: String) instead", "???")
  def fieldAt(pos: Int, signal: Bool, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): Bool = fieldAt(pos, signal, doc)(symbol)

  def fieldAt(pos: Int, signal: Bool, doc: String)(implicit symbol: SymbolName): Bool = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val status = STATUS.fieldAt(pos, Bool(), AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    status := signal
    statusbuf += status
    status
  }

  @deprecated("IntrS1 without mask, sou maskRstVal is invalid, use field(signal: Bool, doc: String) instead", "???")
  def field(signal: Bool, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): Bool = field(signal, doc)(symbol)
  def field(signal: Bool, doc: String)(implicit symbol: SymbolName): Bool = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val status = STATUS.field(Bool(), AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    status := signal
    statusbuf += status
    status
  }
}
