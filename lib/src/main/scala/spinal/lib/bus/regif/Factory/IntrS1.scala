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
class IntrS1(val name: String, offset: BigInt, doc: String, bi: BusIf, sec: Secure, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 1*bi.bw, doc, sec, grp)(bi) with IntrBase {
  val STATUS = this.newRegAt(0, s"${doc} MS2-status Register\n status = raw && (!mask)")(SymbolName(s"${name}_INT_STATUS"))

  @deprecated("IntrS1 without mask, sou maskRstVal is invalid, use fieldAt(pos: Int, signal: T, doc: String) instead", "???")
  def fieldAt[T <: BaseType](pos: Int, signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = fieldAt(pos, signal, doc)(symbol)
  def fieldAt[T <: BaseType](pos: Int, signal: T, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val status = STATUS.fieldAt(pos, signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    status := signal
    statusbuf += status
    status
  }

  @deprecated("IntrS1 without mask, sou maskRstVal is invalid, use field(signal: Bool, doc: String) instead", "???")
  def field[T <: BaseType](signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = field(signal, doc)(symbol)
  def field[T <: BaseType](signal: T, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val status = STATUS.field(signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    status := signal
    statusbuf += status
    status
  }
}
