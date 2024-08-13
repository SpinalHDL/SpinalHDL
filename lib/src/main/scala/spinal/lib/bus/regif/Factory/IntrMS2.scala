package spinal.lib.bus.regif


import spinal.core._

/* MS2(MASK/STATUS) 2 Interrupt Register Group used for 2nd interrupt signal merge
 * 1. MASK:   mask register, 1: int off, 0: int open, default 1, int off
 * 2. STATUS: status register, status = raw && (!mask)
 * ```
 * always@(posedge clk or negedge rstn)
 *   if(!rstn) begin
 *       mask <= 1'b0 ;
 *   end else if(bus_addr == `xxx_MASKS ) begin //RW
 *       mask <= bus_wdata ;
 *   end
 *
 * assign status_x = signal_x && !mask[x] ;
 *
 * always @(*) begin
 *      case(addr) begin
 *          `xxx_MASK :  bus_rdata = bus_wdata
 *          `xxx_STATUS: bus_rdata = {28'b0, status_3....status_0};
 *          ....
 *      endcase
 * end
 *
 * assign  xxx_int = status_3 || status_2 || status_1 || status_0 ;
 * ```
 */

class IntrMS2(val name: String, offset: BigInt, doc: String, bi: BusIf, sec: Secure, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 2*bi.bw, doc, sec, grp)(bi) with IntrBase {
  val MASK   = this.newRegAt(0, s"${doc} MS2-Mask Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${name}_INT_MASK"))
  val STATUS = this.newReg(s"${doc} MS2-status Register\n status = raw && (!mask)")(SymbolName(s"${name}_INT_STATUS"))

  def fieldAt[T <: BaseType](pos: Int, signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val mask   = MASK.fieldAt(pos, signal, AccessType.RW, resetValue = maskRstVal, doc = s"${doc} mask, default 1, int off")(SymbolName(s"${nm}_mask"))
    val status = STATUS.fieldAt(pos, signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
//    status := signal && (!mask)
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }

  def field[T <: BaseType](signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val mask   = MASK.field(signal, AccessType.RW, resetValue = maskRstVal, doc = s"${doc} mask, default 1, int off")(SymbolName(s"${nm}_mask"))
    val status = STATUS.field(signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
//    status := signal && (!mask)
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }
}
