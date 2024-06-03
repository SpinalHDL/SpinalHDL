package spinal.lib.bus.regif

import spinal.core._

/* MMS3(MASKS/MASKC/STATUS) Interrupt 3 Register Group used for 2nd interrupt signal merge
 * 1. MASKS:  mask set register, 1: int off, 0: int open, default 1, int off
 * 2. MASKC:  mask clear register, 1: int off, 0: int open, default 1, int off
 * 3. STATUS: status register, status = raw && (!mask)
 *
 *  MASKS/MASKC are to solve the atomic problem that may arise from operating the same mask address on multiple processor cores.
 *  Currently, two independent mask addresses operate on the same mask register to ensure:
 *  Two processor cores can independently set the state of one bit without affecting other bits
 *
 * ```
 *  always@(posedge clk or negedge rstn)
 *    if(!rstn) begin
 *        mask_x <= 1'b0 ;
 *    end else if(bus_addr == `xxx_MASKS && bus_wdata[n]) begin //W1S
 *        mask_x <= 1'b1 ;
 *    end else if(bus_addr == `xxx_MASKC && bus_wdata[n]) begin //W1C
 *        mask_x <= 1'b0 ;
 *    end
 *
 *   assign status_x = signal_x && !mask_x ;
 *
 *   always @(*) begin
 *       case(addr) begin
 *           `xxx_MASKS:  bus_rdata = {28'b0, mask_3....mask_0};
 *           `xxx_MASKC:  bus_rdata = {28'b0, mask_3....mask_0};
 *           `xxx_STATUS: bus_rdata = {28'b0, status_3....status_0};
 *           ....
 *       endcase
 * end
 *
 * assign  xxx_int = status_3 || status_2 || status_1 || status_0 ;
 * ```
 */
class IntrMMS3(val name: String, offset: BigInt, doc: String, bi: BusIf, sec: Secure, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 3*bi.bw, doc, sec, grp)(bi) with IntrBase {
  val MASKS  = this.newRegAt(0, s"${doc} MMS3-Mask W1S Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${name}_INT_MASKS"))
  val MASKC  = this.newReg(s"${doc} MMS3-Mask W1C Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${name}_INT_MASKC"))
  val STATUS = this.newReg(s"${doc} MMS3-status Register\n status = raw && (!mask)")(SymbolName(s"${name}_INT_STATUS"))

  def fieldAt[T <: BaseType](pos: Int, signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val mask   = MASKS.fieldAt(pos, signal, AccessType.W1S, resetValue = maskRstVal, doc = s"${doc} mask, write 1 set")(SymbolName(s"${nm}_mask"))
                 MASKC.parasiteFieldAt(pos, mask, AccessType.W1C, resetValue = maskRstVal, doc = s"${doc} mask, write 1 clr")
    val status = STATUS.fieldAt(pos, signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }

  def field[T <: BaseType](signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val mask   = MASKS.field(signal, AccessType.W1S, resetValue = maskRstVal, doc = s"${doc} mask, write 1 set")(SymbolName(s"${nm}_mask"))
                 MASKC.parasiteField(mask, AccessType.W1C, resetValue = maskRstVal, doc = s"${doc} mask, write 1 clr")
    val status = STATUS.field(signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }
}
