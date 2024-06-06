package spinal.lib.bus.regif

import spinal.core._

import scala.collection.mutable.ListBuffer

/* OMS3(ORIGIN/MASK/STATUS) 3 Interrupt Register Group, always used for 2nd interrupt signal merge
* 1. ORIGIN: Raw status Register, set when event, clear raw when write 1
* 2. MASK:   Mask Register, 1: int off, 0: int open, default 1, int off
* 3. STATUS: status Register, status = raw && (!mask)
* ```verilog demo
*  always@(posedge clk or negedge rstn)
*    if(!rstn) begin
*        mask <= 32'b0 ;
*    end else if(bus_addr == `xxx_MASK) begin //RW
*        mask <= bus_wdata ;
*    end
*
*  assign status_x = signal_x && !mask[x] ;
*
*  always @(*) begin
*      case(addr) begin
*          `xxx_RAW:    bus_rdata = {28'b0, signal_3....signal_0};
*          `xxx_MASK :  bus_rdata = mask;
*          `xxx_STATUS: bus_rdata = {28'b0, status_3....status_0};
*          ....
*      endcase
*  end
*
*  assign  xxx_int = status_3 || status_2 || status_1 || status_0 ;
*  ```
* */
class IntrOMS3(val name: String, offset: BigInt, doc: String, bi: BusIf, sec: Secure, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 3*bi.bw, doc, sec, grp)(bi) with IntrBase {
  val ORIGIN = this.newRegAt(0, s"${doc} OMS3-Raw status Register\n set when event \n clear raw when write 1")(SymbolName(s"${name}_INT_RAW"))
  val MASK   = this.newReg(s"${doc} OMS3-Mask Register\n1: int off\n0: int open\n default 1, int off")(SymbolName(s"${name}_INT_MASK"))
  val STATUS = this.newReg(s"${doc} OMS3-status Register\n status = raw && (!mask)")(SymbolName(s"${name}_INT_STATUS"))

  def fieldAt[T <: BaseType](pos: Int, signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val origin = ORIGIN.fieldAt(pos, signal, AccessType.RO, resetValue = 0, doc = s"${doc} raw, default 0")(SymbolName(s"${nm}_raw"))
    val mask   = MASK.fieldAt(pos, signal, AccessType.RW, resetValue = maskRstVal, doc = s"${doc} mask, default 1, int off")(SymbolName(s"${nm}_mask"))
    val status = STATUS.fieldAt(pos, signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    origin := signal
//    status := origin && (!mask)
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }

  def field[T <: BaseType](signal: T, maskRstVal: BigInt, doc: String)(implicit symbol: SymbolName): T = {
    val nm = if (symbol.name.startsWith("<local")) signal.getPartialName() else symbol.name
    val origin = ORIGIN.field(signal, AccessType.RO, resetValue = 0, doc = s"${doc} raw, default 0")(SymbolName(s"${nm}_raw"))
    val mask   = MASK.field(signal, AccessType.RW, resetValue = maskRstVal, doc = s"${doc} mask, default 1, int off")(SymbolName(s"${nm}_mask"))
    val status = STATUS.field(signal, AccessType.RO, resetValue = 0, doc = s"${doc} stauts default 0")(SymbolName(s"${nm}_status"))
    origin := signal
//    status := origin && (!mask)
    this.levelLogic(signal, mask, status)
    statusbuf += status
    status
  }
}
