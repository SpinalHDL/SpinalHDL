package spinal.lib.bus.regif

import spinal.core._

/*
  * SC(Set/Clear) Register Group,  always used for clock_eanble/soft_reset signal config
  * 1. SET: set register, write 1 set
  * 2. CLR: clear register, write 1 clear
  * ```verilog demo
  * always@(posedge clk or negedge rstn)
  *   if(!rstn) begin
  *      xxx_cgen_x <= 1'b0 ;
  *   end else if(bus_addr == `xxx_SET && bus_wdata[x]) begin //W1S
  *      xxx_cgen_x <= 1'b1 ;
  *   end else if(bus_addr == `xxx_CLR && bus_wdata[x]) begin //W1C
  *      xxx_cgen_x <= 1'b0 ;
  *   end
  *  always @(*) begin
  *    case(addr) begin
  *       `xxx_SET: bus_rdata = {28'b0, xxx_cgen_3....xxx_cgen_0};
  *       `xxx_CLR: bus_rdata = {28'b0, xxx_cgen_3....xxx_cgen_0};
  *        ....
  *    endcase
  * ```
  */
class RegSC(val name: String, offset: BigInt, doc: String, bi: BusIf, sec: Secure, grp: GrpTag) extends RegSliceGrp(offset, maxSize = 3,  doc, sec, grp)(bi){
  val SET  = this.newReg("SCR set register")(SymbolName(s"${name}_SET"))
  val CLR  = this.newReg("SCR clear register")(SymbolName(s"${name}_CLR"))

  def fieldAt[T <: BaseType](pos: Int, hardType: HardType[T], resetValue:BigInt , doc: String)(implicit symbol: SymbolName): T = {
    val reg = SET.fieldAt(pos, hardType,    AccessType.W1S, resetValue = resetValue, doc = s"doc, write 1 set")(symbol)
              CLR.parasiteFieldAt(pos, reg, AccessType.W1C, resetValue = resetValue, doc = s"${doc}, write 1 clear")
    reg
  }

  def field[T <: BaseType](hardType: HardType[T], resetValue: Int, doc: String)(implicit symbol: SymbolName) = {
    val reg = SET.field(hardType, AccessType.W1S, resetValue = resetValue, doc = s"doc, write 1 set")(symbol)
              CLR.parasiteField(reg, AccessType.W1C, resetValue = resetValue, doc = s"${doc}, write 1 clear")
    reg
  }
}