package spinal.lib.graphic.hdmi

import spinal.core._
import spinal.lib._

object TmdsEncoder{
  def apply(VD : UInt, CD : Bits, VDE : Bool, TMDS :Bits) : TmdsEncoder = {
    val ret = TmdsEncoder()
    ret.io.VD := VD
    ret.io.CD := CD
    ret.io.VDE := VDE
    TMDS := ret.io.TMDS
    ret
  }
}

case class TmdsEncoder() extends Component{
  val io = new Bundle {
    val VD = in UInt(8 bits)
    val CD = in Bits(2 bits)
    val VDE = in Bool()
    val TMDS = out (Reg(Bits(10 bits)))
  }

  val dc_bias = Reg(UInt(4 bits)) randBoot()

  val ones = io.VD.asBools.map(_.asUInt(4 bits)).reduceBalancedTree(_ + _)
  val XNOR = (ones > 4) || (ones === 4 && io.VD(0) === False)
  val dw = Vec(Bool, 9)
  dw(8) := ~XNOR;
  dw(7) := dw(6) ^ io.VD(7) ^ XNOR;
  dw(6) := dw(5) ^ io.VD(6) ^ XNOR;
  dw(5) := dw(4) ^ io.VD(5) ^ XNOR;
  dw(4) := dw(3) ^ io.VD(4) ^ XNOR;
  dw(3) := dw(2) ^ io.VD(3) ^ XNOR;
  dw(2) := dw(1) ^ io.VD(2) ^ XNOR;
  dw(1) := dw(0) ^ io.VD(1) ^ XNOR;
  dw(0) := io.VD(0);

  val dw_disp = dw.take(8).map(_.asUInt(4 bits)).reduceBalancedTree(_ + _) + 12
  val sign_eq = dw_disp(3) === dc_bias(3)

  val delta  = dw_disp - U((dw(8) ^ ~sign_eq) & ~(dw_disp===0 || dc_bias===0));
  val inv_dw = (dw_disp === 0 || dc_bias === 0) ? ~dw(8) | sign_eq;

  val dc_bias_d = inv_dw ? (dc_bias - delta) | (dc_bias + delta)

  val TMDS_data =  inv_dw ## dw(8) ## (dw.take(8).asBits ^ (inv_dw ? B(255) | B(0)))
  val TMDS_code = io.CD(1) ? (io.CD(0) ? B"b1010101011" | B"b0101010100") | (io.CD(0) ? B"b0010101011" | B"b1101010100")

  io.TMDS := (io.VDE ? TMDS_data | TMDS_code);
  dc_bias := (io.VDE ? dc_bias_d | 0)

}

// Not my own, based on : third party verilog
// DVI-D 10b8b TMDS encoder module
//
//module TMDS_encoder(
//  input clk,       // pix clock
//  input [7:0] VD,  // video data (red, green or blue)
//  input [1:0] CD,  // control data
//  input VDE,       // video data enable, to choose between CD (when VDE=0) and VD (when VDE=1)
//  output reg [9:0] TMDS = 0
//);
//
//  reg  [3:0] dc_bias = 0;
//
//  // compute data word
//  wire [3:0] ones = VD[0] + VD[1] + VD[2] + VD[3] + VD[4] + VD[5] + VD[6] + VD[7];
//  wire       XNOR = (ones>4'd4) || (ones==4'd4 && VD[0]==1'b0);
//  wire [8:0] dw;
//  assign dw[8] = ~XNOR;
//  assign dw[7] = dw[6] ^ VD[7] ^ XNOR;
//  assign dw[6] = dw[5] ^ VD[6] ^ XNOR;
//  assign dw[5] = dw[4] ^ VD[5] ^ XNOR;
//  assign dw[4] = dw[3] ^ VD[4] ^ XNOR;
//  assign dw[3] = dw[2] ^ VD[3] ^ XNOR;
//  assign dw[2] = dw[1] ^ VD[2] ^ XNOR;
//  assign dw[1] = dw[0] ^ VD[1] ^ XNOR;
//  assign dw[0] = VD[0];
//
//  // calculate 1/0 disparity & invert as needed to minimize dc bias
//  wire [3:0] dw_disp = dw[0] + dw[1] + dw[2] + dw[3] + dw[4] + dw[5] + dw[6] + dw[7] + 4'b1100;
//  wire       sign_eq = (dw_disp[3] == dc_bias[3]);
//
//  wire [3:0] delta  = dw_disp - ({dw[8] ^ ~sign_eq} & ~(dw_disp==0 || dc_bias==0));
//  wire       inv_dw = (dw_disp==0 || dc_bias==0) ? ~dw[8] : sign_eq;
//
//  wire [3:0] dc_bias_d = inv_dw ? dc_bias - delta : dc_bias + delta;
//
//  // set output signals
//  wire [9:0] TMDS_data = { inv_dw, dw[8], dw[7:0] ^ {8{inv_dw}} };
//  wire [9:0] TMDS_code = CD[1] ? (CD[0] ? 10'b1010101011 : 10'b0101010100)
//                               : (CD[0] ? 10'b0010101011 : 10'b1101010100);
//
//  always @(posedge clk) TMDS    <= VDE ? TMDS_data : TMDS_code;
//  always @(posedge clk) dc_bias <= VDE ? dc_bias_d : 0;
//
//endmodule
