package spinal.lib.graphic.hdmi

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.lattice.ecp5.ODDRX1F
import spinal.lib.graphic.RgbConfig
import spinal.lib.graphic.vga._

case class VgaToHdmiEcp5(vgaCd : ClockDomain, hdmiCd : ClockDomain) extends Component {
  val io = new Bundle {
    val vga = slave(Vga(RgbConfig(8,8,8)))
    val gpdi_dp, gpdi_dn = out Bits(4 bits)
  }

  val TMDS_red, TMDS_green, TMDS_blue = Bits(10 bits)
  val bCd = io.vga.vSync ## io.vga.hSync
  val encode_R = vgaCd(TmdsEncoder(VD = io.vga.color.r, CD = B"00", VDE = io.vga.colorEn, TMDS = TMDS_red))
  val encode_G = vgaCd(TmdsEncoder(VD = io.vga.color.g, CD = B"00", VDE = io.vga.colorEn, TMDS = TMDS_green))
  val encode_B = vgaCd(TmdsEncoder(VD = io.vga.color.b, CD = bCd  , VDE = io.vga.colorEn, TMDS = TMDS_blue))

  val ctr_mod5 = hdmiCd(Reg(UInt(3 bits)) randBoot())
  val shift_ld = hdmiCd(Reg(Bool) randBoot())

  shift_ld :=  ctr_mod5 === 4
  ctr_mod5 := ((ctr_mod5 === 4) ? U"3'd0" | (ctr_mod5 + 1))

  val shift_R, shift_G, shift_B, shift_C = hdmiCd(Reg(Bits(10 bits)))

  shift_R := (shift_ld ? TMDS_red      | shift_R(9 downto 2).resized)
  shift_G := (shift_ld ? TMDS_green    | shift_G(9 downto 2).resized)
  shift_B := (shift_ld ? TMDS_blue     | shift_B(9 downto 2).resized)
  shift_C := (shift_ld ? B"10'h3e0"    | shift_C(9 downto 2).resized)

  val ddr3p = hdmiCd(ODDRX1F (Q = io.gpdi_dp(3), D0 =  shift_C(0), D1 =  shift_C(1)))
  val ddr2p = hdmiCd(ODDRX1F (Q = io.gpdi_dp(2), D0 =  shift_R(0), D1 =  shift_R(1)))
  val ddr1p = hdmiCd(ODDRX1F (Q = io.gpdi_dp(1), D0 =  shift_G(0), D1 =  shift_G(1)))
  val ddr0p = hdmiCd(ODDRX1F (Q = io.gpdi_dp(0), D0 =  shift_B(0), D1 =  shift_B(1)))

  val ddr3n = hdmiCd(ODDRX1F (Q = io.gpdi_dn(3), D0 = ~shift_C(0), D1 = ~shift_C(1)))
  val ddr2n = hdmiCd(ODDRX1F (Q = io.gpdi_dn(2), D0 = ~shift_R(0), D1 = ~shift_R(1)))
  val ddr1n = hdmiCd(ODDRX1F (Q = io.gpdi_dn(1), D0 = ~shift_G(0), D1 = ~shift_G(1)))
  val ddr0n = hdmiCd(ODDRX1F (Q = io.gpdi_dn(0), D0 = ~shift_B(0), D1 = ~shift_B(1)))
}


// Based on
//module hdmi(
//  input  wire pixclk,
//  input  wire pixclk_x5,
//  input  wire [7:0] red, green, blue,
//  input  wire vde, hSync, vSync,
//  output wire [3:0] gpdi_dp, gpdi_dn
//);
//
//  // 10b8b TMDS encoding of RGB and Sync
//  //
//  wire [9:0] TMDS_red, TMDS_green, TMDS_blue;
//  TMDS_encoder encode_R(.clk(pixclk), .VD(red  ), .CD(2'b00)        , .VDE(vde), .TMDS(TMDS_red));
//  TMDS_encoder encode_G(.clk(pixclk), .VD(green), .CD(2'b00)        , .VDE(vde), .TMDS(TMDS_green));
//  TMDS_encoder encode_B(.clk(pixclk), .VD(blue ), .CD({vSync,hSync}), .VDE(vde), .TMDS(TMDS_blue));
//
//  // shift out 10 bits each pix clock (2 DDR bits at a 5x rate)
//  //
//  reg [2:0] ctr_mod5 = 0;
//  reg shift_ld = 0;
//
//  always @(posedge pixclk_x5)
//  begin
//    shift_ld <= (ctr_mod5==4'd4);
//    ctr_mod5 <= (ctr_mod5==4'd4) ? 4'd0 : ctr_mod5 + 4'd1;
//  end
//
//  reg [9:0] shift_R = 0, shift_G = 0, shift_B = 0, shift_C = 0;
//
//  always @(posedge pixclk_x5)
//  begin
//    shift_R <= shift_ld ? TMDS_red   : shift_R[9:2];
//    shift_G <= shift_ld ? TMDS_green : shift_G[9:2];
//    shift_B <= shift_ld ? TMDS_blue  : shift_B[9:2];
//    shift_C <= shift_ld ? 10'h3e0    : shift_C[9:2];
//  end
//
//`ifdef __ICARUS__
//  // (pseudo-) differential DDR driver (pure verilog version)
//  //
//  assign gpdi_dp[3] = pixclk_x5 ? shift_C[0] : shift_C[1];
//  assign gpdi_dp[2] = pixclk_x5 ? shift_R[0] : shift_R[1];
//  assign gpdi_dp[1] = pixclk_x5 ? shift_G[0] : shift_G[1];
//  assign gpdi_dp[0] = pixclk_x5 ? shift_B[0] : shift_B[1];
//
//  assign gpdi_dn = ~ gpdi_dp;
//`else
//  // (pseudo-) differential DDR driver (ECP5 synthesis version)
//  //*
//  ODDRX1F ddr3p( .Q(gpdi_dp[3]), .SCLK(pixclk_x5), .D0(shift_C[0]),  .D1(shift_C[1]),  .RST(0) );
//  ODDRX1F ddr2p( .Q(gpdi_dp[2]), .SCLK(pixclk_x5), .D0(shift_R[0]),  .D1(shift_R[1]),  .RST(0) );
//  ODDRX1F ddr1p( .Q(gpdi_dp[1]), .SCLK(pixclk_x5), .D0(shift_G[0]),  .D1(shift_G[1]),  .RST(0) );
//  ODDRX1F ddr0p( .Q(gpdi_dp[0]), .SCLK(pixclk_x5), .D0(shift_B[0]),  .D1(shift_B[1]),  .RST(0) );
//
//  ODDRX1F ddr3n( .Q(gpdi_dn[3]), .SCLK(pixclk_x5), .D0(~shift_C[0]), .D1(~shift_C[1]), .RST(0) );
//  ODDRX1F ddr2n( .Q(gpdi_dn[2]), .SCLK(pixclk_x5), .D0(~shift_R[0]), .D1(~shift_R[1]), .RST(0) );
//  ODDRX1F ddr1n( .Q(gpdi_dn[1]), .SCLK(pixclk_x5), .D0(~shift_G[0]), .D1(~shift_G[1]), .RST(0) );
//  ODDRX1F ddr0n( .Q(gpdi_dn[0]), .SCLK(pixclk_x5), .D0(~shift_B[0]), .D1(~shift_B[1]), .RST(0) );
//`endif
//
//endmodule