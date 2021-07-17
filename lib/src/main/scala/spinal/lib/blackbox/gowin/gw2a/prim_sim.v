
// ===========Oooo==========================================Oooo========
// =  Copyright (C) 2014-2020 Gowin Semiconductor Technology Co.,Ltd.
// =                     All rights reserved.
// =====================================================================
//
//  __      __      __
//  \ \    /  \    / /   [File name   ] prim_sim.v
//   \ \  / /\ \  / /    [Description ] GW2A verilog functional simulation library
//    \ \/ /  \ \/ /     [Timestamp   ] Fri January 10 11:00:30 2019
//     \  /    \  /      [version     ] 1.9.4
//      \/      \/       
//
// ===========Oooo==========================================Oooo========


`timescale 1ns / 1ps

// MUX2
primitive mux2 (O, I0, I1, S);
output O;
input I0, I1, S;
table
// I0  I1  S     O
   0   ?   0  :  0 ;
   1   ?   0  :  1 ;
   x   ?   0  :  x ;
   ?   0   1  :  0 ;
   ?   1   1  :  1 ;
   ?   x   1  :  x ;
   0   0   x  :  0 ;
   0   1   x  :  x ;
   1   0   x  :  x ;
   1   1   x  :  1 ;
   ?   x   x  :  x ;
   x   ?   x  :  x ;
endtable
endprimitive

// MUXes
module MUX2 (O, I0, I1, S0);

input I0,I1;
input S0;
output O;

mux2 mux2_0 (O, I0, I1, S0);

endmodule //MUX2

module MUX2_LUT5 (O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_lut5 (O, I0, I1, S0);

endmodule //MUX2 LUT_5: use two 4-input LUTs and 1 MUX2_LUT_5 to construct 5-input LUT

module MUX2_LUT6 (O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_lut6 (O, I0, I1, S0);

endmodule //MUX2 LUT_6: use two 5-input LUTs and 1 MUX2_LUT_6 to construct 6-input LUT

module MUX2_LUT7 (O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_lut7 (O, I0, I1, S0);

endmodule //MUX2 LUT_7: use two 6-input LUTs and 1 MUX2_LUT_7 to construct 7-input LUT

module MUX2_LUT8 (O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_lut8 (O, I0, I1, S0);

endmodule //MUX2 LUT_8: use two 7-input LUTs and 1 MUX2_LUT_8 to construct 8-input LUT

module MUX2_MUX8(O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_mux8 (O, I0, I1, S0);

endmodule // MUX2_MUX8:use two 4-input MUXs and 1 MUX2_MUX8's to construct 8-input mux

module MUX2_MUX16(O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_mux16 (O, I0, I1, S0);

endmodule //MUX2_MUX16:use two 8-input MUXs and 1 MUX2_MUX8's to construct 16-input mux

module MUX2_MUX32(O, I0, I1, S0);

input I0,I1;
input S0;
output O;

MUX2 mux2_mux32 (O, I0, I1, S0);

endmodule //MUX2_MUX32:use two 16-input MUXs and 1 MUX2_MUX16's to construct 32-input mux

module MUX4 (O, I0, I1, I2, I3, S0, S1);

input I0, I1, I2, I3;
input S0, S1;
output O;

wire O1,O2;

MUX2 mux2_1(O1, I0, I1, S0);
MUX2 mux2_2(O2, I2, I3, S0);
MUX2 mux2_0(O, O1, O2, S1);

endmodule // MUX4

module MUX8 (O, I0, I1, I2, I3, I4, I5, I6, I7, S0, S1, S2);

input I0, I1, I2, I3, I4, I5, I6, I7;
input S0, S1, S2;
output O;

wire O1, O2;

MUX4 mux4_1(O1, I0, I1, I2, I3, S0, S1);
MUX4 mux4_2(O2, I4, I5, I6, I7, S0, S1);
MUX2 mux2_0(O, O1, O2, S2);

endmodule //MUX8


module MUX16(O, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, S0, S1, S2, S3);

input I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15;
input S0, S1, S2, S3;
output O;

wire O1, O2;

MUX8  mux8_1(O1, I0, I1, I2, I3, I4, I5, I6, I7, S0, S1, S2);
MUX8  mux8_2(O2, I8, I9, I10, I11, I12, I13, I14, I15, S0, S1, S2);

MUX2 mux2_o(O, O1, O2, S3);

endmodule


module MUX32(O, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15,
				I16, I17, I18, I19, I20, I21, I22, I23, I24, I25, I26, I27, I28, I29, I30,
				I31, S0, S1, S2, S3, S4
    );
input I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22, I23, I24, I25, I26, I27, I28, I29, I30, I31;
input S0, S1, S2, S3, S4;
output O;

wire O1, O2;

MUX16 mux16_1(O1, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, S0, S1, S2, S3);
MUX16 mux16_2(O2, I16, I17, I18, I19, I20, I21, I22, I23, I24, I25, I26, I27, I28, I29, I30, I31, S0, S1, S2, S3);

MUX2 mux2_o(O ,O1, O2, S4);

endmodule


//LUTs
module LUT1 (F, I0);

parameter INIT = 2'h0;

input I0;
output F;

MUX2 lut_1 (F, INIT[0], INIT[1], I0);

endmodule //lut1


module LUT2 (F, I0, I1);

parameter INIT = 4'h0;

input I0, I1;
output F;

MUX4 lut_2 (F, INIT[0], INIT[1], INIT[2], INIT[3], I0, I1);

endmodule //lut2


module LUT3 (F, I0, I1, I2);

parameter INIT = 8'h00;

input I0, I1, I2;
output F;

MUX8 lut_3 (F, INIT[0], INIT[1], INIT[2], INIT[3], INIT[4], INIT[5], INIT[6], INIT[7], I0, I1, I2);

endmodule //lut3


module LUT4 (F, I0, I1, I2, I3);

parameter INIT = 16'h0000;

input I0, I1, I2, I3;
output F;

MUX16 lut_4(F, INIT[0], INIT[1], INIT[2], INIT[3], INIT[4], INIT[5], INIT[6], INIT[7], INIT[8], INIT[9], INIT[10], INIT[11], INIT[12], INIT[13], INIT[14], INIT[15], I0, I1, I2, I3);

endmodule //lut4


module LUT5 (F, I0, I1, I2, I3, I4);

parameter INIT = 32'h00000000;

input I0, I1, I2, I3, I4;
output F;

MUX32 lut_5(F, INIT[0], INIT[1], INIT[2], INIT[3], INIT[4], INIT[5], INIT[6], INIT[7], INIT[8], INIT[9], INIT[10], INIT[11], INIT[12], INIT[13], INIT[14], INIT[15], INIT[16], INIT[17], INIT[18], INIT[19], INIT[20], INIT[21], INIT[22], INIT[23], INIT[24], INIT[25], INIT[26], INIT[27], INIT[28], INIT[29], INIT[30], INIT[31], I0, I1, I2, I3, I4);

endmodule//lut5


module LUT6 (F, I0, I1, I2, I3, I4, I5);

parameter INIT = 64'h0000_0000_0000_0000;

input I0, I1, I2, I3, I4, I5;
output F;

wire O1, O2;

defparam lut5_1.INIT = INIT[31:0];
LUT5 lut5_1(O1, I0, I1, I2, I3, I4);

defparam lut5_2.INIT = INIT[63:32];
LUT5 lut5_2(O2, I0, I1, I2, I3, I4);

MUX2 lut_6(F, O1, O2, I5);

endmodule//lut6


module LUT7 (F, I0, I1, I2, I3, I4, I5, I6);

parameter INIT = 128'h0000_0000_0000_0000_0000_0000_0000_0000;

input I0, I1, I2, I3, I4, I5, I6;
output F;

wire O1, O2;

defparam lut6_1.INIT = INIT[63:0];
LUT6 lut6_1(O1, I0, I1, I2, I3, I4, I5);

defparam lut6_2.INIT = INIT[127:64];
LUT6 lut6_2(O2, I0, I1, I2, I3, I4, I5);

MUX2 lut_7(F, O1, O2, I6);

endmodule//lut7


module LUT8 (F, I0, I1, I2, I3, I4, I5, I6, I7);

parameter INIT = 256'h0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;

input I0, I1, I2, I3, I4, I5, I6, I7;
output F;

wire O1, O2;

defparam lut7_1.INIT = INIT[127:0];
LUT7 lut7_1(O1, I0, I1, I2, I3, I4, I5, I6);

defparam lut7_2.INIT = INIT[255:128];
LUT7 lut7_2(O2, I0, I1, I2, I3, I4, I5, I6);

MUX2 lut_8(F, O1, O2, I7);

endmodule//lut8


// ALU
module ALU (SUM, COUT, I0, I1, I3, CIN);

input I0;
input I1;
input I3;
input CIN;
output SUM;
output COUT;

parameter ADD = 0;
parameter SUB = 1;
parameter ADDSUB = 2;
parameter NE = 3;
parameter GE = 4;
parameter LE = 5;
parameter CUP = 6;
parameter CDN = 7;
parameter CUPCDN = 8;
parameter MULT = 9;

parameter ALU_MODE = 0;

reg S, C;

assign SUM = S ^ CIN;
assign COUT = S? CIN : C;

always @(I0, I1, I3,CIN) begin
	case (ALU_MODE)
		ADD: begin // adder; LSB CIN must be 1'b0
			S = I0 ^ I1;
			C = I0;
		end
		SUB: begin // subtractor; LSB CIN must be 1'b1
			S = I0 ^ ~I1;
			C = I0;
		end
		ADDSUB: begin // adder subtractor;
			S = I3? I0 ^ I1 : I0 ^ ~I1;
			C = I0;
		end
		NE: begin // not equal to; LSB CIN must be 1'b0
			S = I0 ^ ~I1;
			C = 1'b1;
		end
		GE: begin // greater than or equal to; LSB CIN must be 1'b1
			S = I0 ^ ~I1;
			C = I0;
		end
		LE: begin // less than or equal to; LSB CIN must be 1'b1
			S = ~I0 ^ I1;
			C = I1;
		end
		CUP: begin // up counter; LSB CIN must be 1'b1
			S = I0;
			C = 1'b0; // or equivalently, I0
		end
		CDN: begin // down counter; LSB CIN must be 1'b0
			S = ~I0;
			C = 1'b1; // or equivalently, I0
		end
		CUPCDN: begin // up down counter; I3 as select bit - 1'b0: down counter, LSB CIN must be 1'b0; 1'b1: up counter, LSB CIN must be 1'b1
			S = I3? I0 : ~I0;
			C = I0;
		end
		MULT: begin // multiplier; LSB CIN must be 1'b0
			S = I0 & I1;
			C = I0 & I1;
		end
		default: begin
		//	$display ("%d: Unsupported ALU mode\n", ALU_PARAM);
		//	$finish;
		end
	endcase
end

endmodule // ALU: 2-input arithmetic logic unit


// Flip-Flops
module DFF (Q, D, CLK);

input D, CLK;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFF (positive clock edge)


module DFFE (Q, D, CLK, CE);

input D, CLK, CE;
output Q;

parameter INIT = 1'b0;

reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFE (positive clock edge; clock enable)


module DFFS (Q, D, CLK, SET);

input D, CLK, SET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (SET)
		Q_reg <= 1'b1;
	else
		Q_reg <= D;	
end

assign Q = Q_reg;

endmodule // DFFS (positive clock edge; synchronous set)


module DFFSE (Q, D, CLK, CE, SET);

input D, CLK, SET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (SET)
		Q_reg <= 1'b1;
	else if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFSE (positive clock edge; synchronous set takes precedence over clock enable)


module DFFR (Q, D, CLK, RESET);

input D, CLK, RESET;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (RESET)
		Q_reg <= 1'b0;
	else
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFR (positive clock edge; synchronous reset)


module DFFRE (Q, D, CLK, CE, RESET);

input D, CLK, RESET, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (RESET)
		Q_reg <= 1'b0;
	else if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFRE (positive clock edge; synchronous reset takes precedence over clock enable)


module DFFP (Q, D, CLK, PRESET);

input D, CLK, PRESET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or PRESET) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(PRESET)
		assign Q_reg = 1'b1;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFP (positive clock edge; asynchronous preset)


module DFFPE (Q, D, CLK, CE, PRESET);

input D, CLK, PRESET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or PRESET) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(PRESET)
		assign Q_reg = 1'b1;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFPE (positive clock edge; asynchronous preset; clock enable)


module DFFC (Q, D, CLK, CLEAR);

input D, CLK, CLEAR;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or CLEAR)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(CLEAR)
		assign Q_reg = 1'b0;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFC (positive clock edge; asynchronous clear)


module DFFCE (Q, D, CLK, CE, CLEAR);

input D, CLK, CLEAR, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or CLEAR) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(CLEAR)
		assign Q_reg = 1'b0;
	else
		deassign Q_reg;
end

always @(posedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFCE (positive clock edge; asynchronous clear; clock enable)


module DFFN (Q, D, CLK);

input D, CLK;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFN (negative clock edge)


module DFFNE (Q, D, CLK, CE);

input D, CLK, CE;
output Q;

parameter INIT = 1'b0;

reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNE (negative clock edge; clock enable)


module DFFNS (Q, D, CLK, SET);

input D, CLK, SET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (SET)
		Q_reg <= 1'b1;
	else
		Q_reg <= D;	
end

assign Q = Q_reg;

endmodule // DFFNS (negative clock edge; synchronous set)


module DFFNSE (Q, D, CLK, CE, SET);

input D, CLK, SET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (SET)
		Q_reg <= 1'b1;
	else if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNSE (negative clock edge; synchronous set takes precedence over clock enable)


module DFFNR (Q, D, CLK, RESET);

input D, CLK, RESET;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (RESET)
		Q_reg <= 1'b0;
	else
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNR (negative clock edge; synchronous reset)


module DFFNRE (Q, D, CLK, CE, RESET);

input D, CLK, RESET, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (RESET)
		Q_reg <= 1'b0;
	else if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNRE (negative clock edge; synchronous reset takes precedence over clock enable)


module DFFNP (Q, D, CLK, PRESET);

input D, CLK, PRESET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or PRESET) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(PRESET)
		assign Q_reg = 1'b1;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNP (negative clock edge; asynchronous preset)


module DFFNPE (Q, D, CLK, CE, PRESET);

input D, CLK, PRESET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or PRESET) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(PRESET)
		assign Q_reg = 1'b1;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNPE (negative clock edge; asynchronous preset; clock enable)


module DFFNC (Q, D, CLK, CLEAR);

input D, CLK, CLEAR;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or CLEAR)begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(CLEAR)
		assign Q_reg = 1'b0;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNC (negative clock edge; asynchronous clear)


module DFFNCE (Q, D, CLK, CE, CLEAR);

input D, CLK, CLEAR, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(gsrt or CLEAR) begin
	if(!gsrt)
		assign Q_reg = INIT;
	else if(CLEAR)
		assign Q_reg = 1'b0;
	else
		deassign Q_reg;
end

always @(negedge CLK) begin
	if (CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DFFNCE (negative clock edge; asynchronous clear; clock enable)


//Latches
module DL (Q, D, G);

input D, G;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DL (high active latch)


module DLE (Q, D, G, CE);

input D, G, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if(G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLE (high active latch; latch enable)


module DLC (Q, D, G, CLEAR);

input D, G, CLEAR;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CLEAR or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (CLEAR)
		Q_reg <= 1'b0;
	else if (G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLC (high active latch; asynchronous clear)


module DLCE (Q, D, G, CE, CLEAR);

input D, G, CLEAR, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CLEAR or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (CLEAR)
		Q_reg <= 1'b0;
	else if (G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLCE (high active latch; asynchronous clear; latch enable)


module DLP (Q, D, G, PRESET);

input D, G, PRESET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or PRESET or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (PRESET)
		Q_reg <= 1'b1;
	else if (G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLP (high active latch; asynchronous preset)


module DLPE (Q, D, G, CE, PRESET);

input D, G, PRESET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or PRESET or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (PRESET)
		Q_reg <= 1'b1;
	else if (G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLPE (high active latch; asynchronous preset; latch enable)


module DLN (Q, D, G);

input D, G;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (!G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLN (low active latch)


module DLNE (Q, D, G, CE);

input D, G, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if(!G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLNE (low active latch; latch enable)


module DLNC (Q, D, G, CLEAR);

input D, G, CLEAR;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CLEAR or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (CLEAR)
		Q_reg <= 1'b0;
	else if (!G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLNC (low active latch; asynchronous clear)


module DLNCE (Q, D, G, CE, CLEAR);

input D, G, CLEAR, CE;
output Q;

parameter INIT = 1'b0;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or CLEAR or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (CLEAR)
		Q_reg <= 1'b0;
	else if (!G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLNCE (low active latch; asynchronous clear; latch enable)


module DLNP (Q, D, G, PRESET);

input D, G, PRESET;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or PRESET or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (PRESET)
		Q_reg <= 1'b1;
	else if (!G)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLNP (low active latch; asynchronous preset)


module DLNPE (Q, D, G, CE, PRESET);

input D, G, PRESET, CE;
output Q;

parameter INIT = 1'b1;
reg Q_reg;
wire gsrt = GSR.GSRO;

initial Q_reg = INIT;

always @(D or G or PRESET or CE or gsrt) begin
	if (!gsrt)
		Q_reg <= INIT;
	else if (PRESET)
		Q_reg <= 1'b1;
	else if (!G && CE)
		Q_reg <= D;
end

assign Q = Q_reg;

endmodule // DLNPE (low active latch; asynchronous preset; latch enable)

// Inverter
module INV (O, I);

input  I;
output O;

assign O = !I;
        
endmodule // inv
//IOBs
module IBUF (O, I);

input  I;
output O;

buf IB (O, I);
        
endmodule //IBUF (input buffer)


module OBUF (O, I);

input  I;
output O;

buf OB (O, I);
    
endmodule //OBUF (output buffer)


module TBUF (O, I, OEN);

input I, OEN;
output O;

bufif0 TB (O, I, OEN); 

endmodule // TBUF (output buffer with tri-state control)


module IOBUF (O, IO, I, OEN);

input I,OEN;
output O;
inout IO;

buf OB (O, IO);
bufif0 IB (IO,I,OEN);
    
endmodule //IOBUF (inout buffer)


module IDDR(Q0, Q1, D, CLK);

input D;
input CLK;
output Q0;
output Q1;

parameter Q0_INIT = 1'b0;
parameter Q1_INIT = 1'b0;

wire gsrt = GSR.GSRO;

reg Q0_oreg, Q1_oreg,Q0_reg, Q1_reg;

initial begin
	Q0_reg = Q0_INIT;
	Q1_reg = Q1_INIT;
    Q0_oreg = Q0_INIT;
	Q1_oreg = Q1_INIT;
end

assign Q0 = Q0_reg;
assign Q1 = Q1_reg;

always @(gsrt) begin
	if(!gsrt) begin
		assign Q0_reg = Q0_INIT;
		assign Q1_reg = Q1_INIT;
        assign Q0_oreg = Q0_INIT;
		assign Q1_oreg = Q1_INIT;
	end
	else begin
		deassign Q0_reg;
		deassign Q1_reg;
        deassign Q0_oreg;
		deassign Q1_oreg;
	end
end

always @(posedge CLK) begin
	Q0_oreg <= D;
    Q0_reg <= Q0_oreg;
	Q1_reg <= Q1_oreg;

end

always @(negedge CLK) begin
	Q1_oreg <= D;
end

endmodule //IDDR (ddr input)

module IDDRC(Q0, Q1, D, CLK, CLEAR);

input D;
input CLK;
input CLEAR;
output Q0;
output Q1;

parameter Q0_INIT = 1'b0;
parameter Q1_INIT = 1'b0;

wire gsrt = GSR.GSRO;

reg Q0_oreg, Q1_oreg,Q0_reg, Q1_reg;

initial begin
	Q0_reg = Q0_INIT;
	Q1_reg = Q1_INIT;
    Q0_oreg = Q0_INIT;
	Q1_oreg = Q1_INIT;
end

assign Q0 = Q0_reg;
assign Q1 = Q1_reg;

always @(gsrt or CLEAR) begin
	if(!gsrt) begin
		assign Q0_reg = Q0_INIT;
		assign Q1_reg = Q1_INIT;
        assign Q0_oreg = Q0_INIT;
		assign Q1_oreg = Q1_INIT;
	end
	else if (CLEAR) begin
		assign Q0_reg = 1'b0;
		assign Q1_reg = 1'b0;
        assign Q0_oreg = 1'b0;
		assign Q1_oreg = 1'b0;
	end
	else begin
		deassign Q0_reg;
		deassign Q1_reg;
        deassign Q0_oreg;
		deassign Q1_oreg;
	end
end

always @(posedge CLK) begin
	Q0_oreg <= D;
    Q0_reg <= Q0_oreg;
	Q1_reg <= Q1_oreg;
end

always @(negedge CLK) begin
	Q1_oreg <= D;
end

endmodule //IDDRC (ddr input, asynchronous clear)


module IDDR_MEM (Q0, Q1, D, WADDR, RADDR, PCLK, ICLK, RESET);

parameter GSREN = "false";  //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, ICLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input RESET;
output  Q0,Q1;
wire grstn;
wire lrstn;
//synthesis translate_off
assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

reg Dd0;
reg [7:0] D0_mem;
reg [7:0] D1_mem;
reg [1:0] Q_data;

always @(posedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd0 <= 0;
   else if (!lrstn)
      Dd0 <= 0;
   else 
      Dd0 <= D;

always @(negedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      D0_mem <= 0;
   else if (!lrstn)
      D0_mem <= 0;
   else 
      D0_mem[WADDR] <= Dd0;

always @(negedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      D1_mem <= 0;
   else if (!lrstn)
      D1_mem <= 0;
   else 
      D1_mem[WADDR] <= D;

always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Q_data <= 0;
   else if (!lrstn)
      Q_data <= 0;
   else
      Q_data <= {D1_mem[RADDR], D0_mem[RADDR]};

assign {Q1,Q0}= Q_data;
//synthesis translate_on

endmodule // IDDR_MEM (ddr input with memory)


module ODDR (Q0, Q1, D0, D1, TX, CLK);

input D0;
input D1;
input TX;
input CLK;
output Q0;
output Q1;

parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output
parameter INIT = 1'b0;

reg Dd0_0,Dd0_1,Dd0_2;
reg Dd1_0,Dd1_1,Dd1_2;
reg Ttx0,Ttx1,DT0,DT1;
wire gsrt = GSR.GSRO;

initial begin
    Dd0_0 = 1'b0;
    Dd0_1 = 1'b0;
    Dd0_2 = 1'b0;
    Dd1_0 = 1'b0;
    Dd1_1 = 1'b0;
    Dd1_2 = 1'b0;
    Ttx0 = 1'b0;
    Ttx1 = 1'b0;
    DT0 = 1'b0;
    DT1 = 1'b0;
end

always @(gsrt) begin
	if(!gsrt) begin
        assign Dd1_2 = INIT;
		assign Dd0_2 = INIT;
        assign Dd1_1 = INIT;
		assign Dd0_1 = INIT;
        assign Dd1_0 = INIT;
		assign Dd0_0 = INIT;
        assign Ttx0 = INIT;
        assign Ttx1 = INIT;
        assign DT0 = INIT;
        assign DT1 = INIT;
	end
	else begin
		deassign Dd1_2;
		deassign Dd0_2;
        deassign Dd1_1;
		deassign Dd0_1;
        deassign Dd1_0;
		deassign Dd0_0;
		deassign Ttx0;
		deassign Ttx1;
		deassign DT0;
		deassign DT1;
	end
end

always @(posedge CLK) begin
	Dd0_0 <= D0;
	Dd1_0 <= D1;
    Dd0_1 <= Dd0_0;
	Dd1_1 <= Dd1_0;
    Ttx0 <= TX;
    Ttx1 <= Ttx0;
end

always @(posedge CLK) begin
	Dd1_2 <= Dd1_1;
    DT0 <= DT1;
end

always @(negedge CLK) begin
	Dd0_2 <= Dd0_1;
    DT1 <= Ttx1;
end

assign Q0 = (CLK)? Dd0_2 : Dd1_2;
assign Q1 = (TXCLK_POL == 1'b0) ? DT0 : DT1;

endmodule // ODDR (ddr output)

module ODDRC (Q0, Q1, D0, D1, TX, CLK, CLEAR);

input D0, D1, TX, CLK, CLEAR;
output Q0,Q1;

parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output
parameter INIT = 1'b0;

wire gsrt = GSR.GSRO;

reg Dd0_0,Dd0_1,Dd0_2;
reg Dd1_0,Dd1_1,Dd1_2;
reg Ttx0,Ttx1,DT0,DT1;

initial begin
    Dd0_0 = 1'b0;
    Dd0_1 = 1'b0;
    Dd0_2 = 1'b0;
    Dd1_0 = 1'b0;
    Dd1_1 = 1'b0;
    Dd1_2 = 1'b0;
    Ttx0 = 1'b0;
    Ttx1 = 1'b0;
    DT0 = 1'b0;
    DT1 = 1'b0;
end

always @(gsrt or CLEAR) begin
	if(!gsrt) begin
		assign Dd1_2 = INIT;
		assign Dd0_2 = INIT;
        assign Dd1_1 = INIT;
		assign Dd0_1 = INIT;
        assign Dd1_0 = INIT;
		assign Dd0_0 = INIT;
	    assign Ttx0 = INIT;
        assign Ttx1 = INIT;
        assign DT0 = INIT;
        assign DT1 = INIT;
    end
	else if(CLEAR) begin
		assign Dd1_2 = 1'b0;
		assign Dd0_2 = 1'b0;
        assign Dd1_1 = 1'b0;
		assign Dd0_1 = 1'b0;
        assign Dd1_0 = 1'b0;
		assign Dd0_0 = 1'b0;
        assign Ttx0 = INIT;
        assign Ttx1 = INIT;
        assign DT0 = INIT;
        assign DT1 = INIT;
	end
	else begin
		deassign Dd1_2;
		deassign Dd0_2;
        deassign Dd1_1;
		deassign Dd0_1;
        deassign Dd1_0;
		deassign Dd0_0;
        deassign Ttx0;
		deassign Ttx1;
		deassign DT0;
		deassign DT1;
	end
end

always @(posedge CLK) begin
	Dd0_0 <= D0;
	Dd1_0 <= D1;
    Dd0_1 <= Dd0_0;
	Dd1_1 <= Dd1_0;
    Ttx0 <= TX;
    Ttx1 <= Ttx0;
end

always @(posedge CLK) begin
	Dd1_2 <= Dd1_1;
    DT0 <= DT1;
end

always @(negedge CLK) begin
	Dd0_2 <= Dd0_1;
    DT1 <= Ttx1;    
end

assign Q0 = (CLK)? Dd0_2 : Dd1_2;
assign Q1 = (TXCLK_POL == 1'b0) ? DT0 : DT1;

endmodule // ODDRC (ddr output with asynchronous clear)


module ODDR_MEM (Q0, Q1, D0, D1, TX, PCLK, TCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1;
input TX, PCLK, TCLK, RESET;
output  Q0, Q1;

//synthesis translate_off
reg [1:0] Dd1;
reg Ttx1;
reg [1:0] Dd2;
reg Ttx2;
reg Qq0;
reg DT0,DT1;
reg Qq1;
wire tclk_sig;
wire grstn;
wire lrstn;

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;
assign tclk_sig = (TCLK_SOURCE == "DQSW") ? TCLK : ~TCLK;

always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn) begin
      Dd1 <= 2'b0;
      Ttx1 <= 1'b0;
   end
   else if (!lrstn) begin
      Dd1 <= 0;
      Ttx1 <= 0;
   end
   else begin
      	Dd1 <= {D1, D0};
      	Ttx1 <= TX;
   end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
   if (!grstn) begin
      Dd2 <= 2'b0;
      Ttx2 <= 1'b0;
   end
   else if (!lrstn) begin
      Dd2 <= 2'b0;
      Ttx2 <= 1'b0;
   end
   else begin
      Dd2 <= Dd1;
      Ttx2 <= Ttx1;
   end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
   if (!grstn) begin
      Qq0 <= 1'b0;
      DT0 <= 1'b0;
   end
   else if (!lrstn) begin
      Qq0 <= 1'b0;
      DT0 <= 1'b0;
   end
   else begin
      Qq0 <= Dd2[1];
      DT0 <= DT1;
   end

always @(negedge tclk_sig or negedge grstn or negedge lrstn)
   if (!grstn) begin
      Qq1 <= 1'b0;
      DT1 <= 1'b0;
   end
   else if (!lrstn) begin
      Qq1 <= 1'b0;
      DT1 <= 1'b0;
   end
   else begin
      Qq1 <= Dd2[0];
      DT1 <= Ttx2;
   end

assign Q0 = tclk_sig ? Qq1 : Qq0;
assign Q1 = (TXCLK_POL == 1'b0) ? DT0 : DT1;

//synthesis translate_on

endmodule // ODDR_MEM (ddr output with memory)

module IDES4 (Q0, Q1, Q2, Q3, D, CALIB, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, FCLK, PCLK, CALIB, RESET;
output Q0,Q1,Q2,Q3;
wire grstn;
wire lrstn; 
//synthesis translate_off

assign grstn = (GSREN == "true") ? GSR.GSRO : 1'b1;
assign lrstn = (LSREN == "true") ? (~RESET) : 1'b1;

reg Dd0,Dd1;
reg [3:0] D_data,data;
reg D_en1,D_en;
reg Dd_sel,calib_state;
reg [3:0] Q_data;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en;
reg Dd0_reg0,Dd0_reg1,Dd1_reg0,Dd1_reg1;

initial begin
    calib_state = 1'b0;
    D_en1 = 1'b0;
    D_en = 1'b0;
    Dd_sel = 1'b0;
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd0 <= 1'b0;
    end else if (!lrstn) begin
        Dd0 <= 1'b0;
    end else begin
        Dd0 <= D;
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd1 <= 1'b0;
    end else if (!lrstn) begin
        Dd1 <= 1'b0;
    end else begin
        Dd1 <= D;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
    end else if (!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
    end else begin
        Dd0_reg0 <= Dd0;
        Dd0_reg1 <= Dd0_reg0;
        Dd1_reg0 <= Dd1;
        Dd1_reg1 <= Dd1_reg0;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~(CALIBdata_rising_p && calib_state);

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        calib_state <= 1'b0;
        D_en1 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= ~D_en1;
        if (CALIBdata_rising_p) begin
            calib_state <= ~calib_state;
            Dd_sel <= ~Dd_sel;
        end else begin
            calib_state <= calib_state;
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en1 <= ~D_en1;
        end else begin
            D_en1 <= D_en1;
        end
    end
end

always @(Dd_sel or Dd0 or Dd0_reg0 or Dd0_reg1 or Dd1_reg0 or Dd1_reg1) begin
    if(Dd_sel) begin
        D_data[3] = Dd0;
        D_data[2] = Dd1_reg0;
        D_data[1] = Dd0_reg0;
        D_data[0] = Dd1_reg1;
    end else begin
        D_data[3] = Dd1_reg0;
        D_data[2] = Dd0_reg0;
        D_data[1] = Dd1_reg1;
        D_data[0] = Dd0_reg1;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        data <= 4'b0;
    end else if (!lrstn) begin
        data <= 4'b0;
    end else if (D_en) begin
        data <= D_data;
    end
end

always @(posedge PCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Q_data <= 4'b0;
    end else if (!lrstn) begin
        Q_data <= 4'b0;
    end else begin
        Q_data <= data;
    end
end

assign {Q3,Q2,Q1,Q0} = Q_data;
//synthesis translate_on

endmodule // IDES4 (4 to 1 deserializer)


module IDES4_MEM (Q0, Q1, Q2, Q3, D, WADDR, RADDR, CALIB, PCLK, FCLK, ICLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, ICLK, FCLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input RESET,CALIB;
output Q0,Q1,Q2,Q3;
wire grstn,lrstn;
reg Dd0;
reg [3:0] D_data,Q_data, data;
reg D_en1,D_en;
reg Dd_sel,calib_state;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en;
reg Dd0_reg0,Dd0_reg1,Dd1_reg0,Dd1_reg1;
reg [7:0] D0_mem;
reg [7:0] D1_mem;

initial begin
    calib_state = 1'b0;
    D_en1 = 1'b0;
    D_en = 1'b0;
    Dd_sel = 1'b0;
end

assign grstn = (GSREN == "true") ? GSR.GSRO : 1'b1;
assign lrstn = (LSREN == "true") ? (~RESET) : 1'b1;

always @(posedge ICLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd0 <= 1'b0;
    end else if (!lrstn) begin
        Dd0 <= 1'b0;
    end else begin
        Dd0 <= D;
    end
end

always @(negedge ICLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        D0_mem <= 8'b0;
    end else if (!lrstn) begin
        D0_mem <= 8'b0;
    end else begin
        D0_mem[WADDR] <= Dd0;
    end
end

always @(negedge ICLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        D1_mem <= 8'b0;
    end else if (!lrstn) begin
        D1_mem <= 8'b0;
    end else begin
        D1_mem[WADDR] <= D;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
    end else if (!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
    end else begin
        Dd0_reg0 <= D0_mem[RADDR];
        Dd0_reg1 <= Dd0_reg0;
        Dd1_reg0 <= D1_mem[RADDR];
        Dd1_reg1 <= Dd1_reg0;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~(CALIBdata_rising_p && calib_state);

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        calib_state <= 1'b0;
        D_en1 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= ~D_en1;
        if (CALIBdata_rising_p) begin
            calib_state <= ~calib_state;
            Dd_sel <= ~Dd_sel;
        end else begin
            calib_state <= calib_state;
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en1 <= ~D_en1;
        end else begin
            D_en1 <= D_en1;
        end
    end
end

always @(Dd_sel or D0_mem[RADDR] or Dd0_reg0 or Dd0_reg1 or Dd1_reg0 or Dd1_reg1) begin
    if(Dd_sel) begin
        D_data[3] = D0_mem[RADDR];
        D_data[2] = Dd1_reg0;
        D_data[1] = Dd0_reg0;
        D_data[0] = Dd1_reg1;
    end else begin
        D_data[3] = Dd1_reg0;
        D_data[2] = Dd0_reg0;
        D_data[1] = Dd1_reg1;
        D_data[0] = Dd0_reg1;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        data <= 4'b0;
    end else if (!lrstn) begin
        data <= 4'b0;
    end else if (D_en) begin
        data <= D_data;
    end
end

always @(posedge PCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Q_data <= 4'b0;
    end else if (!lrstn) begin
        Q_data <= 4'b0;
    end else begin
        Q_data <= data;
    end
end

assign {Q3,Q2,Q1,Q0} = Q_data;

endmodule //IDES4_MEM (4 to 1 deserializer with memory)


module IVIDEO (Q0, Q1, Q2, Q3, Q4, Q5, Q6, D, CALIB, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";   //"true"; "false"

input D, FCLK, PCLK, CALIB, RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6;
wire grstn;
wire lrstn; 
assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

reg Dd0;
reg Dd1;
reg [6:0] D_data;
reg [6:0] data;
reg [6:0] Q_data;
reg Dd_sel;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en,dsel_en;
reg Dd0_reg0,Dd0_reg1,Dd0_reg2,Dd1_reg0,Dd1_reg1,Dd1_reg2,Dd1_reg3;
reg D_en,D_en0,D_en1;

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd0 <= 0;
   else if (!lrstn)
      Dd0 <= 0;
   else
      Dd0 <= D;

always @(negedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd1 <= 0;
   else if (!lrstn)
      Dd1 <= 0;
   else
      Dd1 <= D;

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~CALIBdata_rising_p;
assign dsel_en = (Dd_sel & D_en1 & (~D_en0) & (~CALIBdata_rising_p)) | ((~Dd_sel) & D_en0 & D_en1);
always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        D_en1 <= 1'b0;
        D_en0 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= (~((~Dd_sel) | D_en0 | D_en1)) | (~(Dd_sel | D_en1 | (~D_en0)));
        if (dsel_en) begin
            Dd_sel <= ~Dd_sel;
        end else begin
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en0 <= ~(D_en0 | (Dd_sel & D_en1 & (~D_en0)));
        end else  begin
            D_en0 <= D_en0;
        end
                                                                                   
        if (dcnt_en) begin
            D_en1 <= (~(Dd_sel & D_en1 & (~D_en0))) &(D_en0 ^ D_en1);
        end else begin
            D_en1 <= D_en1;
        end    
    end    
        
end

always @(posedge FCLK or negedge grstn  or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else if(!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else begin
        Dd0_reg0 <= Dd0;
        Dd0_reg1 <= Dd0_reg0;
        Dd0_reg2 <= Dd0_reg1;
        Dd1_reg0 <= Dd1;
        Dd1_reg1 <= Dd1_reg0;
        Dd1_reg2 <= Dd1_reg1;
        Dd1_reg3 <= Dd1_reg2;
    end
end

always @(Dd_sel or Dd0 or Dd0_reg0 or Dd0_reg1 or Dd0_reg2 or Dd1_reg0 or Dd1_reg1 or Dd1_reg2 or Dd1_reg3) begin
    if(Dd_sel) begin
        D_data[6] <= Dd0;
        D_data[5] <= Dd1_reg0;
        D_data[4] <= Dd0_reg0;
        D_data[3] <= Dd1_reg1;
        D_data[2] <= Dd0_reg1;
        D_data[1] <= Dd1_reg2;
        D_data[0] <= Dd0_reg2;
    end else begin
        D_data[6] <= Dd1_reg0;
        D_data[5] <= Dd0_reg0;
        D_data[4] <= Dd1_reg1;
        D_data[3] <= Dd0_reg1;
        D_data[2] <= Dd1_reg2;
        D_data[1] <= Dd0_reg2;
        D_data[0] <= Dd1_reg3;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      data <= 0;
   else if (!lrstn)
      data <= 0;
   else if (D_en)
      data <= D_data;


always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Q_data <= 0;
   else if (!lrstn)
      Q_data <= 0;
   else
      Q_data <= data;

assign {Q6, Q5, Q4, Q3, Q2, Q1, Q0} = Q_data;

endmodule //IVIDEO (7 to 1 deserializer)

module IDES8 (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, D, CALIB, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, FCLK, PCLK, CALIB,RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7;
wire grstn;
wire lrstn;
assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

reg Dd0;
reg Dd1;
reg [7:0] D_data;
reg [7:0] data;
reg D_en,D_en0,D_en1;
reg [7:0] Q_data;
reg Dd_sel,calib_state;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en;
reg Dd0_reg0,Dd0_reg1,Dd0_reg2,Dd0_reg3,Dd1_reg0,Dd1_reg1,Dd1_reg2,Dd1_reg3;

initial begin
    calib_state = 1'b0;
    D_en0 = 1'b0;
    D_en1 = 1'b0;
    D_en = 1'b0;
    Dd_sel = 1'b0;
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd0 <= 0;
    end else if (!lrstn) begin
        Dd0 <= 0;
    end else begin
        Dd0 <= D;
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        Dd1 <= 0;
    end else if (!lrstn) begin
        Dd1 <= 0;
    end else begin
        Dd1 <= D;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~(CALIBdata_rising_p && calib_state);

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        calib_state <= 1'b0;
        D_en1 <= 1'b0;
        D_en0 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= D_en0 & (~D_en1);
        if (CALIBdata_rising_p) begin
            calib_state <= ~calib_state;
            Dd_sel <= ~Dd_sel;
        end else begin
            calib_state <= calib_state;
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en0 <= ~D_en0;
        end else  begin
            D_en0 <= D_en0;
        end
                                                                                   
        if (dcnt_en) begin
            D_en1 <= D_en0 ^ D_en1;
        end else begin
            D_en1 <= D_en1;
        end    
    end    
        
end

always @(posedge FCLK or negedge grstn  or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else if(!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else begin
        Dd0_reg0 <= Dd0;
        Dd0_reg1 <= Dd0_reg0;
        Dd0_reg2 <= Dd0_reg1;
        Dd0_reg3 <= Dd0_reg2;
        Dd1_reg0 <= Dd1;
        Dd1_reg1 <= Dd1_reg0;
        Dd1_reg2 <= Dd1_reg1;
        Dd1_reg3 <= Dd1_reg2;
    end
end

always @(Dd_sel or Dd0 or Dd0_reg0 or Dd0_reg1 or Dd0_reg2 or Dd0_reg3 or Dd1_reg0 or Dd1_reg1 or Dd1_reg2 or Dd1_reg3) begin
    if(Dd_sel) begin
        D_data[7] <= Dd0;
        D_data[6] <= Dd1_reg0;
        D_data[5] <= Dd0_reg0;
        D_data[4] <= Dd1_reg1;
        D_data[3] <= Dd0_reg1;
        D_data[2] <= Dd1_reg2;
        D_data[1] <= Dd0_reg2;
        D_data[0] <= Dd1_reg3;
    end else begin
        D_data[7] <= Dd1_reg0;
        D_data[6] <= Dd0_reg0;
        D_data[5] <= Dd1_reg1;
        D_data[4] <= Dd0_reg1;
        D_data[3] <= Dd1_reg2;
        D_data[2] <= Dd0_reg2;
        D_data[1] <= Dd1_reg3;
        D_data[0] <= Dd0_reg3;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      data <= 0;
   else if (!lrstn)
      data <= 0;
   else if (D_en)
      data <= D_data;

always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Q_data <= 0;
   else if (!lrstn)
      Q_data <= 0;
   else
      Q_data <= data;

assign {Q7, Q6, Q5, Q4, Q3, Q2, Q1, Q0} = Q_data;

endmodule // IDES8 (8 to 1 deserializer)


module IDES8_MEM (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, D, WADDR, RADDR, CALIB, PCLK, FCLK, ICLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, ICLK, FCLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input RESET,CALIB;
output  Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7;
wire grstn;
wire lrstn; 
//synthesis translate_off
assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

reg Dd0;
reg [7:0] Dd0_mem;
reg [7:0] Dd1_mem;
reg [7:0] Q_data;
reg [7:0] D_data;
reg [7:0] data;
reg D_en,D_en0,D_en1;
reg Dd_sel,calib_state;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en;
reg Dd0_reg0,Dd0_reg1,Dd0_reg2,Dd0_reg3,Dd1_reg0,Dd1_reg1,Dd1_reg2,Dd1_reg3;

initial begin
    calib_state = 1'b0;
    D_en0 = 1'b0;
    D_en1 = 1'b0;
    D_en = 1'b0;
    Dd_sel = 1'b0;
end

always @(posedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd0 <= 0;
   else if (!lrstn)
      Dd0 <= 0;
   else
      Dd0 <= D;

always @(negedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd0_mem <= 0;
   else if (!lrstn)
      Dd0_mem <= 0;
   else
      Dd0_mem[WADDR] <= Dd0;

always @(negedge ICLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd1_mem <= 0;
   else if (!lrstn)
      Dd1_mem <= 0;
   else
      Dd1_mem[WADDR] <= D;

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~(CALIBdata_rising_p && calib_state);

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        calib_state <= 1'b0;
        D_en1 <= 1'b0;
        D_en0 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= D_en0 & (~D_en1);
        if (CALIBdata_rising_p) begin
            calib_state <= ~calib_state;
            Dd_sel <= ~Dd_sel;
        end else begin
            calib_state <= calib_state;
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en0 <= ~D_en0;
        end else  begin
            D_en0 <= D_en0;
        end
                                                                                   
        if (dcnt_en) begin
            D_en1 <= D_en0 ^ D_en1;
        end else begin
            D_en1 <= D_en1;
        end    
    end    
        
end

always @(posedge FCLK or negedge grstn  or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else if(!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
    end else begin
        Dd0_reg0 <= Dd0_mem[RADDR];
        Dd0_reg1 <= Dd0_reg0;
        Dd0_reg2 <= Dd0_reg1;
        Dd0_reg3 <= Dd0_reg2;
        Dd1_reg0 <= Dd1_mem[RADDR];
        Dd1_reg1 <= Dd1_reg0;
        Dd1_reg2 <= Dd1_reg1;
        Dd1_reg3 <= Dd1_reg2;
    end
end

always @(Dd_sel or Dd0_mem[RADDR] or Dd0_reg0 or Dd0_reg1 or Dd0_reg2 or Dd0_reg3 or Dd1_reg0 or Dd1_reg1 or Dd1_reg2 or Dd1_reg3) begin
    if(Dd_sel) begin
        D_data[7] <= Dd0_mem[RADDR];
        D_data[6] <= Dd1_reg0;
        D_data[5] <= Dd0_reg0;
        D_data[4] <= Dd1_reg1;
        D_data[3] <= Dd0_reg1;
        D_data[2] <= Dd1_reg2;
        D_data[1] <= Dd0_reg2;
        D_data[0] <= Dd1_reg3;
    end else begin
        D_data[7] <= Dd1_reg0;
        D_data[6] <= Dd0_reg0;
        D_data[5] <= Dd1_reg1;
        D_data[4] <= Dd0_reg1;
        D_data[3] <= Dd1_reg2;
        D_data[2] <= Dd0_reg2;
        D_data[1] <= Dd1_reg3;
        D_data[0] <= Dd0_reg3;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      data <= 0;
   else if (!lrstn)
      data <= 0;
   else if (D_en)
      data <= D_data;

always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Q_data <= 0;
   else if (!lrstn)
      Q_data <= 0;
   else
      Q_data <= data;

assign {Q7, Q6, Q5, Q4, Q3, Q2, Q1, Q0} = Q_data;

//synthesis translate_on

endmodule // IDES8_MEM (8 to 1 deserializer with memory)


module IDES10 (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, D, CALIB, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, FCLK, PCLK, CALIB, RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9;
wire grstn;
wire lrstn;
//synthesis translate_off
assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

reg Dd0;
reg Dd1;
reg [9:0] D_data;
reg [9:0] data;
reg D_en,D_en0,D_en1,D_en2;
reg [9:0] Q_data;
reg Dd_sel,calib_state;
reg reset_delay;
wire CALIBdata_rising_p;
reg [2:0] CALIBdata;
wire dcnt_en,dcnt_reset;
reg Dd0_reg0,Dd0_reg1,Dd0_reg2,Dd0_reg3,Dd0_reg4,Dd1_reg0,Dd1_reg1,Dd1_reg2,Dd1_reg3,Dd1_reg4;

initial begin
    calib_state = 1'b0;
    D_en0 = 1'b0;
    D_en1 = 1'b0;
    D_en2 = 1'b0;
    D_en  = 1'b0;
    Dd_sel = 1'b0;

end

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd0 <= 0;
   else if (!lrstn)
      Dd0 <= 0;
   else
      Dd0 <= D;

always @(negedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Dd1 <= 0;
   else if (!lrstn)
      Dd1 <= 0;
   else
      Dd1 <= D;

always @(posedge FCLK or negedge grstn or negedge lrstn) begin
    if (!grstn) begin
        reset_delay <= 1'b0;
    end else if (!lrstn) begin
        reset_delay <= 1'b0;
    end else begin
        reset_delay <= 1'b1;
    end
end

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        CALIBdata <= 3'b0;
    end else begin
        CALIBdata <= {CALIBdata[1:0], CALIB};
    end
end

assign CALIBdata_rising_p =  CALIBdata[1] && (~CALIBdata[2]);
assign dcnt_en = ~(CALIBdata_rising_p && calib_state);
assign dcnt_reset = D_en2 & (~D_en1) & (~D_en0);

always @(posedge FCLK or negedge reset_delay) begin
    if (!reset_delay) begin
        calib_state <= 1'b0;
        D_en0 <= 1'b0;
        D_en1 <= 1'b0;
        D_en2 <= 1'b0;
        D_en  <= 1'b0;
        Dd_sel <= 1'b0;
    end else begin
        D_en <= (~D_en0) & D_en1;
        if (CALIBdata_rising_p) begin
            calib_state <= ~calib_state;
            Dd_sel <= ~Dd_sel;
        end else begin
            calib_state <= calib_state;
            Dd_sel <= Dd_sel;
        end
        
        if (dcnt_en) begin
            D_en0 <= ~(dcnt_reset | D_en0);
        end else  begin
            D_en0 <= D_en0;
        end
                                                                                   
        if (dcnt_en) begin
            D_en1 <= D_en0 ^ D_en1;
        end else begin
            D_en1 <= D_en1;
        end    

        if (dcnt_en) begin
            D_en2 <= ((D_en0&D_en1) ^ D_en2) & (~dcnt_reset);
        end else begin
            D_en2 <= D_en2;
        end    

    end    
        
end

always @(posedge FCLK or negedge grstn  or negedge lrstn) begin
    if (!grstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd0_reg4 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
        Dd1_reg4 <= 1'b0;
    end else if(!lrstn) begin
        Dd0_reg0 <= 1'b0;
        Dd0_reg1 <= 1'b0;
        Dd0_reg2 <= 1'b0;
        Dd0_reg3 <= 1'b0;
        Dd0_reg4 <= 1'b0;
        Dd1_reg0 <= 1'b0;
        Dd1_reg1 <= 1'b0;
        Dd1_reg2 <= 1'b0;
        Dd1_reg3 <= 1'b0;
        Dd1_reg4 <= 1'b0;        
    end else begin
        Dd0_reg0 <= Dd0;
        Dd0_reg1 <= Dd0_reg0;
        Dd0_reg2 <= Dd0_reg1;
        Dd0_reg3 <= Dd0_reg2;
        Dd0_reg4 <= Dd0_reg3;
        Dd1_reg0 <= Dd1;
        Dd1_reg1 <= Dd1_reg0;
        Dd1_reg2 <= Dd1_reg1;
        Dd1_reg3 <= Dd1_reg2;
        Dd1_reg4 <= Dd1_reg3;
    end
end

always @(Dd_sel or Dd0 or Dd0_reg0 or Dd0_reg1 or Dd0_reg2 or Dd0_reg3 or Dd0_reg4 or Dd1_reg0 or Dd1_reg1 or Dd1_reg2 or Dd1_reg3 or Dd1_reg4) begin
    if(Dd_sel) begin
        D_data[9] <= Dd0;
        D_data[8] <= Dd1_reg0;
        D_data[7] <= Dd0_reg0;
        D_data[6] <= Dd1_reg1;
        D_data[5] <= Dd0_reg1;
        D_data[4] <= Dd1_reg2;
        D_data[3] <= Dd0_reg2;
        D_data[2] <= Dd1_reg3;
        D_data[1] <= Dd0_reg3;
        D_data[0] <= Dd1_reg4;
    end else begin
        D_data[9] <= Dd1_reg0;
        D_data[8] <= Dd0_reg0;
        D_data[7] <= Dd1_reg1;
        D_data[6] <= Dd0_reg1;
        D_data[5] <= Dd1_reg2;
        D_data[4] <= Dd0_reg2;
        D_data[3] <= Dd1_reg3;
        D_data[2] <= Dd0_reg3;
        D_data[1] <= Dd1_reg4;
        D_data[0] <= Dd0_reg4;
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      data <= 0;
   else if (!lrstn)
      data <= 0;
   else if (D_en)
      data <= D_data;

always @(posedge PCLK or negedge grstn or negedge lrstn)
   if (!grstn)
      Q_data <= 0;
   else if (!lrstn)
      Q_data <= 0;
   else
      Q_data <= data;

assign {Q9, Q8, Q7, Q6, Q5, Q4, Q3, Q2, Q1, Q0} = Q_data;

//synthesis translate_on

endmodule // IDES10 (10 to 1 deserializer)

//OSER4
module OSER4 (Q0, Q1, D0, D1, D2, D3, TX0, TX1, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";   //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D3, D2, D1, D0;
input TX1, TX0;
input PCLK, FCLK, RESET;
output  Q0, Q1;

//synthesis translate_off
reg [3:0] Dd1,Dd2,Dd3;
reg [1:0] Ttx1,Ttx2,Ttx3;
reg rstn_dsel,dsel,d_up0,d_up1;
wire d_en0,d_en1;
reg Qq_n,Q_data_n,Qq_p,Q_data_p;
wire grstn,lrstn;

initial begin
    dsel = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 4'b0;
        Ttx1 <= 2'b0;
    end
    else if (!lrstn) begin
        Dd1 <= 4'b0;
        Ttx1 <= 2'b0;
    end
    else begin
        Dd1 <= {D3,D2,D1,D0};
        Ttx1 <= {TX1,TX0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel <= 1'b0;        
    end
    else begin
        rstn_dsel <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        dsel <= 1'b0;
    end else begin
        dsel <= ~dsel;
    end
end

assign d_en0 = ~dsel;
assign d_en1 = (HWL == "true") ? (~dsel) : dsel;

always @(posedge FCLK or negedge rstn_dsel)
begin
    if (!rstn_dsel) begin
        d_up0 <= 1'b0;
        d_up1 <= 1'b0;
    end else begin
        if(d_en0)begin
            d_up0 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
        end

        if(d_en1)begin
            d_up1 <= 1'b1;
        end else begin
            d_up1 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd2 <= 4'b0;
        Ttx2 <= 2'b0;
    end else if (!lrstn) begin
        Dd2 <= 4'b0;
        Ttx2 <= 2'b0;
    end else begin
        if(d_up0)begin
            Dd2 <= Dd1;
            Ttx2 <= Ttx1;
        end else begin
            Dd2 <= Dd2;
            Ttx2 <= Ttx2;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)begin
   if (!grstn) begin
      Dd3 <= 4'b0;
      Ttx3 <= 2'b0;
   end else if (!lrstn) begin
      Dd3 <= 4'b0;
      Ttx3 <= 2'b0;
   end else begin
        if(d_up1)begin
            Dd3 <= Dd2;
            Ttx3 <= Ttx2;
        end else begin
            Dd3[0] <= Dd3[2];
            Dd3[2] <= 1'b0;
            Dd3[1] <= Dd3[3];
            Dd3[3] <= 1'b0;
            Ttx3[0] <= Ttx3[1];
            Ttx3[1] <= 1'b0;
        end
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0; 
    end else if (!lrstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0; 
    end else begin
        Qq_n <= Dd3[0];
        Q_data_n <= Ttx3[0];
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 1'b0;
    end else if(!lrstn) begin
        Qq_p <= 1'b0;
    end else begin
        Qq_p <= Dd3[1];
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Q_data_p <= 1'b0;
    end else if (!lrstn) begin
        Q_data_p <= 1'b0;
    end else begin
        Q_data_p <= Q_data_n;
    end
end

assign Q0 = FCLK ? Qq_n : Qq_p;
assign Q1 = (TXCLK_POL == 1'b0) ? Q_data_p : Q_data_n;

//synthesis translate_on

endmodule // OSER4 (4 to 1 serializer)


module OSER4_MEM (Q0, Q1, D0, D1, D2, D3, TX0, TX1, PCLK, FCLK, TCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3;
input TX0, TX1;
input PCLK, FCLK, TCLK, RESET;
output  Q0,  Q1;

//synthesis translate_off
reg [3:0] Dd1,Dd2,Dd3;
reg [1:0] Ttx1,Ttx2,Ttx3;
reg rstn_dsel0,dsel0,d_up0;
reg rstn_dsel1,dsel1,d_up1;
wire d_en0,d_en1;
reg Qq_n,Q_data_n,Qq_p,Q_data_p;
wire tclk_sig;
wire grstn,lrstn;

initial begin
    dsel0 = 1'b0;
    dsel1 = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;
assign tclk_sig = (TCLK_SOURCE == "DQSW") ? TCLK : ~TCLK;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 4'b0;
        Ttx1 <= 2'b0;
    end
    else if (!lrstn) begin
        Dd1 <= 4'b0;
        Ttx1 <= 2'b0;
    end
    else begin
        Dd1 <= {D3,D2,D1,D0};
        Ttx1 <= {TX1,TX0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel0 <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel0 <= 1'b0;        
    end
    else begin
        rstn_dsel0 <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel0) 
begin
    if (!rstn_dsel0) begin
        dsel0 <= 1'b0;
    end else begin
        dsel0 <= ~dsel0;
    end
end

assign d_en0 = ~dsel0;

always @(posedge FCLK or negedge rstn_dsel0) 
begin
    if (!rstn_dsel0) begin
        d_up0 <= 1'b0;
    end else begin
        if(d_en0)begin
            d_up0 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
        end
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel1 <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel1 <= 1'b0;
    end
    else begin
        rstn_dsel1 <= 1'b1;
    end
end

always @(posedge tclk_sig or negedge rstn_dsel1) 
begin
    if (!rstn_dsel1) begin
        dsel1 <= 1'b0;
    end else begin
        dsel1 <= ~dsel1;
    end
end

assign d_en1 = (HWL == "true") ? ~dsel1 : dsel1;

always @(posedge tclk_sig or negedge rstn_dsel1) 
begin
    if (!rstn_dsel1) begin
        d_up1 <= 1'b0;
    end else begin
        if(d_en1)begin
            d_up1 <= 1'b1;
        end else begin
            d_up1 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd2 <= 4'b0;
        Ttx2 <= 2'b0;
    end else if (!lrstn) begin
        Dd2 <= 4'b0;
        Ttx2 <= 2'b0;
    end else begin
        if(d_up0)begin
            Dd2 <= Dd1;
            Ttx2 <= Ttx1;
        end else begin
            Dd2 <= Dd2;
            Ttx2 <= Ttx2;
        end
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)begin
   if (!grstn) begin
      Dd3 <= 4'b0;
      Ttx3 <= 2'b0;
   end else if (!lrstn) begin
      Dd3 <= 4'b0;
      Ttx3 <= 2'b0;
   end else begin
        if(d_up1)begin
            Dd3 <= Dd2;
            Ttx3 <= Ttx2;
        end else begin
            Dd3[0] <= Dd3[2];
            Dd3[2] <= 1'b0;
            Dd3[1] <= Dd3[3];
            Dd3[3] <= 1'b0;
            Ttx3[0] <= Ttx3[1];
            Ttx3[1] <= 1'b0;
        end
    end
end

always @(negedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0; 
    end else if (!lrstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0; 
    end else begin
        Qq_n <= Dd3[0];
        Q_data_n <= Ttx3[0];
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 1'b0; 
    end else if(!lrstn) begin
        Qq_p <= 1'b0;   
    end else begin
        Qq_p <= Dd3[1];
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Q_data_p <= 1'b0; 
    end else if (!lrstn) begin
        Q_data_p <= 1'b0; 
    end else begin
        Q_data_p <= Q_data_n;
    end
end

assign Q0 = tclk_sig ? Qq_n : Qq_p;
assign Q1 = (TXCLK_POL == 1'b0) ? Q_data_p : Q_data_n;

//synthesis translate_on

endmodule // OSER4_MEM (4 to 1 serializer with memory)

//OVIDEO
module OVIDEO (Q, D0, D1, D2, D3, D4, D5, D6, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D6, D5, D4, D3, D2, D1, D0;
input PCLK, FCLK, RESET;
output  Q;

//synthesis translate_off
reg [6:0] Dd1;
reg rstn_dsel,dcnt0,dcnt1,dsel;
wire dsel_en,dcnt_reset,d_en0,d_en1;
reg d_up0,d_up1;
reg [7:0] Dd2,Dd3,Dd4;
reg Qq_p,Qq_n;
wire grstn;
wire lrstn; 

initial begin
    dcnt0 = 1'b0;
    dcnt1 = 1'b0;
    dsel = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 0;
    end else if (!lrstn) begin
        Dd1 <= 0;
    end else begin
        Dd1 <= {D6,D5,D4,D3,D2,D1,D0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel <= 1'b0;        
    end
    else begin
        rstn_dsel <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel)
begin
    if (!rstn_dsel) begin
        dcnt0 <= 1'b0;
        dcnt1 <= 1'b0;
    end else begin
        dcnt0 <= ~(dcnt0 | dcnt_reset);
        dcnt1 <= ~((dcnt0 ^~ dcnt1) | dcnt_reset);
    end
end

assign dsel_en = (dsel & dcnt1 & (~dcnt0)) | ((~dsel) & dcnt1 & dcnt0);

always @(posedge FCLK or negedge rstn_dsel)
begin
    if (!rstn_dsel) begin
        dsel <= 1'b0;
    end else begin
        if(dsel_en) begin
            dsel <= ~dsel;
        end else begin
            dsel <= dsel;        
        end
    end
end

assign dcnt_reset = (~dcnt0) & dcnt1 & dsel;
assign d_en0 = ((~dsel) & (~dcnt1) & dcnt0) | (dsel & (~dcnt1) & (~dcnt0));

always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        d_up0 <= 1'b0;
    end else begin
        if(d_en0)begin
            d_up0 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
        end
    end
end

assign d_en1 = ((~dsel) & dcnt1 & (~dcnt0)) | (dsel & (~dcnt1) & dcnt0);

always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        d_up1 <= 1'b0;
    end else begin
        if(d_en1)begin
            d_up1 <= 1'b1;
        end else begin
            d_up1 <= 1'b0;
        end
    end
end

always @(Dd1 or Dd3 or dsel)
begin
    if(dsel) begin
        Dd2[0] <= Dd3[6];
        Dd2[1] <= Dd1[0];
        Dd2[2] <= Dd1[1];
        Dd2[3] <= Dd1[2];
        Dd2[4] <= Dd1[3];
        Dd2[5] <= Dd1[4];
        Dd2[6] <= Dd1[5];
        Dd2[7] <= Dd1[6];
    end else begin
        Dd2[0] <= Dd1[0];
        Dd2[1] <= Dd1[1];
        Dd2[2] <= Dd1[2];
        Dd2[3] <= Dd1[3];
        Dd2[4] <= Dd1[4];
        Dd2[5] <= Dd1[5];
        Dd2[6] <= Dd1[6];
        Dd2[7] <= 1'b0;    
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd3 <= 8'b0;
    end else if (!lrstn) begin
        Dd3 <= 8'b0;
    end else begin
        if(d_up0)begin
            Dd3 <= Dd2;
        end else begin
            Dd3 <= Dd3;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd4 <= 8'b0;
    end else if (!lrstn) begin
        Dd4 <= 8'b0;
    end else begin
        if(d_up1)begin
            Dd4 <= Dd3;
        end else begin
            Dd4[0] <= Dd4[2];
            Dd4[1] <= Dd4[3];
            Dd4[2] <= Dd4[4];
            Dd4[3] <= Dd4[5];
            Dd4[4] <= Dd4[6];
            Dd4[5] <= Dd4[7];
            Dd4[6] <= 1'b0;
            Dd4[7] <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 0;
    end else if (!lrstn) begin
        Qq_p <= 0;
    end else begin
        Qq_p <= Dd4[1];
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 0;
    end else if (!lrstn) begin
        Qq_n <= 0;
    end else begin
        Qq_n <= Dd4[0];
    end
end

assign Q = FCLK ? Qq_n : Qq_p;
//synthesis translate_on

endmodule // OVIDEO (7 to 1 serializer)

//OSER8
module OSER8 (Q0, Q1, D0, D1, D2, D3, D4, D5, D6, D7, TX0, TX1, TX2, TX3, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3, D4, D5, D6, D7;
input TX0, TX1, TX2, TX3;
input PCLK, FCLK, RESET;
output  Q0,  Q1;

//synthesis translate_off
reg [7:0] Dd1,Dd2,Dd3;
reg [3:0] Ttx1,Ttx2,Ttx3;
reg rstn_dsel,dcnt0,dcnt1,d_up0,d_up1;
wire d_en0,d_en1;
reg Qq_p,Qq_n,Q_data_p,Q_data_n;
wire grstn,lrstn;

initial begin
    dcnt0 = 1'b0;
    dcnt1 = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 8'b0;
        Ttx1 <= 4'b0;
    end
    else if (!lrstn) begin
        Dd1 <= 8'b0;
        Ttx1 <= 4'b0;
    end
    else begin
        Dd1 <= {D7,D6,D5,D4,D3,D2,D1,D0};
        Ttx1 <= {TX3,TX2,TX1,TX0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel <= 1'b0;        
    end
    else begin
        rstn_dsel <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        dcnt0 <= 1'b0;
        dcnt1 <= 1'b0;
    end else begin
        dcnt0 <= ~dcnt0;
        dcnt1 <= dcnt0 ^ dcnt1;
    end
end

assign d_en0 = dcnt0 & (~dcnt1);
assign d_en1 = (HWL == "true") ? (dcnt0 & (~dcnt1)) : ((~dcnt0) & (~dcnt1));
   
always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        d_up0 <= 1'b0;
    end else begin
        if(d_en0)begin
            d_up0 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge rstn_dsel) 
begin
    if (!rstn_dsel) begin
        d_up1 <= 1'b0;
    end else begin
        if(d_en1)begin
            d_up1 <= 1'b1;
        end else begin
            d_up1 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd2 <= 8'b0;
        Ttx2 <= 4'b0;
    end else if (!lrstn) begin
        Dd2 <= 8'b0;
        Ttx2 <= 4'b0;
    end else begin
        if(d_up0)begin
            Dd2 <= Dd1;
            Ttx2 <= Ttx1;
        end else begin
            Dd2 <= Dd2;
            Ttx2 <= Ttx2;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd3 <= 8'b0;
        Ttx3 <= 4'b0;
    end else if (!lrstn) begin
        Dd3 <= 8'b0;
        Ttx3 <= 4'b0;
    end else begin
        if(d_up1)begin
            Dd3 <= Dd2;
            Ttx3 <= Ttx2;
        end else begin
            Dd3[0] <= Dd3[2];
            Dd3[1] <= Dd3[3];
            Dd3[2] <= Dd3[4];
            Dd3[3] <= Dd3[5];
            Dd3[4] <= Dd3[6];
            Dd3[5] <= Dd3[7];
            Dd3[6] <= 1'b0;
            Dd3[7] <= 1'b0;

            Ttx3[0] <= Ttx3[1];
            Ttx3[1] <= Ttx3[2];
            Ttx3[2] <= Ttx3[3];
            Ttx3[3] <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 1'b0;
        Q_data_p <= 1'b0;
    end else if (!lrstn) begin
        Qq_p <= 1'b0;
        Q_data_p <= 1'b0;
    end else begin
        Qq_p <= Dd3[1];
        Q_data_p <= Q_data_n;
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0;
    end else if (!lrstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0;
    end else begin
        Qq_n <= Dd3[0];
        Q_data_n <= Ttx3[0];
    end
end

assign Q0 = FCLK ? Qq_n : Qq_p;
assign Q1 = (TXCLK_POL == 1'b0) ? Q_data_p : Q_data_n;

//synthesis translate_on

endmodule // OSER8 (8 to 1 serializer)

//OSER8_MEM
module OSER8_MEM (Q0, Q1, D0, D1, D2, D3, D4, D5, D6, D7, TX0, TX1, TX2, TX3, PCLK, FCLK, TCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";    //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3, D4, D5, D6, D7;
input TX0, TX1, TX2, TX3;
input PCLK, FCLK, TCLK, RESET;
output  Q0,  Q1;

//synthesis translate_off
reg [7:0] Dd1,Dd2,Dd3;
reg [3:0] Ttx1,Ttx2,Ttx3;
reg rstn_dsel0,dcnt0,dcnt1,d_up0;
reg rstn_dsel1,hcnt0,hcnt1,d_up1;
wire d_en0,d_en1;
reg Qq_p,Qq_n,Q_data_p,Q_data_n;
wire tclk_sig;
wire grstn,lrstn;

initial begin
    dcnt0 = 1'b0;
    dcnt1 = 1'b0;
    hcnt0 = 1'b0;
    hcnt1 = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;
assign tclk_sig = (TCLK_SOURCE == "DQSW") ? TCLK : ~TCLK;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 8'b0;
        Ttx1 <= 4'b0;
    end
    else if (!lrstn) begin
        Dd1 <= 8'b0;
        Ttx1 <= 4'b0;
    end
    else begin
        Dd1 <= {D7,D6,D5,D4,D3,D2,D1,D0};
        Ttx1 <= {TX3,TX2,TX1,TX0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel0 <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel0 <= 1'b0;        
    end
    else begin
        rstn_dsel0 <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel0) 
begin
    if (!rstn_dsel0) begin
        dcnt0 <= 1'b0;
        dcnt1 <= 1'b0;
    end else begin
        dcnt0 <= ~dcnt0;
        dcnt1 <= dcnt0 ^ dcnt1;
    end
end

assign d_en0 = dcnt0 & (~dcnt1);

always @(posedge FCLK or negedge rstn_dsel0) 
begin
    if (!rstn_dsel0) begin
        d_up0 <= 1'b0;
    end else begin
        if(d_en0)begin
            d_up0 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
        end
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel1 <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel1 <= 1'b0;        
    end
    else begin
        rstn_dsel1 <= 1'b1;
    end
end

always @(posedge tclk_sig or negedge rstn_dsel1) 
begin
    if (!rstn_dsel1) begin
        hcnt0 <= 1'b0;
        hcnt1 <= 1'b0;
    end else begin
        hcnt0 <= ~hcnt0;
        hcnt1 <= hcnt0 ^ hcnt1;
    end
end

assign d_en1 = (HWL == "true") ? (hcnt0 & (~hcnt1)) : ((~hcnt0) & (~hcnt1));

always @(posedge tclk_sig or negedge rstn_dsel1) 
begin
    if (!rstn_dsel1) begin
        d_up1 <= 1'b0;
    end else begin
        if(d_en1)begin
            d_up1 <= 1'b1;
        end else begin
            d_up1 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd2 <= 8'b0;
        Ttx2 <= 4'b0;
    end else if (!lrstn) begin
        Dd2 <= 8'b0;
        Ttx2 <= 4'b0;
    end else begin
        if(d_up0)begin
            Dd2 <= Dd1;
            Ttx2 <= Ttx1;
        end else begin
            Dd2 <= Dd2;
            Ttx2 <= Ttx2;
        end
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin 
    if (!grstn) begin
        Dd3 <= 8'b0;
        Ttx3 <= 4'b0;
    end else if (!lrstn) begin
        Dd3 <= 8'b0;
        Ttx3 <= 4'b0;
    end else begin
        if(d_up1)begin
            Dd3 <= Dd2;
            Ttx3 <= Ttx2;
        end else begin
            Dd3[0] <= Dd3[2];
            Dd3[1] <= Dd3[3];
            Dd3[2] <= Dd3[4];
            Dd3[3] <= Dd3[5];
            Dd3[4] <= Dd3[6];
            Dd3[5] <= Dd3[7];
            Dd3[6] <= 1'b0;
            Dd3[7] <= 1'b0;

            Ttx3[0] <= Ttx3[1];
            Ttx3[1] <= Ttx3[2];
            Ttx3[2] <= Ttx3[3];
            Ttx3[3] <= 1'b0;
        end
    end
end

always @(posedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 1'b0;
        Q_data_p <= 1'b0;
    end else if (!lrstn) begin
        Qq_p <= 1'b0;
        Q_data_p <= 1'b0;
    end else begin
        Qq_p <= Dd3[1];
        Q_data_p <= Q_data_n;
    end
end

always @(negedge tclk_sig or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0;
    end else if (!lrstn) begin
        Qq_n <= 1'b0;
        Q_data_n <= 1'b0;
    end else begin
        Qq_n <= Dd3[0];
        Q_data_n <= Ttx3[0];
    end
end

assign Q0 = tclk_sig ? Qq_n : Qq_p;
assign Q1 = (TXCLK_POL == 1'b0) ? Q_data_p : Q_data_n;

//synthesis translate_on

endmodule // OSER8_MEM (8 to 1 serializer with memory)

//OSER10
module OSER10 (Q, D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, PCLK, FCLK, RESET);

parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D0, D1, D2, D3, D4, D5, D6, D7, D8, D9;
input PCLK, FCLK, RESET;
output Q;

//synthesis translate_off
reg [9:0] Dd1,Dd2,Dd3;
reg rstn_dsel,dcnt0,dcnt1,dcnt2,d_up0,d_up1;
wire d_en,dcnt_reset;
reg Qq_p,Qq_n;
wire grstn,lrstn;

initial begin
    dcnt0 = 1'b0;
    dcnt1 = 1'b0;
    dcnt2 = 1'b0;
end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

always @(posedge PCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd1 <= 0;
    end else if (!lrstn) begin
        Dd1 <= 0;
    end else begin
        Dd1 <= {D9,D8,D7,D6,D5,D4,D3,D2,D1,D0};
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if(!grstn) begin
        rstn_dsel <= 1'b0;
    end
    else if (!lrstn) begin
        rstn_dsel <= 1'b0;        
    end
    else begin
        rstn_dsel <= 1'b1;
    end
end

always @(posedge FCLK or negedge rstn_dsel)
begin
    if (!rstn_dsel) begin
        dcnt0 <= 1'b0;
        dcnt1 <= 1'b0;
        dcnt2 <= 1'b0;
    end else begin
        dcnt0 <= ~(dcnt0 | dcnt_reset);
        dcnt1 <= (dcnt0 ^ dcnt1) & (~dcnt_reset);
        dcnt2 <= (dcnt2 ^ (dcnt0 & dcnt1)) & (~dcnt_reset);
    end
end

assign dcnt_reset = (~dcnt0) & (~dcnt1) & dcnt2;
assign d_en = (~dcnt0) & dcnt1;

always @(posedge FCLK or negedge rstn_dsel)
begin
    if (!rstn_dsel) begin
        d_up0 <= 1'b0;
        d_up1 <= 1'b0;
    end else begin
        if(d_en)begin
            d_up0 <= 1'b1;
            d_up1 <= 1'b1;
        end else begin
            d_up0 <= 1'b0;
            d_up1 <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd2 <= 10'b0;
    end else if (!lrstn) begin
        Dd2 <= 10'b0;
    end else begin
        if(d_up0)begin
            Dd2 <= Dd1;
        end else begin
            Dd2 <= Dd2;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Dd3 <= 10'b0;
    end else if (!lrstn) begin
        Dd3 <= 10'b0;
    end else begin
        if(d_up1)begin
            Dd3 <= Dd2;
        end else begin
            Dd3[0] <= Dd3[2];
            Dd3[1] <= Dd3[3];
            Dd3[2] <= Dd3[4];
            Dd3[3] <= Dd3[5];
            Dd3[4] <= Dd3[6];
            Dd3[5] <= Dd3[7];
            Dd3[6] <= Dd3[8];
            Dd3[7] <= Dd3[9];
            Dd3[8] <= 1'b0;
            Dd3[9] <= 1'b0;
        end
    end
end

always @(posedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_p <= 1'b0;
    end else if (!lrstn) begin
        Qq_p <= 1'b0;
    end else begin
        Qq_p <= Dd3[1];
    end
end

always @(negedge FCLK or negedge grstn or negedge lrstn)
begin
    if (!grstn) begin
        Qq_n <= 1'b0;
    end else if (!lrstn) begin
        Qq_n <= 1'b0;
    end else begin
        Qq_n <= Dd3[0];
    end
end

assign Q = FCLK ? Qq_n : Qq_p;

//synthesis translate_on
endmodule // OSER10 (10 to 1 serializer)

//Input IO logic
module IODELAY (DO, DF, DI, SDTAP, VALUE, SETN);

parameter C_STATIC_DLY = 0; //integer, 0~127

input DI;
input  SDTAP;
input  SETN;
input  VALUE;
output DF;
output DO;

reg [6:0] delay_data;
realtime delay_time;
wire [127:0] delay_in;
reg pre_value;

always @(SDTAP or VALUE) begin
    if (!SDTAP) begin
        delay_data <= C_STATIC_DLY;
    end else begin
        if(pre_value == 1'b1 && VALUE == 1'b0) begin
  	        if (SDTAP) begin
   	            if (SETN && (delay_data != 7'd0))
      		        delay_data <= delay_data - 1;
   	            else if ((!SETN) && (delay_data != 7'd127))
      		        delay_data <= delay_data + 1;
  	        end
        end
    end
end

always @(VALUE) begin
    pre_value <= VALUE;
end

assign DF = (SETN && (delay_data == 7'd0)) || ((!SETN) && (delay_data == 7'd127));

assign #0.025 delay_in[0] =  DI;
generate 
   genvar i;
    for(i=1;i<128;i=i+1) begin: gen_delay
      assign #0.025 delay_in[i] = delay_in[i-1];
    end
endgenerate

assign DO = (delay_data == 0) ? DI : delay_in[delay_data-1];

endmodule // IODELAY (input delay in IOB)


module IEM (LAG, LEAD, D, CLK, MCLK, RESET);

parameter WINSIZE = "SMALL"; //"SMALL"; "MIDSMALL"; "MIDLARGE"; "LARGE"
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"

input D, CLK, RESET, MCLK;
output LAG, LEAD;

//synthesis translate_off
reg Dd1;
reg Dd2;
reg Dd3;
reg Dd4;
reg Dd5;
reg Dd6;
reg Dd7;
reg Dd8;
reg Dd_lead;
reg Dd_lag;
reg Dd_lead0;
reg Dd_mid0;
reg Dd_lag0;
reg Dd_lead1;
reg Dd_mid1;
reg Dd_lag1;
reg LEAD_reg;
reg LAG_reg;
wire grstn;
wire lrstn;

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;
assign lrstn = LSREN == "true" ? (~RESET) : 1'b1;

always @(D) begin
	Dd1 = #0.05 D;
	Dd2 = #0.05 Dd1;
	Dd3 = #0.05 Dd2;
	Dd4 = #0.05 Dd3;
	Dd5 = #0.05 Dd4;
	Dd6 = #0.05 Dd5;
	Dd7 = #0.05 Dd6;
	Dd8 = #0.05 Dd7;
end

always @(*) begin
	 case (WINSIZE)
	    "SMALL"   :  begin Dd_lead = Dd3; Dd_lag = Dd5; end
	    "MIDSMALL":  begin Dd_lead = Dd2; Dd_lag = Dd6; end
	    "MIDLARGE":  begin Dd_lead = Dd1; Dd_lag = Dd7; end
	    "LARGE"   :  begin Dd_lead = D;    Dd_lag = Dd8; end
	    default:  $display ("Warning! Invalid IEM window size setting");
	 endcase
end

assign Dd_mid = Dd3;

always @(posedge CLK or grstn or lrstn)
   if (!grstn) begin
      Dd_lead0 <= 0;
      Dd_mid0   <= 0;
      Dd_lag0   <= 0;
   end
   else if (!lrstn) begin
      Dd_lead0 <= 0;
      Dd_mid0   <= 0;
      Dd_lag0   <= 0;
   end
   else begin
      Dd_lead0 <= Dd_lead;
      Dd_mid0   <= Dd_mid;
      Dd_lag0   <= Dd_lag;
   end

always @(negedge CLK or grstn or lrstn)
   if (!grstn) begin
      Dd_lead1 <= 0;
      Dd_mid1   <= 0;
      Dd_lag1   <= 0;
   end
   else if (!lrstn) begin
      Dd_lead1 <= 0;
      Dd_mid1   <= 0;
      Dd_lag1   <= 0;
   end
   else begin
      Dd_lead1 <= Dd_lead;
      Dd_mid1   <= Dd_mid;
      Dd_lag1   <= Dd_lag;
   end

assign lead0 = Dd_lead0 ^ Dd_mid0;
assign lead1 = Dd_lead1 ^ Dd_mid1;
assign lag0   = Dd_mid0   ^ Dd_lag0;
assign lag   = Dd_mid1   ^ Dd_lag1;

assign lead_sel = lead0 | lead1;
assign lag_sel = lag0 | lag;

always @(lead_sel or MCLK)
   if (lead_sel)
      LEAD_reg <= 1'b1;
   else if (MCLK)
      LEAD_reg <= 1'b0;

always @(lag_sel or MCLK)
   if (lag_sel)
      LAG_reg <= 1'b1;
   else if (MCLK)
      LAG_reg <= 1'b0;

assign LEAD = LEAD_reg;
assign LAG = LAG_reg;
//synthesis translate_on

endmodule // IEM 

// RAM16S1
module RAM16S1 (DO, DI, AD, WRE, CLK);

input CLK;
input WRE;

input [3:0] AD;
input DI;
output DO;

parameter INIT_0 = 16'h0000;

reg [15:0] mem = INIT_0;

assign DO = mem [AD];

always @(posedge CLK) begin
	if (WRE) begin
	    mem [AD] <= DI;
	end
end

endmodule // RAM16S1: single-port S-SRAM(16X1)

//RAM16S2
module RAM16S2 (DO, DI, AD, WRE, CLK);

input CLK;
input WRE;

input  [3:0] AD;
input  [1:0] DI;
output [1:0] DO;

parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;

reg [15:0] mem0;
reg [15:0] mem1;

initial begin
	mem0 = INIT_0;
	mem1 = INIT_1;
end

assign DO[0] = mem0[AD];
assign DO[1] = mem1[AD];

always @(posedge CLK) begin
	if (WRE) begin
		mem0[AD] <= DI[0];
		mem1[AD] <= DI[1];
	end
end

endmodule // RAM16S2: single-port S-SRAM(16X2)

//RAM16S4
module RAM16S4 (DO, DI, AD, WRE, CLK);

input CLK;
input WRE;

input [3:0] AD;
input [3:0] DI;
output [3:0] DO;

parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;
parameter INIT_2 = 16'h0000;
parameter INIT_3 = 16'h0000;

reg [15:0] mem0; 
reg [15:0] mem1;
reg [15:0] mem2;
reg [15:0] mem3;

initial begin
	mem0 = INIT_0;
	mem1 = INIT_1;
	mem2 = INIT_2;
	mem3 = INIT_3;	
end

assign	DO[0] = mem0[AD];
assign	DO[1] = mem1[AD];
assign	DO[2] = mem2[AD];
assign	DO[3] = mem3[AD];

always @(posedge CLK) begin
	if (WRE) begin
		mem0[AD] <= DI[0];
		mem1[AD] <= DI[1];
		mem2[AD] <= DI[2];
		mem3[AD] <= DI[3];
	end
end

endmodule // RAM16S4: single-port S-SRAM(16X4)


module RAM16SDP1 (DO, DI, WAD, RAD, WRE, CLK);

input CLK;
input WRE;
input [3:0] WAD;
input DI;
input [3:0] RAD;
output DO;

parameter INIT_0 = 16'h0000;

reg [15:0] mem;

initial mem = INIT_0;

assign DO = mem[RAD];

always @(posedge CLK) begin
	if (WRE)
		mem[WAD] <= DI;
end

endmodule // RAM16SDP1: Semi dual-port S-SRAM(16X1)


module RAM16SDP2 (DO, DI, WAD, RAD, WRE, CLK);

input CLK;
input WRE;
input [3:0] WAD;
input [1:0] DI;
input [3:0] RAD;
output [1:0] DO;

parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;

reg [15:0] mem0;
reg [15:0] mem1;

initial begin
	mem0 = INIT_0;
	mem1 = INIT_1;
end

assign DO[0] = mem0[RAD];
assign DO[1] = mem1[RAD];

always @(posedge CLK) begin
	if (WRE) begin
		mem0[WAD] <= DI[0];
		mem1[WAD] <= DI[1];
	end
end

endmodule // RAM16SDP2: Semi dual-port S-SRAM(16X2)


module RAM16SDP4 (DO, DI, WAD, RAD, WRE, CLK);

input CLK;
input WRE;

input [3:0] WAD;
input [3:0] DI;
input [3:0] RAD;
output [3:0] DO;

parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;
parameter INIT_2 = 16'h0000;
parameter INIT_3 = 16'h0000;

reg [15:0] mem0;
reg [15:0] mem1;
reg [15:0] mem2;
reg [15:0] mem3;

initial begin
	mem0 = INIT_0;
	mem1 = INIT_1;
	mem2 = INIT_2;
	mem3 = INIT_3;
end

assign DO[0] = mem0[RAD];
assign DO[1] = mem1[RAD];
assign DO[2] = mem2[RAD];
assign DO[3] = mem3[RAD];

always @(posedge CLK) begin
	if (WRE) begin
		mem0[WAD] <= DI[0];
		mem1[WAD] <= DI[1];
		mem2[WAD] <= DI[2];
		mem3[WAD] <= DI[3];
	end
end

endmodule // RAM16SDP4: Semi dual-port S-SRAM(16X4)


module ROM16 (DO, AD);

parameter INIT_0 = 16'h0000;

input [3:0] AD;
output DO;

reg DO;
reg [15:0] mem;

initial mem = INIT_0;

always @(AD) begin
	DO <= mem [AD];
end

endmodule // ROM16: signal-port shadow ROM(16 bit)


//Block SRAM
module SP (DO, DI, BLKSEL, AD, WRE, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] AD;
input [31:0] DI;
input [2:0] BLKSEL;
output [31:0] DO;

reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg bs_en;
wire pce;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg mc;
reg [13:0] addr;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(dwidth)
		1: awidth = 14;
		2: awidth = 13;
		4: awidth = 12;
		8: awidth = 11;
		16: awidth = 10;
		32: awidth = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", dwidth);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg : pl_reg;

assign pce = CE && bs_en;   
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(awidth,AD,WRE,mc)begin
	if(awidth==14)begin
		addr[13:0] = AD[13:0];
		mem_t[0] =ram_MEM[addr];
	end
	else if(awidth==13)begin
		addr[13:0] = {AD[13:1],1'b0};
		mem_t[1:0] ={ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==12)begin
		addr[13:0] = {AD[13:2],2'b00};
		mem_t[3:0] ={ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==11)begin
		addr[13:0] = {AD[13:3],3'b000};
		mem_t[7:0] ={ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==10)begin
		addr[13:0] = {AD[13:4],4'b0000};
		mem_t[15:0] ={ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[13:0] = {AD[13:5],5'b00000};
		mem_t[31:0]={ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

//write and read
always @(posedge CLK) begin
	if (pce) begin
    	if(WRE) begin
		    if(dwidth==1)
			    ram_MEM[addr] <= DI[0];
			else if(dwidth==2)
				{ram_MEM[addr+1],ram_MEM[addr]}<=DI[BIT_WIDTH-1:0];
			else if(dwidth==4)
				{ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]}<=DI[BIT_WIDTH-1:0];
			else if(dwidth==8)
				{ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]}<=DI[7:0];

			else if(dwidth==16) begin
				if(AD[0] == 1'b1)
					{ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]} <= DI[7:0];
				if(AD[1] == 1'b1)
					{ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8]} <= DI[15:8];
			end
			else if(dwidth==32) begin
				if(AD[0] == 1'b1)
					{ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]}<=DI[7:0];
				if(AD[1] == 1'b1)
					{ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8]}<=DI[15:8];
				if(AD[2] == 1'b1)
					{ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16]} <= DI[23:16];	
				if(AD[3] == 1'b1)
					{ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24]} <= DI[31:24];	
			end
		    mc <= ~mc;
        end
	end
end	

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		bp_reg_async <= 0;
	end else begin
		if (pce) begin
    	    if(WRE) begin	
				if (WRITE_MODE == 2'b01) begin
					bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
                    if(dwidth <= 8) begin
					    bp_reg_async[BIT_WIDTH-1:0] <= DI[BIT_WIDTH-1:0];
                    end else if(dwidth==16) begin
						if(AD[0] == 1'b1)
							bp_reg_async[7:0] <= DI[7:0];
						if(AD[1] == 1'b1)
                            bp_reg_async[15:8] <= DI[15:8];
				    end else if(dwidth==32) begin
						if(AD[0] == 1'b1)
                            bp_reg_async[7:0]  <= DI[7:0];
						if(AD[1] == 1'b1)
                            bp_reg_async[15:8] <= DI[15:8];
						if(AD[2] == 1'b1)
                            bp_reg_async[23:16] <= DI[23:16];	
						if(AD[3] == 1'b1)
                            bp_reg_async[31:24] <= DI[31:24];
			        end
				end

				if (WRITE_MODE == 2'b10) begin
					bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
				end
				
			end else begin // WRE==0, read
				bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
			end
		end
	end
end	

always @(posedge CLK) begin
	if (RESET) begin
		bp_reg_sync <= 0;
	end else begin
		if (pce) begin
    	    if(WRE) begin	
				if (WRITE_MODE == 2'b01) begin
					bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
                    if(dwidth <= 8) begin
					    bp_reg_sync[BIT_WIDTH-1:0] <= DI[BIT_WIDTH-1:0];                       
                    end else if(dwidth==16) begin
						if(AD[0] == 1'b1)
							bp_reg_sync[7:0] <= DI[7:0];
						if(AD[1] == 1'b1)
                            bp_reg_sync[15:8] <= DI[15:8];                        
				    end else if(dwidth==32) begin
						if(AD[0] == 1'b1)
                            bp_reg_sync[7:0]  <= DI[7:0];
						if(AD[1] == 1'b1)
                            bp_reg_sync[15:8] <= DI[15:8];
						if(AD[2] == 1'b1)
                            bp_reg_sync[23:16] <= DI[23:16];	
						if(AD[3] == 1'b1)
                            bp_reg_sync[31:24] <= DI[31:24];
			        end
				end

				if (WRITE_MODE == 2'b10) begin
					bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
				end
				
			end else begin // WRE==0, read
				bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
			end
		end
	end
end	

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
	end
end	

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
	end
end	

endmodule  // SP: single port 16k Block SRAM

module SPX9 (DO, DI, BLKSEL, AD, WRE, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH = 36; // 9, 18, 36
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: write enabled
input [2:0] BLKSEL;
input [13:0] AD;
input [35:0] DI;
output [35:0] DO;

reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH-1:0] mem_t;
reg [14:0] addr;
reg mc,bs_en;
wire pce;

integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(dwidth)
		9: awidth = 11;
		18: awidth = 10;
		36: awidth = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", dwidth);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg : pl_reg;

assign pce = CE && bs_en;   
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(AD,awidth,WRE,mc)begin
	if(awidth==11)begin
		addr[14:0] = AD[13:3]*dwidth;
		mem_t[8:0] = {ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==10)begin
		addr[14:0] = AD[13:4]*dwidth;
		mem_t[17:0] = {ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};	
	end
	else if(awidth==9)begin
		addr[14:0] = AD[13:5]*dwidth;
		mem_t[35:0]={ram_MEM[addr+35],ram_MEM[addr+34],ram_MEM[addr+33],ram_MEM[addr+32],ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};	

	end
end

// write and read
always @(posedge CLK) begin
	if (pce) begin
		if (WRE) begin		
			if (dwidth == 9)
				{ram_MEM [addr+8],ram_MEM [addr+7], ram_MEM [addr+6], ram_MEM [addr+5], ram_MEM [addr+4], ram_MEM [addr+3], ram_MEM [addr+2], ram_MEM [addr+1], ram_MEM [addr]} <= DI[8:0];
			else if(dwidth == 18) begin
				if(AD[0] == 1'b1)
					{ram_MEM [addr+8],ram_MEM [addr+7], ram_MEM [addr+6], ram_MEM [addr+5], ram_MEM [addr+4], ram_MEM [addr+3], ram_MEM [addr+2], ram_MEM [addr+1], ram_MEM [addr]} <= DI[8:0];
				if(AD[1] == 1'b1)
					{ram_MEM [addr+17],ram_MEM [addr+16], ram_MEM [addr+15], ram_MEM [addr+14], ram_MEM [addr+13], ram_MEM [addr+12], ram_MEM [addr+11], ram_MEM [addr+10], ram_MEM [addr+9]} <= DI[17:9];					
			end
			else if(dwidth == 36) begin
				if(AD[0] == 1'b1)
					{ram_MEM [addr+8],ram_MEM [addr+7], ram_MEM [addr+6], ram_MEM [addr+5], ram_MEM [addr+4], ram_MEM [addr+3], ram_MEM [addr+2], ram_MEM [addr+1], ram_MEM [addr]} <= DI[8:0];
				if(AD[1] == 1'b1)
					{ram_MEM [addr+17],ram_MEM [addr+16], ram_MEM [addr+15], ram_MEM [addr+14], ram_MEM [addr+13], ram_MEM [addr+12], ram_MEM [addr+11], ram_MEM [addr+10], ram_MEM [addr+9]} <= DI[17:9];
				if(AD[2] == 1'b1)
					{ram_MEM [addr+26],ram_MEM [addr+25], ram_MEM [addr+24], ram_MEM [addr+23], ram_MEM [addr+22], ram_MEM [addr+21], ram_MEM [addr+20], ram_MEM [addr+19], ram_MEM [addr+18]} <= DI[26:18];
				if(AD[3] == 1'b1)
					{ram_MEM [addr+35],ram_MEM [addr+34], ram_MEM [addr+33], ram_MEM [addr+32], ram_MEM [addr+31], ram_MEM [addr+30], ram_MEM [addr+29], ram_MEM [addr+28], ram_MEM [addr+27]} <= DI[35:27];
			end
			mc <= ~mc;
		end
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		bp_reg_async <= 0;
	end else begin
		if (pce) begin
			if (WRE) begin
				if (WRITE_MODE == 2'b01) begin
					bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
                    if(dwidth == 9) begin
					    bp_reg_async[BIT_WIDTH-1:0] <= DI[BIT_WIDTH-1:0];                       
                    end else if(dwidth==18) begin
						if(AD[0] == 1'b1)
							bp_reg_async[8:0] <= DI[8:0];
						if(AD[1] == 1'b1)
                            bp_reg_async[17:9] <= DI[17:9];                        
				    end else if(dwidth==36) begin
						if(AD[0] == 1'b1)
                            bp_reg_async[8:0]  <= DI[8:0];
						if(AD[1] == 1'b1)
                            bp_reg_async[17:9] <= DI[17:9];
						if(AD[2] == 1'b1)
                            bp_reg_async[26:18] <= DI[26:18];	
						if(AD[3] == 1'b1)
                            bp_reg_async[35:27] <= DI[35:27];
			        end

				end
				if (WRITE_MODE == 2'b10) begin
					bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
				end				
	
			end else begin // WRE==0, read
				bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
			end
		end
	end
end

always @(posedge CLK) begin
	if (RESET) begin
		bp_reg_sync <= 0;
	end else begin
		if (pce) begin
			if (WRE) begin
				if (WRITE_MODE == 2'b01) begin
					bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
                    if(dwidth == 9) begin
					    bp_reg_sync[BIT_WIDTH-1:0] <= DI[BIT_WIDTH-1:0];                       
                    end else if(dwidth==18) begin
						if(AD[0] == 1'b1)
							bp_reg_sync[8:0] <= DI[8:0];
						if(AD[1] == 1'b1)
                            bp_reg_sync[17:9] <= DI[17:9];                        
				    end else if(dwidth==36) begin
						if(AD[0] == 1'b1)
                            bp_reg_sync[8:0]  <= DI[8:0];
						if(AD[1] == 1'b1)
                            bp_reg_sync[17:9] <= DI[17:9];
						if(AD[2] == 1'b1)
                            bp_reg_sync[26:18] <= DI[26:18];	
						if(AD[3] == 1'b1)
                            bp_reg_sync[35:27] <= DI[35:27];
			        end

				end
				if (WRITE_MODE == 2'b10) begin
					bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
				end				
	
			end else begin // WRE==0, read
				bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
			end
		end
	end
end


always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
	end
end

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
	end
end

endmodule // SPX9: single port 18k Block SRAM


module SDP (DO, DI, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [31:0] DI;
input [2:0] BLKSEL;
output [31:0] DO;

reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [16383:0] ram_MEM ={INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [13:0] addr_a, addr_b;
reg mc,bs_en;
wire pcea;
wire pceb;

integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		1: bit_width_a0 = 14;
		2: bit_width_a0 = 13;
		4: bit_width_a0 = 12;
		8: bit_width_a0 = 11;
		16: bit_width_a0 = 10;
		32: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		1: bit_width_a1 = 14;
		2: bit_width_a1 = 13;
		4: bit_width_a1 = 12;
		8: bit_width_a1 = 11;
		16: bit_width_a1 = 10;
		32: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_en;   
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,WREA,WREB,mc)begin
	if(bit_width_a0==14)begin
		addr_a[13:0] = ADA[13:0];
		mem_a[0] = ram_MEM[addr_a];
	end
	else if(bit_width_a0==13)begin
		addr_a[13:0] = {ADA[13:1],1'b0};
		mem_a[1:0] = {ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==12)begin
		addr_a[13:0] = {ADA[13:2],2'b00};
		mem_a[3:0] = {ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==11)begin
		addr_a[13:0] = {ADA[13:3],3'b000};
		mem_a[7:0] = {ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[13:0] = {ADA[13:4],4'b0000};
		mem_a[15:0] = {ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[13:0] = {ADA[13:5],5'b00000};
		mem_a[31:0] = {ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	if(bit_width_a1==14)begin
		addr_b[13:0] = ADB[13:0];
		mem_b[0]=ram_MEM[addr_b];
	end
	else if(bit_width_a1==13)begin
		addr_b[13:0] = {ADB[13:1],1'b0};
		mem_b[1:0]={ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==12)begin
		addr_b[13:0] = {ADB[13:2],2'b00};
		mem_b[3:0]={ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==11)begin
		addr_b[13:0] = {ADB[13:3],3'b000};
		mem_b[7:0]={ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[13:0] = {ADB[13:4],4'b0000};
		mem_b[15:0]={ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
	else if(bit_width_a1==9)begin
		addr_b[13:0] = {ADB[13:5],5'b00000};
		mem_b[31:0]={ ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
end

always @(posedge CLKA) begin
	if (pcea) begin
		if (WREA) begin
			if(bit_width_d0==1)
				ram_MEM[addr_a] <= DI[0];
			else if(bit_width_d0==2)
				{ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==4)
				{ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==8)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==16) begin
				if(ADA[0] == 1'b1)
					{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
				if(ADA[1] ==1'b1)
					{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
			end
			else if(bit_width_d0==32) begin
				if(ADA[0] == 1'b1)
					{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
				if(ADA[1] == 1'b1)
					{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
				if(ADA[2] == 1'b1)
					{ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16]} <=DI[23:16];
				if(ADA[3] == 1'b1)
					{ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24]} <=DI[31:24];
			end
			mc <= ~mc;
		end
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb && !WREB) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb && !WREB) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // SDP: Semi dual port 16k Block SRAM

module SDPX9 (DO, DI, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [35:0] DI;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [14:0] addr_a, addr_b;
reg mc,bs_en;
wire pcea,pceb;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		9: bit_width_a0 = 11;
		18: bit_width_a0 = 10;
		36: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		9: bit_width_a1 = 11;
		18: bit_width_a1 = 10;
		36: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_en;   
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,WREA,WREB,mc)begin
	if(bit_width_a0==11)begin
		addr_a[14:0] = ADA[13:3]*bit_width_d0;
		mem_a[8:0] = {ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[14:0] = ADA[13:4]*bit_width_d0;
		mem_a[17:0] = {ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[14:0] = ADA[13:5]*bit_width_d0;
		mem_a[35:0] = {ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end

	if(bit_width_a1==11)begin
		addr_b[14:0] = ADB[13:3]*bit_width_d1;
		mem_b[8:0] = {ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[14:0] = ADB[13:4]*bit_width_d1;
		mem_b[17:0] = {ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==9)begin
		addr_b[14:0] = ADB[13:5]*bit_width_d1;
		mem_b[35:0] = {ram_MEM[addr_b+35],ram_MEM[addr_b+34],ram_MEM[addr_b+33],ram_MEM[addr_b+32],ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @(posedge CLKA) begin		
	if (pcea) begin
		if (WREA) begin
			if(bit_width_d0 == 9) begin
				{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
			end else if(bit_width_d0 == 18) begin
				if(ADA[0] == 1'b1)
					{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
				if(ADA[1] == 1'b1)
					{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
			end
			else if(bit_width_d0 == 36) begin
				if(ADA[0] == 1'b1)
					{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
				if(ADA[1] == 1'b1)
					{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
				if(ADA[2] == 1'b1)
					{ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18]} <= DI[26:18];
				if(ADA[3] == 1'b1)
					{ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27]} <= DI[35:27];
			end
			mc <= ~mc;
		end
	end
end	

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		bp_reg_async <=0;
		pl_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb && !WREB) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		bp_reg_sync <=0;
		pl_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb && !WREB) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // SDPX9: Semi dual port 18k Block SRAM

module DP (DOA, DOB, DIA, DIB, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB);

parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 16; // 1, 2, 4, 8, 16
parameter BIT_WIDTH_1 = 16; // 1, 2, 4, 8, 16
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [15:0] DIA, DIB;
output [15:0] DOA, DOB;

reg [15:0] bpa_reg,bpa_reg_async,bpa_reg_sync;
reg [15:0] pla_reg,pla_reg_async,pla_reg_sync;
reg [15:0] bpb_reg, plb_reg,bpb_reg_async,bpb_reg_sync,plb_reg_async,plb_reg_sync;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg mc,bs_en;
wire pcea,pceb;
reg [13:0] addr_a, addr_b;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bpa_reg = 0;
    pla_reg = 0;
    bpb_reg = 0;
    plb_reg = 0;
    bpa_reg_async = 0;
    bpa_reg_sync = 0;
    pla_reg_async = 0;
    pla_reg_sync = 0;
    bpb_reg_async = 0;
    bpb_reg_sync = 0;
    plb_reg_async = 0;
    plb_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		1: bit_width_a0 = 14;
		2: bit_width_a0 = 13;
		4: bit_width_a0 = 12;
		8: bit_width_a0 = 11;
		16: bit_width_a0 = 10;
	default: begin
	//	$display ("%d: Unsupported data width\n", bit_width_d0);
	//	$finish;
	end
	endcase
	case(bit_width_d1)
		1: bit_width_a1 = 14;
		2: bit_width_a1 = 13;
		4: bit_width_a1 = 12;
		8: bit_width_a1 = 11;
		16: bit_width_a1 = 10;
	default: begin
	//	$display ("%d: Unsupported data width\n", bit_width_d1);
	//	$finish;
	end
	endcase
end

assign DOA = (READ_MODE0 == 1'b0)? bpa_reg : pla_reg;
assign DOB = (READ_MODE1 == 1'b0)? bpb_reg : plb_reg;

assign pcea = CEA && bs_en;   
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,WREA,WREB,mc)begin
	if(bit_width_a0==14)begin
		addr_a[13:0] = ADA[13:0];
		mem_a[0]=ram_MEM[addr_a];
	end
	else if(bit_width_a0==13)begin
		addr_a[13:0] = {ADA[13:1],1'b0};
		mem_a[1:0]={ ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==12)begin
		addr_a[13:0] = {ADA[13:2],2'b00};
		mem_a[3:0]={ ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==11)begin
		addr_a[13:0] = {ADA[13:3],3'b000};
		mem_a[7:0]={ ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[13:0] = {ADA[13:4],4'b0000};
		mem_a[15:0]={ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	if(bit_width_a1==14)begin
		addr_b[13:0] = ADB[13:0];
		mem_b[0]=ram_MEM[addr_b];
	end
	else if(bit_width_a1==13)begin
		addr_b[13:0] = {ADB[13:1],1'b0};
		mem_b[1:0]={ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==12)begin
		addr_b[13:0] = {ADB[13:2],2'b00};
		mem_b[3:0]={ ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==11)begin
		addr_b[13:0] = {ADB[13:3],3'b000};
		mem_b[7:0]={ ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[13:0] = {ADB[13:4],4'b0000};
		mem_b[15:0]={ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @ (bpa_reg_async or bpa_reg_sync or pla_reg_async or pla_reg_sync or bpb_reg_async or bpb_reg_sync or plb_reg_async or plb_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bpa_reg <= bpa_reg_async;
        pla_reg <= pla_reg_async;
        bpb_reg <= bpb_reg_async;
        plb_reg <= plb_reg_async;
    end
    else begin
        bpa_reg <= bpa_reg_sync;
        pla_reg <= pla_reg_sync;
        bpb_reg <= bpb_reg_sync;
        plb_reg <= plb_reg_sync;
    end
end

always @(posedge CLKA) begin
	if (pcea) begin
		if (WREA) begin
			if(bit_width_d0==1)
				ram_MEM[addr_a] <= DIA[0];
			else if(bit_width_d0==2)
				{ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==4)
				{ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==8)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==16) begin
				if(ADA[0] == 1'b1)
					{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]} <= DIA[7:0];
				if(ADA[1] == 1'b1)
					{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]} <= DIA[15:8];
			end
			mc <= ~mc;
		end 
	end
end	

always @(posedge CLKA or posedge RESETA) begin
	if (RESETA) begin
		pla_reg_async <= 0;
		bpa_reg_async <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_async <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if(bit_width_d0<=8)
					    bpa_reg_async[BIT_WIDTH_0-1:0] <= DIA[BIT_WIDTH_0-1:0];
				    else if(bit_width_d0==16) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_async[7:0] <= DIA[7:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_async[15:8]  <= DIA[15:8];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end
				
			end 
			else begin // WREA==0, read
				bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end
		end
	end
end	

always @(posedge CLKA) begin
	if (RESETA) begin
		pla_reg_sync <= 0;
		bpa_reg_sync <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_sync <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if(bit_width_d0<=8)
					    bpa_reg_sync[BIT_WIDTH_0-1:0] <= DIA[BIT_WIDTH_0-1:0];
				    else if(bit_width_d0==16) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_sync[7:0] <= DIA[7:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_sync[15:8]  <= DIA[15:8];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end
				
			end 
			else begin // WREA==0, read
				bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end
		end
	end
end

always @(posedge CLKB) begin
	if (pceb) begin
		if (WREB) begin
			if(bit_width_d1==1)
				ram_MEM[addr_b] <= DIB[0];
			else if(bit_width_d1==2)
				{ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==4)
				{ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==8)
				{ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==16) begin
				if(ADB[0] == 1'b1)
					{ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]} <= DIB[7:0];
				if(ADB[1] == 1'b1)
					{ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8]} <= DIB[15:8];
			end
			mc <= ~mc;
		end 
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		plb_reg_async <= 0;
		bpb_reg_async <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_async <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if(bit_width_d1<=8)
					    bpb_reg_async[BIT_WIDTH_1-1:0] <= DIB[BIT_WIDTH_1-1:0];
				    else if(bit_width_d1==16) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_async[7:0] <= DIB[7:0];
					    if(ADB[1] == 1'b1)
						    bpb_reg_async[15:8]  <= DIB[15:8];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end	

always @(posedge CLKB) begin
	if (RESETB) begin
		plb_reg_sync <= 0;
		bpb_reg_sync <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_sync <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if(bit_width_d1<=8)
					    bpb_reg_sync[BIT_WIDTH_1-1:0] <= DIB[BIT_WIDTH_1-1:0];
				    else if(bit_width_d1==16) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_sync[7:0] <= DIB[7:0];
					    if(ADB[1] == 1'b1)
						    bpb_reg_sync[15:8]  <= DIB[15:8];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

endmodule // DP: true dual port 16k Block SRAM

module DPX9 (DOA, DOB, DIA, DIB, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB);

parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 18; // 9, 18
parameter BIT_WIDTH_1 = 18; // 9, 18
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [17:0] DIA, DIB;
input [2:0] BLKSEL;
output [17:0] DOA, DOB;
reg [17:0] bpa_reg, bpb_reg,bpa_reg_async, bpb_reg_async,bpa_reg_sync, bpb_reg_sync;
reg [17:0] pla_reg, plb_reg,pla_reg_async, plb_reg_async,pla_reg_sync, plb_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg mc, bs_en;
wire pcea,pceb;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH
reg [14:0] addr_a, addr_b;

initial begin
    bpa_reg = 0;
    bpb_reg = 0;
    pla_reg = 0;
    plb_reg = 0;
    bpa_reg_async = 0;
    bpa_reg_sync = 0;
    pla_reg_async = 0;
    pla_reg_sync = 0;
    bpb_reg_async = 0;
    bpb_reg_sync = 0;
    plb_reg_async = 0;
    plb_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		9: bit_width_a0 = 11;
		18: bit_width_a0 = 10;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		9: bit_width_a1 = 11;
		18: bit_width_a1 = 10;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DOA = (READ_MODE0 == 1'b0)? bpa_reg : pla_reg;
assign DOB = (READ_MODE1 == 1'b0)? bpb_reg : plb_reg;

assign pcea = CEA && bs_en;
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end
end

always@(ADA, ADB, bit_width_a0, bit_width_a1,WREA, WREB,mc)begin
    if(bit_width_a0==11)begin
		addr_a[14:0] = ADA[13:3]*bit_width_d0;
		mem_a[8:0]={ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[14:0] = ADA[13:4]*bit_width_d0;
		mem_a[17:0]={ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	
	if(bit_width_a1==11)begin
		addr_b[14:0] = ADB[13:3]*bit_width_d1;
		mem_b[8:0]={ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[14:0] = ADB[13:4]*bit_width_d1;
		mem_b[17:0]={ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @ (bpa_reg_async or bpa_reg_sync or pla_reg_async or pla_reg_sync or bpb_reg_async or bpb_reg_sync or plb_reg_async or plb_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bpa_reg <= bpa_reg_async;
        pla_reg <= pla_reg_async;
        bpb_reg <= bpb_reg_async;
        plb_reg <= plb_reg_async;
    end
    else begin
        bpa_reg <= bpa_reg_sync;
        pla_reg <= pla_reg_sync;
        bpb_reg <= bpb_reg_sync;
        plb_reg <= plb_reg_sync;
    end
end

always @(posedge CLKA) begin
	if (pcea) begin
		if (WREA) begin
			if (bit_width_d0 == 9)
				{ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a] } <= DIA[8:0];
			else if(bit_width_d0 == 18) begin
				if(ADA[0] == 1'b1)
					{ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a] } <= DIA[8:0];
				if(ADA[1] == 1'b1)
					{ ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DIA[17:9];
			end
			mc <= ~mc;
		end
	end
end

always @(posedge CLKA or posedge RESETA) begin
	if (RESETA) begin
		pla_reg_async <= 0;
		bpa_reg_async <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_async <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if (bit_width_d0 == 9)
					    bpa_reg_async[8:0] <= DIA[8:0];
				    else if(bit_width_d0 == 18) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_async[8:0] <= DIA[8:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_async[17:9] <= DIA[17:9];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end

			end else begin // WREA==0, read
				bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end

		end
	end
end

always @(posedge CLKA) begin
	if (RESETA) begin
		pla_reg_sync <= 0;
		bpa_reg_sync <= 0;
	end else begin	
		if(OCEA) begin
			pla_reg_sync <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if (bit_width_d0 == 9)
					    bpa_reg_sync[8:0] <= DIA[8:0];
				    else if(bit_width_d0 == 18) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_sync[8:0] <= DIA[8:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_sync[17:9] <= DIA[17:9];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end

			end else begin // WREA==0, read
				bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end

		end
	end
end

always @(posedge CLKB) begin
	if (pceb) begin
		if (WREB) begin
			if (bit_width_d1 == 9)
				{ ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b] } <= DIB[8:0];
			else if(bit_width_d1 == 18) begin
				if(ADB[0] == 1'b1)
					{ ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b] } <= DIB[8:0];
				if(ADB[1] == 1'b1)
					{ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10], ram_MEM[addr_b+9]} <= DIB[17:9];
			end
			mc <= ~mc;
		end
	end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		plb_reg_async <= 0;
		bpb_reg_async <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_async <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if (bit_width_d1 == 9)
					    bpb_reg_async[8:0] <= DIB[8:0];
				    else if(bit_width_d1 == 18) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_async[8:0] <= DIB[8:0];
				    	if(ADB[1] == 1'b1)
						    bpb_reg_async[17:9] <= DIB[17:9];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		plb_reg_sync <= 0;
		bpb_reg_sync <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_sync <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if (bit_width_d1 == 9)
					    bpb_reg_sync[8:0] <= DIB[8:0];
				    else if(bit_width_d1 == 18) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_sync[8:0] <= DIB[8:0];
				    	if(ADB[1] == 1'b1)
						    bpb_reg_sync[17:9] <= DIB[17:9];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

endmodule // DPX9: true dual port 18k Block SRAM


module ROM (DO, BLKSEL, AD, WRE, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: read disabled
input [13:0] AD;
input [2:0] BLKSEL;
output [31:0] DO;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [13:0] addr;
reg bs_en;
wire pce;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		1: begin 
			awidth = 14;			
		   end
		2: begin 
			awidth = 13;			
		   end
		4: begin 
			awidth = 12;			
		   end
		8: begin 
			awidth = 11; 			
		   end
		16: begin 
			awidth = 10;
		   end
		32: begin 
			awidth = 9;
		   end
		default: begin
	//		$display ("%d: Unsupported data width\n", dwidth);
	//		$finish;
		end
	endcase
end

assign pce = CE && bs_en;
always @ (BLKSEL) begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end
end

always@(AD,awidth,WRE)begin
	if(awidth==14)begin
		addr[13:0] = AD[13:0];
		mem_t[0] = ram_MEM[addr];
	end
	else if(awidth==13)begin
		addr[13:0] = {AD[13:1],1'b0};
		mem_t[1:0] = {ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==12)begin
		addr[13:0] = {AD[13:2],2'b00};
		mem_t[3:0] = {ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==11)begin
		addr[13:0] = {AD[13:3],3'b000};
		mem_t[7:0] = {ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==10)begin
		addr[13:0] = {AD[13:4],4'b0000};
		mem_t[15:0] = {ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[13:0] = {AD[13:5],5'b00000};
		mem_t[31:0] = {ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg : pl_reg;

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pce && !WRE) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pce && !WRE) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

endmodule // ROM: 16k Block ROM

module ROMX9 (DO, BLKSEL, AD, WRE, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 36; // 9, 18, 36
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: read disabled
input [13:0] AD;
input [2:0] BLKSEL;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [14:0] addr;
reg bs_en;
wire pce;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		9: begin awidth = 11; 
		   end
		18: begin awidth = 10; 
			end
		36: begin awidth = 9; 
		    end
		default: begin
		//	$display ("%d: Unsupported data width\n", dwidth);
		//	$finish;
		end
	endcase
end

always@(AD,awidth,WRE)begin	
	if(awidth==11)begin
		addr[14:0] = AD[13:3]*dwidth;
		mem_t[8:0] = {ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};

	end
	else if(awidth==10)begin
		addr[14:0] = AD[13:4]*dwidth;
		mem_t[17:0] = {ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[14:0] = AD[13:5]*dwidth;
		mem_t[35:0] = {ram_MEM[addr+35],ram_MEM[addr+34],ram_MEM[addr+33],ram_MEM[addr+32],ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg: pl_reg;

assign pce = CE && bs_en;
always @ (BLKSEL) begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pce && !WRE) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pce && !WRE) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

endmodule // ROMX9: 18k Block ROM

//rSDP
module rSDP (DO, DI, BLKSEL, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input [13:0] ADA, ADB;
input [31:0] DI;
input [2:0] BLKSEL;
output [31:0] DO;

reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [16383:0] ram_MEM ={INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [13:0] addr_a, addr_b;
reg mc,bs_en;
wire pcea;
wire pceb;

integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		1: bit_width_a0 = 14;
		2: bit_width_a0 = 13;
		4: bit_width_a0 = 12;
		8: bit_width_a0 = 11;
		16: bit_width_a0 = 10;
		32: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		1: bit_width_a1 = 14;
		2: bit_width_a1 = 13;
		4: bit_width_a1 = 12;
		8: bit_width_a1 = 11;
		16: bit_width_a1 = 10;
		32: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_en;   
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,mc)begin
	if(bit_width_a0==14)begin
		addr_a[13:0] = ADA[13:0];
		mem_a[0] = ram_MEM[addr_a];
	end
	else if(bit_width_a0==13)begin
		addr_a[13:0] = {ADA[13:1],1'b0};
		mem_a[1:0] = {ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==12)begin
		addr_a[13:0] = {ADA[13:2],2'b00};
		mem_a[3:0] = {ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==11)begin
		addr_a[13:0] = {ADA[13:3],3'b000};
		mem_a[7:0] = {ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[13:0] = {ADA[13:4],4'b0000};
		mem_a[15:0] = {ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[13:0] = {ADA[13:5],5'b00000};
		mem_a[31:0] = {ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	if(bit_width_a1==14)begin
		addr_b[13:0] = ADB[13:0];
		mem_b[0]=ram_MEM[addr_b];
	end
	else if(bit_width_a1==13)begin
		addr_b[13:0] = {ADB[13:1],1'b0};
		mem_b[1:0]={ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==12)begin
		addr_b[13:0] = {ADB[13:2],2'b00};
		mem_b[3:0]={ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==11)begin
		addr_b[13:0] = {ADB[13:3],3'b000};
		mem_b[7:0]={ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[13:0] = {ADB[13:4],4'b0000};
		mem_b[15:0]={ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
	else if(bit_width_a1==9)begin
		addr_b[13:0] = {ADB[13:5],5'b00000};
		mem_b[31:0]={ ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
end

always @(posedge CLKA) begin
	if (pcea) begin
	    if(bit_width_d0==1)
			ram_MEM[addr_a] <= DI[0];
		else if(bit_width_d0==2)
			{ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==4)
			{ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==8)
			{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==16) begin
			if(ADA[0] == 1'b1)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
			if(ADA[1] ==1'b1)
				{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
		end
		else if(bit_width_d0==32) begin
			if(ADA[0] == 1'b1)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
			if(ADA[2] == 1'b1)
				{ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16]} <=DI[23:16];
			if(ADA[3] == 1'b1)
				{ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24]} <=DI[31:24];
		end
		mc <= ~mc;
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // rSDP: revision Semi dual port 16k Block SRAM

//rSDPX9
module rSDPX9 (DO, DI, BLKSEL, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [35:0] DI;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [14:0] addr_a, addr_b;
reg mc,bs_en;
wire pcea,pceb;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		9: bit_width_a0 = 11;
		18: bit_width_a0 = 10;
		36: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		9: bit_width_a1 = 11;
		18: bit_width_a1 = 10;
		36: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_en;   
assign pceb = CEB && bs_en;
always @ (BLKSEL)
begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end  	
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,mc)begin
	if(bit_width_a0==11)begin
		addr_a[14:0] = ADA[13:3]*bit_width_d0;
		mem_a[8:0] = {ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[14:0] = ADA[13:4]*bit_width_d0;
		mem_a[17:0] = {ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[14:0] = ADA[13:5]*bit_width_d0;
		mem_a[35:0] = {ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end

	if(bit_width_a1==11)begin
		addr_b[14:0] = ADB[13:3]*bit_width_d1;
		mem_b[8:0] = {ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[14:0] = ADB[13:4]*bit_width_d1;
		mem_b[17:0] = {ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==9)begin
		addr_b[14:0] = ADB[13:5]*bit_width_d1;
		mem_b[35:0] = {ram_MEM[addr_b+35],ram_MEM[addr_b+34],ram_MEM[addr_b+33],ram_MEM[addr_b+32],ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @(posedge CLKA) begin		
	if (pcea) begin
		if(bit_width_d0 == 9) begin
			{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
		end else if(bit_width_d0 == 18) begin
			if(ADA[0] == 1'b1)
				{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
		end
		else if(bit_width_d0 == 36) begin
			if(ADA[0] == 1'b1)
				{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
			if(ADA[2] == 1'b1)
				{ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18]} <= DI[26:18];
			if(ADA[3] == 1'b1)
				{ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27]} <= DI[35:27];
		end
		mc <= ~mc;
	end
end	

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		bp_reg_async <=0;
		pl_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		bp_reg_sync <=0;
		pl_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // rSDPX9: revision Semi dual port 18k Block SRAM

//rROM
module rROM (DO, BLKSEL, AD, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input [13:0] AD;
input [2:0] BLKSEL;
output [31:0] DO;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [13:0] addr;
reg bs_en;
wire pce;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		1: begin 
			awidth = 14;			
		   end
		2: begin 
			awidth = 13;			
		   end
		4: begin 
			awidth = 12;			
		   end
		8: begin 
			awidth = 11; 			
		   end
		16: begin 
			awidth = 10;
		   end
		32: begin 
			awidth = 9;
		   end
		default: begin
	//		$display ("%d: Unsupported data width\n", dwidth);
	//		$finish;
		end
	endcase
end

assign pce = CE && bs_en;
always @ (BLKSEL) begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end
end

always@(AD,awidth)begin
	if(awidth==14)begin
		addr[13:0] = AD[13:0];
		mem_t[0] = ram_MEM[addr];
	end
	else if(awidth==13)begin
		addr[13:0] = {AD[13:1],1'b0};
		mem_t[1:0] = {ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==12)begin
		addr[13:0] = {AD[13:2],2'b00};
		mem_t[3:0] = {ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==11)begin
		addr[13:0] = {AD[13:3],3'b000};
		mem_t[7:0] = {ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==10)begin
		addr[13:0] = {AD[13:4],4'b0000};
		mem_t[15:0] = {ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[13:0] = {AD[13:5],5'b00000};
		mem_t[31:0] = {ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg : pl_reg;

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pce) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pce) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

endmodule // rROM: revision 16k Block ROM

//rROMX9
module rROMX9 (DO, BLKSEL, AD, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 36; // 9, 18, 36
parameter BLK_SEL = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input [13:0] AD;
input [2:0] BLKSEL;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [14:0] addr;
reg bs_en;
wire pce;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		9: begin awidth = 11; 
		   end
		18: begin awidth = 10; 
			end
		36: begin awidth = 9; 
		    end
		default: begin
		//	$display ("%d: Unsupported data width\n", dwidth);
		//	$finish;
		end
	endcase
end

always@(AD,awidth)begin	
	if(awidth==11)begin
		addr[14:0] = AD[13:3]*dwidth;
		mem_t[8:0] = {ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};

	end
	else if(awidth==10)begin
		addr[14:0] = AD[13:4]*dwidth;
		mem_t[17:0] = {ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[14:0] = AD[13:5]*dwidth;
		mem_t[35:0] = {ram_MEM[addr+35],ram_MEM[addr+34],ram_MEM[addr+33],ram_MEM[addr+32],ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg: pl_reg;

assign pce = CE && bs_en;
always @ (BLKSEL) begin
	if(BLKSEL == BLK_SEL) begin
		bs_en = 1;
	end else begin
		bs_en = 0;
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pce) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pce) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

endmodule // rROMX9: revision 18k Block ROM

//pROM
module pROM (DO, AD, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 32; // 1, 2, 4, 8, 16, 32
parameter RESET_MODE = "SYNC"; //SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input [13:0] AD;
output [31:0] DO;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [13:0] addr;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		1: begin 
			awidth = 14;			
		   end
		2: begin 
			awidth = 13;			
		   end
		4: begin 
			awidth = 12;			
		   end
		8: begin 
			awidth = 11; 			
		   end
		16: begin 
			awidth = 10;
		   end
		32: begin 
			awidth = 9;
		   end
		default: begin
	//		$display ("%d: Unsupported data width\n", dwidth);
	//		$finish;
		end
	endcase
end

always@(AD,awidth)begin
	if(awidth==14)begin
		addr[13:0] = AD[13:0];
		mem_t[0] = ram_MEM[addr];
	end
	else if(awidth==13)begin
		addr[13:0] = {AD[13:1],1'b0};
		mem_t[1:0] = {ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==12)begin
		addr[13:0] = {AD[13:2],2'b00};
		mem_t[3:0] = {ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==11)begin
		addr[13:0] = {AD[13:3],3'b000};
		mem_t[7:0] = {ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==10)begin
		addr[13:0] = {AD[13:4],4'b0000};
		mem_t[15:0] = {ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[13:0] = {AD[13:5],5'b00000};
		mem_t[31:0] = {ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg : pl_reg;

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (CE) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (CE) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end

endmodule //pROM

//pROMX9
module pROMX9 (DO, AD, CLK, CE, OCE, RESET);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 36; // 9, 18, 36
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLK, CE;
input OCE; // clock enable of memory output register
input RESET; // resets registers, not memory contents
input [13:0] AD;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH-1:0] mem_t;
reg [14:0] addr;
integer dwidth = BIT_WIDTH;
integer awidth; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
end

initial begin
	case(dwidth)
		9: begin awidth = 11; 
		   end
		18: begin awidth = 10; 
			end
		36: begin awidth = 9; 
		    end
		default: begin
		//	$display ("%d: Unsupported data width\n", dwidth);
		//	$finish;
		end
	endcase
end

always@(AD,awidth)begin	
	if(awidth==11)begin
		addr[14:0] = AD[13:3]*dwidth;
		mem_t[8:0] = {ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};

	end
	else if(awidth==10)begin
		addr[14:0] = AD[13:4]*dwidth;
		mem_t[17:0] = {ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
	else if(awidth==9)begin
		addr[14:0] = AD[13:5]*dwidth;
		mem_t[35:0] = {ram_MEM[addr+35],ram_MEM[addr+34],ram_MEM[addr+33],ram_MEM[addr+32],ram_MEM[addr+31],ram_MEM[addr+30],ram_MEM[addr+29],ram_MEM[addr+28],ram_MEM[addr+27],ram_MEM[addr+26],ram_MEM[addr+25],ram_MEM[addr+24],ram_MEM[addr+23],ram_MEM[addr+22],ram_MEM[addr+21],ram_MEM[addr+20],ram_MEM[addr+19],ram_MEM[addr+18],ram_MEM[addr+17],ram_MEM[addr+16],ram_MEM[addr+15],ram_MEM[addr+14],ram_MEM[addr+13],ram_MEM[addr+12],ram_MEM[addr+11],ram_MEM[addr+10],ram_MEM[addr+9],ram_MEM[addr+8],ram_MEM[addr+7],ram_MEM[addr+6],ram_MEM[addr+5],ram_MEM[addr+4],ram_MEM[addr+3],ram_MEM[addr+2],ram_MEM[addr+1],ram_MEM[addr]};
	end
end

assign DO = (READ_MODE === 1'b0)? bp_reg: pl_reg;

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLK or posedge RESET) begin
	if (RESET) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (CE) begin
			bp_reg_async[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end

always @(posedge CLK) begin
	if (RESET) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (CE) begin
			bp_reg_sync[BIT_WIDTH-1:0] <= mem_t[BIT_WIDTH-1:0];
		end
	end
end	

endmodule //pROMX9

//SDPB
module SDPB (DO, DI, BLKSELA, BLKSELB, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input [13:0] ADA, ADB;
input [31:0] DI;
input [2:0] BLKSELA, BLKSELB;
output [31:0] DO;

reg [31:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [31:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [16383:0] ram_MEM ={INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [13:0] addr_a, addr_b;
reg mc,bs_ena,bs_enb;
wire pcea;
wire pceb;

integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		1: bit_width_a0 = 14;
		2: bit_width_a0 = 13;
		4: bit_width_a0 = 12;
		8: bit_width_a0 = 11;
		16: bit_width_a0 = 10;
		32: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		1: bit_width_a1 = 14;
		2: bit_width_a1 = 13;
		4: bit_width_a1 = 12;
		8: bit_width_a1 = 11;
		16: bit_width_a1 = 10;
		32: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_ena;   
assign pceb = CEB && bs_enb;

always @ (BLKSELA, BLKSELB)
begin
	if(BLKSELA == BLK_SEL_0) begin
		bs_ena = 1;
	end else begin
		bs_ena = 0;
	end

    if(BLKSELB == BLK_SEL_1) begin
		bs_enb = 1;
	end else begin
		bs_enb = 0;
	end
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,mc)begin
	if(bit_width_a0==14)begin
		addr_a[13:0] = ADA[13:0];
		mem_a[0] = ram_MEM[addr_a];
	end
	else if(bit_width_a0==13)begin
		addr_a[13:0] = {ADA[13:1],1'b0};
		mem_a[1:0] = {ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==12)begin
		addr_a[13:0] = {ADA[13:2],2'b00};
		mem_a[3:0] = {ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==11)begin
		addr_a[13:0] = {ADA[13:3],3'b000};
		mem_a[7:0] = {ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[13:0] = {ADA[13:4],4'b0000};
		mem_a[15:0] = {ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[13:0] = {ADA[13:5],5'b00000};
		mem_a[31:0] = {ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	if(bit_width_a1==14)begin
		addr_b[13:0] = ADB[13:0];
		mem_b[0]=ram_MEM[addr_b];
	end
	else if(bit_width_a1==13)begin
		addr_b[13:0] = {ADB[13:1],1'b0};
		mem_b[1:0]={ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==12)begin
		addr_b[13:0] = {ADB[13:2],2'b00};
		mem_b[3:0]={ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==11)begin
		addr_b[13:0] = {ADB[13:3],3'b000};
		mem_b[7:0]={ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[13:0] = {ADB[13:4],4'b0000};
		mem_b[15:0]={ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
	else if(bit_width_a1==9)begin
		addr_b[13:0] = {ADB[13:5],5'b00000};
		mem_b[31:0]={ ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};

	end
end

always @(posedge CLKA) begin
	if (pcea) begin
	    if(bit_width_d0==1)
			ram_MEM[addr_a] <= DI[0];
		else if(bit_width_d0==2)
			{ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==4)
			{ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==8)
			{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[BIT_WIDTH_0-1:0];
		else if(bit_width_d0==16) begin
			if(ADA[0] == 1'b1)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
			if(ADA[1] ==1'b1)
				{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
		end
		else if(bit_width_d0==32) begin
			if(ADA[0] == 1'b1)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DI[7:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]}<=DI[15:8];
			if(ADA[2] == 1'b1)
				{ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16]} <=DI[23:16];
			if(ADA[3] == 1'b1)
				{ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24]} <=DI[31:24];
		end
		mc <= ~mc;
	end
end

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		pl_reg_async <= 0;
		bp_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		pl_reg_sync <= 0;
		bp_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // SDPB: Semi dual port 16k Block SRAM

//SDPX9B
module SDPX9B (DO, DI, BLKSELA, BLKSELB, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB);

parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [35:0] DI;
output [35:0] DO;

reg [35:0] bp_reg,bp_reg_async,bp_reg_sync;
reg [35:0] pl_reg,pl_reg_async,pl_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg [14:0] addr_a, addr_b;
reg mc,bs_ena,bs_enb;
wire pcea,pceb;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bp_reg = 0;
    pl_reg = 0;
    bp_reg_async = 0;
    bp_reg_sync = 0;
    pl_reg_async = 0;
    pl_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		9: bit_width_a0 = 11;
		18: bit_width_a0 = 10;
		36: bit_width_a0 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		9: bit_width_a1 = 11;
		18: bit_width_a1 = 10;
		36: bit_width_a1 = 9;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DO = (READ_MODE == 1'b0)? bp_reg: pl_reg;

assign pcea = CEA && bs_ena;   
assign pceb = CEB && bs_enb;
always @ (BLKSELA, BLKSELB)
begin
	if(BLKSELA == BLK_SEL_0) begin
		bs_ena = 1;
	end else begin
		bs_ena = 0;
	end

    if(BLKSELB == BLK_SEL_1) begin
		bs_enb = 1;
	end else begin
		bs_enb = 0;
	end
end

always@(ADA,ADB,bit_width_a0,bit_width_a1,mc)begin
	if(bit_width_a0==11)begin
		addr_a[14:0] = ADA[13:3]*bit_width_d0;
		mem_a[8:0] = {ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[14:0] = ADA[13:4]*bit_width_d0;
		mem_a[17:0] = {ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==9)begin
		addr_a[14:0] = ADA[13:5]*bit_width_d0;
		mem_a[35:0] = {ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27],ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18],ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end

	if(bit_width_a1==11)begin
		addr_b[14:0] = ADB[13:3]*bit_width_d1;
		mem_b[8:0] = {ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[14:0] = ADB[13:4]*bit_width_d1;
		mem_b[17:0] = {ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==9)begin
		addr_b[14:0] = ADB[13:5]*bit_width_d1;
		mem_b[35:0] = {ram_MEM[addr_b+35],ram_MEM[addr_b+34],ram_MEM[addr_b+33],ram_MEM[addr_b+32],ram_MEM[addr_b+31],ram_MEM[addr_b+30],ram_MEM[addr_b+29],ram_MEM[addr_b+28],ram_MEM[addr_b+27],ram_MEM[addr_b+26],ram_MEM[addr_b+25],ram_MEM[addr_b+24],ram_MEM[addr_b+23],ram_MEM[addr_b+22],ram_MEM[addr_b+21],ram_MEM[addr_b+20],ram_MEM[addr_b+19],ram_MEM[addr_b+18],ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @(posedge CLKA) begin		
	if (pcea) begin
		if(bit_width_d0 == 9) begin
			{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
		end else if(bit_width_d0 == 18) begin
			if(ADA[0] == 1'b1)
				{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
		end
		else if(bit_width_d0 == 36) begin
			if(ADA[0] == 1'b1)
				{ram_MEM [addr_a+8],ram_MEM [addr_a+7], ram_MEM [addr_a+6], ram_MEM [addr_a+5], ram_MEM [addr_a+4], ram_MEM [addr_a+3], ram_MEM [addr_a+2], ram_MEM [addr_a+1], ram_MEM [addr_a]} <= DI[8:0];
			if(ADA[1] == 1'b1)
				{ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DI[17:9];
			if(ADA[2] == 1'b1)
				{ram_MEM[addr_a+26],ram_MEM[addr_a+25],ram_MEM[addr_a+24],ram_MEM[addr_a+23],ram_MEM[addr_a+22],ram_MEM[addr_a+21],ram_MEM[addr_a+20],ram_MEM[addr_a+19],ram_MEM[addr_a+18]} <= DI[26:18];
			if(ADA[3] == 1'b1)
				{ram_MEM[addr_a+35],ram_MEM[addr_a+34],ram_MEM[addr_a+33],ram_MEM[addr_a+32],ram_MEM[addr_a+31],ram_MEM[addr_a+30],ram_MEM[addr_a+29],ram_MEM[addr_a+28],ram_MEM[addr_a+27]} <= DI[35:27];
		end
		mc <= ~mc;
	end
end	

always @ (bp_reg_async or bp_reg_sync or pl_reg_async or pl_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bp_reg <= bp_reg_async;
        pl_reg <= pl_reg_async;
    end
    else begin
        bp_reg <= bp_reg_sync;
        pl_reg <= pl_reg_sync;
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		bp_reg_async <=0;
		pl_reg_async <= 0;
	end else begin
		if(OCE) begin
			pl_reg_async <= bp_reg;
		end
		if (pceb) begin
			bp_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		bp_reg_sync <=0;
		pl_reg_sync <= 0;
	end else begin
		if(OCE) begin
			pl_reg_sync <= bp_reg;
		end
		if (pceb) begin
			bp_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
		end
	end
end

endmodule // SDPX9B: Semi dual port 18k Block SRAM

module DPB (DOA, DOB, DIA, DIB, BLKSELA, BLKSELB, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB);

parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 16; // 1, 2, 4, 8, 16
parameter BIT_WIDTH_1 = 16; // 1, 2, 4, 8, 16
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC, ASYNC
parameter INIT_RAM_00 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_01 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 256'h0000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 256'h0000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [15:0] DIA, DIB;
output [15:0] DOA, DOB;

reg [15:0] bpa_reg,bpa_reg_async,bpa_reg_sync;
reg [15:0] pla_reg,pla_reg_async,pla_reg_sync;
reg [15:0] bpb_reg, plb_reg,bpb_reg_async,bpb_reg_sync,plb_reg_async,plb_reg_sync;
reg [16383:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00} ;
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg mc,bs_ena,bs_enb;
wire pcea,pceb;
reg [13:0] addr_a, addr_b;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH

initial begin
    bpa_reg = 0;
    pla_reg = 0;
    bpb_reg = 0;
    plb_reg = 0;
    bpa_reg_async = 0;
    bpa_reg_sync = 0;
    pla_reg_async = 0;
    pla_reg_sync = 0;
    bpb_reg_async = 0;
    bpb_reg_sync = 0;
    plb_reg_async = 0;
    plb_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		1: bit_width_a0 = 14;
		2: bit_width_a0 = 13;
		4: bit_width_a0 = 12;
		8: bit_width_a0 = 11;
		16: bit_width_a0 = 10;
	default: begin
	//	$display ("%d: Unsupported data width\n", bit_width_d0);
	//	$finish;
	end
	endcase
	case(bit_width_d1)
		1: bit_width_a1 = 14;
		2: bit_width_a1 = 13;
		4: bit_width_a1 = 12;
		8: bit_width_a1 = 11;
		16: bit_width_a1 = 10;
	default: begin
	//	$display ("%d: Unsupported data width\n", bit_width_d1);
	//	$finish;
	end
	endcase
end

assign DOA = (READ_MODE0 == 1'b0)? bpa_reg : pla_reg;
assign DOB = (READ_MODE1 == 1'b0)? bpb_reg : plb_reg;

assign pcea = CEA && bs_ena;   
assign pceb = CEB && bs_enb;
always @ (BLKSELA, BLKSELB)
begin
	if(BLKSELA == BLK_SEL_0) begin
		bs_ena = 1;
	end else begin
		bs_ena = 0;
	end

    if(BLKSELB == BLK_SEL_1) begin
		bs_enb = 1;
	end else begin
		bs_enb = 0;
	end

end

always@(ADA,ADB,bit_width_a0,bit_width_a1,WREA,WREB,mc)begin
	if(bit_width_a0==14)begin
		addr_a[13:0] = ADA[13:0];
		mem_a[0]=ram_MEM[addr_a];
	end
	else if(bit_width_a0==13)begin
		addr_a[13:0] = {ADA[13:1],1'b0};
		mem_a[1:0]={ ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==12)begin
		addr_a[13:0] = {ADA[13:2],2'b00};
		mem_a[3:0]={ ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==11)begin
		addr_a[13:0] = {ADA[13:3],3'b000};
		mem_a[7:0]={ ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[13:0] = {ADA[13:4],4'b0000};
		mem_a[15:0]={ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	if(bit_width_a1==14)begin
		addr_b[13:0] = ADB[13:0];
		mem_b[0]=ram_MEM[addr_b];
	end
	else if(bit_width_a1==13)begin
		addr_b[13:0] = {ADB[13:1],1'b0};
		mem_b[1:0]={ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==12)begin
		addr_b[13:0] = {ADB[13:2],2'b00};
		mem_b[3:0]={ ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==11)begin
		addr_b[13:0] = {ADB[13:3],3'b000};
		mem_b[7:0]={ ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[13:0] = {ADB[13:4],4'b0000};
		mem_b[15:0]={ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @ (bpa_reg_async or bpa_reg_sync or pla_reg_async or pla_reg_sync or bpb_reg_async or bpb_reg_sync or plb_reg_async or plb_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bpa_reg <= bpa_reg_async;
        pla_reg <= pla_reg_async;
        bpb_reg <= bpb_reg_async;
        plb_reg <= plb_reg_async;
    end
    else begin
        bpa_reg <= bpa_reg_sync;
        pla_reg <= pla_reg_sync;
        bpb_reg <= bpb_reg_sync;
        plb_reg <= plb_reg_sync;
    end
end

always @(posedge CLKA) begin
	if (pcea) begin
		if (WREA) begin
			if(bit_width_d0==1)
				ram_MEM[addr_a] <= DIA[0];
			else if(bit_width_d0==2)
				{ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==4)
				{ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==8)
				{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]}<=DIA[BIT_WIDTH_0-1:0];
			else if(bit_width_d0==16) begin
				if(ADA[0] == 1'b1)
					{ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]} <= DIA[7:0];
				if(ADA[1] == 1'b1)
					{ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8]} <= DIA[15:8];
			end
			mc <= ~mc;
		end 
	end
end	

always @(posedge CLKA or posedge RESETA) begin
	if (RESETA) begin
		pla_reg_async <= 0;
		bpa_reg_async <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_async <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if(bit_width_d0<=8)
					    bpa_reg_async[BIT_WIDTH_0-1:0] <= DIA[BIT_WIDTH_0-1:0];
				    else if(bit_width_d0==16) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_async[7:0] <= DIA[7:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_async[15:8]  <= DIA[15:8];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end
				
			end 
			else begin // WREA==0, read
				bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end
		end
	end
end	

always @(posedge CLKA) begin
	if (RESETA) begin
		pla_reg_sync <= 0;
		bpa_reg_sync <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_sync <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if(bit_width_d0<=8)
					    bpa_reg_sync[BIT_WIDTH_0-1:0] <= DIA[BIT_WIDTH_0-1:0];
				    else if(bit_width_d0==16) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_sync[7:0] <= DIA[7:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_sync[15:8]  <= DIA[15:8];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end
				
			end 
			else begin // WREA==0, read
				bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end
		end
	end
end

always @(posedge CLKB) begin
	if (pceb) begin
		if (WREB) begin
			if(bit_width_d1==1)
				ram_MEM[addr_b] <= DIB[0];
			else if(bit_width_d1==2)
				{ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==4)
				{ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==8)
				{ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]}<=DIB[BIT_WIDTH_1-1:0];
			else if(bit_width_d1==16) begin
				if(ADB[0] == 1'b1)
					{ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]} <= DIB[7:0];
				if(ADB[1] == 1'b1)
					{ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8]} <= DIB[15:8];
			end
			mc <= ~mc;
		end 
    end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		plb_reg_async <= 0;
		bpb_reg_async <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_async <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if(bit_width_d1<=8)
					    bpb_reg_async[BIT_WIDTH_1-1:0] <= DIB[BIT_WIDTH_1-1:0];
				    else if(bit_width_d1==16) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_async[7:0] <= DIB[7:0];
					    if(ADB[1] == 1'b1)
						    bpb_reg_async[15:8]  <= DIB[15:8];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end	

always @(posedge CLKB) begin
	if (RESETB) begin
		plb_reg_sync <= 0;
		bpb_reg_sync <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_sync <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if(bit_width_d1<=8)
					    bpb_reg_sync[BIT_WIDTH_1-1:0] <= DIB[BIT_WIDTH_1-1:0];
				    else if(bit_width_d1==16) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_sync[7:0] <= DIB[7:0];
					    if(ADB[1] == 1'b1)
						    bpb_reg_sync[15:8]  <= DIB[15:8];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

endmodule // DPB: true dual port 16k Block SRAM

module DPX9B (DOA, DOB, DIA, DIB, BLKSELA, BLKSELB, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB);

parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 18; // 9, 18
parameter BIT_WIDTH_1 = 18; // 9, 18
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
parameter RESET_MODE = "SYNC"; //SYNC,ASYNC
parameter INIT_RAM_00 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000; 
parameter INIT_RAM_01 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_02 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_03 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_04 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_05 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_06 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_07 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_08 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_09 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_0F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_10 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_11 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_12 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_13 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_14 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_15 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_16 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_17 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_18 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_19 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_1F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_20 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_21 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_22 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_23 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_24 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_25 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_26 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_27 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_28 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_29 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_2F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_30 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_31 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_32 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_33 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_34 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_35 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_36 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_37 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_38 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_39 = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3A = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3B = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3C = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3D = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3E = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;
parameter INIT_RAM_3F = 288'h000000000000000000000000000000000000000000000000000000000000000000000000;

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [17:0] DIA, DIB;
input [2:0] BLKSELA, BLKSELB;
output [17:0] DOA, DOB;
reg [17:0] bpa_reg, bpb_reg,bpa_reg_async, bpb_reg_async,bpa_reg_sync, bpb_reg_sync;
reg [17:0] pla_reg, plb_reg,pla_reg_async, plb_reg_async,pla_reg_sync, plb_reg_sync;
reg [18431:0] ram_MEM = {INIT_RAM_3F, INIT_RAM_3E, INIT_RAM_3D, INIT_RAM_3C,INIT_RAM_3B, INIT_RAM_3A, INIT_RAM_39, INIT_RAM_38,INIT_RAM_37, INIT_RAM_36, INIT_RAM_35, INIT_RAM_34,INIT_RAM_33, INIT_RAM_32, INIT_RAM_31, INIT_RAM_30,INIT_RAM_2F, INIT_RAM_2E, INIT_RAM_2D, INIT_RAM_2C,INIT_RAM_2B, INIT_RAM_2A, INIT_RAM_29, INIT_RAM_28,INIT_RAM_27, INIT_RAM_26, INIT_RAM_25, INIT_RAM_24,INIT_RAM_23, INIT_RAM_22, INIT_RAM_21, INIT_RAM_20,INIT_RAM_1F, INIT_RAM_1E, INIT_RAM_1D, INIT_RAM_1C,INIT_RAM_1B, INIT_RAM_1A, INIT_RAM_19, INIT_RAM_18,INIT_RAM_17, INIT_RAM_16, INIT_RAM_15, INIT_RAM_14,INIT_RAM_13, INIT_RAM_12, INIT_RAM_11, INIT_RAM_10,INIT_RAM_0F, INIT_RAM_0E, INIT_RAM_0D, INIT_RAM_0C, INIT_RAM_0B, INIT_RAM_0A, INIT_RAM_09, INIT_RAM_08,INIT_RAM_07, INIT_RAM_06, INIT_RAM_05, INIT_RAM_04,INIT_RAM_03, INIT_RAM_02, INIT_RAM_01, INIT_RAM_00};
reg [BIT_WIDTH_0-1:0] mem_a;
reg [BIT_WIDTH_1-1:0] mem_b;
reg mc, bs_ena, bs_enb;
wire pcea,pceb;
integer bit_width_d0 = BIT_WIDTH_0;
integer bit_width_d1 = BIT_WIDTH_1;
integer bit_width_a0, bit_width_a1; // ADDR_WIDTH
reg [14:0] addr_a, addr_b;

initial begin
    bpa_reg = 0;
    bpb_reg = 0;
    pla_reg = 0;
    plb_reg = 0;
    bpa_reg_async = 0;
    bpa_reg_sync = 0;
    pla_reg_async = 0;
    pla_reg_sync = 0;
    bpb_reg_async = 0;
    bpb_reg_sync = 0;
    plb_reg_async = 0;
    plb_reg_sync = 0;
    mc = 1'b0;
end

initial begin
	case(bit_width_d0)
		9: bit_width_a0 = 11;
		18: bit_width_a0 = 10;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d0);
		//	$finish;
		end
	endcase
	case(bit_width_d1)
		9: bit_width_a1 = 11;
		18: bit_width_a1 = 10;
		default: begin
		//	$display ("%d: Unsupported data width\n", bit_width_d1);
		//	$finish;
		end
	endcase
end

assign DOA = (READ_MODE0 == 1'b0)? bpa_reg : pla_reg;
assign DOB = (READ_MODE1 == 1'b0)? bpb_reg : plb_reg;

assign pcea = CEA && bs_ena;
assign pceb = CEB && bs_enb;
always @ (BLKSELA, BLKSELB)
begin
	if(BLKSELA == BLK_SEL_0) begin
		bs_ena = 1;
	end else begin
		bs_ena = 0;
	end

    if(BLKSELB == BLK_SEL_1) begin
		bs_enb = 1;
	end else begin
		bs_enb = 0;
	end
end

always@(ADA, ADB, bit_width_a0, bit_width_a1,WREA, WREB,mc)begin
    if(bit_width_a0==11)begin
		addr_a[14:0] = ADA[13:3]*bit_width_d0;
		mem_a[8:0]={ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	else if(bit_width_a0==10)begin
		addr_a[14:0] = ADA[13:4]*bit_width_d0;
		mem_a[17:0]={ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9],ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a]};
	end
	
	if(bit_width_a1==11)begin
		addr_b[14:0] = ADB[13:3]*bit_width_d1;
		mem_b[8:0]={ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
	else if(bit_width_a1==10)begin
		addr_b[14:0] = ADB[13:4]*bit_width_d1;
		mem_b[17:0]={ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10],ram_MEM[addr_b+9],ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b]};
	end
end

always @ (bpa_reg_async or bpa_reg_sync or pla_reg_async or pla_reg_sync or bpb_reg_async or bpb_reg_sync or plb_reg_async or plb_reg_sync) begin
    if(RESET_MODE == "ASYNC") begin
        bpa_reg <= bpa_reg_async;
        pla_reg <= pla_reg_async;
        bpb_reg <= bpb_reg_async;
        plb_reg <= plb_reg_async;
    end
    else begin
        bpa_reg <= bpa_reg_sync;
        pla_reg <= pla_reg_sync;
        bpb_reg <= bpb_reg_sync;
        plb_reg <= plb_reg_sync;
    end
end

always @(posedge CLKA) begin
	if (pcea) begin
		if (WREA) begin
			if (bit_width_d0 == 9)
				{ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a] } <= DIA[8:0];
			else if(bit_width_d0 == 18) begin
				if(ADA[0] == 1'b1)
					{ ram_MEM[addr_a+8],ram_MEM[addr_a+7],ram_MEM[addr_a+6],ram_MEM[addr_a+5],ram_MEM[addr_a+4],ram_MEM[addr_a+3],ram_MEM[addr_a+2],ram_MEM[addr_a+1],ram_MEM[addr_a] } <= DIA[8:0];
				if(ADA[1] == 1'b1)
					{ ram_MEM[addr_a+17],ram_MEM[addr_a+16],ram_MEM[addr_a+15],ram_MEM[addr_a+14],ram_MEM[addr_a+13],ram_MEM[addr_a+12],ram_MEM[addr_a+11],ram_MEM[addr_a+10],ram_MEM[addr_a+9]} <= DIA[17:9];
			end
			mc <= ~mc;
		end
	end
end

always @(posedge CLKA or posedge RESETA) begin
	if (RESETA) begin
		pla_reg_async <= 0;
		bpa_reg_async <= 0;
	end else begin
		if(OCEA) begin
			pla_reg_async <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if (bit_width_d0 == 9)
					    bpa_reg_async[8:0] <= DIA[8:0];
				    else if(bit_width_d0 == 18) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_async[8:0] <= DIA[8:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_async[17:9] <= DIA[17:9];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end

			end else begin // WREA==0, read
				bpa_reg_async[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end

		end
	end
end

always @(posedge CLKA) begin
	if (RESETA) begin
		pla_reg_sync <= 0;
		bpa_reg_sync <= 0;
	end else begin	
		if(OCEA) begin
			pla_reg_sync <= bpa_reg;
		end
		if (pcea) begin
			if (WREA) begin
				if (WRITE_MODE0 == 2'b01) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
                    if (bit_width_d0 == 9)
					    bpa_reg_sync[8:0] <= DIA[8:0];
				    else if(bit_width_d0 == 18) begin
					    if(ADA[0] == 1'b1)
						    bpa_reg_sync[8:0] <= DIA[8:0];
					    if(ADA[1] == 1'b1)
						    bpa_reg_sync[17:9] <= DIA[17:9];
				    end
				end

				if (WRITE_MODE0 == 2'b10) begin
					bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
				end

			end else begin // WREA==0, read
				bpa_reg_sync[BIT_WIDTH_0-1:0] <= mem_a[BIT_WIDTH_0-1:0];
			end

		end
	end
end

always @(posedge CLKB) begin
	if (pceb) begin
		if (WREB) begin
			if (bit_width_d1 == 9)
				{ ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b] } <= DIB[8:0];
			else if(bit_width_d1 == 18) begin
				if(ADB[0] == 1'b1)
					{ ram_MEM[addr_b+8],ram_MEM[addr_b+7],ram_MEM[addr_b+6],ram_MEM[addr_b+5],ram_MEM[addr_b+4],ram_MEM[addr_b+3],ram_MEM[addr_b+2],ram_MEM[addr_b+1],ram_MEM[addr_b] } <= DIB[8:0];
				if(ADB[1] == 1'b1)
					{ram_MEM[addr_b+17],ram_MEM[addr_b+16],ram_MEM[addr_b+15],ram_MEM[addr_b+14],ram_MEM[addr_b+13],ram_MEM[addr_b+12],ram_MEM[addr_b+11],ram_MEM[addr_b+10], ram_MEM[addr_b+9]} <= DIB[17:9];
			end
			mc <= ~mc;
		end
	end
end

always @(posedge CLKB or posedge RESETB) begin
	if (RESETB) begin
		plb_reg_async <= 0;
		bpb_reg_async <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_async <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if (bit_width_d1 == 9)
					    bpb_reg_async[8:0] <= DIB[8:0];
				    else if(bit_width_d1 == 18) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_async[8:0] <= DIB[8:0];
				    	if(ADB[1] == 1'b1)
						    bpb_reg_async[17:9] <= DIB[17:9];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_async[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

always @(posedge CLKB) begin
	if (RESETB) begin
		plb_reg_sync <= 0;
		bpb_reg_sync <= 0;
	end else begin
		if(OCEB) begin
			plb_reg_sync <= bpb_reg;
		end
		if (pceb) begin
			if (WREB) begin
				if (WRITE_MODE1 == 2'b01) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
                    if (bit_width_d1 == 9)
					    bpb_reg_sync[8:0] <= DIB[8:0];
				    else if(bit_width_d1 == 18) begin
					    if(ADB[0] == 1'b1)
						    bpb_reg_sync[8:0] <= DIB[8:0];
				    	if(ADB[1] == 1'b1)
						    bpb_reg_sync[17:9] <= DIB[17:9];
				    end
				end

				if (WRITE_MODE1 == 2'b10) begin
					bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
				end

			end else begin
				bpb_reg_sync[BIT_WIDTH_1-1:0] <= mem_b[BIT_WIDTH_1-1:0];
			end
		end
	end
end

endmodule // DPX9B: true dual port 18k Block SRAM


//********DSP primitive ****************
// PADD18
module PADD18 (DOUT, SO, SBO, A, B, SI, SBI, ASEL, CLK, CE, RESET);

input  [17:0] A;
input  [17:0] B;
input  ASEL;
input  CE,CLK,RESET;
input  [17:0] SI,SBI;
output [17:0] SO,SBO;
output [17:0] DOUT;

parameter AREG = 1'b0; // 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0;
parameter ADD_SUB = 1'b0; //1'b0: add; 1'b1:sub
parameter PADD_RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter BSEL_MODE = 1'b1; // 1'b1: shift, 1'b0: parallel input B.
parameter SOREG = 1'b0;

    reg [17:0] inb,ina0,inb0,ina0_reg,ina1_reg,inb_reg,inc_reg,ina1,ina2;
    reg [17:0] ina0_reg_async,ina0_reg_sync,ina1_reg_async,ina1_reg_sync,inb_reg_async,inb_reg_sync;
    reg [17:0] sdob_0;
    wire grstn = GSR.GSRO;

    always @(ina0_reg_sync or ina0_reg_async or inb_reg_sync or inb_reg_async or ina1_reg_async or ina1_reg_sync)
    begin
        if (PADD_RESET_MODE == "ASYNC") begin
            ina0_reg <= ina0_reg_async;
            ina1_reg <= ina1_reg_async;
            inb_reg <= inb_reg_async;
        end
        else if (PADD_RESET_MODE == "SYNC") begin
            ina0_reg <= ina0_reg_sync;
            ina1_reg <= ina1_reg_sync;
            inb_reg <= inb_reg_sync;
        end
    end

    always @(SI or A or ASEL)   
    begin
        if (ASEL == 1'b1) begin
            ina0 = SI;
        end else if(ASEL == 1'b0) begin
            ina0 = A;
        end
    end

    always @(B or SBI)         
    begin
        if (BSEL_MODE == 1'b1) begin
            inb0 = SBI;
        end
        else if (BSEL_MODE == 1'b0) begin
            inb0 = B;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_async <= 0; 
            ina1_reg_async <= 0; 
        end else if (RESET == 1'b1) begin
            ina0_reg_async <= 0;
            ina1_reg_async <= 0;
        end
        else if (CE == 1'b1) begin
            ina0_reg_async <= ina0;
            ina1_reg_async <= ina1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_sync <= 0; 
            ina1_reg_sync <= 0; 
        end else if (RESET == 1'b1) begin
            ina0_reg_sync <= 0;
            ina1_reg_sync <= 0;
        end
        else if (CE == 1'b1) begin
            ina0_reg_sync <= ina0;
            ina1_reg_sync <= ina1;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0; 
        end else if (RESET == 1'b1) begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1) begin
            inb_reg_async <= inb0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0; 
        end else if (RESET == 1'b1) begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1) begin
            inb_reg_sync <= inb0;
        end
    end

    always @(ina0_reg or ina0)
    begin
        if (AREG == 1'b0) begin
            ina1 = ina0;
        end
        else begin
            ina1 = ina0_reg;
        end
    end

    always @(ina1 or ina1_reg)
    begin
        if (SOREG == 1'b0) begin
            ina2 = ina1;
        end
        else begin
            ina2 = ina1_reg;
        end
    end

    assign SO = ina2;

    always @(inb_reg or inb0)
    begin
        if (BREG == 1'b0) begin
            inb = inb0;
        end
        else begin
            inb = inb_reg;
        end
    end

    assign SBO = inb;
   
    assign DOUT = (ADD_SUB == 1'b1) ? (ina1 - inb) : (ina1 + inb);

endmodule

// PADD9
module PADD9 (DOUT, SO, SBO, A, B, SI, SBI, ASEL, CLK, CE, RESET);

input  [8:0] A;
input  [8:0] B;
input  ASEL;
input  CE,CLK,RESET;
input  [8:0] SI,SBI;
output [8:0] SO,SBO;
output [8:0] DOUT;

parameter AREG = 1'b0; // 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0; 
parameter ADD_SUB = 1'b0; //1'b0:add; 1'b1:sub
parameter PADD_RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter BSEL_MODE = 1'b1; // 1'b1: shift, 1'b0: parallel input B.
parameter SOREG = 1'b0;

    reg [8:0] inb,ina0,inb0,ina0_reg,ina1_reg,inb_reg,ina1,ina2;
    reg [8:0] ina0_reg_async,ina0_reg_sync,ina1_reg_async,ina1_reg_sync,inb_reg_async,inb_reg_sync;
    wire grstn = GSR.GSRO;

    always @(ina0_reg_sync or ina0_reg_async or inb_reg_sync or inb_reg_async or ina1_reg_async or ina1_reg_sync)
    begin
        if (PADD_RESET_MODE == "ASYNC") begin
            ina0_reg <= ina0_reg_async;
            ina1_reg <= ina1_reg_async;
            inb_reg <= inb_reg_async;
        end 
        else if (PADD_RESET_MODE == "SYNC") begin
            ina0_reg <= ina0_reg_sync;
            ina1_reg <= ina1_reg_sync;
            inb_reg <= inb_reg_sync;
        end
    end

    always @(SI or A or ASEL)   
    begin
        if (ASEL == 1'b1) begin
            ina0 = SI;
        end else if(ASEL == 1'b0) begin
            ina0 = A;
        end
    end

    always @(B or SBI)         
    begin
        if (BSEL_MODE == 1'b1) begin
            inb0 = SBI;
        end
        else if (BSEL_MODE == 1'b0) begin
            inb0 = B;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_async <= 0; 
            ina1_reg_async <= 0; 
        end else if (RESET == 1'b1) begin
            ina0_reg_async <= 0;
            ina1_reg_async <= 0;
        end
        else if (CE == 1'b1) begin
            ina0_reg_async <= ina0;
            ina1_reg_async <= ina1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_sync <= 0;
            ina1_reg_sync <= 0;
        end else if (RESET == 1'b1) begin
            ina0_reg_sync <= 0;
            ina1_reg_sync <= 0;
        end
        else if (CE == 1'b1) begin
            ina0_reg_sync <= ina0;
            ina1_reg_sync <= ina1;
        end
    end
    
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0; 
        end else if (RESET == 1'b1) begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1) begin
            inb_reg_async <= inb0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0;
        end else if (RESET == 1'b1) begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1) begin
            inb_reg_sync <= inb0;
        end
    end

    always @(ina0_reg or ina0)
    begin
        if (AREG == 1'b0) begin
            ina1 <= ina0;
        end
        else begin
            ina1 <= ina0_reg;
        end
    end

    always @(ina1 or ina1_reg)
    begin
        if (SOREG == 1'b0) begin
            ina2 = ina1;
        end
        else begin
            ina2 = ina1_reg;
        end
    end

    assign  SO = ina2;

    always @(inb_reg or inb0)
    begin
        if (BREG == 1'b0) begin
            inb = inb0;
        end
        else begin
            inb = inb_reg;
        end
    end

    assign SBO = inb;

    assign DOUT = (ADD_SUB == 1'b1) ? (ina1 - inb) : (ina1 + inb);

endmodule


// MULT9X9
module MULT9X9 (DOUT, SOA, SOB, A, B, SIA, SIB, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET);

input  [8:0] A,SIA;
input  [8:0] B,SIB;
input  ASIGN,BSIGN;
input  ASEL,BSEL;
input  CE;
input  CLK;
input  RESET;
output [17:0] DOUT;
output [8:0] SOA,SOB;

parameter AREG = 1'b0;// 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0;
parameter OUT_REG = 1'b0;
parameter PIPE_REG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter SOA_REG = 1'b0; // 1'b0: bypass mode; 1'b1: registered mode
parameter MULT_RESET_MODE = "SYNC"; // SYNC, ASYNC

    reg [8:0] ina,inb,ina_reg,inb_reg,ina1_reg,ina1,a_in,b_in;
    reg [8:0] ina_reg_async,ina_reg_sync,ina1_reg_async,ina1_reg_sync,inb_reg_async,inb_reg_sync;
    reg [17:0] a,b;
    reg asign_0,bsign_0,asign_reg0,bsign_reg0;
    reg asign_reg0_async,asign_reg0_sync,bsign_reg0_async,bsign_reg0_sync;
    wire grstn = GSR.GSRO;

    reg [17:0] out0,out1,out,out0_async,out0_sync,out_async,out_sync,d_out;
    wire [17:0] mult_out;
    
    initial begin
        d_out = 0;
    end

    // RESET mode
    always @(ina_reg_sync or ina_reg_async or inb_reg_sync or inb_reg_async or ina1_reg_async or ina1_reg_sync or asign_reg0_async or asign_reg0_sync or bsign_reg0_async or bsign_reg0_sync or out0_async or out0_sync or out_async or out_sync)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina_reg <= ina_reg_async;
            ina1_reg <= ina1_reg_async;
            inb_reg <= inb_reg_async;
            asign_reg0 <= asign_reg0_async;
            bsign_reg0 <= bsign_reg0_async;
            out0 <= out0_async;
            out <= out_async;
        end 
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina_reg <= ina_reg_sync;
            ina1_reg <= ina1_reg_sync;
            inb_reg <= inb_reg_sync;
            asign_reg0 <= asign_reg0_sync;
            bsign_reg0 <= bsign_reg0_sync;
            out0 <= out0_sync;
            out <= out_sync;
        end
    end

    always @(ASEL or A or SIA)
    begin
        if (ASEL == 1'b0) begin
            a_in = A;
        end else if (ASEL == 1'b1) begin
            a_in= SIA;
        end
    end

    always @(BSEL or B or SIB)
    begin
        if (BSEL == 1'b0) begin
            b_in = B;
        end else if (BSEL == 1'b1) begin
            b_in = SIB;
        end
    end
           
    // input reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_async <= 0;
            ina1_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            ina_reg_async <= 0;
            ina1_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_async <= a_in;
            ina1_reg_async <= ina;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_sync <= 0;
            ina1_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina_reg_sync <= 0;
            ina1_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_sync <= a_in;
            ina1_reg_sync <= ina;
        end
    end   

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_async <= b_in;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_sync <= b_in;
        end
    end

    always @(ina_reg or a_in)
    begin
        if (AREG == 1'b0)
        begin
            ina = a_in;
        end else
        begin
            ina = ina_reg;
        end
    end

    always @(ina1_reg or ina)
    begin
        if (SOA_REG == 1'b0) begin
            ina1 = ina;
        end else if (SOA_REG == 1'b1) begin
            ina1 = ina1_reg;
        end
    end

    assign SOA = ina1;

    always @(inb_reg or b_in)
    begin
        if (BREG == 1'b0)
        begin
            inb = b_in;
        end else
        begin
            inb = inb_reg;
        end
    end

    assign SOB = inb;
    
    // mult operation: asign,bsign->Sign bit
    always @(ina or asign_0)
    begin
        if (asign_0 == 1'b1)
        begin
            a[8:0] = ina[8:0];
            a[17:9] = {ina[8],ina[8],ina[8],ina[8],ina[8],ina[8],ina[8],ina[8],ina[8]};
        end else
        begin
            a[8:0] =  ina[8:0];
            a[17:9] = 0;
        end
    end

    always @(inb or bsign_0)
    begin
        if (bsign_0 == 1'b1)
        begin
            b[8:0] = inb[8:0];
            b[17:9] = {inb[8],inb[8],inb[8],inb[8],inb[8],inb[8],inb[8],inb[8],inb[8]};
        end else
        begin
            b[8:0] = inb[8:0];
            b[17:9] = 0;
        end
    end

    assign mult_out = (!a || !b)? 0 : a * b ;

    // sign reg 
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            asign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_sync <= ASIGN;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_sync <= BSIGN;
        end
    end
    
    always @(ASIGN or asign_reg0)
    begin
        if (ASIGN_REG == 1'b0) begin
            asign_0 = ASIGN;
        end else begin
            asign_0 = asign_reg0;
        end
    end

    always @(BSIGN or bsign_reg0)
    begin
        if (BSIGN_REG == 1'b0) begin
            bsign_0 = BSIGN;
        end else begin
            bsign_0 = bsign_reg0;
        end
    end

    // pipeline reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_async <= mult_out;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_sync <= mult_out;
        end
    end

    always @(mult_out or out0)
    begin
        if (PIPE_REG == 1'b0)
        begin
            out1 = mult_out;
        end else
        begin
            out1 = out0;
        end
    end
    
    // output reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out_async <= 0;
        end else if (RESET == 1'b1) begin
            out_async <= 0;
        end else if (CE == 1'b1) begin
            out_async <= out1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out_sync <= 0;
        end else if (RESET == 1'b1) begin
            out_sync <= 0;
        end else if (CE == 1'b1) begin
            out_sync <= out1;
        end
    end

    always @(out1 or out)
    begin
        if (OUT_REG == 1'b0) begin
            d_out = out1;
        end else begin
            d_out = out;
        end
    end

    assign DOUT = d_out;

endmodule

//MULT18X18
module MULT18X18 (DOUT, SOA, SOB, A, B, SIA, SIB, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET);

input  [17:0] A,SIA;
input  [17:0] B,SIB;
input  ASIGN,BSIGN;
input  ASEL,BSEL;
input  CE;
input  CLK;
input  RESET;
output [35:0] DOUT;
output [17:0] SOA,SOB;

parameter AREG = 1'b0;// 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0;
parameter OUT_REG = 1'b0;
parameter PIPE_REG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter SOA_REG = 1'b0;
parameter MULT_RESET_MODE = "SYNC"; // SYNC, ASYNC

    reg [17:0] ina,inb,ina_reg,inb_reg,ina1_reg,ina1,a_in,b_in;
    reg [17:0] ina_reg_async,ina_reg_sync,ina1_reg_async,ina1_reg_sync,inb_reg_async,inb_reg_sync;
    reg [35:0] a,b;
    reg asign_0,bsign_0,asign_reg0,bsign_reg0;
    reg asign_reg0_async,asign_reg0_sync,bsign_reg0_async,bsign_reg0_sync;
    wire grstn = GSR.GSRO;

    reg [35:0] out0,out1,out,out0_async,out0_sync,out_async,out_sync,m_out;
    wire [35:0] mult_out;

    initial
    begin
        m_out = 0;
    end
    
    // RESET mode
    always @(ina_reg_sync or ina_reg_async or ina1_reg_sync or ina1_reg_async or inb_reg_sync or inb_reg_async or asign_reg0_async or asign_reg0_sync or bsign_reg0_async or bsign_reg0_sync or out0_async or out0_sync or out_async or out_sync)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina_reg <= ina_reg_async;
            ina1_reg <= ina1_reg_async;
            inb_reg <= inb_reg_async;
            asign_reg0 <= asign_reg0_async;
            bsign_reg0 <= bsign_reg0_async;
            out0 <= out0_async;
            out <= out_async;
        end 
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina_reg <= ina_reg_sync;
            ina1_reg <= ina1_reg_sync;
            inb_reg <= inb_reg_sync;
            asign_reg0 <= asign_reg0_sync;
            bsign_reg0 <= bsign_reg0_sync;
            out0 <= out0_sync;
            out <= out_sync;
        end
    end

    always @(ASEL or A or SIA)
    begin
        if (ASEL == 1'b0) begin
            a_in = A;
        end else if (ASEL == 1'b1) begin
            a_in = SIA;
        end
    end

    always @(BSEL or B or SIB)
    begin
        if (BSEL == 1'b0) begin
            b_in = B;
        end else if (BSEL == 1'b1) begin
            b_in = SIB;
        end
    end

    // input reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_async <= 0;
            ina1_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina_reg_async <= 0;
            ina1_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_async <= a_in;
            ina1_reg_async <= ina;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_sync <= 0;
            ina1_reg_sync <= 0;   
        end else if (RESET == 1'b1)
        begin
            ina_reg_sync <= 0;
            ina1_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_sync <= a_in;
            ina1_reg_sync <= ina;          
        end
    end    

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_async <= b_in;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_sync <= b_in;
        end
    end

    always @(ina_reg or a_in)
    begin
        if (AREG == 1'b0)
        begin
            ina = a_in;
        end else
        begin
            ina = ina_reg;
        end
    end

    always @(ina1_reg or ina)
    begin
        if (SOA_REG == 1'b0)
        begin
            ina1 = ina;
        end else
        begin
            ina1 = ina1_reg;
        end
    end

    assign SOA = ina1;

    always @(inb_reg or b_in)
    begin
        if (BREG == 1'b0)
        begin
            inb = b_in;
        end else
        begin
            inb = inb_reg;
        end
    end

    assign SOB = inb;

    // mult operation: asign,bsign->Sign bit
    always @(ina or asign_0)
    begin
        if (asign_0 == 1'b1)
        begin
            a[17:0] = ina[17:0];
            a[35:18] = { ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17]};
        end else
        begin
            a[17:0] =  ina[17:0];
            a[35:18] = 0;
        end
    end

    always @(inb or bsign_0)
    begin
        if (bsign_0 == 1'b1)
        begin
            b[17:0] = inb[17:0];
            b[35:18] = {inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17],inb[17]};
        end else
        begin
            b[17:0] = inb[17:0];
            b[35:18] = 0;
        end
    end

    assign mult_out = (!a || !b)? 0 : a * b ;

    // sign reg 
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            asign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_sync <= ASIGN;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_sync <= BSIGN;
        end
    end
    
    always @(ASIGN or asign_reg0)
    begin
        if (ASIGN_REG == 1'b0) begin
            asign_0 = ASIGN;
        end else begin
            asign_0 = asign_reg0;
        end
    end 

    always @(BSIGN or bsign_reg0)
    begin
        if (BSIGN_REG == 1'b0) begin
            bsign_0 = BSIGN;
        end else begin
            bsign_0 = bsign_reg0;
        end
    end

    // pipeline reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_async <= mult_out;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_sync <= mult_out;
        end
    end

    always @(mult_out or out0)
    begin
        if (PIPE_REG == 1'b0)
        begin
            out1 = mult_out;
        end else
        begin
            out1 = out0;
        end
    end
    
    // output reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out_async <= 0;
        end else if (RESET == 1'b1) begin
            out_async <= 0;
        end else if (CE == 1'b1) begin
            out_async <= out1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out_sync <= 0;
        end else if (RESET == 1'b1) begin
            out_sync <= 0;
        end else if (CE == 1'b1) begin
            out_sync <= out1;
        end
    end

    always @(out1 or out)
    begin
        if (OUT_REG == 1'b0) begin
            m_out = out1;
        end else begin
            m_out = out;
        end
    end

    assign DOUT = m_out;

endmodule

//MULTALU section
// MULT36X36
module MULT36X36 (DOUT, A, B, ASIGN, BSIGN, CLK, CE, RESET);

input  [35:0] A;
input  [35:0] B;
input  ASIGN,BSIGN;
input  CE;
input  CLK;
input  RESET;
output [71:0] DOUT;

parameter AREG = 1'b0;// 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0;
parameter OUT0_REG = 1'b0;
parameter OUT1_REG = 1'b0;
parameter PIPE_REG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter MULT_RESET_MODE = "SYNC"; // SYNC, ASYNC

    reg [35:0] ina,ina_reg,a_in,ina_reg_async,ina_reg_sync;
    reg [35:0] b_in,inb,inb_reg,inb_reg_async,inb_reg_sync;
    reg [71:0] a,b;
    reg asign_0,bsign_0,asign_reg0,bsign_reg0,asign_reg1,bsign_reg1,asign_1,bsign_1;
    reg asign_reg0_async,asign_reg0_sync,bsign_reg0_async,bsign_reg0_sync;
    wire grstn = GSR.GSRO;

    wire [71:0] mult_out;
    reg [71:0] out_pipe_reg_async,out_pipe_reg_sync,out_pipe_reg,out_pipe,out0_reg_async,out0_reg_sync,out0_reg,m_out0,m_out;
    reg [71:18] out1_reg_async,out1_reg_sync,out1_reg,m_out1;


    initial
    begin
       m_out = 0;
    end
    
    // RESET mode
    always @(ina_reg_sync or ina_reg_async or inb_reg_sync or inb_reg_async or asign_reg0_async or asign_reg0_sync or bsign_reg0_async or bsign_reg0_sync or out_pipe_reg_async or out_pipe_reg_sync or out0_reg_async or out0_reg_sync or out1_reg_async or out1_reg_sync)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina_reg <= ina_reg_async;
            inb_reg <= inb_reg_async;
            asign_reg0 <= asign_reg0_async;
            bsign_reg0 <= bsign_reg0_async;
            out_pipe_reg <= out_pipe_reg_async;
            out0_reg <= out0_reg_async;
            out1_reg <= out1_reg_async;
        end 
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina_reg <= ina_reg_sync;
            inb_reg <= inb_reg_sync;
            asign_reg0 <= asign_reg0_sync;
            bsign_reg0 <= bsign_reg0_sync;
            out_pipe_reg <= out_pipe_reg_sync;
            out0_reg <= out0_reg_sync;
            out1_reg <= out1_reg_sync;
        end
    end

    // input reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_async <= A;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_sync <= A;
        end
    end    

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_async <= B;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_sync <= B;
        end
    end

    always @(ina_reg or A)
    begin
        if (AREG == 1'b0)
        begin
            ina = A;
        end else 
        begin
            ina = ina_reg;
        end
    end

    always @(inb_reg or B)
    begin
        if (BREG == 1'b0)
        begin
            inb = B;
        end else
        begin
            inb = inb_reg;
        end
    end
    
    // mult operation: asign,bsign->Sign bit
    always @(ina or asign_0)
    begin
        if (asign_0 == 1'b1)
        begin
            a[35:0] = ina[35:0];
            a[71:36] = { ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35],ina[35]};
        end else
        begin
            a[35:0] =  ina[35:0];
            a[71:36] = 0;
        end
    end

    always @(inb or bsign_0)
    begin
        if (bsign_0 == 1'b1)
        begin
            b[35:0] = inb[35:0];
            b[71:36] = {inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35]};
        end else
        begin
            b[35:0] = inb[35:0];
            b[71:36] = 0;
        end
    end
    
    assign mult_out = (!a || !b)? 0 : a * b ;

    // sign reg 
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_sync <= ASIGN;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_sync <= BSIGN;
        end
    end
    
    always @(ASIGN or asign_reg0)
    begin
        if (ASIGN_REG == 1'b0)
        begin
            asign_0 = ASIGN;
        end else 
        begin
            asign_0 = asign_reg0;
        end
    end

    always @(BSIGN or bsign_reg0)
    begin
        if (BSIGN_REG == 1'b0)
        begin
            bsign_0 = BSIGN;
        end else
        begin
            bsign_0 = bsign_reg0;
        end
    end

    // pipeline reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out_pipe_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out_pipe_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            out_pipe_reg_async <= mult_out;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out_pipe_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out_pipe_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            out_pipe_reg_sync <= mult_out;
        end
    end

    always @(mult_out or out_pipe_reg)
    begin
        if (PIPE_REG == 1'b0)
        begin
            out_pipe = mult_out;
        end else
        begin
            out_pipe = out_pipe_reg;
        end
    end
    
    // output reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) 
        begin
            out0_reg_async <= 0;
        end else if (RESET == 1'b1) 
        begin
            out0_reg_async <= 0;
        end else if (CE == 1'b1) 
        begin
            out0_reg_async <= out_pipe;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) 
        begin
            out0_reg_sync <= 0;
        end else if (RESET == 1'b1) 
        begin
            out0_reg_sync <= 0;
        end else if (CE == 1'b1) 
        begin
            out0_reg_sync <= out_pipe;
        end
    end

    always @(out_pipe or out0_reg)
    begin
        if (OUT0_REG == 1'b0) 
        begin
            m_out0 = out_pipe;
        end else 
        begin
            m_out0 = out0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn)
        begin
            out1_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out1_reg_async <= 0;
        end else if (CE == 1'b1)
        begin
            out1_reg_async <= m_out0[71:18];
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn)
        begin
            out1_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out1_reg_sync <= 0;
        end else if (CE == 1'b1)
        begin
            out1_reg_sync <= m_out0[71:18];
        end
    end

    always @(m_out0 or out1_reg)
    begin
        if (OUT1_REG == 1'b0)
        begin
            m_out1 = m_out0[71:18];
        end else
        begin
            m_out1 = out1_reg;
        end
    end

    always @(m_out0 or m_out1)
    begin
        m_out = {m_out1, m_out0[17:0]};
    end

    assign DOUT = m_out;

endmodule


//MULTALU36X18
module MULTALU36X18 (DOUT, CASO, A, B, C, CASI, ACCLOAD, ASIGN, BSIGN, CLK, CE, RESET);

input  [17:0] A;
input  [35:0] B;
input  [53:0] C;
input  ASIGN,BSIGN,ACCLOAD;
input  CE;
input  CLK;
input  RESET;
input  [54:0] CASI;
output [53:0] DOUT;
output [54:0] CASO;

parameter AREG = 1'b0;// 1'b0: bypass mode, 1'b1: registered mode
parameter BREG = 1'b0;
parameter CREG = 1'b0;
parameter OUT_REG = 1'b0;
parameter PIPE_REG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter ACCLOAD_REG0 = 1'b0;
parameter ACCLOAD_REG1 = 1'b0;
parameter MULT_RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter MULTALU36X18_MODE = 0; //0:36x18 +/- C; 1:ACC/0 + 36x18; 2: 36x18 + CASI
parameter C_ADD_SUB = 1'b0; //1'b0: add; 1'b1: sub.

    reg [17:0] ina,ina_reg,ina_reg_async,ina_reg_sync;
    reg [35:0] b_in,inb,inb_reg,inb_reg_async,inb_reg_sync;
    reg [53:0] a,b,inc,inc_reg,inc_reg_sync,inc_reg_async;
    reg  asign_0,bsign_0,asign_reg0,bsign_reg0,absign_reg,absign;
    reg asign_reg0_async,asign_reg0_sync,bsign_reg0_async,bsign_reg0_sync,absign_reg_async,absign_reg_sync;
    reg accload_0, accload0_reg, accload1_reg,accload_1;
    reg alu_sel;
    reg alu_sel0_reg, alu_sel0, alu_sel1_reg,alu_sel1;
    wire grstn = GSR.GSRO;
    wire absign_0;

    reg [53:0] out0,out1,out_reg,out0_async,out0_sync;
    wire [53:0] mult_out;
    wire [54:0] out_ext,inc_ext,acc_load;
    reg [54:0] dout,out_async,out_sync,m_out;

    initial
    begin
        m_out = 0;
        alu_sel = 1'b1;
        alu_sel0_reg = 1'b0;
        alu_sel1_reg = 1'b0;
    end
    
    // RESET mode
    always @(ina_reg_sync or ina_reg_async or inb_reg_sync or inb_reg_async or inc_reg_sync or inc_reg_async or asign_reg0_async or asign_reg0_sync or bsign_reg0_async or bsign_reg0_sync or absign_reg_async or absign_reg_sync or out0_async or out0_sync or out_async or out_sync)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina_reg <= ina_reg_async;
            inb_reg <= inb_reg_async;
            inc_reg <= inc_reg_async;
            asign_reg0 <= asign_reg0_async;
            bsign_reg0 <= bsign_reg0_async;
            absign_reg <= absign_reg_async;
            out0 <= out0_async;
            out_reg <= out_async;
        end 
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina_reg <= ina_reg_sync;
            inb_reg <= inb_reg_sync;
            inc_reg <= inc_reg_sync;
            asign_reg0 <= asign_reg0_sync;
            bsign_reg0 <= bsign_reg0_sync;
            absign_reg <= absign_reg_sync;
            out0 <= out0_sync;
            out_reg <= out_sync;
        end
    end

    // input reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            ina_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_async <= A;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            ina_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina_reg_sync <= A;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            inb_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_async <= B;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            inb_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb_reg_sync <= B;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            inc_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_async <= C;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inc_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_sync <= C;
        end
    end

    always @(ina_reg or A)
    begin
        if (AREG == 1'b0)
        begin
            ina = A;
        end else
        begin
            ina = ina_reg;
        end
    end

    always @(inb_reg or B)
    begin
        if (BREG == 1'b0)
        begin
            inb = B;
        end else
        begin
            inb = inb_reg;
        end
    end

    always @(inc_reg or C)
    begin
        if (CREG == 1'b0)
        begin
            inc = C;
        end else
        begin
            inc = inc_reg;
        end
    end
    
    // mult operation: asign,bsign->Sign bit
    always @(ina or asign_0)
    begin
        if (asign_0 == 1'b1)
        begin
            a[17:0] = ina[17:0];
            a[53:18] = {ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17],ina[17]};
        end else
        begin
            a[17:0] = ina[17:0];
            a[53:18] = 0;
        end
    end
    
    always @(inb or bsign_0)
    begin
        if (bsign_0 == 1'b1)
        begin
            b[35:0] = inb[35:0];
            b[53:36] = {inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35],inb[35]};
        end else
        begin
            b[35:0] =  inb[35:0];
            b[53:36] = 0;
        end
    end
    
    assign mult_out = (!a || !b)? 0 : a * b;

    // sign reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            asign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign_reg0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            asign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign_reg0_sync <= ASIGN;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign_reg0_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign_reg0_sync <= BSIGN;
        end
    end
    
    always @(ASIGN or asign_reg0)
    begin
        if (ASIGN_REG == 1'b0)
        begin
            asign_0 = ASIGN;
        end else 
        begin
            asign_0 = asign_reg0;
        end
    end 

    always @(BSIGN or bsign_reg0)
    begin
        if (BSIGN_REG == 1'b0)
        begin
            bsign_0 = BSIGN;
        end else begin
            bsign_0 = bsign_reg0;
        end
    end

    assign absign_0 = asign_0 || bsign_0;

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel0_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel0_reg <= alu_sel;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel1_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel1_reg <= alu_sel0;
        end
    end
    
    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload0_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload0_reg <= ACCLOAD;
        end
    end
 
    always @(ACCLOAD or accload0_reg or alu_sel or alu_sel0_reg)
    begin
        if (ACCLOAD_REG0 == 1'b0)
        begin
            accload_0 <= ACCLOAD;
            alu_sel0 <= alu_sel;
        end else
        begin
            accload_0 <= accload0_reg;
            alu_sel0 <= alu_sel0_reg;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload1_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload1_reg <= accload_0;
        end
    end
 
    always @(accload_0 or accload1_reg or alu_sel0 or alu_sel1_reg)
    begin
        if (ACCLOAD_REG1 == 1'b0)
        begin
            accload_1 <= accload_0;
            alu_sel1 <= alu_sel0;
        end else 
        begin
            accload_1 <= accload1_reg;
            alu_sel1 <= alu_sel1_reg;
        end
    end
 
    //pipeline reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out0_async <= 0;
            absign_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_async <= 0;
            absign_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_async <= mult_out;
            absign_reg_async <= absign_0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out0_sync <= 0;
            absign_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out0_sync <= 0;
            absign_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            out0_sync <= mult_out;
            absign_reg_sync <= absign_0;
        end
    end

    always @(mult_out or out0 or absign_0 or absign_reg)
    begin
        if (PIPE_REG == 1'b0)
        begin
            out1 = mult_out;
            absign = absign_0;
        end else
        begin
            out1 = out0;
            absign = absign_reg;
        end
    end
    
    assign inc_ext = (alu_sel1) ? {1'b0, inc[53:0]} : 0;
    assign out_ext = (alu_sel1) ? {(out1[53] & absign),out1[53:0]} : 0;
    assign acc_load = (accload_1) ? m_out : 0;
    
    always @ (inc_ext or CASI or out_ext or acc_load)
    begin
        if(MULTALU36X18_MODE == 0) begin   //36x18 +/- C
            if(C_ADD_SUB == 1'b0) begin
                dout = out_ext + inc_ext;
            end else begin
                dout = out_ext - inc_ext;
            end
        end else if(MULTALU36X18_MODE == 1) begin   //ACC/0 + 36x18
            dout = acc_load + out_ext;
        end else if (MULTALU36X18_MODE == 2) begin  //36x18 + CASI
            dout = out_ext + CASI;
        end
    end

    // output reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out_async <= 0;
        end else if (CE == 1'b1)
        begin
            out_async <= dout;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out_sync <= 0;
        end else if (CE == 1'b1) begin
            out_sync <= dout;
        end
    end

    always @(dout or out_reg)
    begin
        if (OUT_REG == 1'b0)
        begin
            m_out = dout;
        end else begin
            m_out = out_reg;
        end
    end

    assign DOUT = m_out[53:0];
    assign CASO = {m_out[53] & absign,m_out[53:0]};

endmodule


// MULTADDALU18X18
module MULTADDALU18X18 (DOUT, CASO, SOA, SOB, A0, B0, A1, B1, C, SIA, SIB, CASI, ACCLOAD, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET);

input [17:0] A0;
input [17:0] B0;
input [17:0] A1;
input [17:0] B1;
input [53:0] C;
input [17:0] SIA, SIB;
input [1:0] ASIGN, BSIGN;
input [1:0] ASEL, BSEL;
input [54:0] CASI;
input CE;
input CLK;
input RESET;
input ACCLOAD;
output [53:0] DOUT;
output [54:0] CASO;
output [17:0] SOA, SOB;

parameter A0REG = 1'b0; // 1'b0: bypass mode; 1'b1: registered mode
parameter A1REG = 1'b0;
parameter B0REG = 1'b0;
parameter B1REG = 1'b0;
parameter CREG = 1'b0;
parameter PIPE0_REG = 1'b0;
parameter PIPE1_REG = 1'b0;
parameter OUT_REG = 1'b0;
parameter ASIGN0_REG = 1'b0;
parameter ASIGN1_REG = 1'b0;
parameter ACCLOAD_REG0 = 1'b0;
parameter ACCLOAD_REG1 = 1'b0;
parameter BSIGN0_REG = 1'b0;
parameter BSIGN1_REG = 1'b0;
parameter SOA_REG = 1'b0;
parameter B_ADD_SUB = 1'b0; // 1'b0:add; 1'b1:sub
parameter C_ADD_SUB = 1'b0;
parameter MULTADDALU18X18_MODE = 0;//0:18x18 +/- 18x18 +/- C;  1: ACC/0 + 18x18 +/- 18x18; 2:18x18 +/- 18x18 + CASI
parameter MULT_RESET_MODE = "SYNC";// SYNC,ASYNC

    reg asign0_0_reg_async,asign0_0_reg_sync,asign1_0_reg_async,asign1_0_reg_sync,absign_0_reg_async,absign_0_reg_sync,absign_1_reg_async,absign_1_reg_sync;
    reg asign0_0_reg,asign1_0_reg,absign_0_reg,absign_1_reg, asign0_0,asign1_0,absign0_0,absign1_0,absign_0,absign_1;
    reg bsign0_0_reg_async,bsign0_0_reg_sync,bsign1_0_reg_async,bsign1_0_reg_sync;
    reg bsign0_0,bsign0_0_reg,bsign1_0_reg,bsign1_0;
    reg accload_0, accload0_reg, accload1_reg,accload_1;
    reg alu_sel;
    reg alu_sel0, alu_sel0_reg, alu_sel1_reg,alu_sel1;

    reg  [17:0] mina0,mina1,minb0,minb1,ina0_reg, inb0_reg,ina1_reg, inb1_reg,ina2_reg;
    reg  [17:0] ina0, inb0,ina1, inb1,ina2;
    reg [17:0] ina0_reg_async,ina0_reg_sync,ina1_reg_async,ina1_reg_sync,inb0_reg_async,inb0_reg_sync,inb1_reg_async,inb1_reg_sync,ina2_reg_async,ina2_reg_sync;
    reg [53:0] inc_reg_async,inc_reg_sync,inc_reg,inc;
    wire [35:0] mult_out0,mult_out1;
    reg [54:0] out0_0,out1_0;

    reg [35:0] out0_reg_async,out0_reg_sync,out1_reg_async,out1_reg_sync,out0,out0_reg,out1_reg,out1;
    wire absign,absign_0_0,absign_1_0;
    reg [54:0] dout,m_out,out_reg,out_sync,out_async;
    wire [54:0] inc_ext,acc_load;
    reg [35:0] a0, b0,a1, b1;
    wire grstn = GSR.GSRO;

    initial begin
        alu_sel = 1'b1;
        alu_sel0_reg = 1'b0;
        alu_sel1_reg = 1'b0;
    end

    always @(ina0_reg_async or ina0_reg_sync or ina1_reg_async or ina1_reg_sync or ina2_reg_async or ina2_reg_sync or inb0_reg_async or inb0_reg_sync or inb1_reg_async or inb1_reg_sync or inc_reg_async or inc_reg_sync or asign0_0_reg_async or asign0_0_reg_sync or absign_0_reg_async or absign_0_reg_sync or absign_1_reg_async or absign_1_reg_sync or asign1_0_reg_async or asign1_0_reg_sync or bsign0_0_reg_async or bsign0_0_reg_sync or bsign1_0_reg_async or bsign1_0_reg_sync or out0_reg_async or out0_reg_sync or out1_reg_async or out1_reg_sync or out_sync or out_async)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina0_reg <= ina0_reg_async;
            ina1_reg <= ina1_reg_async;
            ina2_reg <= ina2_reg_async;
            inb0_reg <= inb0_reg_async;
            inb1_reg <= inb1_reg_async;
            inc_reg <= inc_reg_async;
            asign0_0_reg <= asign0_0_reg_async;
            asign1_0_reg <= asign1_0_reg_async;
            bsign0_0_reg <= bsign0_0_reg_async;
            bsign1_0_reg <= bsign1_0_reg_async;
            absign_0_reg <= absign_0_reg_async;
            absign_1_reg <= absign_1_reg_async;
            out0_reg <= out0_reg_async;
            out1_reg <= out1_reg_async;
            out_reg <= out_async;
        end
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina0_reg <= ina0_reg_sync;
            ina1_reg <= ina1_reg_sync;
            ina2_reg <= ina2_reg_sync;
            inb0_reg <= inb0_reg_sync;
            inb1_reg <= inb1_reg_sync;
            inc_reg <= inc_reg_sync;
            asign0_0_reg <= asign0_0_reg_sync;
            asign1_0_reg <= asign1_0_reg_sync;
            bsign0_0_reg <= bsign0_0_reg_sync;
            bsign1_0_reg <= bsign1_0_reg_sync;
            absign_0_reg <= absign_0_reg_sync;
            absign_1_reg <= absign_1_reg_sync;
            out0_reg <= out0_reg_sync;
            out1_reg <= out1_reg_sync;
            out_reg <= out_sync;
        end
    end

    always @ (A0 or SIA or ASEL) 
    begin
        if(ASEL[0] == 1'b0) 
        begin
            mina0 = A0;
        end else begin
            mina0 = SIA;
        end
    end

    // in reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_async <= mina0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_sync <= mina0;
        end
    end

    always @ (ina0, A1, ASEL)
    begin
        if(ASEL[1] == 1'b0) 
        begin
            mina1 = A1;
        end else begin
            mina1 = ina0;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina1_reg_async <= 0;
            ina2_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            ina1_reg_async <= 0;
            ina2_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina1_reg_async <= mina1;
            ina2_reg_async <= ina1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina1_reg_sync <= 0; 
            ina2_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina1_reg_sync <= 0;
            ina2_reg_sync <= 0; 
        end
        else if (CE == 1'b1)
        begin
            ina1_reg_sync <= mina1;
            ina2_reg_sync <= ina1; 
        end
    end

    always @(mina0 or ina0_reg)
    begin
        if (A0REG == 1'b0)
        begin
            ina0 = mina0;
        end else
        begin
            ina0 = ina0_reg;
        end
    end

    always @(mina1 or ina1_reg)
    begin
        if (A1REG == 1'b0)
        begin
            ina1 = mina1;
        end else
        begin
            ina1 = ina1_reg;
        end
    end

    always @(ina1 or ina2_reg)
    begin
        if (SOA_REG == 1'b0)
        begin
            ina2 = ina1;
        end else
        begin
            ina2 = ina2_reg;
        end
    end

    assign SOA = ina2;

    always @(B0 or SIB or BSEL) 
    begin
        if(BSEL[0] == 1'b0) 
        begin
            minb0 = B0;
        end else begin
            minb0 = SIB;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_async <= minb0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_sync <= minb0;
        end
    end

    always @ (B1 or inb0 or BSEL)
    begin
        if(BSEL[1] == 1'b0)
        begin
            minb1 = B1;
        end else begin
            minb1 = inb0;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb1_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            inb1_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb1_reg_async <= minb1;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb1_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            inb1_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb1_reg_sync <= minb1;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inc_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_async <= C;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inc_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_sync <= C;
        end
    end
    
    always @(minb0 or inb0_reg)
    begin
        if (B0REG == 1'b0)
        begin
            inb0 = minb0;
        end else 
        begin
            inb0 = inb0_reg;
        end
    end

    always @(minb1 or inb1_reg)
    begin
        if (B1REG == 1'b0)
        begin
            inb1 = minb1;
        end else begin
            inb1 = inb1_reg;
        end
    end

    assign SOB = inb1;

    always @(C or inc_reg)
    begin
        if (CREG == 1'b0)
        begin
            inc = C;
        end else begin
            inc = inc_reg;
        end
    end

    //asign reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign0_0_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign0_0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_0_reg_async <= ASIGN[0];
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign0_0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign0_0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_0_reg_sync <= ASIGN[0];
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign1_0_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign1_0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign1_0_reg_async <= ASIGN[1];
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign1_0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign1_0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign1_0_reg_sync <= ASIGN[1];
        end
    end

    always @(ASIGN[0] or asign0_0_reg)
    begin
        if (ASIGN0_REG == 1'b0)
        begin
            asign0_0 = ASIGN[0];
        end else
        begin
            asign0_0 = asign0_0_reg;
        end
    end

    always @(ASIGN[1] or asign1_0_reg)
    begin
        if (ASIGN1_REG == 1'b0)
        begin
            asign1_0 = ASIGN[1];
        end else 
        begin
            asign1_0 = asign1_0_reg;
        end
    end

    //bsign reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_0_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign0_0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_0_reg_async <= BSIGN[0];
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign0_0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_0_reg_sync <= BSIGN[0];
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign1_0_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign1_0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign1_0_reg_async <= BSIGN[1];
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign1_0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign1_0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign1_0_reg_sync <= BSIGN[1];
        end
    end

    always @(BSIGN[0] or bsign0_0_reg)
    begin
        if (BSIGN0_REG == 1'b0)
        begin
            bsign0_0 = BSIGN[0];
        end else
        begin
            bsign0_0 = bsign0_0_reg;
        end
    end

    always @(BSIGN[1] or bsign1_0_reg)
    begin
        if (BSIGN1_REG == 1'b0)
        begin
            bsign1_0 = BSIGN[1];
        end else begin
            bsign1_0 = bsign1_0_reg;
        end
    end

    assign absign_0_0 = asign0_0 || bsign0_0;
    assign absign_1_0 = asign1_0 || bsign1_0;

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel0_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel0_reg <= alu_sel;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel1_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel1_reg <= alu_sel0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload0_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload0_reg <= ACCLOAD;
        end
    end
 
    always @(ACCLOAD or accload0_reg or alu_sel or alu_sel0_reg)
    begin
        if (ACCLOAD_REG0 == 1'b0)
        begin
            accload_0 <= ACCLOAD;
            alu_sel0 <= alu_sel;
        end else
        begin
            accload_0 <= accload0_reg;
            alu_sel0 <= alu_sel0_reg;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload1_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload1_reg <= accload_0;
        end
    end
 
    always @(accload_0 or accload1_reg or alu_sel0 or alu_sel1_reg)
    begin
        if (ACCLOAD_REG1 == 1'b0)
        begin
            accload_1 <= accload_0;
            alu_sel1 <= alu_sel0;
        end else 
        begin
            accload_1 <= accload1_reg;
            alu_sel1 <= alu_sel1_reg;
        end
    end

    always @(ina0 or asign0_0)
    begin
        if (asign0_0 == 1'b1)
        begin
            a0[17:0] = ina0[17:0];
            a0[35:18] = {ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17],ina0[17]};
        end else
        begin
            a0[17:0] =  ina0[17:0];
            a0[35:18] = 0;
        end
    end

    always @(ina1 or asign1_0)
    begin
        if (asign1_0 == 1'b1)
        begin
            a1[17:0] = ina1[17:0];
            a1[35:18] = {ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17]};
        end else
        begin
            a1[17:0] =  ina1[17:0];
            a1[35:18] = 0;
        end
    end

    always @(inb0 or bsign0_0)
    begin
        if (bsign0_0 == 1'b1)
        begin
            b0[17:0] = inb0[17:0];
            b0[35:18] = {inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17],inb0[17]};
        end else 
        begin
            b0[17:0] = inb0[17:0];
            b0[35:18] = 0;
        end
    end

    always @(inb1 or bsign1_0)
    begin
        if (bsign1_0 == 1'b1)
        begin
            b1[17:0] = inb1[17:0];
            b1[35:18] = {inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17]};
        end else 
        begin
            b1[17:0] = inb1[17:0];
            b1[35:18] = 0;
        end
    end

    assign mult_out0 = (!a0 || !b0)? 0 : a0 * b0 ;
    assign mult_out1 = (!a1 || !b1)? 0 : a1 * b1 ;

    // pipeline reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out0_reg_async <= 0;
            absign_0_reg_async <= 1'b0;
        end else if (RESET == 1'b1)
        begin
            out0_reg_async <= 0;
            absign_0_reg_async <= 1'b0;
        end
        else if (CE == 1'b1)
        begin
            out0_reg_async <= mult_out0;
            absign_0_reg_async <= absign_0_0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out0_reg_sync <= 0;
            absign_0_reg_sync <= 1'b0;
        end else if (RESET == 1'b1)
        begin
            out0_reg_sync <= 0;
            absign_0_reg_sync <= 1'b0;
        end
        else if (CE == 1'b1)
        begin
            out0_reg_sync <= mult_out0;
            absign_0_reg_sync <= absign_0_0;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out1_reg_async <= 0;
            absign_1_reg_async <= 1'b0;
        end else if (RESET == 1'b1)
        begin
            out1_reg_async <= 0;
            absign_1_reg_async <= 1'b0;
        end
        else if (CE == 1'b1)
        begin
            out1_reg_async <= mult_out1;
            absign_1_reg_async <= absign_1_0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out1_reg_sync <= 0;
            absign_1_reg_sync <= 1'b0;
        end else if (RESET == 1'b1)
        begin
            out1_reg_sync <= 0;
            absign_1_reg_sync <= 1'b0;
        end
        else if (CE == 1'b1)
        begin
            out1_reg_sync <= mult_out1;
            absign_1_reg_sync <= absign_1_0;
        end
    end

    always @(mult_out0 or out0_reg or absign_0_0 or absign_0_reg)
    begin
        if (PIPE0_REG == 1'b0)
        begin
            out0 = mult_out0;
            absign_0 = absign_0_0;
        end else
        begin
            out0 = out0_reg;
            absign_0 = absign_0_reg;
        end
    end

    always @(mult_out1 or out1_reg or absign_1_0 or absign_1_reg)
    begin
        if (PIPE1_REG == 1'b0)
        begin
            out1 = mult_out1;
            absign_1 = absign_1_0;
        end else
        begin
            out1 = out1_reg;
            absign_1 = absign_1_reg;
        end
    end

    assign absign = absign_0 || absign_1;

    always @(out0 or absign_0 or alu_sel1)
    begin
        if(alu_sel1) 
        begin
            if (absign_0 == 1'b1)
            begin
                out0_0[35:0] = out0[35:0];
                out0_0[54:36] = {out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35], out0[35]};
            end else 
            begin
                out0_0[35:0] = out0[35:0];
                out0_0[54:36] = 19'b0;
            end
        end else begin
            out0_0[54:0] = 55'b0;
        end
    end

    always @(out1 or absign_1 or alu_sel1)
    begin
        if(alu_sel1) 
        begin
            if (absign_1 == 1'b1)
            begin
                out1_0[35:0] = out1[35:0];
                out1_0[54:36] = {out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35], out1[35]};
            end else
            begin
                out1_0[35:0] = out1[35:0];
                out1_0[54:36] = 19'b0;
            end
        end else begin
            out1_0[54:0] = 55'b0;
        end            
    end

    assign inc_ext = (alu_sel1) ? {1'b0, inc[53:0]} : 0;
    assign acc_load = (accload_1) ? m_out : 0;
    
    always @ (inc_ext or CASI or out0_0 or out1_0 or acc_load)
    begin
        if(MULTADDALU18X18_MODE == 0) begin   //18x18 +/- 18x18 +/- C
            if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b0) begin
                dout = out0_0 + out1_0 + inc_ext;
            end else if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b1) begin
                dout = out0_0 + out1_0 - inc_ext;
            end else if(B_ADD_SUB == 1'b1 && C_ADD_SUB == 1'b0) begin
                dout = out0_0 - out1_0 + inc_ext;
            end else if(B_ADD_SUB == 1'b1 && C_ADD_SUB == 1'b1) begin
                dout = out0_0 - out1_0 - inc_ext;
            end
        end else if(MULTADDALU18X18_MODE == 1) begin   //accumulator,ACC/0 + 18x18 +/- 18x18
            if(B_ADD_SUB == 1'b0) begin
                dout = acc_load + out0_0 + out1_0;
            end else begin
                dout = acc_load + out0_0 - out1_0;
            end
        end else if (MULTADDALU18X18_MODE == 2) begin  //18x18 +/- 18x18 + CASI
            if(B_ADD_SUB == 1'b0) begin
                dout = out0_0 + out1_0 + CASI;
            end else begin
                dout = out0_0 - out1_0 + CASI;
            end
        end
    end

    // output reg
    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out_async <= 0;
        end else if (RESET == 1'b1)
        begin
            out_async <= 0;
        end else if (CE == 1'b1)
        begin
            out_async <= dout;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            out_sync <= 0;
        end else if (CE == 1'b1)
        begin
            out_sync <= dout;
        end
    end

    always @(dout or out_reg)
    begin
        if (OUT_REG == 1'b0)
        begin
            m_out = dout;
        end else
        begin
            m_out = out_reg;
        end
    end

    assign DOUT = m_out;
    assign CASO = {m_out[53] & absign,m_out[53:0]};

endmodule

// MULTALU18X18
module MULTALU18X18 (DOUT, CASO, A, B, C, D, CASI, ACCLOAD, ASIGN, BSIGN, DSIGN, CLK, CE, RESET);
input [17:0] A, B;
input CLK,CE,RESET;
input ASIGN, BSIGN;
input ACCLOAD,DSIGN;
input [53:0] C,D;
input [54:0] CASI;
output [53:0] DOUT;
output [54:0] CASO;

parameter AREG = 1'b0;// 1'b0: bypass mode; 1'b1: registered mode
parameter BREG = 1'b0;
parameter CREG = 1'b0;
parameter DREG = 1'b0;
parameter DSIGN_REG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter ACCLOAD_REG0 = 1'b0;
parameter ACCLOAD_REG1 = 1'b0;
parameter MULT_RESET_MODE = "SYNC"; // SYNC, ASYNC
parameter PIPE_REG = 1'b0;
parameter OUT_REG = 1'b0;
parameter B_ADD_SUB = 1'b0; //1'b0: "+"; 1'b1: "-";
parameter C_ADD_SUB = 1'b0;
parameter MULTALU18X18_MODE = 0; //0:ACC/0 +/- 18x18 +/- C; 1:ACC/0 +/- 18x18 + CASI; 2: 18x18 +/- D + CASI;


    reg [17:0] ina0_reg_async,ina0_reg_sync,ina0_reg,inb0_reg,ina1,inb1,inb0_reg_async,inb0_reg_sync;
    reg [35:0] ina,inb;
    reg asign0_reg_async,asign0_reg_sync,bsign0_reg_async,bsign0_reg_sync,asign_0,bsign_0,asign0_reg, bsign0_reg,absign_reg_async, absign_reg,absign_reg_sync, absign;
    reg dsign_reg_async,dsign_reg_sync,dsign_0,dsign_reg;
    reg accload_0, accload0_reg, accload1_reg,accload_1;
    reg alu_sel;
    reg alu_sel0, alu_sel0_reg, alu_sel1_reg,alu_sel1;
    reg [53:0] ind_0, ind_reg, ind_reg_async, ind_reg_sync;
    reg [53:0] inc_reg_async,inc_reg_sync,inc_reg,inc;
    reg [35:0] out0_async, out0_sync, out0,out1;
    wire [35:0] mult_out;
    reg [54:0] ppout1_ext,acc_reg_async, acc_reg_sync, acc_reg;
    wire [54:0] acc_load,ind_ext,inc_ext;
    reg [54:0] acc_out,dout;
    wire grstn = GSR.GSRO;

    initial 
    begin
        acc_reg = 55'b0;
        alu_sel = 1'b1;
        alu_sel0_reg = 1'b0;
        alu_sel1_reg = 1'b0;
    end

    always @(ina0_reg_sync or ina0_reg_async or inb0_reg_sync or inb0_reg_async or inc_reg_async or inc_reg_sync or asign0_reg_async or asign0_reg_sync or bsign0_reg_async or bsign0_reg_sync or absign_reg_async or absign_reg_sync or dsign_reg_async or dsign_reg_sync or ind_reg_async or ind_reg_sync or acc_reg_async or acc_reg_sync or out0_async or out0_sync)
    begin
        if (MULT_RESET_MODE == "ASYNC")
        begin
            ina0_reg <= ina0_reg_async;
            inb0_reg <= inb0_reg_async;
            inc_reg <= inc_reg_async;
            asign0_reg <= asign0_reg_async;
            bsign0_reg <= bsign0_reg_async;
            absign_reg <= absign_reg_async;
            dsign_reg <= dsign_reg_async;
            ind_reg <= ind_reg_async;
            out0 <= out0_async;
            acc_reg <= acc_reg_async;
        end
        else if (MULT_RESET_MODE == "SYNC")
        begin
            ina0_reg <= ina0_reg_sync;
            inb0_reg <= inb0_reg_sync;
            inc_reg <= inc_reg_sync;
            asign0_reg <= asign0_reg_sync;
            bsign0_reg <= bsign0_reg_sync;
            absign_reg <= absign_reg_sync;
            dsign_reg <= dsign_reg_sync;
            ind_reg <= ind_reg_sync;
            out0 <= out0_sync;
            acc_reg <= acc_reg_sync;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            ina0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_async <= A;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            ina0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_sync <= A;
        end
    end

    always @(A or ina0_reg)
    begin
        if (AREG == 1'b0)
        begin
            ina1 = A;
        end else begin
            ina1 = ina0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_async <= B;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inb0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_sync <= B;
        end
    end

    always @(B or inb0_reg)
    begin
        if (BREG == 1'b0)
        begin
            inb1 = B;
        end else 
        begin
            inb1 = inb0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            inc_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_async <= C;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inc_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            inc_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inc_reg_sync <= C;
        end
    end

    always @(C or inc_reg)
    begin
        if (CREG == 1'b0)
        begin
            inc = C;
        end else begin
            inc = inc_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign0_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            asign0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_reg_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            asign0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_reg_sync <= ASIGN;
        end
    end

    always @(ASIGN or asign0_reg)
    begin
        if (ASIGN_REG == 1'b0)
        begin
            asign_0 = ASIGN;
        end else
        begin
            asign_0 = asign0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_reg_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            bsign0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_reg_sync <= BSIGN;
        end
    end

    always @(BSIGN or bsign0_reg)
    begin
        if (BSIGN_REG == 1'b0)
        begin
            bsign_0 = BSIGN;
        end else
        begin
            bsign_0 = bsign0_reg;
        end
    end

    assign absign_0 = asign_0 || bsign_0;

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            dsign_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            dsign_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            dsign_reg_async <= DSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            dsign_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            dsign_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            dsign_reg_sync <= DSIGN;
        end
    end

    always @(DSIGN or dsign_reg)
    begin
        if (DSIGN_REG == 1'b0)
        begin
            dsign_0 = DSIGN;
        end else
        begin
            dsign_0 = dsign_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ind_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            ind_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ind_reg_async <= D;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ind_reg_sync <= 0; 
        end else if (RESET == 1'b1)
        begin
            ind_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ind_reg_sync <= D;
        end
    end

    always @(D or ind_reg)
    begin
        if (DREG == 1'b0)
        begin
            ind_0 = D;
        end else
        begin
            ind_0 = ind_reg;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel0_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel0_reg <= alu_sel;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel1_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel1_reg <= alu_sel0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload0_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload0_reg <= ACCLOAD;
        end
    end
 
    always @(ACCLOAD or accload0_reg or alu_sel or alu_sel0_reg)
    begin
        if (ACCLOAD_REG0 == 1'b0)
        begin
            accload_0 <= ACCLOAD;
            alu_sel0 <= alu_sel;
        end else
        begin
            accload_0 <= accload0_reg;
            alu_sel0 <= alu_sel0_reg;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload1_reg <= 0;
        end else if (CE == 1'b1)
        begin
            accload1_reg <= accload_0;
        end
    end
 
    always @(accload_0 or accload1_reg or alu_sel0 or alu_sel1_reg)
    begin
        if (ACCLOAD_REG1 == 1'b0)
        begin
            accload_1 <= accload_0;
            alu_sel1 <= alu_sel0;
        end else 
        begin
            accload_1 <= accload1_reg;
            alu_sel1 <= alu_sel1_reg;
        end
    end
 
    always @(ina1 or asign_0)
    begin
        if (asign_0 == 1'b1)
        begin
            ina[17:0] = ina1;
            ina[35:18] = {ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17],ina1[17]};
        end else
        begin
            ina[17:0] =  ina1;
            ina[35:18] = 0;
        end
    end

    always @(inb1 or bsign_0)
    begin
        if (bsign_0 == 1'b1)
        begin
            inb[17:0] = inb1;
            inb[35:18] = {inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17],inb1[17]};
        end else
        begin
            inb[17:0] = inb1;
            inb[35:18] = 0;
        end
    end

    assign mult_out = (!ina || !inb)? 0 : ina * inb ;

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            out0_async <= 0;
            absign_reg_async <= 0;  
        end else if (RESET == 1'b1)
        begin
            out0_async <= 0;
            absign_reg_async <= 0;  
        end
        else if (CE == 1'b1)
        begin
            out0_async <= mult_out;
            absign_reg_async <= absign_0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            out0_sync <= 0;
            absign_reg_sync <= 0;  
        end else if (RESET == 1'b1)
        begin
            out0_sync <= 0;
            absign_reg_sync <= 0;  
        end
        else if (CE == 1'b1)
        begin
            out0_sync <= mult_out;
            absign_reg_sync <= absign_0;  
        end
    end
    
    always @(mult_out or out0 or absign_0 or absign_reg)
    begin
        if (PIPE_REG == 1'b0)
        begin
            out1 = mult_out;
            absign = absign_0;
        end else
        begin
            out1 = out0;
            absign = absign_reg;
        end
    end

    always @(out1 or absign or alu_sel1)
    begin
        if(alu_sel1)
        begin
            if (absign == 1'b1)
            begin
                ppout1_ext[35:0] = out1[35:0];
                ppout1_ext[54:36] = {out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35],out1[35]};
            end else
            begin
                ppout1_ext[35:0] =  out1[35:0];
                ppout1_ext[54:36] = 19'b0;
            end
        end else begin
            ppout1_ext[54:0] = 55'b0;
        end
    end

    assign acc_load = (accload_1) ? dout : 0;
    assign ind_ext = (alu_sel1) ? {dsign_0 & ind_0[53], ind_0[53:0]} : 0;
    assign inc_ext = (alu_sel1) ? {1'b0,inc[53:0]} : 0;
    
    always @(acc_load or CASI or ppout1_ext or ind_ext or inc_ext)
    begin
        if(MULTALU18X18_MODE == 0)           //ACC/0 +/- 18x18 +/- C
        begin
            if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b0)
            begin
                acc_out = acc_load + ppout1_ext + inc_ext;
            end else if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b1)
            begin
                acc_out = acc_load + ppout1_ext - inc_ext;
            end else if(B_ADD_SUB == 1'b1 && C_ADD_SUB == 1'b0)
            begin
                acc_out = acc_load - ppout1_ext + inc_ext;
            end else
            begin
                acc_out = acc_load - ppout1_ext - inc_ext;
            end
        end else if(MULTALU18X18_MODE == 1)  //ACC/0 +/- 18x18 + CASI
        begin
            if(B_ADD_SUB == 1'b0) 
            begin
                acc_out = acc_load + ppout1_ext + CASI;
            end else
            begin
                acc_out = acc_load - ppout1_ext + CASI;                
            end        
        end else if(MULTALU18X18_MODE == 2)  //18x18 +/- D + CASI
        begin
            if(B_ADD_SUB == 1'b0) 
            begin
                acc_out = ppout1_ext + ind_ext + CASI;
            end else
            begin
                acc_out = ppout1_ext - ind_ext + CASI;                
            end
        end 
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            acc_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            acc_reg_async <= 0;
        end else if (CE == 1'b1)
        begin
            acc_reg_async <= acc_out;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            acc_reg_sync <= 0;
        end else  if (RESET == 1'b1)
        begin
            acc_reg_sync <= 0;
        end else if (CE == 1'b1)
        begin
            acc_reg_sync <= acc_out;
        end
    end

    always @(acc_reg or acc_out)
    begin
        if (OUT_REG == 1'b0)
        begin
            dout = acc_out;
        end else
        begin
            dout = acc_reg;
        end
    end

    assign DOUT = dout;
    assign CASO = {dout[53] & absign,dout[53:0]};

endmodule

//ALU-direct mode
module ALU54D (DOUT, CASO, A, B, CASI, ACCLOAD, ASIGN, BSIGN, CLK, CE, RESET);
input [53:0] A, B;
input ASIGN,BSIGN;
input ACCLOAD;
input [54:0] CASI;
input CLK, CE, RESET;
output [53:0] DOUT;
output [54:0] CASO;

parameter AREG = 1'b0; //1'b0:bypass mode; 1'b1: register mode
parameter BREG = 1'b0;
parameter ASIGN_REG = 1'b0;
parameter BSIGN_REG = 1'b0;
parameter ACCLOAD_REG = 1'b0;
parameter OUT_REG = 1'b0;
parameter B_ADD_SUB = 1'b0; //1'b0: add; 1'b1:sub;
parameter C_ADD_SUB = 1'b0;
parameter ALUD_MODE = 0;//0:ACC/0 +/- B +/- A; 1:ACC/0 +/- B + CASI; 2:A +/- B + CASI;
parameter ALU_RESET_MODE = "SYNC";//SYNC, ASYNC

    reg [53:0] ina0_reg_async, ina0_reg_sync, ina0_reg, inb0_reg,ina0_sig,inb0_sig, inb0_reg_async, inb0_reg_sync;
    reg asign0_reg0_async,asign0_reg0_sync,bsign0_reg0_async,bsign0_reg0_sync,asign0_0,bsign0_0,asign0_reg0, bsign0_reg0;
    reg accload_0, accload0_reg;
    reg alu_sel;
    reg alu_sel0, alu_sel0_reg;
    reg [54:0] ina0_ext,inb0_ext,dout_reg_async, dout_reg_sync, dout_reg;
    wire [54:0] acc_load;
    reg [54:0] m_out,dout;
    wire grstn = GSR.GSRO;

    initial
    begin
        dout_reg = 55'b0;
        alu_sel = 1'b1;
        alu_sel0_reg = 1'b0;
    end

    always @(ina0_reg_sync or ina0_reg_async or inb0_reg_sync or inb0_reg_async or asign0_reg0_async or asign0_reg0_sync or bsign0_reg0_async or bsign0_reg0_sync or dout_reg_async or dout_reg_sync)
    begin
        if (ALU_RESET_MODE == "ASYNC")
        begin
            ina0_reg <= ina0_reg_async;
            inb0_reg <= inb0_reg_async;
            asign0_reg0 <= asign0_reg0_async;
            bsign0_reg0 <= bsign0_reg0_async;
            dout_reg <= dout_reg_async;
        end 
        else if (ALU_RESET_MODE == "SYNC")
        begin
            ina0_reg <= ina0_reg_sync;
            inb0_reg <= inb0_reg_sync;
            asign0_reg0 <= asign0_reg0_sync;
            bsign0_reg0 <= bsign0_reg0_sync;
            dout_reg <= dout_reg_sync;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_async <= 0; 
        end else if (RESET == 1'b1)
        begin
            ina0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_async <= A;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            ina0_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            ina0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            ina0_reg_sync <= A;
        end
    end

    always @(A or ina0_reg)
    begin
        if (AREG == 1'b0)
        begin
            ina0_sig = A;
        end else begin
            ina0_sig = ina0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            inb0_reg_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_async <= B;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            inb0_reg_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            inb0_reg_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            inb0_reg_sync <= B;
        end
    end

    always @(B or inb0_reg)
    begin
        if (BREG == 1'b0)
        begin
            inb0_sig = B;
        end else begin
            inb0_sig = inb0_reg;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            asign0_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            asign0_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_reg0_async <= ASIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            asign0_reg0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            asign0_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            asign0_reg0_sync <= ASIGN;
        end
    end

    always @(ASIGN or asign0_reg0)
    begin
        if (ASIGN_REG == 1'b0)
        begin
            asign0_0 = ASIGN;
        end else
        begin
            asign0_0 = asign0_reg0;
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_reg0_async <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign0_reg0_async <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_reg0_async <= BSIGN;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            bsign0_reg0_sync <= 0;
        end else if (RESET == 1'b1)
        begin
            bsign0_reg0_sync <= 0;
        end
        else if (CE == 1'b1)
        begin
            bsign0_reg0_sync <= BSIGN;
        end
    end

    always @(BSIGN or bsign0_reg0)
    begin
        if (BSIGN_REG == 1'b0)
        begin
            bsign0_0 = BSIGN;
        end else
        begin
            bsign0_0 = bsign0_reg0;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            accload0_reg <= 0; 
        end else if (CE == 1'b1)
        begin
            accload0_reg <= ACCLOAD;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            alu_sel0_reg <= 0;
        end else if (CE == 1'b1)
        begin
            alu_sel0_reg <= alu_sel;
        end
    end
 
    always @(ACCLOAD or accload0_reg or alu_sel or alu_sel0_reg)
    begin
        if (ACCLOAD_REG == 1'b0)
        begin
            accload_0 <= ACCLOAD;
            alu_sel0 <= alu_sel;
        end else
        begin
            accload_0 <= accload0_reg;
            alu_sel0 <= alu_sel0_reg;
        end
    end

    always @(asign0_0 or bsign0_0 or ina0_sig or inb0_sig or alu_sel0)
    begin
        if(alu_sel0 == 1'b1) begin
            if (asign0_0 == 1'b1)
            begin
                ina0_ext[53:0] = ina0_sig[53:0];
                ina0_ext[54] = ina0_sig[53];
            end else
            begin
                ina0_ext[53:0] = ina0_sig[53:0];
                ina0_ext[54] = 1'b0;
            end

            if (bsign0_0 == 1'b1)
            begin
                inb0_ext[53:0] = inb0_sig[53:0];
                inb0_ext[54] = inb0_sig[53];
            end else
            begin
                inb0_ext[53:0] = inb0_sig[53:0];
                inb0_ext[54] = 1'b0;
            end
        end else begin
            ina0_ext[54:0] = 55'b0;
            inb0_ext[54:0] = 55'b0;
        end

    end

    assign acc_load = (accload_0) ? m_out : 0;
    
    always @(acc_load or CASI or ina0_ext or inb0_ext)
    begin
        if(ALUD_MODE == 0)           //ACC/0 +/- B +/- A
        begin
            if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b0)
            begin
                dout = acc_load + inb0_ext + ina0_ext;
            end else if(B_ADD_SUB == 1'b0 && C_ADD_SUB == 1'b1)
            begin
                dout = acc_load + inb0_ext - ina0_ext;
            end else if(B_ADD_SUB == 1'b1 && C_ADD_SUB == 1'b0)
            begin
                dout = acc_load - inb0_ext + ina0_ext;
            end else
            begin
                dout = acc_load - inb0_ext - ina0_ext;
            end
        end else if(ALUD_MODE == 1)  //ACC/0 +/- B + CASI
        begin
            if(B_ADD_SUB == 1'b0)
            begin
                dout = acc_load + inb0_ext + CASI;
            end else
            begin
                dout = acc_load - inb0_ext + CASI;
            end        
        end else if(ALUD_MODE == 2)  //A +/- B + CASI
        begin
            if(B_ADD_SUB == 1'b0)
            begin
                dout = ina0_ext + inb0_ext + CASI;
            end else
            begin
                dout = ina0_ext - inb0_ext + CASI;
            end
        end
    end

    always @(posedge CLK or posedge RESET or negedge grstn)
    begin
        if (!grstn) begin
            dout_reg_async <= 0;
        end else if (RESET == 1'b1)
        begin
            dout_reg_async <= 0;
        end else if (CE == 1'b1)
        begin
            dout_reg_async <= dout;
        end
    end

    always @(posedge CLK or negedge grstn)
    begin
        if (!grstn) begin
            dout_reg_sync <= 0;
        end else  if (RESET == 1'b1)
        begin
            dout_reg_sync <= 0;
        end else if (CE == 1'b1)
        begin
            dout_reg_sync <= dout;
        end
    end

    always @(dout_reg or dout)
    begin
        if (OUT_REG == 1'b0)
        begin
            m_out = dout;
        end else
        begin
            m_out = dout_reg;
        end
    end

    assign DOUT = m_out[53:0];
    assign CASO = m_out;

endmodule

//clock buffers
module BUFG (O, I);

output O;
input I;

buf BG (O, I);

endmodule // BUFG (global clock buffer)


module BUFS (O, I);

output O;
input I;

buf BS (O, I);

endmodule //BUFS (long wire clock buffer)


//Misc
module GND (G);

output G;

wire G;

assign G = 1'b0;

endmodule


module VCC (V);

output V;

wire V;

assign V = 1'b1;

endmodule


module GSR (GSRI);

input GSRI;

wire GSRO;

assign GSRO = GSRI;

endmodule //GSR (global set/reset control)

//OSC
module OSC (OSCOUT);
parameter  FREQ_DIV = 100; // 2~128,only even num
parameter  DEVICE = "GW2A-18";//GW2A-18,GW2A-55,GW2AR-18,GW2A-55C,GW2A-18C,GW2AR-18C
output OSCOUT;

reg oscr;
realtime half_clk;

assign OSCOUT = oscr;

initial  begin
    oscr = 1'b0;
    half_clk = 2*FREQ_DIV;
end

always   
       begin
            #half_clk;
            oscr = 1'b1;	   
            #half_clk;
            oscr = 1'b0;
       end

endmodule

//PLL start
module PLL (CLKOUT, CLKOUTP, CLKOUTD, CLKOUTD3, LOCK, CLKIN, CLKFB, FBDSEL, IDSEL, ODSEL, DUTYDA, PSDA, FDLY, RESET, RESET_P, RESET_I, RESET_S);
input CLKIN;
input CLKFB;
input RESET;
input RESET_P;
input RESET_I;
input RESET_S;
input [5:0] FBDSEL;
input [5:0] IDSEL;
input [5:0] ODSEL;
input [3:0] PSDA,FDLY;
input [3:0] DUTYDA;

output CLKOUT;
output LOCK;
output CLKOUTP;
output CLKOUTD;
output CLKOUTD3;

parameter FCLKIN = "100.0"; // frequency of the CLKIN(M)
parameter DYN_IDIV_SEL= "false";//true:IDSEL; false:IDIV_SEL
parameter IDIV_SEL = 0; // 0:1,1:2...63:64. 1~64
parameter DYN_FBDIV_SEL= "false";//true:FBDSEL; false:FBDIV_SEL
parameter FBDIV_SEL = 0; // 0:1,1:2...63:64. 1~64
parameter DYN_ODIV_SEL= "false";//true:ODSEL; false:ODIV_SEL
parameter ODIV_SEL = 8; // 2/4/8/16/32/48/64/80/96/112/128

parameter PSDA_SEL= "0000";//
parameter DYN_DA_EN = "false";//true:PSDA or DUTYDA or FDA; false: DA_SEL
parameter DUTYDA_SEL= "1000";//

parameter CLKOUT_FT_DIR = 1'b1; // CLKOUT fine tuning direction. 1'b1 only
parameter CLKOUTP_FT_DIR = 1'b1; // 1'b1 only
parameter CLKOUT_DLY_STEP = 0; // 0,1,2,4
parameter CLKOUTP_DLY_STEP = 0; // 0,1,2

parameter CLKFB_SEL = "internal"; //"internal", "external";
parameter CLKOUT_BYPASS = "false";  //"true"; "false"
parameter CLKOUTP_BYPASS = "false";   //"true"; "false"
parameter CLKOUTD_BYPASS = "false";  //"true"; "false"
parameter DYN_SDIV_SEL = 2; // 2~128,only even num
parameter CLKOUTD_SRC =  "CLKOUT";  //CLKOUT,CLKOUTP
parameter CLKOUTD3_SRC = "CLKOUT"; //CLKOUT,CLKOUTP
parameter DEVICE = "GW2A-18";//"GW2A-18","GW2A-55","GW2AR-18","GW2A-55C","GW2A-18C","GW2AR-18C"

wire resetn;
wire [5:0] IDIV_SEL_reg,FBDIV_SEL_reg;
wire [5:0] IDIV_dyn,FBDIV_dyn;
reg [5:0] IDIV_SEL_reg1,FBDIV_SEL_reg1,ODSEL_reg;
wire div_dyn_change;
integer IDIV_reg,FBDIV_reg;
wire clk_div_src;
reg clk_effect,oclk_effect,oclk_build;
realtime curtime,pretime,fb_delay;
realtime clkin_cycle[4:0];
realtime clkin_period,clkin_period1,clkout_period,tclkout_half,tclkout_half_new;
realtime clkfb_curtime,clkin_curtime,FB_dly,FB_dly0;
reg clkin_init,fb_clk_init;
reg clkout,clk_out,clkfb_reg,clkoutp,clk_ps_reg,clk_ps_reg0;
reg clkfb;
reg lock_reg;
realtime ps_dly,f_dly,clkout_duty, ps_value, duty_value,tclkp_duty;
real unit_div=1.0, real_fbdiv=1.0;
integer cnt_div;
reg clkout_div_reg;
integer multi_clkin;
wire div3_in;
integer cnt_div3;
reg div3_reg;
reg clkfb_init,div3_init,pre_div3_in;


initial begin 
IDIV_reg = 1;
FBDIV_reg = 1;
clkin_cycle[0] = 0;
clkin_cycle[1] = 0;
clkin_cycle[2] = 0;
clkin_cycle[3] = 0;
clkin_cycle[4] = 0;
clkin_period = 0;
clkin_period1 = 0;
clkout_period = 0;
clk_effect = 1'b0;
oclk_effect = 1'b0;
oclk_build = 1'b0;
clkfb_reg = 1'b0;
clkout = 1'b0;
clk_out = 1'b0;
clkfb = 1'b0;
clkoutp = 1'b0;
clkin_init = 1'b1;
fb_clk_init = 1'b1;
clkfb_init = 1'b1;
FB_dly = 0.0;
FB_dly0 = 0.0;
clkin_curtime = 0.0;
clkfb_curtime = 0.0;
lock_reg = 0;
clk_ps_reg=0;
clk_ps_reg0=0;
clkout_div_reg=0;
cnt_div=0;
div3_init = 1'b1;
cnt_div3=0;
div3_reg=0;
f_dly = 0.0;
ps_dly = 0.0;
////////////
end

assign resetn = ~( RESET | RESET_P | RESET_I | RESET_S);

// determine period of CLKIN and clkout
always @(posedge CLKIN or negedge resetn) begin
    if(!resetn) begin
        clk_effect <= 1'b0;
    end else begin
        pretime <= curtime;
        curtime <= $realtime;

        if(pretime>0) begin
	        clkin_cycle[0] <= curtime -  pretime;
        end

        if(clkin_cycle[0] > 0) begin
            clkin_cycle[1] <= clkin_cycle[0];
	        clkin_cycle[2] <= clkin_cycle[1];
	        clkin_cycle[3] <= clkin_cycle[2];
            clkin_cycle[4] <= clkin_cycle[3];
        end
    
        if (clkin_cycle[0] > 0) begin
            if(((clkin_cycle[0] - clkin_period1 < 0.01) && (clkin_cycle[0] - clkin_period1 > -0.01)) &&(!div_dyn_change)) begin
                clk_effect <= 1'b1;
                clkin_period <= clkin_period1;
            end else begin
                clk_effect <= 1'b0;
            end
        end
    end
end

always @(clkin_cycle[0] or clkin_cycle[1] or clkin_cycle[2] or clkin_cycle[3] or clkin_cycle[4]  or clkin_period1) begin
    if(clkin_cycle[0]!=clkin_period1) begin
		clkin_period1 <= (clkin_cycle[0]+clkin_cycle[1]+clkin_cycle[2]+clkin_cycle[3]+clkin_cycle[4])/5;
    end
end

/*IDSEL/FBDSEL    IDIV_dyn/FBDIV_dyn
111111	divider   /1
111110	divider   /2
.	.
.	.
.	.
000000	divider   /64
*/
assign IDIV_dyn = 64 - IDSEL;
assign FBDIV_dyn = 64 - FBDSEL;

assign IDIV_SEL_reg = (RESET_I == 1'b1)? 1 : ((DYN_IDIV_SEL == "true") ? IDIV_dyn : (IDIV_SEL+1)) ;
assign FBDIV_SEL_reg = (DYN_FBDIV_SEL == "true") ? FBDIV_dyn : (FBDIV_SEL+1) ;

always @(posedge CLKIN) begin
    IDIV_SEL_reg1 <= IDIV_SEL_reg;
    FBDIV_SEL_reg1 <= FBDIV_SEL_reg;
    ODSEL_reg <= ODSEL;
end

assign div_dyn_change = (IDIV_SEL_reg1 != IDIV_SEL_reg) || (FBDIV_SEL_reg1 != FBDIV_SEL_reg) || (ODSEL_reg != ODSEL);

always @(clkin_period or IDIV_SEL_reg or FBDIV_SEL_reg) begin
    real_fbdiv = (FBDIV_SEL_reg * unit_div);
    clkout_period = ((clkin_period * IDIV_SEL_reg) / real_fbdiv);
    tclkout_half = (clkout_period / 2);
end

realtime clk_tlock_cur;
realtime max_tlock;
integer cnt_lock;
initial begin
    clk_tlock_cur = 0.0;
    max_tlock = 0.0;
    cnt_lock = 0;
end

// lock time
always @(posedge CLKIN or negedge resetn) begin
    if (resetn == 1'b0) begin
        max_tlock <= 0.0;
    end else begin
        if((clkin_cycle[0] >= 2) && (clkin_cycle[0] <= 40)) begin
            max_tlock <= 50000;
        end else if ((clkin_cycle[0] > 40) && (clkin_cycle[0] <= 500)) begin
            max_tlock <= 200000;
        end
    end
end

always @(posedge CLKIN or negedge resetn) begin
    if (resetn == 1'b0) begin
        lock_reg <= 1'b0;
        oclk_effect <= 1'b0;
    end else begin
        if(clk_effect == 1'b1) begin
            cnt_lock <= cnt_lock + 1;

            if(cnt_lock > ((max_tlock/clkin_period) - 10)) begin
                oclk_effect <= 1'b1;
            end else begin
                oclk_effect <= 1'b0;
            end

            if(cnt_lock > (max_tlock/clkin_period)) begin
                lock_reg <= 1'b1;
            end else begin
                lock_reg <= 1'b0;
            end
        end else begin
            oclk_effect <= 1'b0;
            cnt_lock <= 0;
            lock_reg <= 1'b0;
        end
    end
end

// calculate CLKFB feedback delay
always @(posedge CLKIN) begin
    if (clkin_init == 1'b1) begin
        clkin_curtime=$realtime;
        clkin_init = 1'b0;
    end
end

always @(posedge CLKFB) begin
    if (fb_clk_init == 1'b1) begin
        clkfb_curtime=$realtime;
        fb_clk_init = 1'b0;
    end
end

always @(CLKFB or CLKIN) begin
    if ((clkfb_curtime > 0) && (clkin_curtime > 0)) begin
        FB_dly0 = clkfb_curtime - clkin_curtime;
        if ((FB_dly0 >= 0) && (clkin_cycle[0] > 0)) begin
            multi_clkin = FB_dly0 / (clkin_cycle[0]);
            FB_dly = clkin_cycle[0] - (FB_dly0 - (clkin_cycle[0]) * multi_clkin);
        end
    end
end

// clkout
always @(clkfb_reg or oclk_effect) begin
    if(oclk_effect == 1'b0) begin
        clkfb_reg = 1'b0;
    end
    else begin
        if(clkfb_init == 1'b1) begin
            clkfb_reg <= 1'b1;
            clkfb_init = 1'b0;
        end
        else begin
            clkfb_reg <= #tclkout_half ~clkfb_reg;
        end
    end
end

always @(clkfb_reg) begin
    if (CLKFB_SEL == "internal") begin
        clkfb <= clkfb_reg;
    end else begin
        clkfb <= #(FB_dly) clkfb_reg;
    end
end

always @(posedge clkfb) begin
    clkout <= 1'b1;
    #tclkout_half_new
    clkout <= 1'b0;
end

always @(CLKIN or oclk_effect or clkout or resetn) begin
    if (resetn == 1'b0) begin
        clk_out <= 1'b0;
    end else if(CLKOUT_BYPASS == "true") begin
        clk_out <= CLKIN;
    end
    //else if (oclk_effect == 1'b1) begin
    else begin
        clk_out <= clkout;
    end
end

assign CLKOUT = clk_out;
assign LOCK = lock_reg;  

//clkout_p
// DYN_DA_EN == "false".
// phase_shift_value
always @(*) begin
    case (PSDA_SEL)
	    "0000": ps_value = (clkout_period *  0)/16;
	    "0001": ps_value = (clkout_period *  1)/16;
	    "0010": ps_value = (clkout_period *  2)/16;
	    "0011": ps_value = (clkout_period *  3)/16;
	    "0100": ps_value = (clkout_period *  4)/16;
	    "0101": ps_value = (clkout_period *  5)/16;
	    "0110": ps_value = (clkout_period *  6)/16;
	    "0111": ps_value = (clkout_period *  7)/16;
	    "1000": ps_value = (clkout_period *  8)/16;
	    "1001": ps_value = (clkout_period *  9)/16;
	    "1010": ps_value = (clkout_period * 10)/16;
	    "1011": ps_value = (clkout_period * 11)/16;
	    "1100": ps_value = (clkout_period * 12)/16;
	    "1101": ps_value = (clkout_period * 13)/16;
	    "1110": ps_value = (clkout_period * 14)/16;
	    "1111": ps_value = (clkout_period * 15)/16;
	endcase
end

always @(*) begin
	case (DUTYDA_SEL)
	    "0000": duty_value = (clkout_period *  0)/16;
	    "0001": duty_value = (clkout_period *  1)/16;
	    "0010": duty_value = (clkout_period *  2)/16;
	    "0011": duty_value = (clkout_period *  3)/16;
	    "0100": duty_value = (clkout_period *  4)/16;
	    "0101": duty_value = (clkout_period *  5)/16;
	    "0110": duty_value = (clkout_period *  6)/16;
	    "0111": duty_value = (clkout_period *  7)/16;
	    "1000": duty_value = (clkout_period *  8)/16;
	    "1001": duty_value = (clkout_period *  9)/16;
	    "1010": duty_value = (clkout_period * 10)/16;
	    "1011": duty_value = (clkout_period * 11)/16;
	    "1100": duty_value = (clkout_period * 12)/16;
	    "1101": duty_value = (clkout_period * 13)/16;
	    "1110": duty_value = (clkout_period * 14)/16;
	    "1111": duty_value = (clkout_period * 15)/16;
	endcase
end

//DYN_DA_EN = "true"
always @(FDLY) begin
    if(DYN_DA_EN == "true") begin
        case(FDLY)
            4'b1111  : f_dly = 0.000;
            4'b1110  : f_dly = 0.125;
            4'b1101  : f_dly = 0.250;
            4'b1011  : f_dly = 0.500;
            4'b0111  : f_dly = 1.000;
            default : f_dly = 0.000;
        endcase
    end
end

always @ (PSDA or DUTYDA or ps_value or duty_value) begin
    if (DYN_DA_EN == "true") begin
        ps_dly = (clkout_period *PSDA)/16;
        if (DUTYDA > PSDA) begin
            clkout_duty = (clkout_period * (DUTYDA - PSDA))/16;
        end else if (DUTYDA < PSDA) begin
            clkout_duty = (clkout_period*(16 + DUTYDA - PSDA))/16;
        end else begin
            clkout_duty = (clkout_period)/2;
        end
    end else begin
        ps_dly= ps_value;
        clkout_duty = duty_value;
    end
end

always @(tclkout_half or clkout_duty) begin
    if (DYN_DA_EN == "false") begin
        tclkout_half_new <= tclkout_half;
        tclkp_duty <= clkout_duty;
    end else begin
        if (CLKOUT_FT_DIR == 1'b1) begin
            tclkout_half_new <= tclkout_half - (0.05 * CLKOUT_DLY_STEP);
        end else begin
            tclkout_half_new <= tclkout_half + (0.05 * CLKOUT_DLY_STEP);
        end

        if (CLKOUTP_FT_DIR == 1'b1) begin
            tclkp_duty <= clkout_duty - (0.05 * CLKOUTP_DLY_STEP);
	    end else begin
            tclkp_duty <= clkout_duty + (0.05 * CLKOUTP_DLY_STEP);
        end
	end
end

always @(posedge clkfb) begin
    clkoutp <= 1'b1;
    #tclkp_duty
    clkoutp <= 1'b0;
end

always @(clkoutp) begin
    clk_ps_reg0 <= #(ps_dly+f_dly) clkoutp;    
end
      
always @(CLKIN or oclk_effect or clk_ps_reg0 or resetn) begin
    if (resetn == 1'b0) begin
        clk_ps_reg <= 1'b0;
    end else if(CLKOUTP_BYPASS == "true") begin
        clk_ps_reg <= CLKIN;
    end 
    //else if (oclk_effect == 1'b1) begin
    else begin
        clk_ps_reg <= clk_ps_reg0;
    end
end

assign CLKOUTP = clk_ps_reg;

//divide
assign clk_div_src = (CLKOUTD_SRC=="CLKOUTP") ? clk_ps_reg0:clkout;

always @(posedge clk_div_src or posedge RESET_S) begin
    if (RESET_S) begin
        cnt_div <= 0;
	    clkout_div_reg <= 0;
    end else begin
        cnt_div = cnt_div + 1;
		if (cnt_div == DYN_SDIV_SEL/2) begin
	        clkout_div_reg <= ~clkout_div_reg;
			cnt_div <= 0;
        end
	end
end    
    
assign CLKOUTD = (CLKOUTD_BYPASS == "true") ? CLKIN : clkout_div_reg;

// div3
assign div3_in=(CLKOUTD3_SRC=="CLKOUTP")?clk_ps_reg:clk_out; 

always @ (div3_in) begin
    pre_div3_in <= div3_in;
end

always @(div3_in or posedge RESET_S) begin
    if(div3_init == 1'b1) begin
        if(pre_div3_in == 1'b1 && div3_in == 1'b0) begin
	        div3_reg <= 1;
            div3_init = 1'b0;
            cnt_div3 = 0;
        end
    end else if(RESET_S == 1'b1) begin
         div3_reg <= 0;
         cnt_div3 = 0;
    end else begin
        cnt_div3 = cnt_div3+1;
        if(cnt_div3 == 3) begin
            div3_reg <= ~div3_reg;
            cnt_div3 = 0;
        end
    end
end

assign CLKOUTD3 = div3_reg;

endmodule


//*************add LVDS******************

//true LVDS
module TLVDS_IBUF (O, I, IB);
output O;
input  I, IB;
reg O_oreg;
assign O = O_oreg;
always @(I or IB) begin
	if (I == 1'b1 && IB == 1'b0)
			O_oreg <= I;
	else if (I == 1'b0 && IB == 1'b1)
			O_oreg <= I;
    else if (I == 1'bx || IB == 1'bx)
			O_oreg <= 1'bx;
end
endmodule

module TLVDS_OBUF (O, OB, I);
output O, OB;
input  I;
supply0 gst;

bufif0 TB (O, I, gst);
notif0 YB (OB, I, gst);
endmodule

module TLVDS_TBUF (O, OB, I, OEN);
output O, OB;
input  I, OEN;
bufif0 TB (O, I, OEN);
notif0 YB (OB, I, OEN);
endmodule

module TLVDS_IOBUF (O, IO, IOB, I, OEN);
output   O;
inout IO, IOB;
input I, OEN;
reg O;
bufif0 IB (IO, I, OEN);
notif0 YB (IOB, I, OEN);
always @(IO or IOB) begin
        if (IO == 1'b1 && IOB == 1'b0)
            O <= IO;
        else if (IO == 1'b0 && IOB == 1'b1)
            O <= IO;
        else if (IO == 1'bx || IOB == 1'bx)
            O <= 1'bx;
end
endmodule

//emulated LVDS
module ELVDS_IBUF (O, I, IB);
output O;
input  I, IB;
reg O_oreg;
assign O = O_oreg;
always @(I or IB) begin
	if (I == 1'b1 && IB == 1'b0)
			O_oreg <= I;
	else if (I == 1'b0 && IB == 1'b1)
			O_oreg <= I;
    else if (I == 1'bx || IB == 1'bx)
			O_oreg <= 1'bx;
end
endmodule

module ELVDS_OBUF (O, OB, I);
output O, OB;
input  I;
supply0 gst;

bufif0 TB (O, I, gst);
notif0 YB (OB, I, gst);
endmodule

module ELVDS_TBUF (O, OB, I, OEN);
output O, OB;
input  I, OEN;
bufif0 TB (O, I, OEN);
notif0 YB (OB, I, OEN);
endmodule

module ELVDS_IOBUF (O, IO, IOB, I, OEN);
output   O;
inout IO, IOB;
input I, OEN;
reg O;
bufif0 IB (IO, I, OEN);
notif0 YB (IOB, I, OEN);
always @(IO or IOB) begin
        if (IO == 1'b1 && IOB == 1'b0)
            O <= IO;
        else if (IO == 1'b0 && IOB == 1'b1)
            O <= IO;
        else if (IO == 1'bx || IOB == 1'bx)
            O <= 1'bx;
end
endmodule

//DLL
module DLL (STEP, LOCK, UPDNCNTL, STOP, CLKIN, RESET);

input CLKIN;
input STOP;
input UPDNCNTL;
input RESET;

output [7:0]STEP;
output LOCK;

parameter DLL_FORCE = 0;//1: force lock and code; 0: code/lock generated from DLL loop
parameter CODESCAL="000";//001 010 011 100 101 110 111
parameter SCAL_EN="true";//true,false
parameter DIV_SEL = 1'b0; // 1'b0,normal lock mode; 1'b1,fast lock mode

reg inner_period,osc_clk_out;
realtime clkin_edge,clkin_period,clkin_period_pre;
real del;
reg [7:0]step_reg,step_reg_sig;
reg stop_0,stop_1n;
integer cnt;
reg clk_effect,lock_reg;
wire reset_sig;
wire grstn = GSR.GSRO;

initial
begin
del=0.025;
clk_effect = 1'b0;
step_reg = 8'b00000000;
lock_reg = 1'b0;
end

    assign reset_sig = RESET | (~grstn);    

    always @(posedge CLKIN or posedge reset_sig) begin
        if(reset_sig) begin
            clk_effect <= 0;
            clkin_period <= 0;
        end else begin
            clkin_edge<=$realtime;
            clkin_period<=$realtime-clkin_edge;
            clkin_period_pre<=clkin_period;
        end

        if(clkin_period > 0) begin    
            if((clkin_period-clkin_period_pre <0.001) &&(clkin_period-clkin_period_pre>-0.001)) begin
                clk_effect = 1;
            end else begin
                clk_effect = 0;
            end
        end
           
    end

    always @ (negedge CLKIN or posedge reset_sig) begin
        if (reset_sig) begin
            stop_0 <= 1'b0;
        end else begin
            stop_0 <= STOP;
        end
    end

    always @ (negedge CLKIN or posedge reset_sig) begin
        if (reset_sig) begin
            stop_1n <= 1'b1;
        end else begin
            stop_1n <= ~stop_0;
        end
    end

    assign clk_out = CLKIN && stop_1n;

    always @(posedge clk_out or posedge reset_sig) begin
        if (reset_sig) begin
            cnt <= 0;
            lock_reg <= 1'b0;
        end else begin
            cnt <= cnt + 1;
            if(DIV_SEL == 1'b0) begin
                if(cnt >= 33600) begin
                    lock_reg <= 1'b1;
                end else begin                
                    lock_reg <= 1'b0;
                end               
            end

            if(DIV_SEL == 1'b1) begin
                if(cnt >= 2100) begin
                    lock_reg <= 1'b1;
                end else begin                
                    lock_reg <= 1'b0;
                end               
            end
        end
    end

    assign LOCK = (DLL_FORCE==1) ? 1'b1: lock_reg;

    always @(*) begin
        if(clk_effect) begin
            if(SCAL_EN=="true") begin
                case(CODESCAL)
                    "000": step_reg<=clkin_period/del/360*101;
                    "001": step_reg<=clkin_period/del/360*112;
                    "010": step_reg<=clkin_period/del/360*123;
                    "011": step_reg<=clkin_period/del/360*135;
                    "100": step_reg<=clkin_period/del/360*79;
                    "101": step_reg<=clkin_period/del/360*68;
                    "110": step_reg<=clkin_period/del/360*57;
                    "111": step_reg<=clkin_period/del/360*45;
                endcase
            end else begin
                step_reg<=clkin_period/del/360*90;
            end
        end
    end

    always @(step_reg or reset_sig or UPDNCNTL)
    begin
        if(reset_sig == 1'b1) begin
            step_reg_sig <= 8'b00000000;
        end else if(UPDNCNTL==0) begin
            step_reg_sig <= step_reg;
        end
    end

    assign STEP = (DLL_FORCE==1) ? 255 : step_reg_sig;

endmodule


// CLKDIV
module CLKDIV(CLKOUT, CALIB, HCLKIN, RESETN); 

input HCLKIN;
input RESETN;


input CALIB;
output CLKOUT;

parameter DIV_MODE = "2"; //"2", "3.5", "4", "5"
parameter GSREN = "false"; //"false", "true"

reg reset_0;
reg calib_0,calib_1,calib_2,calib_state;
wire calib_en,calib_resetn,cnt_enable;
reg select245,select3p5,select5;
wire dsel_en,clk_div2,clk_div_0,clk_div;
reg d_sel,cnt_0,cnt_1,cnt_2,clk_div_1;

wire grstn;

initial begin
    cnt_0 = 1'b0;
    cnt_1 = 1'b0;
    d_sel = 1'b0;
    select3p5 = 1'b0;
    select245 = 1'b1;
    select5 = 1'b0;
    calib_state = 1'b0;
    clk_div_1 = 1'b0;

    if (DIV_MODE == "2" || DIV_MODE == "4" || DIV_MODE == "5")
    begin
        select245 = 1'b1;
        select3p5 = 1'b0;
    end else if (DIV_MODE == "3.5") begin
        select3p5 = 1'b1;
        select245 = 1'b0;        
    end

    if (DIV_MODE == "5")
     begin
        select5 = 1'b1;
     end

end

assign grstn = GSREN == "true" ? GSR.GSRO : 1'b1;

always @(posedge HCLKIN or negedge grstn or negedge RESETN) begin
    if (!grstn) begin
        reset_0 <= 1'b0;
    end else if (!RESETN) begin
        reset_0 <= 1'b0;
    end else begin
        reset_0 <= 1'b1;
    end
end

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        calib_0 <= 1'b0;
    end else begin
        calib_0 <= ~CALIB;
    end
end

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        calib_1 <= 0;
    end else begin
        calib_1 <= calib_0;
    end
end

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        calib_2 <= 1'b0;
    end else begin
        calib_2 <= calib_1;
    end
end

assign calib_resetn =  ~(calib_1 && (~calib_2));
assign calib_en = ~(calib_resetn | (~select245));

always @ (posedge HCLKIN or negedge reset_0)
begin
    if (!reset_0)  begin
        calib_state <= 1'b0;
    end else begin
        if (calib_en == 1'b1) begin
            calib_state <= ~calib_state;
        end else begin
            calib_state <= calib_state;
        end
    end
end

assign cnt_enable = (~((~calib_resetn)&calib_state) & select245) | (calib_resetn & select3p5);

assign dsel_en = (d_sel& cnt_0 & cnt_1 & select3p5) | (calib_resetn & (~d_sel) & (~cnt_0) & cnt_1 & select3p5);

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        d_sel  <= 1'b0;
    end else if(dsel_en == 1'b1) begin
        d_sel  <= ~d_sel;
    end else if(dsel_en == 1'b0) begin
        d_sel <= d_sel;
    end
end

assign cnt_reset = (select5 & (~cnt_0) & (~cnt_1) & cnt_2 ) | (select3p5 & (~d_sel) & (~cnt_0) & cnt_1);

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        cnt_0  <= 1'b1;
    end else if(cnt_enable == 1'b1) begin
        cnt_0  <= ~(cnt_0 | cnt_reset);
    end else if(cnt_enable == 1'b0) begin
        cnt_0 <= cnt_0;
    end
end

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        cnt_1  <= 1'b1;
    end else if(cnt_enable == 1'b1) begin
        cnt_1  <= ~(cnt_reset | (cnt_0 ^~ cnt_1));
    end else if(cnt_enable == 1'b0) begin
        cnt_1 <= cnt_1;
    end
end

always @(posedge HCLKIN or negedge reset_0) begin
    if (!reset_0) begin
        cnt_2  <= 1'b0;
    end else if(cnt_enable == 1'b1) begin
        cnt_2  <= ~(cnt_reset | (cnt_2 ^~ (cnt_0 & cnt_1)));
    end else if(cnt_enable == 1'b0) begin
        cnt_2 <= cnt_2;
    end
end

assign clk_div_0 = ~cnt_1;
always @(negedge HCLKIN or negedge reset_0) begin
    if(!reset_0)
        clk_div_1 <= 1'b0;
    else
        clk_div_1 <= clk_div_0;
end

assign clk_div = (d_sel == 1'b1) ? clk_div_1 : clk_div_0;
assign clk_div2 = ~cnt_0;

assign CLKOUT = (DIV_MODE == "2") ? clk_div2 : clk_div;

endmodule

//DHCEN
module DHCEN (CLKOUT, CLKIN, CE);
input CLKIN,CE;
output CLKOUT;

reg ce_reg0,ce_reg1,ce_reg2,ce_reg3;

always @(negedge CLKIN)
begin
    ce_reg0 <= ~CE;
    ce_reg1 <= ce_reg0;
    ce_reg2 <= ce_reg1;
    ce_reg3 <= ce_reg2;
end

assign CLKOUT = CLKIN & ce_reg3;

endmodule

// DQS
module DQS(DQSR90, DQSW0, DQSW270, RPOINT, WPOINT, RVALID, RBURST, RFLAG, WFLAG, DQSIN, DLLSTEP, WSTEP, READ, RLOADN, RMOVE, RDIR, WLOADN, WMOVE, WDIR, HOLD, RCLKSEL, PCLK, FCLK, RESET);
input DQSIN,PCLK,FCLK,RESET;
input [3:0] READ;
input [2:0] RCLKSEL;
input [7:0] DLLSTEP;
input [7:0] WSTEP;
input RLOADN, RMOVE, RDIR, WLOADN, WMOVE, WDIR, HOLD;
output DQSR90, DQSW0, DQSW270; 
output [2:0] RPOINT, WPOINT;
output RVALID,RBURST, RFLAG, WFLAG;

    parameter FIFO_MODE_SEL = 1'b0; // FIFO mode select,1'b0: DDR memory mode;1'b1: GDDR mode
    parameter RD_PNTR = 3'b000; // FIFO read pointer setting
    parameter DQS_MODE = "X1"; // "X1", "X2_DDR2", "X2_DDR3","X4","X2_DDR3_EXT"
    parameter HWL = "false";     //"true"; "false"
    parameter GSREN = "false"; //false, true


reg hold_0,hold_1,hold_2,hold_cnt0,hold_cnt1;
wire hold_en,fclk_in,fclk_hold;
realtime dly_time,del,wstep_dly,rstep_dly;
reg [7:0] wstep_reg,rstep_reg,wstep_init;
reg WFLAG,pre_wmove;
reg clk_rd,clk_rd_sft,rstn_0,d_cnt0_0,d_cnt0_1,update0,d_cnt1_0,d_cnt1_1,rstn_1,update1;
reg rd_q1,rd_dq_x1,rd_dq_x2,rd_dq_x4,rd_q1_ext,rd_dq_x2_ext;
reg update0_set,update1_set;
reg rd_q3,rd_d23,rd_d123,rd_dq,rd_dqq,rd_reg,rd_post,rd_pre,dqs_en;
reg [3:0] rd_d0,rd_d1,rd_d_ext,rd_d1_ext;
wire rd_reg_dly,rd_en,dqs_set,dqs_r_clean,rd_dq_del;

wire dqs_en_dly,rstn_det,cnt_det1,cnt_det0_0,cnt_det0_1,cnt_det0;
reg cnt0_det1,cnt0_det1_reg,cnt0_det0,cnt0_det0_reg,cnt1_det0,cnt1_det0_reg;
reg RFLAG,pre_rmove;
wire [255:0] dqsw270_dly_in,dqsr90_dly_in,dqsw0_dly_in;
wire reset_f,reset_wpt,wpt_d_0,wpt_d_1,wpt_d_2;
reg reset_wfq,wpt_q_0,wpt_q_1,wpt_q_2;
reg [2:0] WPOINT;
wire [2:0] rpointer,wpt_q;
reg resetn_rfq,rpt_q_0,rpt_q_1,rpt_q_2;
wire resetn_rpt_0,resetn_rpt_1,resetn_rpt_2,set_rpt_0,set_rpt_1,set_rpt_2,rpt_d_0,rpt_d_1,rpt_d_2;
reg resetn_up,cnt0_up,cnt1_up;
wire update_rd;
reg update_a,update_rp,shift_0,shift_1,shift_q,sign_q,rd_q,rd_q_1,rd_up,rvalid_reg;
wire wr_dqcomp,wr_qqcomp,shift,rqen,rpt_en,ren;
wire ff_mode_sel,x1_mode_sel,r_valid;
wire grstn,reset;
assign grstn = (GSREN == "true") ? GSR.GSRO : 1'b1;
assign reset = RESET | (~grstn);
assign rpointer = RD_PNTR;
assign ff_mode_sel = FIFO_MODE_SEL;

initial
begin
   del = 0.025;
   wstep_reg = 8'b00000000;
   rstep_reg = 8'b00000000;
   wstep_init = 8'b00000000;
   RFLAG = 0;
   WFLAG = 0;
   wstep_dly = 0.0;
   dly_time = 0.0;
   hold_cnt0 = 1'b0;
   hold_cnt1 = 1'b0;
   d_cnt0_0 = 1'b0;
   d_cnt0_1 = 1'b0;
   d_cnt1_0 = 1'b0;
   d_cnt1_1 = 1'b0;
   cnt0_det1 = 1'b0;
   cnt0_det0 = 1'b0;
   cnt1_det0 = 1'b0;
   cnt0_up = 1'b0;
   cnt1_up = 1'b0;
end

// dqs write
always @ (negedge FCLK or posedge reset)  
begin
    if (reset == 1'b1) begin
        hold_0 <= 1'b0;
        hold_1 <= 1'b0;
        hold_2 <= 1'b0;
    end else begin
        hold_0 <= HOLD;
        hold_1 <= hold_0;
        hold_2 <= hold_1;
    end
end

always @ (negedge FCLK or posedge reset)
begin
    if (reset == 1'b1) begin
        hold_cnt0 <= 1'b0;
        hold_cnt1 <= 1'b0;
    end else begin
        if (hold_en == 1'b1)  begin
            hold_cnt0 <= ~hold_cnt0;
            hold_cnt1 <= (hold_cnt0 ^ hold_cnt1);
        end
    end
end

assign hold_en = hold_2 | hold_cnt1 | hold_cnt0;
assign fclk_hold = ~hold_en & FCLK;
assign fclk_in = (DQS_MODE == "X1") ? PCLK : fclk_hold;

always @(wstep_reg) 
begin
    if ((wstep_reg == 8'b11111111 && WDIR == 1'b0) || (wstep_reg == 8'b00000000 && WDIR == 1'b1)) begin
        WFLAG <= 1'b1;
    end else begin
        WFLAG <= 1'b0;
    end
end

always @ (WMOVE)
begin
    pre_wmove <= WMOVE;
end

always @(DLLSTEP or WSTEP) begin
	if(DQS_MODE == "X2_DDR2" || DQS_MODE == "X1")
    begin
	 	wstep_init <= DLLSTEP;
    end else if(DQS_MODE == "X2_DDR3" || DQS_MODE == "X4" || DQS_MODE == "X2_DDR3_EXT")
    begin
        if((DLLSTEP+WSTEP) >= 255) begin
		    wstep_init <= 255;
	    end else begin
	 	    wstep_init <= DLLSTEP+WSTEP;
	    end
    end
end

always @(wstep_init or WLOADN or WMOVE or WDIR) 
begin
    if (WLOADN == 1'b0) begin
        wstep_reg <= wstep_init;
    end else begin
        if (WMOVE == 1'b0 && pre_wmove == 1'b1) begin
            if (WLOADN == 1'b1) begin
                if (WDIR == 1'b0) begin  // plus(+)
                    if (WFLAG == 1'b0 || (wstep_reg <= 8'b00000000)) begin
                        wstep_reg <= wstep_reg + 1;
                    end 
                end else if (WDIR == 1'b1) begin  // minus (-)
                    if (WFLAG == 1'b0 || (wstep_reg == 8'b11111111)) begin
                        wstep_reg <= wstep_reg - 1;
                    end
                end
            end
        end
    end
end

/*********** DQSW/DQSR gen_delay begin *************/
assign dqsw0_dly_in[0] = fclk_in;
assign dqsw270_dly_in[0] =  fclk_in;
assign dqsr90_dly_in[0] = dqs_r_clean;
generate 
   genvar i;
    for(i=1;i<256;i=i+1) begin: gen_delay
      assign #0.025 dqsw0_dly_in[i] = dqsw0_dly_in[i-1];
      assign #0.025 dqsw270_dly_in[i] = dqsw270_dly_in[i-1];
      assign #0.025 dqsr90_dly_in[i] = dqsr90_dly_in[i-1];
    end
endgenerate

wire DQSW90;
assign DQSW0 = (DQS_MODE == "X1") ? fclk_in : dqsw0_dly_in[WSTEP];
assign DQSW90 = (wstep_reg == 0) ? fclk_in : dqsw270_dly_in[wstep_reg];
assign DQSW270 = ~DQSW90;
assign DQSR90 = (rstep_reg == 0) ? dqs_r_clean : dqsr90_dly_in[rstep_reg];
/************* DQSW/DQSR gen_delay end ************/

// clkmux
always @(DQSW270, RCLKSEL[0], DQSW0)
begin
    if(RCLKSEL[0] == 1'b0) begin
        clk_rd <= DQSW0;
    end else if(RCLKSEL[0] == 1'b1) begin
        clk_rd <= ~DQSW270;
    end
end

always @(clk_rd or RCLKSEL[1])
begin
    if(RCLKSEL[1] == 1'b0) begin
         clk_rd_sft <= ~clk_rd;
    end else if(RCLKSEL[1] == 1'b1) begin
         clk_rd_sft <= clk_rd;
    end
end

// dqs read

always @ (posedge FCLK or posedge reset) 
begin
    if (reset == 1'b1)begin
        rstn_0 <= 1'b0;
    end else begin
        rstn_0 <= 1'b1;
   end
end
                                                                                             
always @ (posedge FCLK or negedge rstn_0)
begin
    if (rstn_0 == 1'b0) begin
        d_cnt0_0 <= 1'b0;
        d_cnt0_1 <= 1'b0;
    end else begin
        d_cnt0_0 <= ~d_cnt0_0; 
        d_cnt0_1 <= (d_cnt0_0 ^ d_cnt0_1);
    end
end

always @(d_cnt0_0,d_cnt0_1) begin
    if(DQS_MODE == "X2_DDR2" || DQS_MODE == "X2_DDR3" || DQS_MODE == "X2_DDR3_EXT") begin
        update0_set <= ~d_cnt0_0;
    end else if(DQS_MODE == "X4") begin
        update0_set <= ~d_cnt0_1 & d_cnt0_0; 
    end    
end

always @ (posedge FCLK or negedge rstn_0)
begin
    if (rstn_0 == 1'b0) begin
        update0 <= 1'b0;
    end else begin
        if (update0_set == 1'b1) begin
            update0 <= 1'b1;
        end else begin
            update0 <= 1'b0;
         end
   end
end

always @ (posedge clk_rd or posedge reset)
begin
    if (reset == 1'b1) begin
        rstn_1 <= 1'b0;
    end else begin
        rstn_1 <= 1'b1;
    end
end                                                                                                

always @ (posedge clk_rd or negedge rstn_1)
begin
    if (rstn_1 == 1'b0) begin
        d_cnt1_0 <= 1'b0;
        d_cnt1_1 <= 1'b0;
    end else begin
         d_cnt1_0 <= ~d_cnt1_0;
         d_cnt1_1 <= (d_cnt1_1 ^ d_cnt1_0);
    end
end

always @(d_cnt1_0,d_cnt1_1) begin
    if(DQS_MODE == "X2_DDR2" || DQS_MODE == "X2_DDR3" || DQS_MODE == "X2_DDR3_EXT") begin
        if(HWL == "false") begin
            update1_set <= d_cnt1_0;            
        end else begin        
            update1_set <= ~d_cnt1_0;
        end
    end else if(DQS_MODE == "X4") begin
        if(HWL == "false") begin
            update1_set <= (~d_cnt1_1) & (~d_cnt1_0);        
        end else begin
            update1_set <= ~d_cnt1_1 & d_cnt1_0;
        end
    end
end

always @ (posedge clk_rd or negedge rstn_1)
begin
    if (rstn_1 == 1'b0) begin
        update1 <= 1'b0;
    end else begin
        if (update1_set == 1'b1) begin
            update1 <= 1'b1;
        end else begin
            update1 <= 1'b0;
        end
    end
end

always @(posedge PCLK or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_d0 <= 4'b0;
        rd_d_ext <= 4'b0;
    end else begin
        rd_d0 <= READ;
        rd_d_ext <= rd_d0;
    end
end

always @ (posedge clk_rd or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_dq_x1 <= 1'b0;
    end else begin
        rd_dq_x1 <= rd_d0[0];
    end
end

always @ (posedge FCLK or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_d1 <= 4'b0;
        rd_d1_ext <= 4'b0;
    end else begin
        if (update0 == 1'b1) begin
            rd_d1 <= rd_d0;
            rd_d1_ext <= rd_d_ext;            
        end  else if (update0 == 1'b0) begin
            rd_d1 <= rd_d1;
            rd_d1_ext <= rd_d1_ext;
        end
    end
end

always @ (posedge clk_rd or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_q1 <= 1'b0;
        rd_q1_ext <= 1'b0;
        rd_q3 <= 1'b0;
    end else begin
        if (update1 == 1'b1) begin
            rd_q1 <= rd_d1[1];
            rd_q1_ext <= rd_d1_ext[1];
            rd_q3 <= rd_d1[3];
        end
    end
end

always @ (posedge clk_rd or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_d23 <= 1'b0;
        rd_d123 <= 1'b0;
        rd_dq_x4 <= 1'b0;
        rd_dq_x2 <= 1'b0;
        rd_dq_x2_ext <= 1'b0;
    end else begin
        if (update1 == 1'b1) begin
            rd_d23 <= rd_d1[2];
            rd_d123 <= rd_d1[1];
            rd_dq_x4 <= rd_d1[0];
            rd_dq_x2 <= rd_d1[0];
            rd_dq_x2_ext <= rd_d1_ext[0];
        end else if (update1 == 1'b0) begin
            rd_d23 <= rd_q3;
            rd_d123 <= rd_d23;
            rd_dq_x4 <= rd_d123;
            rd_dq_x2 <= rd_q1;
            rd_dq_x2_ext <= rd_q1_ext;
        end
    end
end

always @(rd_dq_x1,rd_dq_x2,rd_dq_x4,rd_dq_x2_ext) begin
    if(DQS_MODE == "X1") begin
        rd_dq <= rd_dq_x1;
    end else if(DQS_MODE == "X2_DDR2" || DQS_MODE == "X2_DDR3") begin
        rd_dq <= rd_dq_x2;
    end else if(DQS_MODE == "X2_DDR3_EXT") begin
        rd_dq <= rd_dq_x2_ext;
    end else if(DQS_MODE == "X4") begin
        rd_dq <= rd_dq_x4;
    end
end

assign #0.2 rd_dq_del = rd_dq;
   
always @ (posedge clk_rd or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_dqq <= 1'b0;
    end else begin
        rd_dqq <= rd_dq_del;
    end
end

always @ (rd_dq or rd_dqq or RCLKSEL[2])
begin
    if(RCLKSEL[2] == 1'b0) begin
        rd_reg = rd_dq;
    end else if(RCLKSEL[2] == 1'b1) begin
        rd_reg = rd_dqq;
    end
end

assign #0.2 rd_reg_dly = rd_reg;

always @ (posedge clk_rd_sft or posedge reset)
begin
    if (reset == 1'b1) begin
      rd_post <= 1'b0;
    end else begin
      rd_post <= rd_reg_dly;
    end
end

always @ (negedge clk_rd_sft or posedge reset)
begin
    if (reset == 1'b1) begin
        rd_pre <= 1'b0;
    end else begin
        rd_pre <= rd_post;
    end
end

assign rd_en = rd_post & rd_pre;
assign dqs_set = rd_en & (~reset);

always @ (negedge dqs_r_clean or posedge reset or posedge dqs_set)
begin
    if (reset == 1'b1) begin
        dqs_en <= 1'b0;
    end else if (dqs_set == 1'b1) begin
        dqs_en <= 1'b1;
    end  else  begin
        dqs_en <= rd_en;
    end
end

assign dqs_r_clean = DQSIN & dqs_en;

//// burst-det
assign #0.2 dqs_en_dly = ~dqs_en;
assign rstn_det = ~(dqs_en & dqs_en_dly) & (~reset);

always @ (posedge dqs_r_clean or negedge rstn_det)
begin
    if (rstn_det == 1'b0)  begin
        cnt0_det1 <= 1'b0;
        cnt0_det1_reg <= 1'b0;
    end else begin
        if (dqs_en == 1'b1) begin
            cnt0_det1 <= ~cnt0_det1;
            cnt0_det1_reg <= cnt0_det1;
        end
    end
end
assign cnt_det1 = ((~cnt0_det1) & cnt0_det1_reg) &(~dqs_en);

always @ (negedge dqs_r_clean or negedge rstn_det) 
begin
    if (rstn_det == 1'b0) begin
        cnt0_det0 <= 1'b0;
        cnt0_det0_reg <= 1'b0;
    end else begin
        cnt0_det0 <= ~cnt0_det0;
        cnt0_det0_reg <= cnt0_det0;
    end
end
assign cnt_det0_0 = ~((~cnt0_det0) & cnt0_det0_reg);
                              
always @ (negedge DQSR90 or negedge rstn_det)
begin
    if (rstn_det == 1'b0)  begin
        cnt1_det0 <= 1'b0;
        cnt1_det0_reg <= 1'b0;
    end else begin
        if (rd_en == 1'b1)   begin
            cnt1_det0 <= ~cnt1_det0;
            cnt1_det0_reg <= cnt1_det0;
        end
    end
end

assign cnt_det0_1 =  ~(cnt1_det0 & (~cnt1_det0_reg));                                
assign cnt_det0 = ~(cnt_det0_0 | cnt_det0_1);
assign RBURST = cnt_det0 & cnt_det1;

always @(rstep_reg) 
begin
    if ((rstep_reg == 8'b11111111 && RDIR == 1'b0) || (rstep_reg == 8'b00000000 && RDIR == 1'b1)) begin
        RFLAG <= 1'b1;
    end else begin
        RFLAG <= 1'b0;
    end
end

always @ (RMOVE)
begin
    pre_rmove <= RMOVE;
end

always @(DLLSTEP, RLOADN, RMOVE)
begin
    if (RLOADN == 1'b0) begin
        rstep_reg <= DLLSTEP;
    end else begin
        if (RMOVE == 1'b0 && pre_rmove == 1'b1) begin
            if (RLOADN == 1'b1) begin
                if (RDIR == 1'b0) begin  // plus(+)
                    if (RFLAG == 1'b0 || (rstep_reg <= 8'b00000000)) begin
                        rstep_reg <= rstep_reg + 1;
                    end 
                end else if (RDIR == 1'b1) begin  // minus (-)
                    if (RFLAG == 1'b0 || (rstep_reg == 8'b11111111)) begin
                        rstep_reg <= rstep_reg - 1;
                    end
                end
            end
        end
    end
end


// dqs fifo_ctrl
assign fclk_fifo = (DQS_MODE == "X1") ? PCLK : FCLK;
assign reset_f = reset | HOLD;
always @(posedge DQSR90 or  posedge reset_f)
begin
    if (reset_f == 1'b1) begin
        reset_wfq <= 1'b1;
    end else  begin
        reset_wfq <= reset_f;
    end
end

assign reset_wpt = (FIFO_MODE_SEL == 1'b0) ? reset_f : reset_wfq;

assign wpt_d_0 = wpt_q_1 ~^ wpt_q_2;
always @ (posedge DQSR90 or posedge reset_wpt)
begin
    if(reset_wpt==1'b1) begin
        wpt_q_0 <= 1'b0;
        wpt_q_1 <= 1'b0;
        wpt_q_2 <= 1'b0;
    end else begin
        wpt_q_0 <= wpt_d_0;
        wpt_q_1 <= wpt_d_1;
        wpt_q_2 <= wpt_d_2;
    end
end
assign wpt_q = {wpt_q_2,wpt_q_1,wpt_q_0};
assign wpt_d_1 = ((~wpt_q_2) & wpt_q_0) | (wpt_q_1 & (~wpt_q_0));
assign wpt_d_2 = (wpt_q_1 & (~wpt_q_0)) | (wpt_q_2 & wpt_q_0);

always @ (negedge DQSR90 or posedge reset_wpt)
begin
    if(reset_wpt==1'b1) begin
        WPOINT <= 3'b0;
    end else begin
        WPOINT <= wpt_q;
    end
end

////read pointer
always @ (posedge fclk_fifo or posedge reset_f)
begin
    if (reset_f == 1'b1) begin
        resetn_rfq <= 1'b0;
    end else begin
        resetn_rfq <= 1'b1;
    end
end
assign resetn_rpt_0 = resetn_rfq | rpointer[0];
assign resetn_rpt_1 = resetn_rfq | rpointer[1];
assign resetn_rpt_2 = resetn_rfq | rpointer[2];

assign set_rpt_0 = ~(resetn_rfq | ~rpointer[0]);
assign set_rpt_1 = ~(resetn_rfq | ~rpointer[1]);
assign set_rpt_2 = ~(resetn_rfq | ~rpointer[2]);

assign rpt_d_0 = rpt_q_1 ^~ rpt_q_2;
assign rpt_d_1 = ((~rpt_q_2)&rpt_q_0) | (rpt_q_1&(~rpt_q_0));
assign rpt_d_2 = (rpt_q_1&(~rpt_q_0)) | (rpt_q_2&rpt_q_0);

assign rpt_en = (DQS_MODE == "X1")? ren : rqen;

always @(posedge fclk_fifo or negedge resetn_rpt_0 or posedge set_rpt_0)
begin
    if (resetn_rpt_0 == 1'b0) begin
        rpt_q_0 <= 1'b0;
    end else if (set_rpt_0 == 1'b1) begin
        rpt_q_0 <= 1'b1;
    end else begin
        if (rpt_en == 1'b1)  begin
            rpt_q_0 <= rpt_d_0;
        end
    end
end

always @(posedge fclk_fifo or negedge resetn_rpt_1 or posedge set_rpt_1)
begin
    if (resetn_rpt_1 == 1'b0) begin
        rpt_q_1 <= 1'b0;
    end else if (set_rpt_1 == 1'b1) begin
        rpt_q_1 <= 1'b1;
    end else begin
        if (rpt_en == 1'b1)  begin
            rpt_q_1 <= rpt_d_1;
        end
    end
end

always @(posedge fclk_fifo or negedge resetn_rpt_2 or posedge set_rpt_2)
begin
    if (resetn_rpt_2 == 1'b0) begin
        rpt_q_2 <= 1'b0;
    end else if (set_rpt_2 == 1'b1) begin
        rpt_q_2 <= 1'b1;
    end else begin
        if (rpt_en == 1'b1)  begin
            rpt_q_2 <= rpt_d_2;
        end
     end
end

assign RPOINT = {rpt_q_2,rpt_q_1,rpt_q_0};

always @ (posedge fclk_fifo or posedge reset)
begin
    if (reset == 1'b1) begin
        resetn_up <= 1'b0;
    end else begin
        resetn_up <= 1'b1;
    end
end

always @ (posedge fclk_fifo or negedge resetn_up)
begin
    if (resetn_up == 1'b0) begin
        cnt0_up <= 1'b0;
        cnt1_up <= 1'b0;
    end else begin
        cnt0_up <= ~cnt0_up;
        cnt1_up <= (cnt0_up ^ cnt1_up);
   end
end
 
assign update_rd = (DQS_MODE == "X4") ? (~cnt1_up & cnt0_up) : (~cnt0_up); 

always @ (posedge fclk_fifo or negedge resetn_up)
begin
    if (resetn_up == 1'b0) begin
        update_a <= 1'b0;
    end else begin
        if (update_rd == 1'b1) begin
            update_a <= 1'b1;
        end else begin
            update_a <= 1'b0;
         end
   end
end

always @(posedge fclk_fifo or posedge reset_f)
begin
    if (reset_f == 1'b1)  begin
        update_rp <= 1'b0;
    end else begin
        update_rp <= update_a;
    end
end

assign wr_dqcomp = (wpt_q_2 ^ rpt_d_2) | (wpt_q_1 ^ rpt_d_1) | (wpt_q_0 ^ rpt_d_0);
assign wr_qqcomp = (wpt_q_2 ^ rpt_q_2) | (wpt_q_1 ^ rpt_q_1) | (wpt_q_0 ^ rpt_q_0);
assign shift = (~ff_mode_sel & ((wr_dqcomp & sign_q) | (wr_qqcomp & (~sign_q))));

always @(fclk_fifo, resetn_rfq, shift)
begin
    if (resetn_rfq == 1'b0)  begin
        shift_0 <= 1'b0;
        shift_1 <= 1'b0;
    end else begin
        if (fclk_fifo == 1'b0) begin
            shift_0 <= shift;
        end

        if (fclk_fifo == 1'b1) begin
            shift_1 <= shift;
        end
    end
end

always @(negedge fclk_fifo or negedge resetn_rfq)
begin
    if (resetn_rfq == 1'b0)  begin
        shift_q <= 1'b0;
    end else begin
        shift_q <= (shift_0 & ~sign_q);
    end
end

assign x1_mode_sel = (DQS_MODE == "X1") ? 1'b1 : 1'b0;

always @(posedge fclk_fifo or negedge resetn_rfq)
begin
    if (resetn_rfq == 1'b0)  begin
        sign_q <= 1'b0;
    end else begin
        sign_q <= (shift_1 & (shift_q || sign_q || x1_mode_sel));
    end
end

assign ren = sign_q | ff_mode_sel;

always @(posedge fclk_fifo or negedge resetn_rfq)
begin
    if (resetn_rfq == 1'b0) begin
        rd_q <= 1'b0;
        rd_q_1 <= 1'b0;
    end else begin
        if (update_a == 1'b1) begin
            rd_q <= (shift_1 & (shift_q || sign_q || x1_mode_sel));
        end else begin
            rd_q_1 <= rd_q;      
        end
    end
end

assign rqen = rd_q | ff_mode_sel;

always @(posedge fclk_fifo or negedge resetn_rfq)
begin
    if (resetn_rfq == 1'b0) begin
        rd_up <= 1'b0;
    end else  begin
        if (update_rp == 1'b1)  begin
            rd_up <= rd_q_1;
        end
    end
end

assign r_valid = (DQS_MODE == "X1") ? ren : rd_up;

always @(posedge PCLK or negedge resetn_rfq)
begin
    if (resetn_rfq == 1'b0) begin
        rvalid_reg <= 1'b0;
    end else  begin
        rvalid_reg <= r_valid;
    end
end

assign RVALID = rvalid_reg;

endmodule


// DLLDLY
module DLLDLY (CLKOUT, FLAG, DLLSTEP, LOADN, MOVE, DIR, CLKIN);

input CLKIN;
input [7:0] DLLSTEP;
input DIR,LOADN,MOVE;
output CLKOUT;
output FLAG;

parameter DLL_INSEL = 1'b0; //1'b0:bypass mode, 1'b1: use dll_delay cell
parameter DLY_SIGN = 1'b0; // 1'b0:'+',  1'b1: '-'
parameter DLY_ADJ = 0; // 0~255, dly_sign=0 :dly_adj; dly_sign=1: -256+dly_adj

reg drn, premove, clk_effect;
reg flag;
reg [7:0] step_reg;
wire [7:0] dllstep_adj;
real dly_adj;
realtime del, step_dly;
realtime clkin_edge,clkin_period,clkin_period_pre;
reg CLKOUT;
wire grstn = GSR.GSRO; 

initial
begin
   step_reg = 8'b00000000;
   del = 0.025;
   clk_effect = 1'b0;
   flag = 0;
   dly_adj = 0;
end

initial begin
    if(DLY_SIGN == 1'b0) begin
        dly_adj = DLY_ADJ;
    end else if(DLY_SIGN == 1'b1) begin
        dly_adj = (-256) + DLY_ADJ;
    end
end

    always @(posedge CLKIN) begin
        if(!grstn)
            clkin_edge<=0;
        else
            clkin_edge<=$realtime;
            clkin_period<=$realtime-clkin_edge;
            clkin_period_pre<=clkin_period;

        if(clkin_period > 0)     
            //if((clkin_period_pre-clkin_period<0.001) &&(clkin_period_pre-clkin_period>-0.001))
            if(clkin_period_pre == clkin_period)
                clk_effect = 1;
            else
                clk_effect = 0;
    end 
    
    always @(step_reg,DIR) begin
        if ((step_reg == 8'b11111111 && DIR == 1'b0) || (step_reg == 8'b00000000 && DIR == 1'b1))
            flag <= 1'b1;
        else
            flag <= 1'b0;
    end

    assign FLAG = flag;

    always @ (MOVE)
    begin
        premove <= MOVE;
    end

    assign dllstep_adj = ((DLLSTEP + dly_adj) <= 0) ? 0 : ((DLLSTEP + dly_adj) >= 255) ? 255 : (DLLSTEP + dly_adj);

    always @(DLLSTEP, LOADN, MOVE, CLKIN, clk_effect) begin
        if (clk_effect == 1'b1) begin
           if (LOADN == 1'b0) begin
               step_reg <= dllstep_adj; 
           end else begin
               if (MOVE === 1'b0 && premove === 1'b1) begin
                   if (LOADN == 1'b1) begin
                       if (DIR == 1'b0) begin  // plus(+)
                           if (flag == 1'b0 || (step_reg == 8'b00000000)) begin
                               step_reg <= step_reg + 1;
                           end 
                       end else if (DIR == 1'b1) begin  // minus (-)
                           if (flag == 1'b0 || (step_reg == 8'b11111111)) begin
                               step_reg <= step_reg - 1;
                           end
                       end
                   end
               end
            end
        end
    end

    always @(step_reg) begin
        if(DLL_INSEL == 1'b1) begin
            step_dly = step_reg * del;
        end else begin
            step_dly = 0;
        end
    end

    always @(CLKIN) begin
        CLKOUT <= #(step_dly) CLKIN;
    end
    
endmodule


//DCS
module DCS (CLKOUT, CLK0, CLK1, CLK2, CLK3, CLKSEL, SELFORCE);
input CLK0, CLK1, CLK2, CLK3, SELFORCE;
input [3:0] CLKSEL;
output CLKOUT;

  parameter DCS_MODE = "RISING";  //CLK0,CLK1,CLK2,CLK3,GND,VCC,RISING,FALLING,CLK0_GND,CLK0_VCC,CLK1_GND,CLK1_VCC,CLK2_GND,CLK2_VCC,CLK3_GND,CLK3_VCC

wire clk0_out,clk1_out,clk2_out,clk3_out,gnd_out,vcc_out;//CLK,GND,VCC
reg flag_g0,flag_v0,flag_g1,flag_v1,flag_g2,flag_v2,flag_g3,flag_v3;//CLK_GND,CLK_VCC
wire clk0_gnd,clk0_vcc,clk1_gnd,clk1_vcc,clk2_gnd,clk2_vcc,clk3_gnd,clk3_vcc,clk3_vc;

wire clkout_f0,clkout_f1,clkout_f2,clkout_f3,clkout_f;//FALLING
reg flag_f0,flag_f1,flag_f2,flag_f3;

wire clkout_r0,clkout_r1,clkout_r2,clkout_r3,clkout_r;//RISING
reg flag_r0,flag_r1,flag_r2,flag_r3;

wire selforce_out;
reg dcsout;
reg clkout;

initial begin
    flag_g0 = 1'b0;
    flag_v0 = 1'b0;
    flag_g1 = 1'b0;
    flag_v1 = 1'b0;
    flag_g2 = 1'b0;
    flag_v2 = 1'b0;
    flag_g3 = 1'b0;
    flag_v3 = 1'b0;

    flag_f0 = 1'b0;
    flag_f1 = 1'b0;
    flag_f2 = 1'b0;
    flag_f3 = 1'b0;
    flag_r0 = 1'b0;
    flag_r1 = 1'b0;
    flag_r2 = 1'b0;
    flag_r3 = 1'b0;
    clkout = 1'b0;
end

//-------------------------CLK,GND,VCC-------------------
assign clk0_out = CLK0;
assign clk1_out = CLK1;
assign clk2_out = CLK2;
assign clk3_out = CLK3;

assign gnd_out = 1'b0;
assign vcc_out = 1'b1;

//-----------------------------FALLING----------------------
always @(negedge CLK0) begin
    if(CLKSEL[0] && !flag_f1 && !flag_f2 && !flag_f3)
        flag_f0 <= 1'b1;
    else
        flag_f0 <= 1'b0;
end
assign clkout_f0 = CLK0 & flag_f0;

always @(negedge CLK1) begin
    if(CLKSEL[1] && !flag_f0 && !flag_f2 && !flag_f3)
        flag_f1 <= 1'b1;
    else
        flag_f1 <= 1'b0;
end
assign clkout_f1 = CLK1 & flag_f1;

always @(negedge CLK2) begin
    if(CLKSEL[2] && !flag_f0 && !flag_f1 && !flag_f3)
        flag_f2 <= 1'b1;
    else
        flag_f2 <= 1'b0;
end
assign clkout_f2 = CLK2 & flag_f2;

always @(negedge CLK3) begin
    if(CLKSEL[3] && !flag_f0 && !flag_f1 && !flag_f2)
        flag_f3 <= 1'b1;
    else
        flag_f3 <= 1'b0;
end
assign clkout_f3 = CLK3 & flag_f3;

assign clkout_f = flag_f0 ? clkout_f0 : flag_f1 ? clkout_f1 : flag_f2 ? clkout_f2 : flag_f3 ? clkout_f3 : 0;

//-----------------------------RISING----------------------
always @(posedge CLK0) begin
    if(CLKSEL[0] && !flag_r1 && !flag_r2 && !flag_r3)
        flag_r0 <= 1'b1;
    else
        flag_r0 <= 1'b0;
end
assign clkout_r0 = ~(~CLK0 & flag_r0);

always @(posedge CLK1) begin
    if(CLKSEL[1] && !flag_r0 && !flag_r2 && !flag_r3)
        flag_r1 <= 1'b1;
    else
        flag_r1 <= 1'b0;
end
assign clkout_r1 = ~(~CLK1 & flag_r1);

always @(posedge CLK2) begin
    if(CLKSEL[2] && !flag_r0 && !flag_r1 && !flag_r3)
        flag_r2 <= 1'b1;
    else
        flag_r2 <= 1'b0;
end
assign clkout_r2 = ~(~CLK2 & flag_r2);

always @(posedge CLK3) begin
    if(CLKSEL[3] && !flag_r0 && !flag_r1 && !flag_r2)
        flag_r3 <= 1'b1;
    else
        flag_r3 <= 1'b0;
end
assign clkout_r3 = ~(~CLK3 & flag_r3);

assign clkout_r = flag_r0 ? clkout_r0 : flag_r1 ? clkout_r1 : flag_r2 ? clkout_r2 : flag_r3 ? clkout_r3 : 1;

//-----------------------------CLK0_GND----------------------
always @(negedge CLK0) begin
    if(CLKSEL[0])
        flag_g0 <= 1'b1;
    else
        flag_g0 <= 1'b0;
end

assign clk0_gnd = flag_g0 & CLK0;

//-----------------------------CLK0_VCC----------------------
always @(posedge CLK0) begin
    if(CLKSEL[0])
        flag_v0 <= 1'b1;
    else
        flag_v0 <= 1'b0;
end

assign clk0_vcc = ~(flag_v0 & (~CLK0));

//-----------------------------CLK1_GND----------------------
always @(negedge CLK1) begin
    if(CLKSEL[1])
        flag_g1 <= 1'b1;
    else
        flag_g1 <= 1'b0;
end

assign clk1_gnd = flag_g1 & CLK1;

//-----------------------------CLK1_VCC----------------------
always @(posedge CLK1) begin
    if(CLKSEL[1])
        flag_v1 <= 1'b1;
    else
        flag_v1 <= 1'b0;
end

assign clk1_vcc = ~(flag_v1 & (~CLK1));

//-----------------------------CLK2_GND----------------------
always @(negedge CLK2) begin
    if(CLKSEL[2])
        flag_g2 <= 1'b1;
    else
        flag_g2 <= 1'b0;
end

assign clk2_gnd = flag_g2 & CLK2;

//-----------------------------CLK2_VCC----------------------
always @(posedge CLK2) begin
    if(CLKSEL[2])
        flag_v2 <= 1'b1;
    else
        flag_v2 <= 1'b0;
end

assign clk2_vcc = ~(flag_v2 & (~CLK2));

//-----------------------------CLK3_GND----------------------
always @(negedge CLK3) begin
    if(CLKSEL[3])
        flag_g3 <= 1'b1;
    else
        flag_g3 <= 1'b0;
end

assign clk3_gnd = flag_g3 & CLK3;

//-----------------------------CLK3_VCC----------------------
always @(posedge CLK3) begin
    if(CLKSEL[3])begin
        flag_v3 <= 1'b1;
    end else begin
        flag_v3 <= 1'b0;
    end
end

assign clk3_vcc = ~(flag_v3 & (~CLK3));
assign clk3_vc = flag_v3 ? CLK3 : 1'b1;

//--------------------------------dcsout-------------------------------
always @(clk0_out,clk1_out,clk2_out,clk3_out,gnd_out,vcc_out,clk0_gnd,clk0_vcc,clk1_gnd,clk1_vcc,clk2_gnd,clk2_vcc,clk3_gnd,clk3_vcc,clkout_f,clkout_r) begin
    if(DCS_MODE == "CLK0")
        dcsout <= clk0_out;
    else if(DCS_MODE == "CLK1")
        dcsout <= clk1_out;
    else if(DCS_MODE == "CLK2")
        dcsout <= clk2_out;
    else if(DCS_MODE == "CLK3")
        dcsout <= clk3_out;
    else if(DCS_MODE == "GND")
        dcsout <= gnd_out;
    else if(DCS_MODE == "VCC")
        dcsout <= vcc_out;
    else if(DCS_MODE == "FALLING")
        dcsout <= clkout_f;
    else if(DCS_MODE == "RISING")
        dcsout <= clkout_r;
    else if(DCS_MODE == "CLK0_GND")
        dcsout <= clk0_gnd;
    else if(DCS_MODE == "CLK0_VCC")
        dcsout <= clk0_vcc;
    else if(DCS_MODE == "CLK1_GND")
        dcsout <= clk1_gnd;
    else if(DCS_MODE == "CLK1_VCC")
        dcsout <= clk1_vcc;
    else if(DCS_MODE == "CLK2_GND")
        dcsout <= clk2_gnd;
    else if(DCS_MODE == "CLK2_VCC")
        dcsout <= clk2_vcc;
    else if(DCS_MODE == "CLK3_GND")
        dcsout <= clk3_gnd;
    else if(DCS_MODE == "CLK3_VCC")
        dcsout <= clk3_vcc;
    else
        dcsout <= 0;
end

//--------------------------------clkout-------------------------------
assign selforce_out = (CLKSEL == 4'b0001) ? CLK0 : (CLKSEL == 4'b0010) ? CLK1 : (CLKSEL == 4'b0100) ? CLK2 : (CLKSEL == 4'b1000) ? CLK3 : 0;
always @(dcsout or selforce_out or SELFORCE) begin
    if(!SELFORCE)
        clkout <= dcsout;
    else
        clkout <= selforce_out;
end
        
assign CLKOUT = clkout;

endmodule

//DQCE
module DQCE(CLKOUT, CLKIN, CE);
input CLKIN;
input CE;
output CLKOUT;
reg ce_reg;

always @ (negedge CLKIN) begin
    ce_reg <= CE;
end

assign CLKOUT = CLKIN & ce_reg;

endmodule


