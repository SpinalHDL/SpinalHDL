
// ===========Oooo==========================================Oooo========
// =  Copyright (C) 2014-2020 Gowin Semiconductor Technology Co.,Ltd.
// =                     All rights reserved.
// =====================================================================
//
//  __      __      __
//  \ \    /  \    / /   [File name   ] prim_syn.v
//   \ \  / /\ \  / /    [Description ] GW2A verilog functional synthesis library
//    \ \/ /  \ \/ /     [Timestamp   ] Fri January 10 11:00:30 2019
//     \  /    \  /      [version     ] 1.9.4
//      \/      \/       
//
// ===========Oooo==========================================Oooo========



`timescale 1ns / 1ps


module MUX2 (O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2

module MUX2_LUT5 (O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2 LUT_5: use two 4-input LUTs and 1 MUX2_LUT_5 to construct 5-input LUT

module MUX2_LUT6 (O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2 LUT_6: use two 5-input LUTs and 1 MUX2_LUT_6 to construct 6-input LUT

module MUX2_LUT7 (O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2 LUT_7: use two 6-input LUTs and 1 MUX2_LUT_7 to construct 7-input LUT

module MUX2_LUT8 (O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2 LUT_8: use two 7-input LUTs and 1 MUX2_LUT_8 to construct 8-input LUT

module MUX2_MUX8(O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule // MUX2_MUX8:use two 4-input MUXs and 1 MUX2_MUX8's to construct 8-input mux

module MUX2_MUX16(O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2_MUX16:use two 8-input MUXs and 1 MUX2_MUX8's to construct 16-input mux

module MUX2_MUX32(O, I0, I1, S0)  /* synthesis syn_black_box  */;
input I0,I1;
input S0;
output O;
endmodule //MUX2_MUX32:use two 16-input MUXs and 1 MUX2_MUX16's to construct 32-input mux

module MUX4 (O, I0, I1, I2, I3, S0, S1)  /* synthesis syn_black_box  */;
input I0, I1, I2, I3;
input S0, S1;
output O;
endmodule // MUX4

module MUX8 (O, I0, I1, I2, I3, I4, I5, I6, I7, S0, S1, S2)  /* synthesis syn_black_box  */;
input I0, I1, I2, I3, I4, I5, I6, I7;
input S0, S1, S2;
output O;
endmodule //MUX8

module MUX16(O, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, S0, S1, S2, S3)  /* synthesis syn_black_box  */;

input I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15;
input S0, S1, S2, S3;
output O;
endmodule

module MUX32(O, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15,I16, I17, I18, I19, I20, I21, I22, I23, I24, I25, I26, I27, I28, I29, I30,I31, S0, S1, S2, S3, S4)  /* synthesis syn_black_box  */;
input I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15, I16, I17, I18, I19, I20, I21, I22, I23, I24, I25, I26, I27, I28, I29, I30, I31;
input S0, S1, S2, S3, S4;
output O;
endmodule


module LUT1 (F, I0)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 2'h0;
input I0;
output F;
endmodule //lut1

module LUT2 (F, I0, I1)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 4'h0;
input I0, I1;
output F;
endmodule //lut2

module LUT3 (F, I0, I1, I2)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 8'h00;
input I0, I1, I2;
output F;
endmodule //lut3

module LUT4 (F, I0, I1, I2, I3)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 16'h0000;
input I0, I1, I2, I3;
output F;
endmodule //lut4

module LUT5 (F, I0, I1, I2, I3, I4)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 32'h00000000;
input I0, I1, I2, I3, I4;
output F;
endmodule//lut5

module LUT6 (F, I0, I1, I2, I3, I4, I5)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 64'h0000_0000_0000_0000;
input I0, I1, I2, I3, I4, I5;
output F;
endmodule//lut6

module LUT7 (F, I0, I1, I2, I3, I4, I5, I6)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 128'h0000_0000_0000_0000_0000_0000_0000_0000;
input I0, I1, I2, I3, I4, I5, I6;
output F;
endmodule//lut7

module LUT8 (F, I0, I1, I2, I3, I4, I5, I6, I7)  /* synthesis syn_black_box  xc_map=lut */;
parameter INIT = 256'h0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
input I0, I1, I2, I3, I4, I5, I6, I7;
output F;
endmodule//lut8

module ALU (SUM, COUT, I0, I1, I3, CIN)  /* synthesis syn_black_box  */;

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

endmodule // ALU: 2-input arithmetic logic unit


module DFF (Q, D, CLK)  /* synthesis syn_black_box  */;
input D, CLK;
output Q;
parameter INIT = 1'b0;
endmodule // DFF (positive clock edge)

module DFFE (Q, D, CLK, CE)  /* synthesis syn_black_box  */;
input D, CLK, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFE (positive clock edge; clock enable)

module DFFS (Q, D, CLK, SET)  /* synthesis syn_black_box  */;
input D, CLK, SET;
output Q;
parameter INIT = 1'b1;
endmodule // DFFS (positive clock edge; synchronous set)

module DFFSE (Q, D, CLK, CE, SET)  /* synthesis syn_black_box  */;
input D, CLK, SET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DFFSE (positive clock edge; synchronous set takes precedence over clock enable)

module DFFR (Q, D, CLK, RESET)  /* synthesis syn_black_box  */;
input D, CLK, RESET;
output Q;
parameter INIT = 1'b0;
endmodule // DFFR (positive clock edge; synchronous reset)

module DFFRE (Q, D, CLK, CE, RESET)  /* synthesis syn_black_box  */;
input D, CLK, RESET, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFRE (positive clock edge; synchronous reset takes precedence over clock enable)

module DFFP (Q, D, CLK, PRESET)  /* synthesis syn_black_box  */;
input D, CLK, PRESET;
output Q;
parameter INIT = 1'b1;
endmodule // DFFP (positive clock edge; asynchronous preset)

module DFFPE (Q, D, CLK, CE, PRESET)  /* synthesis syn_black_box  */;
input D, CLK, PRESET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DFFPE (positive clock edge; asynchronous preset; clock enable)

module DFFC (Q, D, CLK, CLEAR)  /* synthesis syn_black_box  */;
input D, CLK, CLEAR;
output Q;
parameter INIT = 1'b0;
endmodule // DFFC (positive clock edge; asynchronous clear)

module DFFCE (Q, D, CLK, CE, CLEAR)  /* synthesis syn_black_box  */;
input D, CLK, CLEAR, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFCE (positive clock edge; asynchronous clear; clock enable)

module DFFN (Q, D, CLK)  /* synthesis syn_black_box  */;
input D, CLK;
output Q;
parameter INIT = 1'b0;
endmodule // DFFN (negative clock edge)

module DFFNE (Q, D, CLK, CE)  /* synthesis syn_black_box  */;
input D, CLK, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFNE (negative clock edge; clock enable)

module DFFNS (Q, D, CLK, SET)  /* synthesis syn_black_box  */;
input D, CLK, SET;
output Q;
parameter INIT = 1'b1;
endmodule // DFFNS (negative clock edge; synchronous set)

module DFFNSE (Q, D, CLK, CE, SET)  /* synthesis syn_black_box  */;
input D, CLK, SET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DFFNSE (negative clock edge; synchronous set takes precedence over clock enable)

module DFFNR (Q, D, CLK, RESET)  /* synthesis syn_black_box  */;
input D, CLK, RESET;
output Q;
parameter INIT = 1'b0;
endmodule // DFFNR (negative clock edge; synchronous reset)

module DFFNRE (Q, D, CLK, CE, RESET)  /* synthesis syn_black_box  */;
input D, CLK, RESET, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFNRE (negative clock edge; synchronous reset takes precedence over clock enable)

module DFFNP (Q, D, CLK, PRESET)  /* synthesis syn_black_box  */;
input D, CLK, PRESET;
output Q;
parameter INIT = 1'b1;
endmodule // DFFNP (negative clock edge; asynchronous preset)

module DFFNPE (Q, D, CLK, CE, PRESET)  /* synthesis syn_black_box  */;
input D, CLK, PRESET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DFFNPE (negative clock edge; asynchronous preset; clock enable)

module DFFNC (Q, D, CLK, CLEAR)  /* synthesis syn_black_box  */;
input D, CLK, CLEAR;
output Q;
parameter INIT = 1'b0;
endmodule // DFFNC (negative clock edge; asynchronous clear)

module DFFNCE (Q, D, CLK, CE, CLEAR)  /* synthesis syn_black_box  */;
input D, CLK, CLEAR, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DFFNCE (negative clock edge; asynchronous clear; clock enable)

module DL (Q, D, G)  /* synthesis syn_black_box  */;
input D, G;
output Q;
parameter INIT = 1'b0;
endmodule // DL (high active latch)

module DLE (Q, D, G, CE)  /* synthesis syn_black_box  */;
input D, G, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DLE (high active latch; latch enable)

module DLC (Q, D, G, CLEAR)  /* synthesis syn_black_box  */;
input D, G, CLEAR;
output Q;
parameter INIT = 1'b0;
endmodule // DLC (high active latch; asynchronous clear)

module DLCE (Q, D, G, CE, CLEAR)  /* synthesis syn_black_box  */;
input D, G, CLEAR, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DLCE (high active latch; asynchronous clear; latch enable)

module DLP (Q, D, G, PRESET)  /* synthesis syn_black_box  */;
input D, G, PRESET;
output Q;
parameter INIT = 1'b1;
endmodule // DLP (high active latch; asynchronous preset)

module DLPE (Q, D, G, CE, PRESET)  /* synthesis syn_black_box  */;
input D, G, PRESET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DLPE (high active latch; asynchronous preset; latch enable)

module DLN (Q, D, G)  /* synthesis syn_black_box  */;
input D, G;
output Q;
parameter INIT = 1'b0;
endmodule // DLN (low active latch)

module DLNE (Q, D, G, CE)  /* synthesis syn_black_box  */;
input D, G, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DLNE (low active latch; latch enable)

module DLNC (Q, D, G, CLEAR)  /* synthesis syn_black_box  */;
input D, G, CLEAR;
output Q;
parameter INIT = 1'b0;
endmodule // DLNC (low active latch; asynchronous clear)

module DLNCE (Q, D, G, CE, CLEAR)  /* synthesis syn_black_box  */;
input D, G, CLEAR, CE;
output Q;
parameter INIT = 1'b0;
endmodule // DLNCE (low active latch; asynchronous clear; latch enable)

module DLNP (Q, D, G, PRESET)  /* synthesis syn_black_box  */;
input D, G, PRESET;
output Q;
parameter INIT = 1'b1;
endmodule // DLNP (low active latch; asynchronous preset)

module DLNPE (Q, D, G, CE, PRESET)  /* synthesis syn_black_box  */;
input D, G, PRESET, CE;
output Q;
parameter INIT = 1'b1;
endmodule // DLNPE (low active latch; asynchronous preset; latch enable)

module INV (O, I)  /* synthesis syn_black_box  */;
input  I;
output O;
endmodule // inv

module IBUF (O, I)  /* synthesis syn_black_box  */;
input  I;
output O;
endmodule //IBUF (input buffer)

module OBUF (O, I)  /* synthesis syn_black_box  */;
input  I;
output O;
endmodule //OBUF (output buffer)

module TBUF (O, I, OEN)  /* synthesis syn_black_box black_box_pad_pin = "O"  */;
input I, OEN;
output O;
endmodule // TBUF (output buffer with tri-state control)

module IOBUF (O, IO, I, OEN)  /* synthesis syn_black_box  */;
input I,OEN;
output O;
inout IO;
endmodule //IOBUF (inout buffer)

module IDDR(Q0, Q1, D, CLK)  /* synthesis syn_black_box  */;
input D;
input CLK;
output Q0;
output Q1;
parameter Q0_INIT = 1'b0;
parameter Q1_INIT = 1'b0;
endmodule //IDDR (ddr input)

module IDDRC(Q0, Q1, D, CLK, CLEAR)  /* synthesis syn_black_box  */;
input D;
input CLK;
input CLEAR;
output Q0;
output Q1;
parameter Q0_INIT = 1'b0;
parameter Q1_INIT = 1'b0;
endmodule //IDDRC (ddr input, asynchronous clear)

module IDDR_MEM (Q0, Q1, D, WADDR, RADDR, PCLK, ICLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false";  //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, ICLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input RESET;
output  Q0,Q1;
endmodule // IDDR_MEM (ddr input with asynchronous preset)

module ODDR (Q0, Q1, D0, D1, TX, CLK)  /* synthesis syn_black_box  */;
input D0;
input D1;
input TX;
input CLK;
output Q0;
output Q1;
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output
parameter INIT = 1'b0;
endmodule // ODDRS (ddr output)

module ODDRC (Q0, Q1, D0, D1, TX, CLK, CLEAR)  /* synthesis syn_black_box  */;
input D0, D1, TX, CLK, CLEAR;
output Q0, Q1;
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output
parameter INIT = 1'b0;
endmodule // ODDRC (ddr output with asynchronous clear)

module ODDR_MEM (Q0, Q1, D0, D1, TX, PCLK, TCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1;
input TX, PCLK, TCLK, RESET;
output  Q0, Q1;
endmodule // ODDR_MEM (ddr output with memory)

module IDES4 (Q0, Q1, Q2, Q3, D, CALIB, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, FCLK, PCLK, CALIB, RESET;
output Q0,Q1,Q2,Q3;
endmodule // IDES4 (4 to 1 deserializer)

module IDES4_MEM (Q0, Q1, Q2, Q3, D, WADDR, RADDR, CALIB, PCLK, FCLK, ICLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, ICLK, FCLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input CALIB, RESET;
output Q0,Q1,Q2,Q3;
endmodule //IDES4_MEM (4 to 1 deserializer with memory)

module IVIDEO (Q0, Q1, Q2, Q3, Q4, Q5, Q6, D, CALIB, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";   //"true"; "false"
input D, FCLK, PCLK, CALIB, RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6;
endmodule //IVIDEO (7 to 1 deserializer)

module IDES8 (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, D, CALIB, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, FCLK, PCLK, CALIB, RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7;
endmodule // IDES8 (8 to 1 deserializer)

module IDES8_MEM (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, D, WADDR, RADDR, CALIB, PCLK, FCLK, ICLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, ICLK, FCLK, PCLK;
input [2:0] WADDR;
input [2:0] RADDR;
input CALIB, RESET;
output  Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7;
endmodule // IDES8_MEM (8 to 1 deserializer with memory)

module IDES10 (Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, D, CALIB, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, FCLK, PCLK, CALIB, RESET;
output Q0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9;
endmodule // IDES10 (10 to 1 deserializer)

module OSER4 (Q0, Q1, D0, D1, D2, D3, TX0, TX1, PCLK, FCLK, RESET)  /* synthesis syn_black_box */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";   //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D3, D2, D1, D0;
input TX1, TX0;
input PCLK, FCLK, RESET;
output  Q0, Q1;
endmodule // OSER4 (4 to 1 serializer)

module OSER4_MEM (Q0, Q1, D0, D1, D2, D3, TX0, TX1, PCLK, FCLK, TCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3;
input TX0, TX1;
input PCLK, FCLK, TCLK, RESET;
output  Q0,  Q1;
endmodule // OSER4_MEM (4 to 1 serializer with memory)

module OVIDEO (Q, D0, D1, D2, D3, D4, D5, D6, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D6, D5, D4, D3, D2, D1, D0;
input PCLK, FCLK, RESET;
output  Q;
endmodule // OVIDEO (7 to 1 serializer)

module OSER8 (Q0, Q1, D0, D1, D2, D3, D4, D5, D6, D7, TX0, TX1, TX2, TX3, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";     //"true"; "false"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3, D4, D5, D6, D7;
input TX0, TX1, TX2, TX3;
input PCLK, FCLK, RESET;
output  Q0,  Q1;
endmodule // OSER8 (8 to 1 serializer)

module OSER8_MEM (Q0, Q1, D0, D1, D2, D3, D4, D5, D6, D7, TX0, TX1, TX2, TX3, PCLK, FCLK, TCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
parameter HWL = "false";    //"true"; "false"
parameter TCLK_SOURCE = "DQSW"; //"DQSW","DQSW270"
parameter TXCLK_POL = 1'b0; //1'b0:Rising edge output; 1'b1:Falling edge output

input D0, D1, D2, D3, D4, D5, D6, D7;
input TX0, TX1, TX2, TX3;
input PCLK, FCLK, TCLK, RESET;
output  Q0,  Q1;
endmodule // OSER8_MEM (8 to 1 serializer with memory)

module OSER10 (Q, D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, PCLK, FCLK, RESET)  /* synthesis syn_black_box  */;
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D0, D1, D2, D3, D4, D5, D6, D7, D8, D9;
input PCLK, FCLK, RESET;
output  Q;
endmodule // OSER10 (10 to 1 serializer)

module IODELAY (DO, DF, DI, SDTAP, VALUE, SETN)  /* synthesis syn_black_box  */;
parameter C_STATIC_DLY = 0; //integer, 0~127
input DI;
input  SDTAP;
input  SETN;
input  VALUE;
output DF;
output DO;
endmodule // IODELAY

module IEM (LAG, LEAD, D, CLK, MCLK, RESET)  /* synthesis syn_black_box  */;
parameter WINSIZE = "SMALL"; //"SMALL"; "MIDSMALL"; "MIDLARGE"; "LARGE"
parameter GSREN = "false"; //"true"; "false"
parameter LSREN = "true";    //"true"; "false"
input D, CLK, RESET, MCLK;
output LAG, LEAD;
endmodule // IEM

module RAM16S1 (DO, DI, AD, WRE, CLK)  /* synthesis syn_black_box  */;
input CLK;
input WRE;
input [3:0] AD;
input DI;
output DO;
parameter INIT_0 = 16'h0000;
endmodule // RAM16S1: signal-port S-SRAM(16X1)

module RAM16S2 (DO, DI, AD, WRE, CLK)  /* synthesis syn_black_box  */;
input CLK;
input WRE; 
input [3:0] AD;
input [1:0] DI;
output [1:0] DO;
parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;
endmodule // RAM16S2: single-port S-SRAM(16X2)

module RAM16S4 (DO, DI, AD, WRE, CLK)  /* synthesis syn_black_box  */;
input CLK;
input WRE; 
input [3:0] AD;
input [3:0] DI;
output [3:0] DO;
parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;
parameter INIT_2 = 16'h0000;
parameter INIT_3 = 16'h0000;
endmodule // RAM16S4: single-port S-SRAM(16X4)

module RAM16SDP1 (DO, DI, WAD, RAD, WRE, CLK)  /* synthesis syn_black_box  */;
input CLK;
input WRE;
input [3:0] WAD;
input DI;
input [3:0] RAD;
output DO;
parameter INIT_0 = 16'h0000;
endmodule // RAM16SDP1: simple dual-port S-SRAM(16X1)

module RAM16SDP2 (DO, DI, WAD, RAD, WRE, CLK)  /* synthesis syn_black_box  */;
input CLK;
input WRE; 
input [3:0] WAD;
input [1:0] DI;
input [3:0] RAD;
output [1:0] DO;
parameter INIT_0 = 16'h0000;
parameter INIT_1 = 16'h0000;
endmodule // RAM16SDP2: simple dual-port S-SRAM(16X2)

module RAM16SDP4 (DO, DI, WAD, RAD, WRE, CLK)  /* synthesis syn_black_box  */;
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
endmodule // RAM16SDP4: simple dual-port S-SRAM(16X4)

module ROM16 (DO, AD)  /* synthesis syn_black_box  */;
parameter INIT_0 = 16'h0000;
input [3:0] AD;
output DO;
endmodule // ROM16: signal-port shadow ROM(16 bit)

module SP (DO, DI, BLKSEL, AD, WRE, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
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
input RESET; // resets input and output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] AD;
input [31:0] DI;
input [2:0] BLKSEL;
output [31:0] DO;
endmodule // SP: single port 16k Block SRAM

module SPX9 (DO, DI, BLKSEL, AD, WRE, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
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
input RESET; // resets input and output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] AD;
input [2:0] BLKSEL;
input [35:0] DI;
output [35:0] DO;
endmodule // SPX9: single port 18k Block SRAM

module SDP (DO, DI, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [31:0] DI;
output [31:0] DO;
endmodule // SDP: simple dual port 16k Block SRAM

module SDPX9 (DO, DI, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [35:0] DI;
output [35:0] DO;
endmodule // SDPX9: simple dual port 18k Block SRAM

module DP (DOA, DOB, DIA, DIB, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 16; // 1, 2, 4, 8, 16
parameter BIT_WIDTH_1 = 16; // 1, 2, 4, 8, 16
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

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [15:0] DIA, DIB;
output [15:0] DOA, DOB;
endmodule // DP: true dual port 16k Block SRAM

module DPX9 (DOA, DOB, DIA, DIB, BLKSEL, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 18; // 9, 18
parameter BIT_WIDTH_1 = 18; // 9, 18
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

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [17:0] DIA, DIB;
output [17:0] DOA, DOB;
endmodule // DPX9: true dual port 18k Block SRAM

module ROM (DO, BLKSEL, AD, WRE, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
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
input RESET; // resets input and output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: read disabled
input [13:0] AD;
input [2:0] BLKSEL;
output [31:0] DO;
endmodule // ROM: 16k Block ROM

module ROMX9 (DO, BLKSEL, AD, WRE, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
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
input RESET; // resets input and output registers, not memory contents
input WRE; // 1'b0: read enabled; 1'b1: read disabled
input [13:0] AD;
input [2:0] BLKSEL;
output [35:0] DO;
endmodule // ROMX9: 18k Block ROM

//rSDP
module rSDP (DO, DI, BLKSEL, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [31:0] DI;
output [31:0] DO;
endmodule // rSDP: revision simple dual port 16k Block SRAM

//rSDPX9
module rSDPX9 (DO, DI, BLKSEL, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSEL;
input [35:0] DI;
output [35:0] DO;
endmodule // rSDPX9: revision simple dual port 18k Block SRAM

//rROM
module rROM (DO, BLKSEL, AD, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
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
input RESET; // resets input and output registers, not memory contents
input [13:0] AD;
input [2:0] BLKSEL;
output [31:0] DO;
endmodule // rROM: revision 16k Block ROM

//rROMX9
module rROMX9 (DO, BLKSEL, AD, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
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
input RESET; // resets input and output registers, not memory contents
input [13:0] AD;
input [2:0] BLKSEL;
output [35:0] DO;
endmodule // rROMX9: revision 18k Block ROM

//pROM
module pROM (DO, AD, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 32; // 1, 2, 4, 8, 16, 32
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
input RESET; // resets input and output registers, not memory contents
input [13:0] AD;
output [31:0] DO;
endmodule // pROM

//pROMX9
module pROMX9 (DO, AD, CLK, CE, OCE, RESET)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH = 36; // 9, 18, 36
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
input RESET; // resets input and output registers, not memory contents
input [13:0] AD;
output [35:0] DO;
endmodule // pROMX9


//SDPB
module SDPB (DO, DI, BLKSELA, BLKSELB, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 32; // 1, 2, 4, 8, 16, 32
parameter BIT_WIDTH_1 = 32; // 1, 2, 4, 8, 16, 32
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [31:0] DI;
output [31:0] DO;
endmodule // SDPB: Semi dual port 16k Block SRAM

//SDPX9B
module SDPX9B (DO, DI, BLKSELA, BLKSELB, ADA, ADB, CLKA, CLKB, CEA, CEB, OCE, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter BIT_WIDTH_0 = 36; // 9, 18, 36
parameter BIT_WIDTH_1 = 36; // 9, 18, 36
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
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

input CLKA, CEA, CLKB, CEB;
input OCE; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [35:0] DI;
output [35:0] DO;
endmodule // SDPX9B: Semi dual port 18k Block SRAM

//DPB
module DPB (DOA, DOB, DIA, DIB, BLKSELA, BLKSELB, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 16; // 1, 2, 4, 8, 16
parameter BIT_WIDTH_1 = 16; // 1, 2, 4, 8, 16
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
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

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [15:0] DIA, DIB;
output [15:0] DOA, DOB;
endmodule // DPB: true dual port 16k Block SRAM

module DPX9B (DOA, DOB, DIA, DIB, BLKSELA, BLKSELB, ADA, ADB, WREA, WREB, CLKA, CLKB, CEA, CEB, OCEA, OCEB, RESETA, RESETB)  /* synthesis syn_black_box  */;
parameter READ_MODE0 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter READ_MODE1 = 1'b0; // 1'b0: bypass mode; 1'b1: pipeline mode
parameter WRITE_MODE0 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter WRITE_MODE1 = 2'b00; // 2'b00: normal mode; 2'b01: write-through mode; 2'b10: read-before-write mode
parameter BIT_WIDTH_0 = 18; // 9, 18
parameter BIT_WIDTH_1 = 18; // 9, 18
parameter BLK_SEL_0 = 3'b000;
parameter BLK_SEL_1 = 3'b000;
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

input CLKA, CEA, CLKB, CEB;
input OCEA, OCEB; // clock enable of memory output register
input RESETA, RESETB; // resets input and output registers, not memory contents
input WREA, WREB; // 1'b0: read enabled; 1'b1: write enabled
input [13:0] ADA, ADB;
input [2:0] BLKSELA, BLKSELB;
input [17:0] DIA, DIB;
output [17:0] DOA, DOB;
endmodule // DPX9B: true dual port 18k Block SRAM


// PADD18
module PADD18 (DOUT, SO, SBO, A, B, SI, SBI, ASEL, CLK, CE, RESET) /* synthesis syn_black_box  */;

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


endmodule

// PADD9
module PADD9 (DOUT, SO, SBO, A, B, SI, SBI, ASEL, CLK, CE, RESET) /* synthesis syn_black_box */;

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

endmodule

// MULT9X9
module MULT9X9 (DOUT, SOA, SOB, A, B, SIA, SIB, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box */;

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

endmodule

//MULT18X18
module MULT18X18 (DOUT, SOA, SOB, A, B, SIA, SIB, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;

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

endmodule

//MULTALU section
// MULT36X36
module MULT36X36 (DOUT, A, B, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;

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

endmodule

//MULTALU36X18
module MULTALU36X18 (DOUT, CASO, A, B, C, CASI, ACCLOAD, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;

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

endmodule

// MULTADDALU18X18
module MULTADDALU18X18 (DOUT, CASO, SOA, SOB, A0, B0, A1, B1, C, SIA, SIB, CASI, ACCLOAD, ASEL, BSEL, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;

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

endmodule

// MULTALU18X18
module MULTALU18X18 (DOUT, CASO, A, B, C, D, CASI, ACCLOAD, ASIGN, BSIGN, DSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;
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

endmodule

//ALU-direct mode
module ALU54D (DOUT, CASO, A, B, CASI, ACCLOAD, ASIGN, BSIGN, CLK, CE, RESET) /* synthesis syn_black_box  */;
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

endmodule


//PLL start
module PLL (CLKOUT, CLKOUTP, CLKOUTD, CLKOUTD3, LOCK, CLKIN, CLKFB, FBDSEL, IDSEL, ODSEL, DUTYDA, PSDA, FDLY, RESET, RESET_P, RESET_I, RESET_S)/* synthesis syn_black_box  */;
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

parameter FCLKIN = "100.0"; // frequency of the clkin(M)
parameter DYN_IDIV_SEL= "false";//true:IDSEL; false:IDIV_SEL
parameter IDIV_SEL = 0; // 0:1,1:2...63:64. 1~64
parameter DYN_FBDIV_SEL= "false";
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

parameter CLKFB_SEL = "internal";//"internal","external";
parameter CLKOUT_BYPASS = "false";  //"true"; "false"
parameter CLKOUTP_BYPASS = "false";   //"true"; "false"
parameter CLKOUTD_BYPASS = "false";  //"true"; "false"
parameter DYN_SDIV_SEL = 2; // 2~128,only even num
parameter CLKOUTD_SRC =  "CLKOUT";  //CLKOUT,CLKOUTP
parameter CLKOUTD3_SRC = "CLKOUT"; //CLKOUT,CLKOUTP
parameter DEVICE = "GW2A-18";//"GW2A-18","GW2A-55","GW2AR-18","GW2A-55C","GW2A-18C","GW2AR-18C"

endmodule



module BUFG (O, I)  /* synthesis syn_black_box  */;
output O;
input I;
endmodule // BUFG (global clock buffer)

module BUFS (O, I)  /* synthesis syn_black_box  */;
output O;
input I;
endmodule //BUFS (long wire clock buffer)

module GND (G)  /* synthesis syn_black_box  */;
output G;
endmodule

module VCC (V)  /* synthesis syn_black_box  */;
output V;
endmodule

module GSR (GSRI)  /* synthesis syn_black_box  syn_noprune = 1 */;
input GSRI;
endmodule //GSR (global set/reset control)

module OSC ( OSCOUT)  /* synthesis syn_black_box  */;
parameter  FREQ_DIV = 100; // 2~128,only even num
parameter  DEVICE = "GW2A-18";//GW2A-18,GW2A-55,GW2AR-18,GW2A-55C,GW2A-18C,GW2AR-18C
output OSCOUT;
endmodule


//true LVDS
module TLVDS_IBUF (O, I, IB)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "I, IB" */;
output O;
input  I, IB;
endmodule

module TLVDS_OBUF (O, OB, I)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "O, OB" */;
output O, OB;
input  I;
endmodule

module TLVDS_TBUF (O, OB, I, OEN)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "O, OB" */;
output O, OB;
input  I, OEN;
endmodule

module TLVDS_IOBUF (O, IO, IOB, I, OEN)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "IO, IOB" */;
output   O;
inout IO, IOB;
input I, OEN;
endmodule

//emulated LVDS
module ELVDS_IBUF (O, I, IB)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "I, IB" */;
output O;
input  I, IB;
endmodule

module ELVDS_OBUF (O, OB, I)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "O, OB" */;
output O, OB;
input  I;
endmodule

module ELVDS_TBUF (O, OB, I, OEN)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "O, OB" */;
output O, OB;
input  I, OEN;
endmodule

module ELVDS_IOBUF (O, IO, IOB, I, OEN)  /* synthesis syn_black_box syn_lib_cell=1 black_box_pad_pin = "IO, IOB" */;
output   O;
inout IO, IOB;
input I, OEN;
endmodule

//DLL
module DLL (STEP, LOCK, UPDNCNTL, STOP, CLKIN, RESET) /* synthesis syn_black_box  */;

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

endmodule
//--------end DLL -----------

// CLKDIV
module CLKDIV(CLKOUT, CALIB, HCLKIN, RESETN) /* synthesis syn_black_box  */; 

input HCLKIN;
input RESETN;
input CALIB;
output CLKOUT;

parameter DIV_MODE = "2"; //"2", "3.5", "4", "5"
parameter GSREN = "false"; //"false", "true"

endmodule

//DHCEN
module DHCEN (CLKOUT, CLKIN, CE) /* synthesis syn_black_box  */;
input CLKIN,CE;
output CLKOUT;

endmodule

// DQS
module DQS(DQSR90, DQSW0, DQSW270, RPOINT, WPOINT, RVALID, RBURST, RFLAG, WFLAG, DQSIN, DLLSTEP, WSTEP, READ, RLOADN, RMOVE, RDIR, WLOADN, WMOVE, WDIR, HOLD, RCLKSEL, PCLK, FCLK, RESET) /* synthesis syn_black_box  */;
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


endmodule    

// DLLDLY
module DLLDLY (CLKOUT, FLAG, DLLSTEP, LOADN, MOVE, DIR, CLKIN)  /* synthesis syn_black_box  */;

input CLKIN;
input [7:0] DLLSTEP;
input DIR,LOADN,MOVE;
output CLKOUT;
output FLAG;

parameter DLL_INSEL = 1'b0; //1'b0:bypass mode, 1'b1: use dll_delay cell
parameter DLY_SIGN = 1'b0; // 1'b0:'+',  1'b1: '-'
parameter DLY_ADJ = 0; // 0~255, dly_sign=0 :dly_adj; dly_sign=1: -256+dly_adj

endmodule

//DCS
module DCS (CLKOUT, CLK0, CLK1, CLK2, CLK3, CLKSEL, SELFORCE) /* synthesis syn_black_box  */;
input CLK0, CLK1, CLK2, CLK3, SELFORCE;
input [3:0] CLKSEL;
output CLKOUT;

  parameter DCS_MODE = "RISING";  //CLK0,CLK1,CLK2,CLK3,GND,VCC,RISING,FALLING,CLK0_GND,CLK0_VCC,CLK1_GND,CLK1_VCC,CLK2_GND,CLK2_VCC,CLK3_GND,CLK3_VCC

endmodule

//DQCE
module DQCE(CLKOUT, CLKIN, CE) /* synthesis syn_black_box  */;
input CLKIN;
input CE;
output CLKOUT;

endmodule


