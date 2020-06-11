///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1995/2009 Xilinx, Inc.
// All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /    Vendor : Xilinx
// \   \   \/     Version : 8.1i (I.13)
//  \   \         Description : Xilinx Timing Simulation Library Component
//  /   /                  Differential Signaling Output Buffer
// /___/   /\     Filename : OBUFDS.v
// \   \  /  \    Timestamp : Tue Mar  1 14:57:54 PST 2005
//  \___\/\___\
//
// Revision:
//    03/23/04 - Initial version.
//    03/01/05 - Added global GTS.
//    03/01/05 - Added LOC parameter.
//    05/23/05 - Declared tri0 GTS.
//    07/21/05 - CR 212974 -- matched unisim parameters as requested by other tools
//    12/13/11 - Added `celldefine and `endcelldefine (CR 524859).
//    10/22/14 - Added #1 to $finish (CR 808642).
// End Revision

`timescale 1 ps/1 ps

`celldefine

module OBUFDS (O, OB, I);

    parameter CAPACITANCE = "DONT_CARE";
    parameter IOSTANDARD = "DEFAULT";
    parameter SLEW = "SLOW";
    
`ifdef XIL_TIMING

    parameter LOC = " UNPLACED";

`endif

    
    output O, OB;

    input  I;
    tri0 GTS = glbl.GTS;

    initial begin

        case (CAPACITANCE)

            "LOW", "NORMAL", "DONT_CARE" : ;
            default : begin
                          $display("Attribute Syntax Error : The attribute CAPACITANCE on OBUFDS instance %m is set to %s.  Legal values for this attribute are DONT_CARE, LOW or NORMAL.", CAPACITANCE);
                          #1 $finish;
                      end

        endcase

    end

    bufif0 (O, I, GTS);
    notif0 (OB, I, GTS);

`ifdef XIL_TIMING

    specify
	(I => O) = (0:0:0, 0:0:0);
	(I => OB) = (0:0:0, 0:0:0);
	specparam PATHPULSE$ = 0;
    endspecify

`endif

endmodule

`endcelldefine
