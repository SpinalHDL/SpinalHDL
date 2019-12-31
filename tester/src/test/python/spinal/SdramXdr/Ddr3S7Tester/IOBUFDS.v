///////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1995/2004 Xilinx, Inc.
// All Right Reserved.
///////////////////////////////////////////////////////////////////////////////
//   ____  ____
//  /   /\/   /
// /___/  \  /    Vendor : Xilinx
// \   \   \/     Version : 10.1
//  \   \         Description : Xilinx Unified Simulation Library Component
//  /   /                  3-State Diffential Signaling I/O Buffer
// /___/   /\     Filename : IOBUFDS.v
// \   \  /  \
//  \___\/\___\
//
// Revision:
//    03/23/04 - Initial version.
//    05/23/07 - Changed timescale to 1 ps / 1 ps.
//    05/23/07 - Added wire declaration for internal signals.
//    07/26/07 - Add else to handle x case for o_out (CR 424214). 
//    07/16/08 - Added IBUF_LOW_PWR attribute.
//    03/19/09 - CR 511590 - Added Z condition handling
//    04/22/09 - CR 519127 - Changed IBUF_LOW_PWR default to TRUE.
//    10/14/09 - CR 535630 - Added DIFF_TERM attribute.
//    05/12/10 - CR 559468 - Added DRC warnings for LVDS_25 bus architectures.
//    12/01/10 - CR 584500 - added attribute SLEW
//    08/08/11 - CR 616816 - ncsim compile error during XIL_TIMING
//    12/13/11 - Added `celldefine and `endcelldefine (CR 524859).
//    07/13/12 - 669215 - add parameter DQS_BIAS
//    08/28/12 - 675511 - add DQS_BIAS functionality
//    09/11/12 - 677753 - remove X glitch on O
//    10/22/14 - Added #1 to $finish (CR 808642).
// End Revision

`timescale 1 ps / 1 ps

`celldefine

module IOBUFDS (O, IO, IOB, I, T);

`ifdef XIL_TIMING
    parameter LOC = "UNPLACED";
`endif // `ifdef XIL_TIMING
    parameter DIFF_TERM = "FALSE";
    parameter DQS_BIAS = "FALSE";
    parameter IBUF_LOW_PWR = "TRUE";
    parameter IOSTANDARD = "DEFAULT";
    parameter SLEW = "SLOW";

  localparam MODULE_NAME = "IOBUFDS";


    output O;
    inout  IO, IOB;
    input  I, T;

    wire i_in, io_in, iob_in, t_in;
    reg o_out, io_out, iob_out;
    reg O_int;

    reg DQS_BIAS_BINARY = 1'b0;

    wire t_or_gts; 
    tri0 GTS = glbl.GTS;

    assign i_in = I;
    assign t_in = T;
    assign io_in = IO;
    assign iob_in = IOB;

    assign t_or_gts = GTS || t_in;
    assign IO  = t_or_gts ? 1'bz :  i_in;
    assign IOB = t_or_gts ? 1'bz : ~i_in;

    initial begin

        case (DQS_BIAS)

            "TRUE"  : DQS_BIAS_BINARY <= #1 1'b1;
            "FALSE" : DQS_BIAS_BINARY <= #1 1'b0;
            default : begin
                          $display("Attribute Syntax Error : The attribute DQS_BIAS on %s instance %m is set to %s.  Legal values for this attribute are TRUE or FALSE.", MODULE_NAME, DQS_BIAS);
                          #1 $finish;
                      end

        endcase

       case (DIFF_TERM)

            "TRUE", "FALSE" : ;
            default : begin
                          $display("Attribute Syntax Error : The attribute DIFF_TERM on %s instance %m is set to %s.  Legal values for this attribute are TRUE or FALSE.", MODULE_NAME, DIFF_TERM);
                          #1 $finish;
                      end

        endcase // case(DIFF_TERM)

        case (IBUF_LOW_PWR)

            "FALSE", "TRUE" : ;
            default : begin
                          $display("Attribute Syntax Error : The attribute IBUF_LOW_PWR on %s instance %m is set to %s.  Legal values for this attribute are TRUE or FALSE.", MODULE_NAME, IBUF_LOW_PWR);
                          #1 $finish;
                      end

        endcase

      if((IOSTANDARD == "LVDS_25") || (IOSTANDARD == "LVDSEXT_25")) begin
            $display("DRC Warning : The IOSTANDARD attribute on %s instance %m is set to %s.  LVDS_25 is a fixed impedance structure optimized to 100ohm differential. If the intended usage is a bus architecture, please use BLVDS. This is only intended to be used in point to point transmissions that do not have turn around timing requirements", MODULE_NAME, IOSTANDARD);
        end

    end

    always @(io_in or iob_in or DQS_BIAS_BINARY) begin
        if (io_in == 1'b1 && iob_in == 1'b0)
          o_out <= 1'b1;
        else if (io_in == 1'b0 && iob_in == 1'b1)
          o_out <= 1'b0;
        else if ((io_in === 1'bz || io_in == 1'b0) && (iob_in === 1'bz || iob_in == 1'b1))
          if (DQS_BIAS_BINARY == 1'b1)
            o_out <= 1'b0;
          else
            o_out <= 1'bx;
        else if ((io_in === 1'bx) || (iob_in == 1'bx))
          o_out <= 1'bx;
        end

//    assign O  =  (t_in === 1'b0) ? 1'b1 : ((t_in === 1'b1) ? o_out : 1'bx));
    assign O  =  o_out;



`ifdef XIL_TIMING
    specify
        (I => O)                = (0:0:0,  0:0:0);
        (I => IO)               = (0:0:0,  0:0:0);
        (I => IOB)              = (0:0:0,  0:0:0);
        (IO => O)               = (0:0:0,  0:0:0);
        (IO => IOB)             = (0:0:0,  0:0:0);
        (IOB => O)              = (0:0:0,  0:0:0);
        (IOB => IO)             = (0:0:0,  0:0:0);
        (T => O)                = (0:0:0,  0:0:0);
        (T => IO)               = (0:0:0,  0:0:0);
        (T => IOB)              = (0:0:0,  0:0:0);
        specparam PATHPULSE$ = 0;
    endspecify
`endif // `ifdef XIL_TIMING

endmodule

`endcelldefine
