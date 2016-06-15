module BlackBoxToTest
#(parameter aWidth=0, parameter bWidth=0)
(
  input [aWidth-1:0] io_inA,
  input [bWidth-1:0] io_inB,
  output reg [aWidth-1:0] io_outA,
  output reg [bWidth-1:0] io_outB,
  input  io_clockPin,
  input  io_resetPin
);


  always @ (posedge io_clockPin or posedge io_resetPin)
  begin
    if (io_resetPin) begin
      io_outA <= 0;
      io_outB <= 0;
    end else begin
      io_outA <= io_outA + io_inA;
      io_outB <= io_outB + io_inB;
    end
  end
endmodule
