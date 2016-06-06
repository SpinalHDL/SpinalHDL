
module Shift (A, Y1, Y2);

                 input [7:0] A;
                 output [7:0] Y1, Y2;
                 parameter B=3; reg [7:0] Y1, Y2;
 assign Y2 = A>>B; //logical shift right
                 always @(A)
                 begin
                                  Y1=A<<B; //logical shift left

                 end
endmodule
/*
module TopLevel
(
    input [3:0] a,
    input [3:0] b
);
     logic [3:0] x;
     logic [3:0] yy;
     logic [3:0] z;
  assign x = (a - b);
  assign z[3 : 0] = 4; //nothing
  always @ (a,b)
  begin
    if (a[0]) begin
        yy <= (a + b);  //reg
    end
  end



endmodule*/

