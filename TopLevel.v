module Sub
(
  input [3:0] cmd,
  output [3:0] rsp 
);

  assign rsp = (cmd + cmd);
endmodule

module TopLevel
(
  input [3:0] a,
  input [3:0] b,
  output [3:0] x,
  output reg [3:0] y,
  output [3:0] z,
  output [3:0] subOut,
  input  clk,
  input  reset 
);

  wire[3:0] l;
  reg [3:0] m;
  reg [3:0] n;
  reg [3:0] o;
  reg [3:0] p;
  reg [3:0] q;
  wire[3:0] zz_1;
  Sub sub (
    .cmd(zz_1),
    .rsp(subOut) 
  );
  assign x = (a - b);
  assign z[3 : 0] = (4'b0110);
  assign l = (a & b);
  assign zz_1 = (a + b);
  always @ (a,b)
  begin
    y <= (a + b);
    y[0] <= 0;
  end

  always @ (a,b)
  begin
    m <= a;
    if (a == b) begin
      m <= b;
    end
  end

  always @ (posedge clk or posedge reset)
  begin
    if (reset) begin
      p <= (4'b0010);
      q <= (4'b0010);
    end else begin
      p <= (a & b);
      q <= a;
      if (a == b) begin
        q <= b;
      end
    end
  end

  always @ (posedge clk)
  begin
    n <= (a & b);
    o <= a;
    if (a == b) begin
      o <= b;
    end
  end

endmodule

