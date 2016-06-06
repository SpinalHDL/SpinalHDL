module Sub
(
  input unsigned [3:0] cmd,
  output unsigned [3:0] rsp 
);

  wire unsigned [3:0] zz_1;
  assign rsp = zz_1;
  assign zz_1 = (cmd + cmd);
endmodule

module TopLevel
(
  input unsigned [3:0] a,
  input unsigned [3:0] b,
  output unsigned [3:0] x,
  output reg unsigned [3:0] y,
  output unsigned [3:0] z,
  output unsigned [3:0] subOut,
  input  clk,
  input  reset 
);

  wire unsigned [3:0] zz_1;
  wire unsigned [3:0] zz_2;
  wire unsigned [3:0] l;
  reg unsigned [3:0] m;
  reg unsigned [3:0] n;
  reg unsigned [3:0] o;
  reg unsigned [3:0] p;
  reg unsigned [3:0] q;
  wire unsigned [3:0] zz_3;
  wire unsigned [4:0] r;
  wire unsigned [3:0] zz_4;
  wire unsigned [1:0] zz_5;
  wire unsigned [4:0] zz_6;
  Sub sub (
    .cmd(zz_3),
    .rsp(subOut) 
  );
  assign x = zz_1;
  assign z[3 : 0] = $unsigned(4'b0110);
  assign zz_1 = (a - b);
  assign zz_2 = (a + b);
  assign l = (a & b);
  assign zz_3 = (a + b);
  assign r = zz_6;
  assign zz_4 = (a - b);
  assign zz_5 = pkg_shiftRight(zz_4,2);
  assign zz_6 = pkg_resize(zz_5,5);
  always @ (zz_2)
  begin
    y <= zz_2;
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
      p <= $unsigned(4'b0010);
      q <= $unsigned(4'b0010);
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

