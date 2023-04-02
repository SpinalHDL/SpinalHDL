module PortBlackBox
(
  output  read,
  input  write,
  input  writeEnable,
  inout  pad
);


  assign pad = writeEnable ? write : 1'bZ;
  assign read = pad;
endmodule
