module BlackBoxed
(
  output  bus3_cmd_read ,
  input  bus3_cmd_write ,
  input  bus3_cmd_writeenable ,
  inout  bus3_gpio
);


  assign bus3_gpio = bus3_cmd_writeenable ? bus3_cmd_write : 1'bZ;
  assign bus3_cmd_read = bus3_gpio;
endmodule
