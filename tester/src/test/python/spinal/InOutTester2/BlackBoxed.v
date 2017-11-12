module BlackBoxed
(
  output[7:0]  bus3_cmd_read ,
  input [7:0] bus3_cmd_write ,
  input  bus3_cmd_writeenable ,
  inout [7:0] bus3_gpio
);


  assign bus3_gpio = bus3_cmd_writeenable ? bus3_cmd_write : 8'bZZZZZZZZ;
  assign bus3_cmd_read = bus3_gpio;
endmodule
