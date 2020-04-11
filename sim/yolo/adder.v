module adder(nibble1, nibble2, sum, carry_out);  
input  [3:0] nibble1;  
input  [3:0] nibble2;  
output [3:0] sum;  
output carry_out;  
wire [4:0] temp; 
  
  assign temp = nibble1 + nibble2;  
  assign sum = temp [3:0];  
  assign carry_out  = temp [4];  
endmodule
