
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity BlackBoxed is
    port(
      bus3_cmd_read : out std_logic;
      bus3_cmd_write : in std_logic;
      bus3_cmd_writeenable : in std_logic;
      bus3_gpio : inout std_logic
    );
end BlackBoxed;

architecture arch of BlackBoxed is
begin
    bus3_gpio <= bus3_cmd_write when bus3_cmd_writeenable = '1' else 'Z';
    bus3_cmd_read <= bus3_gpio;
end arch;

