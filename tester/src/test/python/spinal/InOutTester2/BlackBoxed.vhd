
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity BlackBoxed is
    port(
      bus3_cmd_read : out unsigned(7 downto 0);
      bus3_cmd_write : in unsigned(7 downto 0);
      bus3_cmd_writeenable : in std_logic;
      bus3_gpio : inout unsigned(7 downto 0)
    );
end BlackBoxed;

architecture arch of BlackBoxed is
begin
    bus3_gpio <= bus3_cmd_write when bus3_cmd_writeenable = '1' else (others => 'Z');
    bus3_cmd_read <= bus3_gpio;
end arch;

