library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity PortBlackBox is
    port(
      read : out std_logic;
      write : in std_logic;
      writeEnable : in std_logic;
      pad : inout std_logic
    );
end PortBlackBox;

architecture arch of PortBlackBox is
begin
    pad <= write when writeEnable = '1' else 'Z';
    read <= pad;
end arch;