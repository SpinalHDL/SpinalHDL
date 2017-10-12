
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity Dummy is
    port(
      clk_in : in std_logic;
      clk_out : out std_logic := '0'
    );
end Dummy;

architecture arch of Dummy is
  signal clk_out_buff :  std_logic := '0';
begin
  process(clk_in)
  begin
    if rising_edge(clk_in) then
      clk_out_buff <= not clk_out_buff;
    end if;
  end process;

  clk_out <= clk_out_buff;
end arch;

