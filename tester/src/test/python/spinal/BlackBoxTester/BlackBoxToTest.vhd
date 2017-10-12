
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity BlackBoxToTest is
    generic(
      aWidth : integer;
      bWidth : integer
    );
    port(
      io_clockPin : in std_logic;
      io_resetPin : in std_logic;
      io_inA : in unsigned(7 downto 0);
      io_inB : in unsigned(15 downto 0);
      io_outA : out unsigned(7 downto 0);
      io_outB : out unsigned(15 downto 0)
    );
end BlackBoxToTest;

architecture arch of BlackBoxToTest is
  signal outA :  unsigned(7 downto 0);
  signal outB :  unsigned(15 downto 0);
begin
  process(io_clockPin, io_resetPin)
  begin
    if io_resetPin = '1' then
      outA <= (others => '0');
      outB <= (others => '0');
    elsif rising_edge(io_clockPin) then
      outA <= outA + io_inA;
      outB <= outB + io_inB;
    end if;
  end process;

  io_outA <= outA;
  io_outB <= outB;
end arch;

