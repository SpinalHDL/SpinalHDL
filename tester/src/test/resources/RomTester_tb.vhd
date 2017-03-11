-- Generator : SpinalHDL v0.10.11    git head : 0c70412cef8cefee91b782be5d259d098116053a
-- Date      : 11/03/2017, 11:59:59
-- Component : RomTester

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_scala2hdl.all;
use work.all;
use work.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity RomTester_tb is
end RomTester_tb;

architecture arch of RomTester_tb is
  signal address : unsigned(2 downto 0);
  signal data_bool : std_logic;
  signal data_bits : std_logic_vector(8 downto 0);
  signal data_uint : unsigned(9 downto 0);
  signal data_sint : signed(10 downto 0);
  signal data_enumeration : MyEnum;
  -- #spinalBegin userDeclarations
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics

  process
  begin
    address <= "000";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "000000000"  and data_uint = "0000000000" and data_sint = "00000000000" and data_enumeration = a report "bad rom" severity failure;

    address <= "001";
    wait for 1 ns;
    assert data_bool = '1' and data_bits = "000000000"  and data_uint = "0000000000" and data_sint = "00000000000" and data_enumeration = a report "bad rom" severity failure;

    address <= "010";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "111111111"  and data_uint = "0000000000" and data_sint = "00000000000" and data_enumeration = a report "bad rom" severity failure;

    address <= "011";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "000000000"  and data_uint = "1111111111" and data_sint = "00000000000" and data_enumeration = a report "bad rom" severity failure;

    address <= "100";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "000000000"  and data_uint = "0000000000" and data_sint = "11111111111" and data_enumeration = a report "bad rom" severity failure;

    address <= "101";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "000000000"  and data_uint = "0000000000" and data_sint = "00000000000" and data_enumeration = c report "bad rom" severity failure;

    address <= "110";
    wait for 1 ns;
    assert data_bool = '0' and data_bits = "000101011"  and data_uint = "0001001010" and data_sint = "00001011000" and data_enumeration = b report "bad rom" severity failure;



    wait;
  end process;
  -- #spinalEnd userLogics
  uut : entity work.RomTester
    port map (
      address =>  address,
      data_bool =>  data_bool,
      data_bits =>  data_bits,
      data_uint =>  data_uint,
      data_sint =>  data_sint,
      data_enumeration =>  data_enumeration 
    );
end arch;
