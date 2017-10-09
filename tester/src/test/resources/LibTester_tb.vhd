library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_LibTester;
use lib_LibTester.pkg_scala2hdl.all;
use lib_LibTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity LibTester_tb is
end LibTester_tb;

architecture arch of LibTester_tb is
  signal io_inSIntA : signed(15 downto 0);
  signal io_inSIntB : signed(15 downto 0);
  signal io_outSInt : signed(31 downto 0);
  signal io_outSIntRef : signed(31 downto 0);
  signal clk : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  process
  begin
    clk <= '0';
    wait for 5 ns;
    if done = 1 then
      wait;
    end if;
    assert now < 10 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;  
  
  
  process
    variable seed1, seed2: positive;
    variable testCounter : integer := 0;

    procedure random(signal that: out signed) is
      variable rand: real;
      variable int_rand: integer;
    begin
      UNIFORM(seed1, seed2, rand);
      int_rand := INTEGER((rand*(2.0**that'length)));
      that <= (signed(to_unsigned(int_rand, that'LENGTH)));
    end random;
  begin


    wait until rising_edge(clk);
    random(io_inSIntA);
    random(io_inSIntB);
	if testCounter >10 then
	--assert io_inSIntA*io_inSIntB = io_outSIntRef report "io_outSIntRef fail" severity failure;
	  assert io_outSInt = io_outSIntRef report "io_outSInt fail" severity failure;
	end if;
    testCounter := testCounter + 1;
    if testCounter = 10000 then  
      done := done + 1;    
	  wait;
    end if;
  end process;
  -- #spinalEnd userLogics
  uut : entity lib_LibTester.LibTester
    port map (
	  clk => clk,
      io_inSIntA =>  io_inSIntA,
      io_inSIntB =>  io_inSIntB,
      io_outSInt =>  io_outSInt,
      io_outSIntRef =>  io_outSIntRef,
      reset => '0'
    );
end arch;
