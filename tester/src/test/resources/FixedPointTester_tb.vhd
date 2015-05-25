library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_FixedPointTester;
use lib_FixedPointTester.pkg_scala2hdl.all;
use lib_FixedPointTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity FixedPointTester_tb is
end FixedPointTester_tb;

architecture arch of FixedPointTester_tb is
  signal io_inSFix_0_raw : signed(15 downto 0);
  signal io_inSFix_1_raw : signed(11 downto 0);
  signal io_outSFix_0_raw : signed(15 downto 0);
  signal io_outSFix_1_raw : signed(23 downto 0);
  signal io_inBundleA_a_sfix_raw : signed(7 downto 0);
  signal io_outBundleA_a_sfix_raw : signed(5 downto 0);
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  signal clk : std_logic;

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
      int_rand := INTEGER(TRUNC(rand*(4.0**that'length)));
      that <= (to_signed(int_rand, that'LENGTH));
    end random;
  begin
    random(io_inSFix_0_raw);
    random(io_inSFix_1_raw);
    random(io_inBundleA_a_sfix_raw);

    wait for 1 ns;
--    asyncProcess <= not asyncProcess;

    wait until rising_edge(clk);

    if testCounter /= 0 then
      assert io_outSFix_0_raw = io_inSFix_0_raw + ((1 downto 0 => io_inSFix_1_raw(io_inSFix_1_raw'high)) & io_inSFix_1_raw & "00") report "io_outSFix_0_raw fail" severity failure;
      assert io_outSFix_1_raw <= pkg_resize(pkg_shiftRight((io_inSFix_0_raw * io_inSFix_1_raw),6),24) report "io_outSFix_1_raw fail" severity failure;
      assert io_outBundleA_a_sfix_raw = io_inBundleA_a_sfix_raw(io_inBundleA_a_sfix_raw'high downto 2) report "io_outBundleA_a_sfix_raw fail" severity failure;
    end if;


    testCounter := testCounter + 1;
    if testCounter = 1000 then
      done := done + 1;
    end if;
  end process;



  -- #spinalEnd userLogics
  uut : entity lib_FixedPointTester.FixedPointTester
    port map (
      io_inSFix_0_raw =>  io_inSFix_0_raw,
      io_inSFix_1_raw =>  io_inSFix_1_raw,
      io_outSFix_0_raw =>  io_outSFix_0_raw,
      io_outSFix_1_raw =>  io_outSFix_1_raw,
      io_inBundleA_a_sfix_raw =>  io_inBundleA_a_sfix_raw,
      io_outBundleA_a_sfix_raw =>  io_outBundleA_a_sfix_raw 
    );
end arch;
