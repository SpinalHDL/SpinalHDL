library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

library lib_InternalClockTester;
use lib_InternalClockTester.pkg_scala2hdl.all;
use lib_InternalClockTester.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity InternalClockTester_tb is
end InternalClockTester_tb;

architecture arch of InternalClockTester_tb is
  signal io_internalClkCounter : unsigned(7 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  signal asyncProcess : std_logic := '0';
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
    assert now < 1 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;


  process
  begin
    reset <= '1';
    wait for 200 ns;
    reset <= '0';

    wait;
  end process;

  process
  begin
    wait until io_internalClkCounter'event and io_internalClkCounter = X"00";
    wait until io_internalClkCounter'event and io_internalClkCounter = X"02";
      wait for 500 ps;
    while io_internalClkCounter /= X"FF" loop
      wait for 20 ns;
      assert io_internalClkCounter'DELAYED(1 ns) + X"01"  = io_internalClkCounter report "io_internalClkCounter fail" severity failure;
    end loop;
    done := done + 1;
    wait;
  end process;
  -- #spinalEnd userLogics
  uut : entity lib_InternalClockTester.InternalClockTester
    port map (
      io_internalClkCounter =>  io_internalClkCounter,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
