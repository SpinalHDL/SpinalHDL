library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_SerdesSerialTester;
use lib_SerdesSerialTester.pkg_scala2hdl.all;
use lib_SerdesSerialTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity SerdesSerialTester_tb is
end SerdesSerialTester_tb;

architecture arch of SerdesSerialTester_tb is
  signal io_rx_valid : std_logic;
  signal io_rx_payload : std_logic_vector(7 downto 0);
  signal io_tx_valid : std_logic;
  signal io_tx_ready : std_logic;
  signal io_tx_payload : std_logic_vector(7 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
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
    assert now < 1 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;




  process
    procedure waitClk(value:  integer) is
    begin
      for i in 0 to value -1 loop
        wait until rising_edge(clk);  
      end loop;
    end waitClk;
  begin
    reset <= '1';
    waitClk(3);
    reset <= '0';
    wait;
  end process;

  process
    procedure waitClk(value:  integer) is
    begin
      for i in 0 to value -1 loop
        wait until rising_edge(clk);  
      end loop;
    end waitClk;
    
    procedure tx(that : std_logic_vector(7 downto 0)) is
    begin
      io_rx_valid <= '1';
      io_rx_payload <= that;
      wait until rising_edge(clk);
      io_rx_valid <= '0';
    end tx;

    procedure txPacket(thatIn : std_logic_vector) is
      variable checksum : unsigned(15 downto 0) := X"0000";
      variable that : std_logic_vector( thatIn'high downto 0) := thatIn;
      variable i : integer;
    begin
      tx(X"A5");
      tx(X"D8");
      i := (that'length / 8) -1;
      while i >= 0 loop
        tx(that(i*8+7 downto i*8));
        checksum := checksum + unsigned(that(i*8+7 downto i*8));
        i := i - 1;
      end loop;
      tx(X"A5");
      tx(X"9A");    
      tx(std_logic_vector(checksum(7 downto 0)));
      tx(std_logic_vector(checksum(15 downto 8))); 
    end txPacket;
  begin
    io_rx_valid <= '0';
    waitClk(3);
    wait until rising_edge(clk) and reset = '0';
    txPacket(X"03"); waitClk(50);
    txPacket(X"02"); waitClk(50);
    txPacket(X"03"); waitClk(50);
    txPacket(X"01_0000_0000_112233"); waitClk(50);
    txPacket(X"01_0000_0300_445566"); waitClk(50);
	wait for 20 us;
	waitClk(1);
	txPacket(X"01_0200"); 
	wait for 20 us;
	waitClk(1);
	txPacket(X"01_0600"); 
	
    wait;
  end process;

  process
    procedure waitClk(value:  integer) is
    begin
      for i in 0 to value -1 loop
        wait until rising_edge(clk);  
      end loop;
    end waitClk;
    
    procedure rx is
    begin
      io_tx_ready <= '1';
      wait until rising_edge(clk) and io_tx_valid = '1';
      io_tx_ready <= '0';
   --   return io_tx_payload;
    end rx;
  begin
    io_tx_ready <= '0';
    waitClk(3);
    wait until rising_edge(clk) and reset = '0';
    for i in 0 to 20 loop
      rx;
    end loop;

    done := done + 1;

    wait;
  end process;
  
  -- #spinalEnd userLogics
  uut : entity lib_SerdesSerialTester.SerdesSerialTester
    port map (
      io_rx_valid =>  io_rx_valid,
      io_rx_payload =>  io_rx_payload,
      io_tx_valid =>  io_tx_valid,
      io_tx_ready =>  io_tx_ready,
      io_tx_payload =>  io_tx_payload,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
