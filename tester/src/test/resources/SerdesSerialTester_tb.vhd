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
  signal io_rx_data : std_logic_vector(7 downto 0);
  signal io_tx_valid : std_logic;
  signal io_tx_ready : std_logic;
  signal io_tx_data : std_logic_vector(7 downto 0);
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
    
    procedure tx(that : std_logic_vector) is
    begin
      io_rx_valid <= '1';
      io_rx_data <= that;
      wait until rising_edge(clk);
      io_rx_valid <= '0';
    end tx;
  begin
    io_rx_valid <= '0';
    waitClk(3);
    wait until rising_edge(clk) and reset = '0';
    tx(X"A5");
    tx(X"D8");
    tx(X"01");
    tx(X"02");
    tx(X"03");
    tx(X"A5");
    tx(X"9A");    
    tx(X"06");
    tx(X"00");   
    
    waitClk(50);
    
    tx(X"A5");
    tx(X"D8");
    tx(X"01");
    tx(X"02");
    tx(X"03");
    tx(X"A5");
    tx(X"9A");   
    tx(X"06");
    tx(X"00");  
    
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
   --   return io_tx_data;
    end rx;
  begin
    io_tx_ready <= '0';
    waitClk(3);
    wait until rising_edge(clk) and reset = '0';
    while true loop
      rx;
    end loop;
  end process;
  
  -- #spinalEnd userLogics
  uut : entity lib_SerdesSerialTester.SerdesSerialTester
    port map (
      io_rx_valid =>  io_rx_valid,
      io_rx_data =>  io_rx_data,
      io_tx_valid =>  io_tx_valid,
      io_tx_ready =>  io_tx_ready,
      io_tx_data =>  io_tx_data,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
