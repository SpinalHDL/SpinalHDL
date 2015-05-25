library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_MandelbrotDemo;
use lib_MandelbrotDemo.pkg_scala2hdl.all;
use lib_MandelbrotDemo.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity MandelbrotDemo_tb is
end MandelbrotDemo_tb;

architecture arch of MandelbrotDemo_tb is
  signal io_uart_txd : std_logic;
  signal io_uart_rxd : std_logic;
  signal io_memoryWrite_valid : std_logic;
  signal io_memoryWrite_ready : std_logic;
  signal io_memoryWrite_data_address : unsigned(31 downto 0);
  signal io_memoryWrite_data_data : unsigned(31 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  -- #spinalEnd userLogics
  uut : entity lib_MandelbrotDemo.MandelbrotDemo
    port map (
      io_uart_txd =>  io_uart_txd,
      io_uart_rxd =>  io_uart_rxd,
      io_memoryWrite_valid =>  io_memoryWrite_valid,
      io_memoryWrite_ready =>  io_memoryWrite_ready,
      io_memoryWrite_data_address =>  io_memoryWrite_data_address,
      io_memoryWrite_data_data =>  io_memoryWrite_data_data,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
