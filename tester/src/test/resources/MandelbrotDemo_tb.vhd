library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_MandelbrotDemo;
use lib_MandelbrotDemo.pkg_scala2hdl.all;
use lib_MandelbrotDemo.pkg_enum.all;

-- #spinalBegin userLibrary
library STD;
use STD.textio.all;
library ieee;
use IEEE.std_logic_textio.all;
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
    assert now < 20 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;


  process
  begin
    reset <= '1';
    io_memoryWrite_ready <= '1';
    wait for 100 ns;
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);

    wait for 10 ms;

  end process;

  -- matlab => M = csvread('E:\FPGA\intelij\spinalModelSIm\MandelbrotDemo.out')
  process
    variable VEC_LINE : line;
    file VEC_FILE : text is out "MandelbrotDemo.out";
  begin
    wait until rising_edge(clk) and reset = '0' and io_memoryWrite_valid = '1';
	write (VEC_LINE, to_integer(unsigned(io_memoryWrite_data_data(23 downto 0))));
	if io_memoryWrite_data_address(3 downto 0) /= "1111" then
        write (VEC_LINE, ',');
	else
		report("line done");
		writeline (VEC_FILE, VEC_LINE);
	end if;

	if io_memoryWrite_data_address = X"000000FF" then
		done := done + 1;
	end if;
  end process;   
  
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
