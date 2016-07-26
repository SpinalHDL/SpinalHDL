library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_MandelbrotTester;
use lib_MandelbrotTester.pkg_scala2hdl.all;
use lib_MandelbrotTester.pkg_enum.all;

-- #spinalBegin userLibrary
library STD;
use STD.textio.all;
library ieee;
use IEEE.std_logic_textio.all;
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity MandelbrotTester_tb is
end MandelbrotTester_tb;

architecture arch of MandelbrotTester_tb is
  signal io_cmdPort_valid : std_logic;
  signal io_cmdPort_payload_last : std_logic;
  signal io_cmdPort_payload_fragment : std_logic_vector(7 downto 0);
  signal io_retPort_valid : std_logic;
  signal io_retPort_ready : std_logic;
  signal io_retPort_payload_last : std_logic;
  signal io_retPort_payload_fragment : std_logic_vector(7 downto 0);
  signal io_pixelResult_valid : std_logic;
  signal io_pixelResult_ready : std_logic;
  signal io_pixelResult_payload_last : std_logic;
  signal io_pixelResult_payload_fragment_iteration : unsigned(4 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  signal clk2 : std_logic;

  shared variable seed1, seed2: positive;
  impure function randomStdLogic return std_logic is
    variable rand: real;
  begin
    UNIFORM(seed1, seed2, rand);
    if rand < 0.5 then
      return '1';
    else
      return '0';
    end if;
  end randomStdLogic;
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics

  process
  begin
    clk <= '0';
    clk2 <= '0';
    wait for 5 ns;
   -- while reset /= '0' loop
    --  wait for 5 ns;
   -- end loop;
    if done = 1 then
      wait;
    end if;
    assert now < 20 ms report "timeout" severity failure;
    clk <= '1';
    if reset = '0' then
      clk2 <= '1';
    end if;
    wait for 5 ns;
  end process;


  process
  begin
    io_cmdPort_valid <= '0';
    reset <= '1';
    io_pixelResult_ready <= '1';
    io_retPort_ready <= '1';
    wait for 50 ns;
   -- wait until rising_edge(clk);
    reset <= '0';
  --  wait until rising_edge(clk);
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    while true loop
      io_pixelResult_ready <= randomStdLogic and randomStdLogic;
      wait until rising_edge(clk);
    end loop;


  end process;

  -- matlab => M = csvread('E:\FPGA\intelij\spinalModelSIm\MandelbrotTester.out')
  process
    variable VEC_LINE : line;
    file VEC_FILE : text is out "MandelbrotTester.out";
    variable hCounter : integer := 0;
  begin
    wait until rising_edge(clk) and reset = '0' and io_pixelResult_valid = '1' and io_pixelResult_ready = '1';
	write (VEC_LINE, to_integer(unsigned(io_pixelResult_payload_fragment_iteration)));
	hCounter := hCounter + 1;
	if hCounter /= 16 then
        write (VEC_LINE, ',');
	else
	    hCounter := 0;
		report("line done");
		writeline (VEC_FILE, VEC_LINE);
	end if;

	if io_pixelResult_payload_last = '1' then
		done := done + 1;
	end if;
  end process;

  -- #spinalEnd userLogics
  uut : entity lib_MandelbrotTester.MandelbrotTester
    port map (
      io_cmdPort_valid =>  io_cmdPort_valid,
      io_cmdPort_payload_last =>  io_cmdPort_payload_last,
      io_cmdPort_payload_fragment =>  io_cmdPort_payload_fragment,
      io_retPort_valid =>  io_retPort_valid,
      io_retPort_ready =>  io_retPort_ready,
      io_retPort_payload_last =>  io_retPort_payload_last,
      io_retPort_payload_fragment =>  io_retPort_payload_fragment,
      io_pixelResult_valid =>  io_pixelResult_valid,
      io_pixelResult_ready =>  io_pixelResult_ready,
      io_pixelResult_payload_last =>  io_pixelResult_payload_last,
      io_pixelResult_payload_fragment_iteration =>  io_pixelResult_payload_fragment_iteration,
      clk =>  clk2,
      reset => reset
    );
end arch;
