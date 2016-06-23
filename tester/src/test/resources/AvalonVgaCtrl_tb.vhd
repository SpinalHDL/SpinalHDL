library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_AvalonVgaCtrl;
use lib_AvalonVgaCtrl.pkg_scala2hdl.all;
use lib_AvalonVgaCtrl.all;
use lib_AvalonVgaCtrl.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity AvalonVgaCtrl_tb is
end AvalonVgaCtrl_tb;

architecture arch of AvalonVgaCtrl_tb is
  signal io_mem_read : std_logic;
  signal io_mem_waitRequestn : std_logic;
  signal io_mem_address : unsigned(29 downto 0);
  signal io_mem_burstCount : unsigned(3 downto 0);
  signal io_mem_readDataValid : std_logic;
  signal io_mem_readData : std_logic_vector(31 downto 0);
  signal io_vga_vSync : std_logic;
  signal io_vga_hSync : std_logic;
  signal io_vga_colorEn : std_logic;
  signal io_vga_color_r : unsigned(7 downto 0);
  signal io_vga_color_g : unsigned(7 downto 0);
  signal io_vga_color_b : unsigned(7 downto 0);
  signal vga_clk : std_logic;
  signal vga_reset : std_logic;
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  
  shared variable seed1, seed2: positive;
  impure function randomStdLogic(prob : real) return std_logic is
    variable rand: real;
  begin
    UNIFORM(seed1, seed2, rand);
    if rand < prob then
      return '1';
    else
      return '0';
    end if;
  end randomStdLogic;    
  
  procedure waitRandom(prob : real)  is
    variable rand: real;
  begin
    UNIFORM(seed1, seed2, rand);
    while rand < prob loop
      wait until rising_edge(clk);
      UNIFORM(seed1, seed2, rand);
    end loop;
  end waitRandom;    
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  process
  begin
    clk <= '0';
    wait for 10 ns;
    if done = 1 then
      wait;
    end if;
    assert now < 100 ms report "timeout" severity failure;
    clk <= '1';
    wait for 10 ns;
  end process;
  
  process
  begin
    vga_clk <= '0';
    wait for 20 ns;
    if done = 1 then
      wait;
    end if;
    vga_clk <= '1';
    wait for 20 ns;
  end process;
  
  process
  begin
    reset <= '1';
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);
    wait;
  end process;
  
  process
  begin
    vga_reset <= '1';
    wait until rising_edge(vga_clk);
    wait until rising_edge(vga_clk);
    wait until rising_edge(vga_clk);
    vga_reset <= '0';
    wait until rising_edge(vga_clk);
    wait;
  end process;
  process
    variable mem_address : unsigned(29 downto 0);
    variable mem_burstCount : unsigned(3 downto 0);
  begin
    io_mem_readDataValid <= '0';
    io_mem_waitRequestn <= '0';
    waitRandom(0.4);
    wait until rising_edge(clk) and reset = '0';
    if io_mem_read = '1' then
      io_mem_waitRequestn <= '1';
      mem_address := io_mem_address;
      mem_burstCount := io_mem_burstCount;
      wait until rising_edge(clk);
      io_mem_waitRequestn <= '0';
      waitRandom(0.9);
      while mem_burstCount /= 0 loop
        waitRandom(0.1);
        io_mem_readDataValid <= '1';
        io_mem_readData <= "00" & std_logic_vector(mem_address);
     --   if mem_burstCount = 1 then
      --    io_mem_endOfPacket <= '1';
      --  else
      --    io_mem_endOfPacket <= '0';
       -- end if;
        mem_address := mem_address + 1;
        mem_burstCount := mem_burstCount -1;
        wait until rising_edge(clk);
        io_mem_readDataValid <= '0';
      end loop;
    end if;
  end process;
  
  -- #spinalEnd userLogics
  uut : entity lib_AvalonVgaCtrl.AvalonVgaCtrl
    port map (
      io_mem_read =>  io_mem_read,
      io_mem_waitRequestn =>  io_mem_waitRequestn,
      io_mem_address =>  io_mem_address,
      io_mem_burstCount =>  io_mem_burstCount,
      io_mem_readDataValid =>  io_mem_readDataValid,
      io_mem_readData =>  io_mem_readData,
      io_vga_vSync =>  io_vga_vSync,
      io_vga_hSync =>  io_vga_hSync,
      io_vga_colorEn =>  io_vga_colorEn,
      io_vga_color_r =>  io_vga_color_r,
      io_vga_color_g =>  io_vga_color_g,
      io_vga_color_b =>  io_vga_color_b,
      vga_clk =>  vga_clk,
      vga_reset =>  vga_reset,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
