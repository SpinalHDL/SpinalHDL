library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

library lib_BundleTester;
use lib_BundleTester.pkg_scala2hdl.all;
use lib_BundleTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity BundleTester_tb is
end BundleTester_tb;

architecture arch of BundleTester_tb is
  signal io_conds_0 : std_logic;
  signal io_conds_1 : std_logic;
  signal io_conds_2 : std_logic;
  signal io_conds_3 : std_logic;
  signal io_conds_4 : std_logic;
  signal io_conds_5 : std_logic;
  signal io_conds_6 : std_logic;
  signal io_conds_7 : std_logic;
  signal io_inAA_0_a : std_logic;
  signal io_inAA_0_c : std_logic;
  signal io_inAA_0_b : std_logic;
  signal io_inAA_0_d : std_logic;
  signal io_inAA_1_a : std_logic;
  signal io_inAA_1_c : std_logic;
  signal io_inAA_1_b : std_logic;
  signal io_inAA_1_d : std_logic;
  signal io_inAA_2_a : std_logic;
  signal io_inAA_2_c : std_logic;
  signal io_inAA_2_b : std_logic;
  signal io_inAA_2_d : std_logic;
  signal io_inA_0_a : std_logic;
  signal io_inA_0_c : std_logic;
  signal io_outAA_a : std_logic;
  signal io_outAA_c : std_logic;
  signal io_outAA_b : std_logic;
  signal io_outAA_d : std_logic;
  -- #spinalBegin userDeclarations
  signal clk : std_logic;
  signal asyncProcess : std_logic := '0';
  
  shared variable done : boolean := false;
  
  signal io_outAA_a_ref : std_logic;
  signal io_outAA_c_ref : std_logic;
  signal io_outAA_b_ref : std_logic;
  signal io_outAA_d_ref : std_logic;
  
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  process
  begin
    clk <= '0';
    wait for 5 ns;
    if done then
      wait;
    end if;
    clk <= '1';
    wait for 5 ns;
  end process;
  
  process(asyncProcess)
  begin
    if io_conds_0  = '1' then
      io_outAA_a_ref <= io_inA_0_a;
      io_outAA_c_ref <= io_inA_0_c;
      io_outAA_b_ref <= io_inAA_0_b;
      io_outAA_d_ref <= io_inAA_0_d;
    else
      io_outAA_a_ref <= '0';
      io_outAA_c_ref <= '0';
      io_outAA_b_ref <= '0';
      io_outAA_d_ref <= '0';
    end if;
    
    if io_conds_1 = '1' then
      io_outAA_a_ref <= io_inAA_1_a;
      io_outAA_c_ref <= io_inAA_1_c;
      io_outAA_b_ref <= io_inAA_1_b;
      io_outAA_d_ref <= io_inAA_1_d;
    end if;   
    
  end process;
  

  process
    variable seed1, seed2: positive;              
    variable testCounter : integer := 0;
    procedure random(signal that: out std_logic) is 
      variable rand: real;                          
      variable int_rand: integer;                    
      variable vector: std_logic_vector(11 DOWNTO 0);  
    begin
      UNIFORM(seed1, seed2, rand);   
      int_rand := INTEGER(TRUNC(rand*4096.0));                       
      vector := std_logic_vector(to_unsigned(int_rand, vector'LENGTH));  
      that <= vector(3);
    end random;
    
     procedure random(signal that: out unsigned) is 
      variable rand: real;                          
      variable int_rand: integer;                     
    begin
      UNIFORM(seed1, seed2, rand);   
      int_rand := INTEGER(TRUNC(rand*(2.0**that'length)));                       
      that <= (to_unsigned(int_rand, that'LENGTH));  
    end random;
  begin
    random(io_conds_0);
    random(io_conds_1);
    random(io_conds_2);
    random(io_conds_3);
    random(io_conds_4);
    random(io_conds_5);
    random(io_conds_6);
    random(io_conds_7);

    random(io_inAA_0_a);
    random(io_inAA_0_c);
    random(io_inAA_0_b);
    random(io_inAA_0_d);
    random(io_inAA_1_a);
    random(io_inAA_1_c);
    random(io_inAA_1_b);
    random(io_inAA_1_d);
    random(io_inAA_2_a);
    random(io_inAA_2_c);
    random(io_inAA_2_b);
    random(io_inAA_2_d);
    random(io_inA_0_a);
    random(io_inA_0_c);
    wait for 1 ns;
    asyncProcess <= not asyncProcess;
    
    wait until rising_edge(clk);

    if testCounter /= 0 then
      assert io_outAA_a_ref = io_outAA_a report "io_outAA_a fail" severity failure;
      assert io_outAA_b_ref = io_outAA_b report "io_outAA_b fail" severity failure;
      assert io_outAA_c_ref = io_outAA_c report "io_outAA_c fail" severity failure;
      assert io_outAA_d_ref = io_outAA_d report "io_outAA_d fail" severity failure;
    end if;

    
    testCounter := testCounter + 1;
    if testCounter = 10000 then
      done := true;
    end if;
  end process;  
  
  -- #spinalEnd userLogics
  uut : entity lib_BundleTester.BundleTester
    port map (
      io_conds_0 =>  io_conds_0,
      io_conds_1 =>  io_conds_1,
      io_conds_2 =>  io_conds_2,
      io_conds_3 =>  io_conds_3,
      io_conds_4 =>  io_conds_4,
      io_conds_5 =>  io_conds_5,
      io_conds_6 =>  io_conds_6,
      io_conds_7 =>  io_conds_7,
      io_inAA_0_a =>  io_inAA_0_a,
      io_inAA_0_c =>  io_inAA_0_c,
      io_inAA_0_b =>  io_inAA_0_b,
      io_inAA_0_d =>  io_inAA_0_d,
      io_inAA_1_a =>  io_inAA_1_a,
      io_inAA_1_c =>  io_inAA_1_c,
      io_inAA_1_b =>  io_inAA_1_b,
      io_inAA_1_d =>  io_inAA_1_d,
      io_inAA_2_a =>  io_inAA_2_a,
      io_inAA_2_c =>  io_inAA_2_c,
      io_inAA_2_b =>  io_inAA_2_b,
      io_inAA_2_d =>  io_inAA_2_d,
      io_inA_0_a =>  io_inA_0_a,
      io_inA_0_c =>  io_inA_0_c,
      io_outAA_a =>  io_outAA_a,
      io_outAA_c =>  io_outAA_c,
      io_outAA_b =>  io_outAA_b,
      io_outAA_d =>  io_outAA_d 
    );
end arch;
