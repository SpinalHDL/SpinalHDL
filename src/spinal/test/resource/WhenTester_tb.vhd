library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

library work;
use work.pkg_scala2hdl.all;
use work.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity WhenTester_tb is
end WhenTester_tb;

architecture arch of WhenTester_tb is
  signal io_conds_0 : std_logic;
  signal io_conds_1 : std_logic;
  signal io_conds_2 : std_logic;
  signal io_conds_3 : std_logic;
  signal io_conds_4 : std_logic;
  signal io_data_0 : unsigned(7 downto 0);
  signal io_data_1 : unsigned(7 downto 0);
  signal io_data_2 : unsigned(7 downto 0);
  signal io_data_3 : unsigned(7 downto 0);
  signal io_data_4 : unsigned(7 downto 0);
  signal io_outDefault : unsigned(7 downto 0);
  -- #spinalBegin userDeclarations
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics

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
    random(io_data_0);
    random(io_data_1);
    random(io_data_2);
    random(io_data_3);
    random(io_data_4);
    wait for 10 ns;
    if io_conds_0 = '1' then
      assert io_outDefault = io_data_1 report "io_outDefault fail" severity failure;
    else
      assert io_outDefault = io_data_0 report "io_outDefault fail" severity failure;
    end if;
    
    
    testCounter := testCounter + 1;
    if testCounter = 10000 then
      wait;
    end if;
    
  end process;
                                  
    
  -- #spinalEnd userLogics
  uut : entity work.WhenTester
    port map (
      io_conds_0 =>  io_conds_0,
      io_conds_1 =>  io_conds_1,
      io_conds_2 =>  io_conds_2,
      io_conds_3 =>  io_conds_3,
      io_conds_4 =>  io_conds_4,
      io_data_0 =>  io_data_0,
      io_data_1 =>  io_data_1,
      io_data_2 =>  io_data_2,
      io_data_3 =>  io_data_3,
      io_data_4 =>  io_data_4,
      io_outDefault =>  io_outDefault 
    );
end arch;
