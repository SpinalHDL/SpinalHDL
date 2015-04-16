library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

library lib_CommonTester;
use lib_CommonTester.pkg_scala2hdl.all;
use lib_CommonTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity CommonTester_tb is
end CommonTester_tb;

architecture arch of CommonTester_tb is
  signal io_conds_0 : std_logic;
  signal io_conds_1 : std_logic;
  signal io_conds_2 : std_logic;
  signal io_conds_3 : std_logic;
  signal io_conds_4 : std_logic;
  signal io_conds_5 : std_logic;
  signal io_conds_6 : std_logic;
  signal io_conds_7 : std_logic;
  signal io_inUIntA : unsigned(7 downto 0);
  signal io_inUIntB : unsigned(7 downto 0);
  signal io_outUIntAdder : unsigned(7 downto 0);
  signal io_inAA_bod_gggg : std_logic;
  signal io_inAA_bod_aosi : unsigned(2 downto 0);
  signal io_inAA_ahe : std_logic;
  signal io_inAA_zwg : std_logic;
  signal io_inAA_vsw : std_logic;
  signal io_inAA_lwee : unsigned(4 downto 0);
  signal io_inAABits : std_logic_vector(11 downto 0);
  signal io_outAA_bod_gggg : std_logic;
  signal io_outAA_bod_aosi : unsigned(2 downto 0);
  signal io_outAA_ahe : std_logic;
  signal io_outAA_zwg : std_logic;
  signal io_outAA_vsw : std_logic;
  signal io_outAA_lwee : unsigned(4 downto 0);
  signal io_outAABits : std_logic_vector(11 downto 0);
  signal io_assign_sel_0 : unsigned(3 downto 0);
  signal io_assign_sel_1 : unsigned(3 downto 0);
  signal io_assign_sel_2 : unsigned(3 downto 0);
  signal io_assign_sel_3 : unsigned(3 downto 0);
  signal io_assign_bitDemux : std_logic_vector(15 downto 0);
  signal io_complexLiteral : unsigned(15 downto 0);
  -- #spinalBegin userDeclarations
  signal clk : std_logic;
  signal asyncProcess : std_logic := '0';

  signal io_assign_bitDemux_ref : std_logic_vector(15 downto 0);

  shared variable done : boolean := false; 
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
    io_assign_bitDemux_ref <= (others => '0');
    io_assign_bitDemux_ref(to_integer(io_assign_sel_0)) <= io_conds_0;
    if io_conds_1 = '1' then
      io_assign_bitDemux_ref(to_integer(io_assign_sel_1)) <= io_conds_2;
    elsif io_conds_3 = '1' then
      io_assign_bitDemux_ref(to_integer(io_assign_sel_0)) <= io_conds_4;
    end if;
    if io_conds_5 = '1' then
      io_assign_bitDemux_ref(to_integer(io_assign_sel_1)) <= io_conds_6;
    end if;
    io_assign_bitDemux_ref(5) <= '1';
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

    procedure random(signal that: out std_logic_vector) is
      variable rand: real;
      variable int_rand: integer;
    begin
      UNIFORM(seed1, seed2, rand);
      int_rand := INTEGER(TRUNC(rand*(2.0**that'length)));
      that <= std_logic_vector(to_unsigned(int_rand, that'LENGTH));
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

    random(io_inAA_bod_gggg);
    random(io_inAA_bod_aosi);
    random(io_inAA_ahe);
    random(io_inAA_zwg);
    random(io_inAA_vsw);
    random(io_inAA_lwee);
    random(io_inAABits);

    random(io_inUIntA);
    random(io_inUIntB);

    random(io_assign_sel_0);
    random(io_assign_sel_1);
    random(io_assign_sel_2);
    random(io_assign_sel_3);


    wait for 1 ns;
    asyncProcess <= not asyncProcess;
    
    wait until rising_edge(clk);

    if testCounter /= 0 then
	  assert io_complexLiteral = "0001011101100010" report "io_complexLiteral fail " & integer'image(to_integer(io_complexLiteral)) severity failure;
      assert io_inAABits(0) = io_outAA_bod_gggg report "io_outAA_bod_gggg fail" severity failure;
      assert io_inAABits(3 downto 1) = std_logic_vector(io_outAA_bod_aosi) report "io_inAA_bod_aosi fail" severity failure;
      assert io_inAABits(4) = io_outAA_ahe report "io_outAA_ahe fail" severity failure;
      assert io_inAABits(5) = io_outAA_zwg report "io_outAA_zwg fail" severity failure;
      assert io_inAABits(6) = io_outAA_vsw report "io_outAA_vsw fail" severity failure;
      assert io_inAABits(11 downto 7) = std_logic_vector(io_outAA_lwee) report "io_outAA_lwee fail" severity failure;
      
      assert io_outAABits(0) = io_inAA_bod_gggg report "io_outAABits fail" severity failure;
      assert io_outAABits(3 downto 1) = std_logic_vector(io_inAA_bod_aosi) report "io_outAABits fail" severity failure;
      assert io_outAABits(4) = io_inAA_ahe report "io_outAABits fail" severity failure;
      assert io_outAABits(5) = io_inAA_zwg report "io_outAABits fail" severity failure;
      assert io_outAABits(6) = io_inAA_vsw report "io_outAABits fail" severity failure;
      assert io_outAABits(11 downto 7) = std_logic_vector(io_inAA_lwee) report "io_outAABits fail" severity failure;

      assert io_inUIntA + io_inUIntB = io_outUIntAdder  report "io_outUIntAdder fail" severity failure;

      assert io_assign_bitDemux = io_assign_bitDemux_ref  report "io_assign_bitDemux_ref fail" severity failure;
   end if;

    
    testCounter := testCounter + 1;
    if testCounter = 10000 then
      done := true;
    end if;
  end process;  
  
  

  -- #spinalEnd userLogics
  uut : entity lib_CommonTester.CommonTester
    port map (
      io_conds_0 =>  io_conds_0,
      io_conds_1 =>  io_conds_1,
      io_conds_2 =>  io_conds_2,
      io_conds_3 =>  io_conds_3,
      io_conds_4 =>  io_conds_4,
      io_conds_5 =>  io_conds_5,
      io_conds_6 =>  io_conds_6,
      io_conds_7 =>  io_conds_7,
      io_inUIntA =>  io_inUIntA,
      io_inUIntB =>  io_inUIntB,
      io_outUIntAdder =>  io_outUIntAdder,
      io_inAA_bod_gggg =>  io_inAA_bod_gggg,
      io_inAA_bod_aosi =>  io_inAA_bod_aosi,
      io_inAA_ahe =>  io_inAA_ahe,
      io_inAA_zwg =>  io_inAA_zwg,
      io_inAA_vsw =>  io_inAA_vsw,
      io_inAA_lwee =>  io_inAA_lwee,
      io_inAABits =>  io_inAABits,
      io_outAA_bod_gggg =>  io_outAA_bod_gggg,
      io_outAA_bod_aosi =>  io_outAA_bod_aosi,
      io_outAA_ahe =>  io_outAA_ahe,
      io_outAA_zwg =>  io_outAA_zwg,
      io_outAA_vsw =>  io_outAA_vsw,
      io_outAA_lwee =>  io_outAA_lwee,
      io_outAABits =>  io_outAABits,
      io_assign_sel_0 =>  io_assign_sel_0,
      io_assign_sel_1 =>  io_assign_sel_1,
      io_assign_sel_2 =>  io_assign_sel_2,
      io_assign_sel_3 =>  io_assign_sel_3,
	  io_complexLiteral => io_complexLiteral,
      io_assign_bitDemux =>  io_assign_bitDemux 
    );
end arch;
