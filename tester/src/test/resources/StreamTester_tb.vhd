library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.pkg_scala2hdl.all;
use work.all;
use work.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity StreamTester_tb is
end StreamTester_tb;

architecture arch of StreamTester_tb is
  signal io_slave0_valid : std_logic;
  signal io_slave0_ready : std_logic;
  signal io_slave0_payload_a : unsigned(7 downto 0);
  signal io_slave0_payload_b : std_logic;
  signal io_master0_valid : std_logic;
  signal io_master0_ready : std_logic;
  signal io_master0_payload_a : unsigned(7 downto 0);
  signal io_master0_payload_b : std_logic;
  signal io_fifo0_occupancy : unsigned(4 downto 0);
  signal forkInput_valid : std_logic;
  signal forkInput_ready : std_logic;
  signal forkInput_payload : std_logic_vector(7 downto 0);
  signal forkOutputs_0_valid : std_logic;
  signal forkOutputs_0_ready : std_logic;
  signal forkOutputs_0_payload : std_logic_vector(7 downto 0);
  signal forkOutputs_1_valid : std_logic;
  signal forkOutputs_1_ready : std_logic;
  signal forkOutputs_1_payload : std_logic_vector(7 downto 0);
  signal forkOutputs_2_valid : std_logic;
  signal forkOutputs_2_ready : std_logic;
  signal forkOutputs_2_payload : std_logic_vector(7 downto 0);
  signal dispatcherInOrderInput_valid : std_logic;
  signal dispatcherInOrderInput_ready : std_logic;
  signal dispatcherInOrderInput_payload : std_logic_vector(7 downto 0);
  signal dispatcherInOrderOutputs_0_valid : std_logic;
  signal dispatcherInOrderOutputs_0_ready : std_logic;
  signal dispatcherInOrderOutputs_0_payload : std_logic_vector(7 downto 0);
  signal dispatcherInOrderOutputs_1_valid : std_logic;
  signal dispatcherInOrderOutputs_1_ready : std_logic;
  signal dispatcherInOrderOutputs_1_payload : std_logic_vector(7 downto 0);
  signal dispatcherInOrderOutputs_2_valid : std_logic;
  signal dispatcherInOrderOutputs_2_ready : std_logic;
  signal dispatcherInOrderOutputs_2_payload : std_logic_vector(7 downto 0);
  signal streamFlowArbiterInputStream_valid : std_logic;
  signal streamFlowArbiterInputStream_ready : std_logic;
  signal streamFlowArbiterInputStream_payload : std_logic_vector(7 downto 0);
  signal streamFlowArbiterInputFlow_valid : std_logic;
  signal streamFlowArbiterInputFlow_payload : std_logic_vector(7 downto 0);
  signal streamFlowArbiterOutput_valid : std_logic;
  signal streamFlowArbiterOutput_payload : std_logic_vector(7 downto 0);
  signal arbiterInOrderInputs_0_valid : std_logic;
  signal arbiterInOrderInputs_0_ready : std_logic;
  signal arbiterInOrderInputs_0_payload : std_logic_vector(7 downto 0);
  signal arbiterInOrderInputs_1_valid : std_logic;
  signal arbiterInOrderInputs_1_ready : std_logic;
  signal arbiterInOrderInputs_1_payload : std_logic_vector(7 downto 0);
  signal arbiterInOrderInputs_2_valid : std_logic;
  signal arbiterInOrderInputs_2_ready : std_logic;
  signal arbiterInOrderInputs_2_payload : std_logic_vector(7 downto 0);
  signal arbiterInOrderOutput_valid : std_logic;
  signal arbiterInOrderOutput_ready : std_logic;
  signal arbiterInOrderOutput_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstInputs_0_valid : std_logic;
  signal arbiterLowIdPortFirstInputs_0_ready : std_logic;
  signal arbiterLowIdPortFirstInputs_0_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstInputs_1_valid : std_logic;
  signal arbiterLowIdPortFirstInputs_1_ready : std_logic;
  signal arbiterLowIdPortFirstInputs_1_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstInputs_2_valid : std_logic;
  signal arbiterLowIdPortFirstInputs_2_ready : std_logic;
  signal arbiterLowIdPortFirstInputs_2_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstOutput_valid : std_logic;
  signal arbiterLowIdPortFirstOutput_ready : std_logic;
  signal arbiterLowIdPortFirstOutput_payload : std_logic_vector(7 downto 0);
  signal arbiterRoundRobinInputs_0_valid : std_logic;
  signal arbiterRoundRobinInputs_0_ready : std_logic;
  signal arbiterRoundRobinInputs_0_payload : std_logic_vector(7 downto 0);
  signal arbiterRoundRobinInputs_1_valid : std_logic;
  signal arbiterRoundRobinInputs_1_ready : std_logic;
  signal arbiterRoundRobinInputs_1_payload : std_logic_vector(7 downto 0);
  signal arbiterRoundRobinInputs_2_valid : std_logic;
  signal arbiterRoundRobinInputs_2_ready : std_logic;
  signal arbiterRoundRobinInputs_2_payload : std_logic_vector(7 downto 0);
  signal arbiterRoundRobinOutput_valid : std_logic;
  signal arbiterRoundRobinOutput_ready : std_logic;
  signal arbiterRoundRobinOutput_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstNoLockInputs_0_valid : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_0_ready : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_0_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstNoLockInputs_1_valid : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_1_ready : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_1_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstNoLockInputs_2_valid : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_2_ready : std_logic;
  signal arbiterLowIdPortFirstNoLockInputs_2_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstNoLockOutput_valid : std_logic;
  signal arbiterLowIdPortFirstNoLockOutput_ready : std_logic;
  signal arbiterLowIdPortFirstNoLockOutput_payload : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstFragmentLockInputs_0_valid : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_0_ready : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_0_payload_last : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_0_payload_fragment : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstFragmentLockInputs_1_valid : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_1_ready : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_1_payload_last : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_1_payload_fragment : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstFragmentLockInputs_2_valid : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_2_ready : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_2_payload_last : std_logic;
  signal arbiterLowIdPortFirstFragmentLockInputs_2_payload_fragment : std_logic_vector(7 downto 0);
  signal arbiterLowIdPortFirstFragmentLockOutput_valid : std_logic;
  signal arbiterLowIdPortFirstFragmentLockOutput_ready : std_logic;
  signal arbiterLowIdPortFirstFragmentLockOutput_payload_last : std_logic;
  signal arbiterLowIdPortFirstFragmentLockOutput_payload_fragment : std_logic_vector(7 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  
  signal slave0_counter : unsigned(7 downto 0);
  signal master0_counter : unsigned(7 downto 0);
  constant slave0_transactionCount : integer := 10000;
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
    
    procedure slave0_push is
    begin
      io_slave0_valid <= '1';
      io_slave0_payload_a <= slave0_counter;
      io_slave0_payload_b <= slave0_counter(2) xor slave0_counter(0);
      slave0_counter <= slave0_counter + X"01";
      wait until rising_edge(clk) and io_slave0_ready = '1';
      io_slave0_valid <= '0';
      io_slave0_payload_a <= (others => 'X');
      io_slave0_payload_b <= 'X';
    end slave0_push;
    
    
    variable seed1, seed2: positive;
    impure function randomBool return boolean is
      variable rand: real;
    begin
      UNIFORM(seed1, seed2, rand);
      return rand >= 0.5;
    end randomBool;
    
  begin
    reset <= '1';
    slave0_counter <= X"00";
    io_slave0_valid <= '0';
    waitClk(3);
    reset <= '0';

    for i in 0 to slave0_transactionCount loop
        slave0_push;
        while randomBool loop
          wait until rising_edge(clk);
        end loop;
    end loop;
    
    wait;
  end process;
  
  process
    procedure waitClk(value:  integer) is
    begin
      for i in 0 to value -1 loop
        wait until rising_edge(clk);  
      end loop;
    end waitClk;
    
    procedure master0_pop is
    begin
      wait until rising_edge(clk) and io_master0_valid = '1' and io_master0_ready = '1';
      assert (io_master0_payload_a = master0_counter and (io_master0_payload_b = (master0_counter(2) xor master0_counter(0)))) report "master0_pop fail" severity failure;
      master0_counter <= master0_counter + X"01";
    end master0_pop;
 
  begin
    master0_counter <= X"00";
    wait until rising_edge(clk) and reset = '0';

    
    for i in 0 to slave0_transactionCount loop
      master0_pop;
    end loop;
    
    done := done + 1;
    wait;
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
  begin
    wait until rising_edge(clk);
    random(io_master0_ready);
  end process;
  
  -- #spinalEnd userLogics
  uut : entity work.StreamTester
    port map (
      io_slave0_valid =>  io_slave0_valid,
      io_slave0_ready =>  io_slave0_ready,
      io_slave0_payload_a =>  io_slave0_payload_a,
      io_slave0_payload_b =>  io_slave0_payload_b,
      io_master0_valid =>  io_master0_valid,
      io_master0_ready =>  io_master0_ready,
      io_master0_payload_a =>  io_master0_payload_a,
      io_master0_payload_b =>  io_master0_payload_b,
      io_fifo0_occupancy =>  io_fifo0_occupancy,
      forkInput_valid =>  forkInput_valid,
      forkInput_ready =>  forkInput_ready,
      forkInput_payload =>  forkInput_payload,
      forkOutputs_0_valid =>  forkOutputs_0_valid,
      forkOutputs_0_ready =>  forkOutputs_0_ready,
      forkOutputs_0_payload =>  forkOutputs_0_payload,
      forkOutputs_1_valid =>  forkOutputs_1_valid,
      forkOutputs_1_ready =>  forkOutputs_1_ready,
      forkOutputs_1_payload =>  forkOutputs_1_payload,
      forkOutputs_2_valid =>  forkOutputs_2_valid,
      forkOutputs_2_ready =>  forkOutputs_2_ready,
      forkOutputs_2_payload =>  forkOutputs_2_payload,
      dispatcherInOrderInput_valid =>  dispatcherInOrderInput_valid,
      dispatcherInOrderInput_ready =>  dispatcherInOrderInput_ready,
      dispatcherInOrderInput_payload =>  dispatcherInOrderInput_payload,
      dispatcherInOrderOutputs_0_valid =>  dispatcherInOrderOutputs_0_valid,
      dispatcherInOrderOutputs_0_ready =>  dispatcherInOrderOutputs_0_ready,
      dispatcherInOrderOutputs_0_payload =>  dispatcherInOrderOutputs_0_payload,
      dispatcherInOrderOutputs_1_valid =>  dispatcherInOrderOutputs_1_valid,
      dispatcherInOrderOutputs_1_ready =>  dispatcherInOrderOutputs_1_ready,
      dispatcherInOrderOutputs_1_payload =>  dispatcherInOrderOutputs_1_payload,
      dispatcherInOrderOutputs_2_valid =>  dispatcherInOrderOutputs_2_valid,
      dispatcherInOrderOutputs_2_ready =>  dispatcherInOrderOutputs_2_ready,
      dispatcherInOrderOutputs_2_payload =>  dispatcherInOrderOutputs_2_payload,
      streamFlowArbiterInputStream_valid =>  streamFlowArbiterInputStream_valid,
      streamFlowArbiterInputStream_ready =>  streamFlowArbiterInputStream_ready,
      streamFlowArbiterInputStream_payload =>  streamFlowArbiterInputStream_payload,
      streamFlowArbiterInputFlow_valid =>  streamFlowArbiterInputFlow_valid,
      streamFlowArbiterInputFlow_payload =>  streamFlowArbiterInputFlow_payload,
      streamFlowArbiterOutput_valid =>  streamFlowArbiterOutput_valid,
      streamFlowArbiterOutput_payload =>  streamFlowArbiterOutput_payload,
      arbiterInOrderInputs_0_valid =>  arbiterInOrderInputs_0_valid,
      arbiterInOrderInputs_0_ready =>  arbiterInOrderInputs_0_ready,
      arbiterInOrderInputs_0_payload =>  arbiterInOrderInputs_0_payload,
      arbiterInOrderInputs_1_valid =>  arbiterInOrderInputs_1_valid,
      arbiterInOrderInputs_1_ready =>  arbiterInOrderInputs_1_ready,
      arbiterInOrderInputs_1_payload =>  arbiterInOrderInputs_1_payload,
      arbiterInOrderInputs_2_valid =>  arbiterInOrderInputs_2_valid,
      arbiterInOrderInputs_2_ready =>  arbiterInOrderInputs_2_ready,
      arbiterInOrderInputs_2_payload =>  arbiterInOrderInputs_2_payload,
      arbiterInOrderOutput_valid =>  arbiterInOrderOutput_valid,
      arbiterInOrderOutput_ready =>  arbiterInOrderOutput_ready,
      arbiterInOrderOutput_payload =>  arbiterInOrderOutput_payload,
      arbiterLowIdPortFirstInputs_0_valid =>  arbiterLowIdPortFirstInputs_0_valid,
      arbiterLowIdPortFirstInputs_0_ready =>  arbiterLowIdPortFirstInputs_0_ready,
      arbiterLowIdPortFirstInputs_0_payload =>  arbiterLowIdPortFirstInputs_0_payload,
      arbiterLowIdPortFirstInputs_1_valid =>  arbiterLowIdPortFirstInputs_1_valid,
      arbiterLowIdPortFirstInputs_1_ready =>  arbiterLowIdPortFirstInputs_1_ready,
      arbiterLowIdPortFirstInputs_1_payload =>  arbiterLowIdPortFirstInputs_1_payload,
      arbiterLowIdPortFirstInputs_2_valid =>  arbiterLowIdPortFirstInputs_2_valid,
      arbiterLowIdPortFirstInputs_2_ready =>  arbiterLowIdPortFirstInputs_2_ready,
      arbiterLowIdPortFirstInputs_2_payload =>  arbiterLowIdPortFirstInputs_2_payload,
      arbiterLowIdPortFirstOutput_valid =>  arbiterLowIdPortFirstOutput_valid,
      arbiterLowIdPortFirstOutput_ready =>  arbiterLowIdPortFirstOutput_ready,
      arbiterLowIdPortFirstOutput_payload =>  arbiterLowIdPortFirstOutput_payload,
      arbiterRoundRobinInputs_0_valid =>  arbiterRoundRobinInputs_0_valid,
      arbiterRoundRobinInputs_0_ready =>  arbiterRoundRobinInputs_0_ready,
      arbiterRoundRobinInputs_0_payload =>  arbiterRoundRobinInputs_0_payload,
      arbiterRoundRobinInputs_1_valid =>  arbiterRoundRobinInputs_1_valid,
      arbiterRoundRobinInputs_1_ready =>  arbiterRoundRobinInputs_1_ready,
      arbiterRoundRobinInputs_1_payload =>  arbiterRoundRobinInputs_1_payload,
      arbiterRoundRobinInputs_2_valid =>  arbiterRoundRobinInputs_2_valid,
      arbiterRoundRobinInputs_2_ready =>  arbiterRoundRobinInputs_2_ready,
      arbiterRoundRobinInputs_2_payload =>  arbiterRoundRobinInputs_2_payload,
      arbiterRoundRobinOutput_valid =>  arbiterRoundRobinOutput_valid,
      arbiterRoundRobinOutput_ready =>  arbiterRoundRobinOutput_ready,
      arbiterRoundRobinOutput_payload =>  arbiterRoundRobinOutput_payload,
      arbiterLowIdPortFirstNoLockInputs_0_valid =>  arbiterLowIdPortFirstNoLockInputs_0_valid,
      arbiterLowIdPortFirstNoLockInputs_0_ready =>  arbiterLowIdPortFirstNoLockInputs_0_ready,
      arbiterLowIdPortFirstNoLockInputs_0_payload =>  arbiterLowIdPortFirstNoLockInputs_0_payload,
      arbiterLowIdPortFirstNoLockInputs_1_valid =>  arbiterLowIdPortFirstNoLockInputs_1_valid,
      arbiterLowIdPortFirstNoLockInputs_1_ready =>  arbiterLowIdPortFirstNoLockInputs_1_ready,
      arbiterLowIdPortFirstNoLockInputs_1_payload =>  arbiterLowIdPortFirstNoLockInputs_1_payload,
      arbiterLowIdPortFirstNoLockInputs_2_valid =>  arbiterLowIdPortFirstNoLockInputs_2_valid,
      arbiterLowIdPortFirstNoLockInputs_2_ready =>  arbiterLowIdPortFirstNoLockInputs_2_ready,
      arbiterLowIdPortFirstNoLockInputs_2_payload =>  arbiterLowIdPortFirstNoLockInputs_2_payload,
      arbiterLowIdPortFirstNoLockOutput_valid =>  arbiterLowIdPortFirstNoLockOutput_valid,
      arbiterLowIdPortFirstNoLockOutput_ready =>  arbiterLowIdPortFirstNoLockOutput_ready,
      arbiterLowIdPortFirstNoLockOutput_payload =>  arbiterLowIdPortFirstNoLockOutput_payload,
      arbiterLowIdPortFirstFragmentLockInputs_0_valid =>  arbiterLowIdPortFirstFragmentLockInputs_0_valid,
      arbiterLowIdPortFirstFragmentLockInputs_0_ready =>  arbiterLowIdPortFirstFragmentLockInputs_0_ready,
      arbiterLowIdPortFirstFragmentLockInputs_0_payload_last =>  arbiterLowIdPortFirstFragmentLockInputs_0_payload_last,
      arbiterLowIdPortFirstFragmentLockInputs_0_payload_fragment =>  arbiterLowIdPortFirstFragmentLockInputs_0_payload_fragment,
      arbiterLowIdPortFirstFragmentLockInputs_1_valid =>  arbiterLowIdPortFirstFragmentLockInputs_1_valid,
      arbiterLowIdPortFirstFragmentLockInputs_1_ready =>  arbiterLowIdPortFirstFragmentLockInputs_1_ready,
      arbiterLowIdPortFirstFragmentLockInputs_1_payload_last =>  arbiterLowIdPortFirstFragmentLockInputs_1_payload_last,
      arbiterLowIdPortFirstFragmentLockInputs_1_payload_fragment =>  arbiterLowIdPortFirstFragmentLockInputs_1_payload_fragment,
      arbiterLowIdPortFirstFragmentLockInputs_2_valid =>  arbiterLowIdPortFirstFragmentLockInputs_2_valid,
      arbiterLowIdPortFirstFragmentLockInputs_2_ready =>  arbiterLowIdPortFirstFragmentLockInputs_2_ready,
      arbiterLowIdPortFirstFragmentLockInputs_2_payload_last =>  arbiterLowIdPortFirstFragmentLockInputs_2_payload_last,
      arbiterLowIdPortFirstFragmentLockInputs_2_payload_fragment =>  arbiterLowIdPortFirstFragmentLockInputs_2_payload_fragment,
      arbiterLowIdPortFirstFragmentLockOutput_valid =>  arbiterLowIdPortFirstFragmentLockOutput_valid,
      arbiterLowIdPortFirstFragmentLockOutput_ready =>  arbiterLowIdPortFirstFragmentLockOutput_ready,
      arbiterLowIdPortFirstFragmentLockOutput_payload_last =>  arbiterLowIdPortFirstFragmentLockOutput_payload_last,
      arbiterLowIdPortFirstFragmentLockOutput_payload_fragment =>  arbiterLowIdPortFirstFragmentLockOutput_payload_fragment,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
