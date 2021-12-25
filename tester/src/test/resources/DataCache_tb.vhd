library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_DataCache;
use lib_DataCache.pkg_scala2hdl.all;
use lib_DataCache.all;
use lib_DataCache.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity DataCache_tb is
end DataCache_tb;

architecture arch of DataCache_tb is
  signal io_cpu_cmd_valid : std_logic;
  signal io_cpu_cmd_ready : std_logic;
  signal io_cpu_cmd_payload_kind : DataCacheCpuCmdKind;
  signal io_cpu_cmd_payload_wr : std_logic;
  signal io_cpu_cmd_payload_address : unsigned(11 downto 0);
  signal io_cpu_cmd_payload_data : std_logic_vector(15 downto 0);
  signal io_cpu_cmd_payload_mask : std_logic_vector(1 downto 0);
  signal io_cpu_cmd_payload_bypass : std_logic;
  signal io_cpu_cmd_payload_all : std_logic;
  signal io_cpu_rsp_valid : std_logic;
  signal io_cpu_rsp_payload_data : std_logic_vector(15 downto 0);
  signal io_mem_cmd_valid : std_logic;
  signal io_mem_cmd_ready : std_logic;
  signal io_mem_cmd_payload_wr : std_logic;
  signal io_mem_cmd_payload_address : unsigned(11 downto 0);
  signal io_mem_cmd_payload_data : std_logic_vector(15 downto 0);
  signal io_mem_cmd_payload_mask : std_logic_vector(1 downto 0);
  signal io_mem_cmd_payload_length : unsigned(3 downto 0);
  signal io_mem_rsp_valid : std_logic;
  signal io_mem_rsp_payload_data : std_logic_vector(15 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  
  constant memSize : integer := 2**io_cpu_cmd_payload_address'length;
  type memType is array (0 to memSize-1) of std_logic_vector(15 downto 0);
  shared variable ram : memType;  
  shared variable ramCpu : memType;  
  
  constant cpuPendingRspSize : integer := 16; 
  type cpuPendingRspType is array (0 to cpuPendingRspSize-1) of std_logic_vector(15 downto 0);
  shared variable cpuPendingRsp : cpuPendingRspType;  
  shared variable cpuPendingRspHit,cpuPendingRspTarget : integer := 0;
 
  shared variable do_mem_cmd_payload_address  : unsigned(11 downto 0);
  shared variable do_mem_cmd_payload_length   : unsigned(3 downto 0) := (others => '0');

  signal cpuRspcounter : integer := 0;
  
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
  
  impure function randomStdLogicVector(size : integer) return std_logic_vector is
    variable rand: real;
    variable int_rand: integer;
  begin
    UNIFORM(seed1, seed2, rand);
    int_rand := INTEGER((rand*(2.0**size)));
    return (std_logic_vector(to_unsigned(int_rand, size)));
  end randomStdLogicVector;
  
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
    wait for 5 ns;
    if done = 1 then
      wait;
    end if;
    assert now < 200 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;


  
  process
  begin
    wait until rising_edge(clk);
    io_mem_cmd_ready <= randomStdLogic(0.6);
    if do_mem_cmd_payload_length /= 0 then
      io_mem_cmd_ready <= '0';
    end if;
  end process;
  
  process
  begin
    wait until rising_edge(clk) and io_mem_cmd_valid = '1' and io_mem_cmd_ready = '1';
    if io_mem_cmd_payload_wr = '0' then
      do_mem_cmd_payload_address   := io_mem_cmd_payload_address;
      do_mem_cmd_payload_length    := io_mem_cmd_payload_length ;
    else
      for i in 0 to to_integer(io_mem_cmd_payload_length)-1 loop
        for i2 in 0 to 1 loop
          if io_mem_cmd_payload_mask(i2) = '1' then
            ram(to_integer(io_mem_cmd_payload_address)/2 + i)(i2*8+7 downto i2*8) := io_mem_cmd_payload_data(i2*8+7 downto i2*8);
          end if;
        end loop;
        if i /= to_integer(io_mem_cmd_payload_length)-1 then
          wait until rising_edge(clk) and io_mem_cmd_valid = '1' and io_mem_cmd_ready = '1';
        end if;
      end loop;
    end if;
  end process;
  
  process
  begin
    wait until rising_edge(clk);
    io_mem_rsp_valid <= '0';
    if do_mem_cmd_payload_length /= 0 then      
      waitRandom(0.3);
      io_mem_rsp_valid <= '1';
      io_mem_rsp_payload_data <= ram(to_integer(do_mem_cmd_payload_address)/2);
      do_mem_cmd_payload_length := do_mem_cmd_payload_length - 1;
      do_mem_cmd_payload_address := do_mem_cmd_payload_address + 2;
    end if;
  end process;
    
  
  process
    variable v_address : unsigned(11 downto 0);
    procedure cpuReadCmd(address : unsigned;bypass : std_logic) is
    begin
      io_cpu_cmd_valid <= '1';
      io_cpu_cmd_payload_kind <= MEMORY;
      io_cpu_cmd_payload_wr <= '0';
      io_cpu_cmd_payload_address <= address and X"FFE";
      io_cpu_cmd_payload_data <= (others => 'X');
      io_cpu_cmd_payload_mask <= randomStdLogicVector(2);
      io_cpu_cmd_payload_bypass <= bypass;
      cpuPendingRsp(cpuPendingRspTarget) := ramCpu(to_integer(address)/2);
      cpuPendingRspTarget := (cpuPendingRspTarget + 1) mod cpuPendingRspSize;
      assert(cpuPendingRspTarget /= cpuPendingRspHit) severity failure;
      wait until rising_edge(clk) and io_cpu_cmd_ready = '1';
      io_cpu_cmd_valid <= '0';
      io_cpu_cmd_payload_wr <= 'X';
      io_cpu_cmd_payload_address <= (others => 'X');
      io_cpu_cmd_payload_data <= (others => 'X');
      io_cpu_cmd_payload_mask <= (others => 'X');
      io_cpu_cmd_payload_bypass <= 'X';
    end procedure;
    procedure cpuWriteCmd(address : unsigned;data : std_logic_vector;bypass : std_logic) is
      variable mask : std_logic_vector(1 downto 0);
    begin
      mask := randomStdLogicVector(2);
      io_cpu_cmd_valid <= '1';
      io_cpu_cmd_payload_kind <= MEMORY;
      io_cpu_cmd_payload_wr <= '1';
      io_cpu_cmd_payload_address <= address and X"FFE";
      io_cpu_cmd_payload_data <= data;
      io_cpu_cmd_payload_mask <= mask;
      io_cpu_cmd_payload_bypass <= bypass;
      for i in 0 to 1 loop
        if mask(i) = '1' then
          ramCpu(to_integer(address)/2)(i*8+7 downto i*8) := data(i*8+7 downto i*8);
        end if;
      end loop;
      wait until rising_edge(clk) and io_cpu_cmd_ready = '1';
      io_cpu_cmd_valid <= '0';
      io_cpu_cmd_payload_wr <= 'X';
      io_cpu_cmd_payload_address <= (others => 'X');
      io_cpu_cmd_payload_data <= (others => 'X');
      io_cpu_cmd_payload_mask <= (others => 'X');
      io_cpu_cmd_payload_bypass <= 'X';
    end procedure;
  begin
    reset <= '1';
    io_cpu_cmd_valid <= '0';
    for i in ram'range loop
      ram(i) := std_logic_vector(to_unsigned(i*2,16));
      ramCpu(i) := ram(i);
    end loop;
    wait for 100 ns;
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);
      while true loop
        waitRandom(0.3);
        if randomStdLogic(0.95) = '1' then --MEMORY access
          if randomStdLogic(0.5) = '1' then --Cached
            if randomStdLogic(0.5) = '1' then --write
              cpuWriteCmd( unsigned(randomStdLogicVector(12)) and X"7FF",randomStdLogicVector(16),'0');     
            else --read
              cpuReadCmd( unsigned(randomStdLogicVector(12)) and X"7FF",'0');
           end if;  
          else --not cached
            if randomStdLogic(0.5) = '1' then --write
              cpuWriteCmd( unsigned(randomStdLogicVector(12)) or X"800",randomStdLogicVector(16),'1');     
            else --read
              cpuReadCmd( unsigned(randomStdLogicVector(12)) or X"800",'1');
            end if;  
          end if;
        else --evict/flush
          v_address :=  unsigned(randomStdLogicVector(12)) and X"7FE";
          if randomStdLogic(0.5) = '1' then --evict
            wait until rising_edge(clk) and io_cpu_cmd_ready = '1';
            wait until rising_edge(clk);
            wait until rising_edge(clk);
            wait until rising_edge(clk);
            wait until rising_edge(clk) and io_cpu_cmd_ready = '1' and io_mem_cmd_valid = '0'; --wait victim buffer cleaning
            io_cpu_cmd_valid <= '1';
            io_cpu_cmd_payload_kind <= EVICT;
            if randomStdLogic(0.9) = '1' then --line
              io_cpu_cmd_payload_address <= v_address;
              io_cpu_cmd_payload_all <= '0';
              for i in to_integer(v_address and X"FF0")/2 to to_integer(v_address and X"FF0")/2 + 7 loop
                ramCpu(i) := ram(i);
              end loop;
            else
              io_cpu_cmd_payload_address <= (others => '0');
              io_cpu_cmd_payload_all <= '1';
              ramCpu := ram;
            end if;
          else
            io_cpu_cmd_valid <= '1';
            io_cpu_cmd_payload_kind <= FLUSH;
            if randomStdLogic(0.9) = '1' then --line
              io_cpu_cmd_payload_address <= v_address;
              io_cpu_cmd_payload_all <= '0';
            else
              io_cpu_cmd_payload_address <= (others => '0');
              io_cpu_cmd_payload_all <= '1';
            end if;
          end if;
          wait until rising_edge(clk) and io_cpu_cmd_ready = '1';
          io_cpu_cmd_valid <= '0';
        end if;
      end loop;
    wait;
  end process;
  

  process
    procedure cpuReadRsp(data : std_logic_vector) is
    begin
      wait until rising_edge(clk) and io_cpu_rsp_valid = '1';
      assert io_cpu_rsp_payload_data = data report "read mismatch" severity failure;
    end procedure;
    variable counter : integer := 0;
  begin
    reset <= '1';
    wait for 100 ns;
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);
    
    while cpuRspcounter < 100000 loop
      wait until rising_edge(clk) and io_cpu_rsp_valid = '1';
      assert(cpuPendingRspTarget /= cpuPendingRspHit) severity failure;
      assert io_cpu_rsp_payload_data = cpuPendingRsp(cpuPendingRspHit) report "read mismatch" severity error;
      cpuPendingRspHit := (cpuPendingRspHit + 1) mod cpuPendingRspSize;
      cpuRspcounter <= cpuRspcounter + 1;
    end loop;
    wait for 100 ns;
    done := done + 1;
    wait;
  end process;
  
  -- #spinalEnd userLogics
  uut : entity lib_DataCache.DataCache
    port map (
      io_cpu_cmd_valid =>  io_cpu_cmd_valid,
      io_cpu_cmd_ready =>  io_cpu_cmd_ready,
      io_cpu_cmd_payload_kind =>  io_cpu_cmd_payload_kind,
      io_cpu_cmd_payload_wr =>  io_cpu_cmd_payload_wr,
      io_cpu_cmd_payload_address =>  io_cpu_cmd_payload_address,
      io_cpu_cmd_payload_data =>  io_cpu_cmd_payload_data,
      io_cpu_cmd_payload_mask =>  io_cpu_cmd_payload_mask,
      io_cpu_cmd_payload_bypass =>  io_cpu_cmd_payload_bypass,
      io_cpu_cmd_payload_all =>  io_cpu_cmd_payload_all,
      io_cpu_rsp_valid =>  io_cpu_rsp_valid,
      io_cpu_rsp_payload_data =>  io_cpu_rsp_payload_data,
      io_mem_cmd_valid =>  io_mem_cmd_valid,
      io_mem_cmd_ready =>  io_mem_cmd_ready,
      io_mem_cmd_payload_wr =>  io_mem_cmd_payload_wr,
      io_mem_cmd_payload_address =>  io_mem_cmd_payload_address,
      io_mem_cmd_payload_data =>  io_mem_cmd_payload_data,
      io_mem_cmd_payload_mask =>  io_mem_cmd_payload_mask,
      io_mem_cmd_payload_length =>  io_mem_cmd_payload_length,
      io_mem_rsp_valid =>  io_mem_rsp_valid,
      io_mem_rsp_payload_data =>  io_mem_rsp_payload_data,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
