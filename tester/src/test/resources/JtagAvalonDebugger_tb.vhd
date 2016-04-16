library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_JtagAvalonDebugger;
use lib_JtagAvalonDebugger.pkg_scala2hdl.all;
use lib_JtagAvalonDebugger.all;
use lib_JtagAvalonDebugger.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity JtagAvalonDebugger_tb is
end JtagAvalonDebugger_tb;

architecture arch of JtagAvalonDebugger_tb is
  signal io_jtag_tms : std_logic;
  signal io_jtag_tdi : std_logic;
  signal io_jtag_tdo : std_logic;
  signal io_mem_read : std_logic;
  signal io_mem_write : std_logic;
  signal io_mem_waitRequestn : std_logic;
  signal io_mem_address : std_logic_vector(31 downto 0);
  signal io_mem_byteEnable : std_logic_vector(3 downto 0);
  signal io_mem_writeData : std_logic_vector(31 downto 0);
  signal io_mem_readDataValid : std_logic;
  signal io_mem_readData : std_logic_vector(31 downto 0);
  signal clk : std_logic;
  signal tck : std_logic;
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
    assert now < 10 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;
  
  
  process
    procedure jtagTick(tms : std_logic;tdi : std_logic) is
    begin
      tck <= '0';
      io_jtag_tms <= tms;
      io_jtag_tdi <= tdi;
      wait for 100 ns;
      tck <= '1';
      wait for 100 ns;
      tck <= '0';
    end procedure;
    

    procedure jtagStateMove(inst : std_logic_vector) is 
    begin
      for i in inst'range loop
        jtagTick(inst(i),'X');
      end loop;
    end procedure;
    
    procedure jtagDataMove(data : std_logic_vector) is 
    begin
      for i in data'high downto 0  loop
        jtagTick('0',data(i));
      end loop;
    end procedure;
    
    procedure jtagInit is
    begin
      jtagStateMove("111110");
    end procedure;
    
    

    procedure jtagDrIn is 
    begin
      jtagStateMove("100");
    end procedure;

    procedure jtagDrOut(data : std_logic_vector) is 
    begin
      for i in data'high downto 0  loop
        if i = 0 then
          jtagTick('1',data(i));
        else
          jtagTick('0',data(i));
        end if;
      end loop;
      jtagStateMove("10");
    end procedure;
        
        
    procedure jtagInst(inst : std_logic_vector) is 
    begin
      jtagStateMove("1100");
      jtagDrOut(inst);
    end procedure;
    
  begin
    reset <= '1';
    io_mem_waitRequestn <= '0';
    io_mem_readDataValid <= '0';
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);
    jtagInit;
    
    jtagInst(X"2");
    jtagDrIn;
    jtagDataMove(X"00");
    jtagDataMove(X"123CC678");
    jtagDataMove(X"123DD678");
    jtagDataMove("1");
    jtagDrOut("10");  
    
    wait for 200 ns;
    wait until rising_edge(clk);
    io_mem_waitRequestn <= '1';
    wait until rising_edge(clk);
    io_mem_waitRequestn <= '0';
    io_mem_readDataValid <= '1';
    io_mem_readData <= X"EEAA1234";
    wait until rising_edge(clk);
    io_mem_readDataValid <= '0';
    
    wait for 200 ns;
    wait until rising_edge(clk); 
    

    jtagDrIn;
    jtagDataMove(X"00");
    jtagDataMove(X"123AA678");
    jtagDataMove(X"123BB678");
    jtagDataMove("1");
    jtagDrOut("10");  
    
    wait for 200 ns;
    wait until rising_edge(clk);
    io_mem_waitRequestn <= '1';
    wait until rising_edge(clk);
    io_mem_waitRequestn <= '0';
    io_mem_readDataValid <= '1';
    io_mem_readData <= X"EE551234";
    wait until rising_edge(clk);
    io_mem_readDataValid <= '0';
    
    wait for 1000 ns;
    done := done + 1;
    wait;
  end process;
  -- #spinalEnd userLogics
  uut : entity lib_JtagAvalonDebugger.JtagAvalonDebugger
    port map (
      io_jtag_tms =>  io_jtag_tms,
      io_jtag_tdi =>  io_jtag_tdi,
      io_jtag_tdo =>  io_jtag_tdo,
      io_mem_read =>  io_mem_read,
      io_mem_write =>  io_mem_write,
      io_mem_waitRequestn =>  io_mem_waitRequestn,
      io_mem_address =>  io_mem_address,
      io_mem_byteEnable =>  io_mem_byteEnable,
      io_mem_writeData =>  io_mem_writeData,
      io_mem_readDataValid =>  io_mem_readDataValid,
      io_mem_readData =>  io_mem_readData,
      clk =>  clk,
      tck =>  tck,
      reset =>  reset 
    );
end arch;
