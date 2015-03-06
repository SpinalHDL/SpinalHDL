library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.all;

library work;
use work.pkg_scala2hdl.all;
use work.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity UartTester_tb is
end UartTester_tb;

architecture arch of UartTester_tb is
  signal io_uart_config_dataLength : unsigned(2 downto 0);
  signal io_uart_config_stop : StopType;
  signal io_uart_config_parity : ParityType;
  signal io_uart_clockDivider : unsigned(19 downto 0);
  signal io_uart_write_valid : std_logic;
  signal io_uart_write_ready : std_logic;
  signal io_uart_write_data : std_logic_vector(7 downto 0);
  signal io_uart_read_valid : std_logic;
  signal io_uart_read_data : std_logic_vector(7 downto 0);
  signal io_uart_uart_txd : std_logic;
  signal io_uart_uart_rxd : std_logic;
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
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
  
  
  process 
    procedure uartCtrlWrite(that:  std_logic_vector(7 downto 0)) is            
    begin
      io_uart_write_valid <= '1';
      io_uart_write_data <= that;
      wait until rising_edge(clk) and io_uart_write_ready = '1';
      io_uart_write_valid <= '0';
      io_uart_write_data <= (others => 'U');
    end uartCtrlWrite;

  begin
    reset <= '1';
    io_uart_config_dataLength <= "111";
    io_uart_config_stop <= eStop1bit;
    io_uart_config_parity <= eParityEven; 
    io_uart_clockDivider <= X"00063";
    
    io_uart_write_valid <= '0';
    
    wait for 10 us;
    wait until rising_edge(clk);
    reset <= '0';
    wait for 10 us;
    wait until rising_edge(clk);   
    uartCtrlWrite(X"30");
    wait for 10 us;
    wait until rising_edge(clk); 
    uartCtrlWrite(X"AA");
     wait for 150 us;   
    
    done := true;
    wait;
  end process;
  
  io_uart_uart_rxd <= io_uart_uart_txd;
  
  -- #spinalEnd userLogics
  uut : entity work.UartTester
    port map (
      io_uart_config_dataLength =>  io_uart_config_dataLength,
      io_uart_config_stop =>  io_uart_config_stop,
      io_uart_config_parity =>  io_uart_config_parity,
      io_uart_clockDivider =>  io_uart_clockDivider,
      io_uart_write_valid =>  io_uart_write_valid,
      io_uart_write_ready =>  io_uart_write_ready,
      io_uart_write_data =>  io_uart_write_data,
      io_uart_read_valid =>  io_uart_read_valid,
      io_uart_read_data =>  io_uart_read_data,
      io_uart_uart_txd =>  io_uart_uart_txd,
      io_uart_uart_rxd =>  io_uart_uart_rxd,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
