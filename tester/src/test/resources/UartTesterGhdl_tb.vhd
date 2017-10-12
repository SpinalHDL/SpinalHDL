library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_UartTesterGhdl;
use lib_UartTesterGhdl.pkg_scala2hdl.all;
use lib_UartTesterGhdl.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity UartTesterGhdl_tb is
end UartTesterGhdl_tb;

architecture arch of UartTesterGhdl_tb is
  signal io_uart_config_frame_dataLength : unsigned(2 downto 0);
  signal io_uart_config_frame_stop : UartStopType_binary_sequancial_type;
  signal io_uart_config_frame_parity : UartParityType_binary_sequancial_type;
  signal io_uart_config_clockDivider : unsigned(19 downto 0);
  signal io_uart_write_valid : std_logic;
  signal io_uart_write_ready : std_logic;
  signal io_uart_write_payload : std_logic_vector(7 downto 0);
  signal io_uart_read_valid : std_logic;
  signal io_uart_read_payload : std_logic_vector(7 downto 0);
  signal io_uart_uart_txd : std_logic;
  signal io_uart_uart_rxd : std_logic;
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  constant clockDivider : integer := 3;
  constant baudPeriod : time := (10 ns * ((clockDivider+1)*8)) ;
  signal asyncProcess : std_logic := '0';
  shared variable done : integer := 0;
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  process
  begin
    clk <= '0';
    wait for 5 ns;
    if done = 2 then
      wait;
    end if;
    assert now < 10 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;
  
  

  process 
    procedure uartCtrlWrite(that:  std_logic_vector(7 downto 0)) is            
    begin
      io_uart_write_valid <= '1';
      io_uart_write_payload <= that;
      wait until rising_edge(clk) and io_uart_write_ready = '1';
      io_uart_write_valid <= '0';
      io_uart_write_payload <= (others => 'U');
    end uartCtrlWrite;

  begin
    reset <= '1';
    io_uart_config_frame_dataLength <= "111";
    io_uart_config_frame_stop <= UartStopType_binary_sequancial_ONE;
    io_uart_config_frame_parity <= UartParityType_binary_sequancial_EVEN;
    io_uart_config_clockDivider <= to_unsigned(clockDivider,20);

    io_uart_write_valid <= '0';
    
    wait for 10 us;
    wait until rising_edge(clk);
    reset <= '0';
    wait for 10 us;
    wait until rising_edge(clk);   
    uartCtrlWrite(X"30");
    wait for 10 us;
    wait until rising_edge(clk); 
    uartCtrlWrite(X"AB");
    wait for 10 us;
    for i in 0 to 255 loop
      uartCtrlWrite(std_logic_vector(to_unsigned(i,8)));
    end loop;

    wait for 50 us;

    reset <= '1';
    io_uart_config_frame_dataLength <= "111";
    io_uart_config_frame_stop <= UartStopType_binary_sequancial_ONE;
    io_uart_config_frame_parity <= UartParityType_binary_sequancial_NONE;
    io_uart_config_clockDivider <= to_unsigned(clockDivider,20);

    io_uart_write_valid <= '0';

    wait for 10 us;
    wait until rising_edge(clk);
    reset <= '0';
    wait for 10 us;
    wait until rising_edge(clk);
    uartCtrlWrite(X"30");
    wait for 10 us;
    wait until rising_edge(clk);
    uartCtrlWrite(X"AB");
    wait for 10 us;
    for i in 0 to 255 loop
      uartCtrlWrite(std_logic_vector(to_unsigned(i,8)));
    end loop;


    wait;
  end process;


  process
    procedure checkBit(value:  std_logic) is
    begin
        wait for baudPeriod;
        assert io_uart_uart_txd'DELAYED'LAST_ACTIVE >= baudPeriod and io_uart_uart_txd'DELAYED = value report "io_uart_uart_txd fail" severity failure;
    end checkBit;

    procedure checkTx(that:  std_logic_vector(7 downto 0)) is
      variable parity : std_logic;
      variable stopTime : time;
    begin
        if io_uart_config_frame_parity = UartParityType_binary_sequancial_EVEN then
          parity := '0';
        else
          parity := '1';
        end if;
        
        if io_uart_config_frame_stop = UartStopType_binary_sequancial_ONE then
          stopTime := baudPeriod;
        else
          stopTime := 2 * baudPeriod;
        end if;
        
        if io_uart_uart_txd = '1' then
          wait until io_uart_uart_txd'event;
        end if;
        
        assert io_uart_uart_txd'DELAYED'LAST_ACTIVE >= stopTime  and io_uart_uart_txd'DELAYED(1 ns) = '1' report "io_uart_uart_txd fail" severity failure;
        
        checkBit('0');
        
        for i in 0 to that'high loop
          checkBit(that(i));
          parity := parity xor that(i);
        end loop;

        if io_uart_config_frame_parity /= UartParityType_binary_sequancial_NONE then
          checkBit(parity);
        end if;
        
        checkBit('1');
        if io_uart_config_frame_stop = UartStopType_binary_sequancial_TWO then
         checkBit('1');       
        end if;
    end checkTx;
  begin
    wait for 10 ns;
    checkTx(X"30");
    checkTx(X"AB");
    for i in 0 to 255 loop
      checkTx(std_logic_vector(to_unsigned(i,8)));
    end loop;

    checkTx(X"30");
    checkTx(X"AB");
    for i in 0 to 255 loop
      checkTx(std_logic_vector(to_unsigned(i,8)));
    end loop;
    done := done + 1;
    wait;
  end process;
  
  
  process
    procedure checkRx(that:  std_logic_vector(7 downto 0)) is
    begin
      wait until rising_edge(clk) and io_uart_read_valid = '1';
      assert io_uart_read_payload = that report "io_uart_read_payload fail" severity failure;
    end checkRx;
  begin
    wait until rising_edge(clk) and reset = '0';
    checkRx(X"30");
    checkRx(X"AB");
    for i in 0 to 255 loop
      checkRx(std_logic_vector(to_unsigned(i,8)));
    end loop;

    checkRx(X"30");
    checkRx(X"AB");
    for i in 0 to 255 loop
      checkRx(std_logic_vector(to_unsigned(i,8)));
    end loop;
    done := done + 1;
    wait;
  end process;
  
  
  
  io_uart_uart_rxd <= io_uart_uart_txd;
  
  -- #spinalEnd userLogics
  uut : entity lib_UartTesterGhdl.UartTesterGhdl
    port map (
      io_uart_config_frame_dataLength =>  io_uart_config_frame_dataLength,
      io_uart_config_frame_stop =>  io_uart_config_frame_stop,
      io_uart_config_frame_parity =>  io_uart_config_frame_parity,
      io_uart_config_clockDivider =>  io_uart_config_clockDivider,
      io_uart_write_valid =>  io_uart_write_valid,
      io_uart_write_ready =>  io_uart_write_ready,
      io_uart_write_payload =>  io_uart_write_payload,
      io_uart_read_valid =>  io_uart_read_valid,
      io_uart_read_payload =>  io_uart_read_payload,
      io_uart_uart_txd =>  io_uart_uart_txd,
      io_uart_uart_rxd =>  io_uart_uart_rxd,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
