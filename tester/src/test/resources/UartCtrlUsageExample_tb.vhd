library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;

entity UartCtrlUsageExample_tb is
end UartCtrlUsageExample_tb;

architecture arch of UartCtrlUsageExample_tb is
  signal io_uart_txd : std_logic;
  signal io_uart_rxd : std_logic;
  signal io_switchs : std_logic_vector(7 downto 0);
  signal io_leds : std_logic_vector(7 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
begin
  process
  begin
    clk <= '0';
    wait for 10 ns;
    clk <= '1';
    wait for 10 ns;
  end process;

  io_uart_rxd <= io_uart_txd;

  process
  begin
    reset <= '1';
    io_switchs <= X"00";
    wait for 1 us;
    wait until rising_edge(clk);
    reset <= '0';
    while true loop
      wait for 300 us;
      io_switchs <= std_logic_vector(unsigned(io_switchs) + 1);
    end loop;
  end process;

  uut : entity work.UartCtrlUsageExample
    port map (
      io_uart_txd =>  io_uart_txd,
      io_uart_rxd =>  io_uart_rxd,
      io_switchs =>  io_switchs,
      io_leds =>  io_leds,
      clk =>  clk,
      reset =>  reset
    );
end arch;