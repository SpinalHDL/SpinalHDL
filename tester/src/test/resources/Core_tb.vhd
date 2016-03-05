library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library riscv;
use riscv.pkg_scala2hdl.all;
use riscv.pkg_enum.all;

-- #spinalBegin userLibrary
-- #spinalEnd userLibrary


entity Core_tb is
end Core_tb;

architecture arch of Core_tb is
  signal io_iCmd_valid : std_logic;
  signal io_iCmd_ready : std_logic;
  signal io_iCmd_payload_address : unsigned(31 downto 0);
  signal io_iRsp_valid : std_logic;
  signal io_iRsp_ready : std_logic;
  signal io_iRsp_payload : std_logic_vector(31 downto 0);
  signal io_dCmd_valid : std_logic;
  signal io_dCmd_ready : std_logic;
  signal io_dCmd_payload_wr : std_logic;
  signal io_dCmd_payload_address : unsigned(31 downto 0);
  signal io_dCmd_payload_data : std_logic_vector(31 downto 0);
  signal io_dCmd_payload_size : unsigned(1 downto 0);
  signal io_dRsp_valid : std_logic;
  signal io_dRsp_payload : std_logic_vector(31 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  shared variable done : integer := 0;
  constant memSize : integer := 1024*128;
  type memType is array (0 to memSize-1) of std_logic_vector(7 downto 0);
  shared variable mem : memType;
  
  
  
  function Load_Data return memType is
    use std.textio.all;
    use ieee.std_logic_textio.all;
    file ROMFILE: TEXT open READ_MODE is "E:/vm/share/a.hex";
    variable newline: line;
    variable newchar: character;
    variable newbyte: std_logic_vector(7 downto 0);
    variable newword: std_logic_vector(15 downto 0);
    variable offset : integer;
    variable NextAddr, ByteCount: integer;
    variable NewROM: memType := (others => (others => '0'));
    variable valid: boolean := True;
  begin
    while (valid) loop
        readline(ROMFILE, newline);
        read(newline,newchar,valid);                      --ERROR HERE!!!
        if (newchar = ':') and (valid = True) then
          hread(newline,newbyte);
          ByteCount := to_integer(unsigned(newbyte));
          hread(newline,newword);
          NextAddr := to_integer(unsigned(newword)) + offset;
          hread(newline,newbyte);
          case(newbyte) is
            when X"00" => 
              for i in 1 to ByteCount loop
                  hread(newline,newbyte);
                  NewROM(NextAddr) := newbyte;
                  NextAddr := NextAddr + 1;
              end loop;        

            when X"02" =>   
              hread(newline,newword);
              offset := to_integer(unsigned(newword)) * (16);
            when others => 
              valid := False;
          end case;
        end if;
    end loop;

    file_close(ROMFILE);
    return NewROM;
  end;
  
  
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
    assert now < 20 ms report "timeout" severity failure;
    clk <= '1';
    wait for 5 ns;
  end process;


  process
  begin
    reset <= '1';

    wait for 100 ns;
    wait until rising_edge(clk);
    reset <= '0';
    
    wait for 200 ns;
    --done := done + 1;
    wait;
  end process;

  io_iCmd_ready <= not io_iRsp_valid or io_iRsp_ready;
  io_dCmd_ready <= '1';
  process(clk,reset)
  begin
    if reset = '1' then
      io_iRsp_valid <= '0';
      io_dRsp_valid <= '0';
      mem := Load_Data;
    elsif rising_edge(clk) then
      if io_iRsp_ready = '1' then
        io_iRsp_valid <= '0';
        io_iRsp_payload <= (others => 'X');
        if io_iCmd_valid = '1' then
          io_iRsp_valid <= '1';
          for i in 0 to 3 loop
            io_iRsp_payload(i*8+7 downto i*8) <= mem(to_integer(unsigned(io_iCmd_payload_address)) + i);
          end loop;
        end if;
      end if;
      
      io_dRsp_valid <= '0';
      io_dRsp_payload <= (others => 'X');
      if io_dCmd_valid = '1' then
        if io_dCmd_payload_wr = '1' then
          for i in 0 to (2 ** to_integer(io_dCmd_payload_size))-1 loop
            mem(to_integer(unsigned(io_dCmd_payload_address)) + i) := io_dCmd_payload_data(i*8+7 downto i*8);
          end loop;
        else
          io_dRsp_valid <= '1';
          for i in 0 to 3 loop
          io_dRsp_payload(i*8+7 downto i*8) <= mem(to_integer(unsigned(io_dCmd_payload_address)) + i);
          end loop;        
        end if;
      end if;
    end if;
  end process;

  -- #spinalEnd userLogics
  uut : entity riscv.Core
    port map (
      io_iCmd_valid =>  io_iCmd_valid,
      io_iCmd_ready =>  io_iCmd_ready,
      io_iCmd_payload_address =>  io_iCmd_payload_address,
      io_iRsp_valid =>  io_iRsp_valid,
      io_iRsp_ready =>  io_iRsp_ready,
      io_iRsp_payload =>  io_iRsp_payload,
      io_dCmd_valid =>  io_dCmd_valid,
      io_dCmd_ready =>  io_dCmd_ready,
      io_dCmd_payload_wr =>  io_dCmd_payload_wr,
      io_dCmd_payload_address =>  io_dCmd_payload_address,
      io_dCmd_payload_data =>  io_dCmd_payload_data,
      io_dCmd_payload_size =>  io_dCmd_payload_size,
      io_dRsp_valid =>  io_dRsp_valid,
      io_dRsp_payload =>  io_dRsp_payload,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
