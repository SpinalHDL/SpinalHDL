library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pck_myhdl_090 is

    attribute enum_encoding: string;

    function stdl (arg: boolean) return std_logic;

    function stdl (arg: integer) return std_logic;

    function to_unsigned (arg: boolean; size: natural) return unsigned;

    function to_signed (arg: boolean; size: natural) return signed;

    function to_integer(arg: boolean) return integer;

    function to_integer(arg: std_logic) return integer;

    function to_unsigned (arg: std_logic; size: natural) return unsigned;

    function to_signed (arg: std_logic; size: natural) return signed;

    function bool (arg: std_logic) return boolean;

    function bool (arg: unsigned) return boolean;

    function bool (arg: signed) return boolean;

    function bool (arg: integer) return boolean;

    function "-" (arg: unsigned) return signed;

end pck_myhdl_090;


package body pck_myhdl_090 is

    function stdl (arg: boolean) return std_logic is
    begin
        if arg then
            return '1';
        else
            return '0';
        end if;
    end function stdl;

    function stdl (arg: integer) return std_logic is
    begin
        if arg /= 0 then
            return '1';
        else
            return '0';
        end if;
    end function stdl;


    function to_unsigned (arg: boolean; size: natural) return unsigned is
        variable res: unsigned(size-1 downto 0) := (others => '0');
    begin
        if arg then
            res(0):= '1';
        end if;
        return res;
    end function to_unsigned;

    function to_signed (arg: boolean; size: natural) return signed is
        variable res: signed(size-1 downto 0) := (others => '0');
    begin
        if arg then
            res(0) := '1';
        end if;
        return res;
    end function to_signed;

    function to_integer(arg: boolean) return integer is
    begin
        if arg then
            return 1;
        else
            return 0;
        end if;
    end function to_integer;

    function to_integer(arg: std_logic) return integer is
    begin
        if arg = '1' then
            return 1;
        else
            return 0;
        end if;
    end function to_integer;

    function to_unsigned (arg: std_logic; size: natural) return unsigned is
        variable res: unsigned(size-1 downto 0) := (others => '0');
    begin
        res(0):= arg;
        return res;
    end function to_unsigned;

    function to_signed (arg: std_logic; size: natural) return signed is
        variable res: signed(size-1 downto 0) := (others => '0');
    begin
        res(0) := arg;
        return res;
    end function to_signed;

    function bool (arg: std_logic) return boolean is
    begin
        return arg = '1';
    end function bool;

    function bool (arg: unsigned) return boolean is
    begin
        return arg /= 0;
    end function bool;

    function bool (arg: signed) return boolean is
    begin
        return arg /= 0;
    end function bool;

    function bool (arg: integer) return boolean is
    begin
        return arg /= 0;
    end function bool;

    function "-" (arg: unsigned) return signed is
    begin
        return - signed(resize(arg, arg'length+1));
    end function "-";

end pck_myhdl_090;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use std.textio.all;

use work.pck_myhdl_090.all;

entity gray_counter is
    port (
        gray_count: out unsigned(7 downto 0);
        enable: in std_logic;
        clock: in std_logic;
        reset: in std_logic
    );
end entity gray_counter;


architecture MyHDL of gray_counter is


constant n: integer := 8;



signal even: std_logic;
signal gray: unsigned(7 downto 0);

begin





GRAY_COUNTER_SEQ: process (clock) is
    variable found: std_logic;
    variable word: unsigned(7 downto 0);
begin
    if rising_edge(clock) then
        if (reset = '1') then
            even <= '1';
            gray <= to_unsigned(0, 8);
        else
            word := resize(unsigned'(unsigned'("1") & gray((n - 2)-1 downto 0) & even), 8);
            if bool(enable) then
                found := '0';
                for i in 0 to n-1 loop
                    if ((word(i) = '1') and (not bool(found))) then
                        gray(i) <= stdl((not bool(gray(i))));
                        found := '1';
                    end if;
                end loop;
                even <= stdl((not bool(even)));
            end if;
        end if;
    end if;
end process GRAY_COUNTER_SEQ;



gray_count <= gray;

end architecture MyHDL;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library lib_GrayCounterTester;
use lib_GrayCounterTester.pkg_scala2hdl.all;
use lib_GrayCounterTester.pkg_enum.all;

-- #spinalBegin userLibrary
library IEEE;
use ieee.math_real.all;
-- #spinalEnd userLibrary


entity GrayCounterTester_tb is
end GrayCounterTester_tb;

architecture arch of GrayCounterTester_tb is
  signal enable : std_logic;
  signal gray : unsigned(7 downto 0);
  signal clk : std_logic;
  signal reset : std_logic;
  -- #spinalBegin userDeclarations
  component gray_counter is
      port (
          gray_count: out unsigned(7 downto 0);
          enable: in std_logic;
          clock: in std_logic;
          reset: in std_logic
      );
  end component;
  signal gray_ref : unsigned(7 downto 0);
  shared variable done : integer := 0;
  -- #spinalEnd userDeclarations
begin
  -- #spinalBegin userLogics
  ref : gray_counter
    port map (
      enable =>  enable,
      gray_count =>  gray_ref,
      clock =>  clk,
      reset =>  reset
    );

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
    variable seed1, seed2: positive;
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
    reset <= '1';
    enable <= '0';
    wait until rising_edge(clk);
    reset <= '0';
    wait until rising_edge(clk);

    for i in 0 to 1000 loop
      random(enable);
      assert gray = gray_ref report "Gray fail" severity failure;
      --report integer'image(to_integer(gray)) & "  " & integer'image(to_integer(gray_ref));
      wait until rising_edge(clk);
    end loop;
    done := done + 1;

  end process;
  -- #spinalEnd userLogics
  uut : entity lib_GrayCounterTester.GrayCounterTester
    port map (
      enable =>  enable,
      gray =>  gray,
      clk =>  clk,
      reset =>  reset 
    );
end arch;
