
-- ===========Oooo==========================================Oooo========
-- =  Copyright (C) 2014-2020 Gowin Semiconductor Technology Co.,Ltd.
-- =                     All rights reserved.
-- =====================================================================
--
--  __      __      __
--  \ \    /  \    / /   [File name   ] prim_syn.vhd
--   \ \  / /\ \  / /    [Description ] GW1N-4S VHDL functional synthesis library
--    \ \/ /  \ \/ /     [Timestamp   ] Tue November 5 11:00:30 2018
--     \  /    \  /      [version     ] 1.5
--      \/      \/         
--
-- ===========Oooo==========================================Oooo========


---------------------------package global------------------------------

LIBRARY ieee; 
USE ieee.std_logic_1164.all; 
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

PACKAGE components IS 
   attribute syn_black_box: boolean ;
   attribute syn_black_box of Components : package is true;
   attribute black_box_pad_pin: string;
   attribute syn_noprune : boolean;
   attribute xc_map: string;
   attribute xc_map of Components : package is "lut"; 	
--package glb is
--	signal GSRN : std_logic := '1';
--end glb;
--package body glb 
--end glb;
------------------------------GSR---------------------------------------

COMPONENT GSR 
    PORT (
         GSRI : in std_logic
    );
end COMPONENT;

	attribute syn_black_box of GSR : Component is true;
    attribute syn_noprune of GSR : Component is true;
--------------------------LUT1------------------------------
COMPONENT LUT1 
    GENERIC ( INIT : bit_vector := X"0" );
    PORT (
	    F : out std_logic;
        I0 : in std_logic
    );
end COMPONENT;

attribute syn_black_box of LUT1 : Component is true;
attribute xc_map of LUT1 : component is "lut";
	
--------------------------LUT2 -----------------------------
COMPONENT LUT2 
    GENERIC ( INIT : bit_vector := X"0" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic
    );
end COMPONENT;

	attribute syn_black_box of LUT2 : Component is true;
	attribute xc_map of LUT2 : component is "lut";
--------------------------LUT3------------------------------
COMPONENT LUT3 
    GENERIC ( INIT : bit_vector := X"00" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT3 : Component is true;
	attribute xc_map of LUT3 : component is "lut";
--------------------------LUT4 -----------------------------
COMPONENT LUT4 
    GENERIC ( INIT : bit_vector := X"0000" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic;
   	    I3 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT4 : Component is true;
	attribute xc_map of LUT4 : component is "lut";
--------------------------LUT5 -----------------------------
COMPONENT LUT5 
    GENERIC ( INIT : bit_vector := X"00000000" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic;
   	    I3 : in std_logic;
   	    I4 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT5 : Component is true;
	attribute xc_map of LUT5 : component is "lut";
--------------------------LUT6 -----------------------------

COMPONENT LUT6 
    GENERIC ( INIT : bit_vector := X"0000000000000000" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic;
   	    I3 : in std_logic;
   	    I4 : in std_logic;
   	    I5 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT6 : Component is true;
	attribute xc_map of LUT6 : component is "lut";
--------------------------LUT7 -----------------------------

COMPONENT LUT7 
    GENERIC ( INIT : bit_vector := X"00000000000000000000000000000000" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic;
   	    I3 : in std_logic;
   	    I4 : in std_logic;
    	I5 : in std_logic;
   	    I6 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT7 : Component is true;
	attribute xc_map of LUT7 : component is "lut";
--------------------------LUT8 -----------------------------

COMPONENT LUT8 
    GENERIC ( INIT : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000" );
    PORT (
   	    F : out std_logic;
   	    I0 : in std_logic;
   	    I1 : in std_logic;
   	    I2 : in std_logic;
   	    I3 : in std_logic;
   	    I4 : in std_logic;
   	    I5 : in std_logic;
   	    I6 : in std_logic;
   	    I7 : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of LUT8 : Component is true;
	attribute xc_map of LUT8 : component is "lut";
--------------------------MUX2------------------------------

COMPONENT MUX2 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2 : Component is true;
--------------------------MUX2_LUT5------------------------------

COMPONENT MUX2_LUT5 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_LUT5 : Component is true;
--------------------------MUX2_LUT6------------------------------

COMPONENT MUX2_LUT6 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_LUT6 : Component is true;
--------------------------MUX2_LUT7------------------------------

COMPONENT MUX2_LUT7 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_LUT7 : Component is true;
--------------------------MUX2_LUT8------------------------------

COMPONENT MUX2_LUT8 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_LUT8 : Component is true;
--------------------------MUX2_MUX8------------------------------

COMPONENT MUX2_MUX8 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_MUX8 : Component is true;
--------------------------MUX2_MUX16------------------------------

COMPONENT MUX2_MUX16 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_MUX16 : Component is true;
--------------------------MUX2_MUX32------------------------------

COMPONENT MUX2_MUX32 
    PORT (
	    I0 : in std_logic;
	    I1 : in std_logic;
	    S0 : in std_logic;
	    O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX2_MUX32 : Component is true;
--------------------------MUX4------------------------------

COMPONENT MUX4 
    PORT (
	 I0 : in std_logic;
	 I1 : in std_logic;
	 I2 : in std_logic; 
	 I3 : in std_logic;
	 S0 : in std_logic;
	 S1 : in std_logic;
	 O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX4 : Component is true;
--------------------------MUX8------------------------------

COMPONENT MUX8 
    PORT (
	 I0 : in std_logic;
	 I1 : in std_logic;
	 I2 : in std_logic; 
	 I3 : in std_logic;
	 I4 : in std_logic;
	 I5 : in std_logic;
	 I6 : in std_logic;
	 I7 : in std_logic;
	 S0 : in std_logic;
	 S1 : in std_logic;
	 S2 : in std_logic;
	 O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX8 : Component is true;
--------------------------MUX16-----------------------------

COMPONENT MUX16  
    PORT (
	 I0 : in std_logic;
	 I1 : in std_logic;
	 I2 : in std_logic; 
	 I3 : in std_logic;
	 I4 : in std_logic;
	 I5 : in std_logic;
	 I6 : in std_logic;
	 I7 : in std_logic;
	 I8 : in std_logic;
	 I9 : in std_logic;
	 I10 : in std_logic;
	 I11 : in std_logic;
	 I12 : in std_logic;
	 I13 : in std_logic;
	 I14 : in std_logic;
	 I15 : in std_logic;
	 S0 : in std_logic;
	 S1 : in std_logic;
	 S2 : in std_logic;
	 S3 : in std_logic;
	 O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX16 : Component is true;
--------------------------MUX32-----------------------------

COMPONENT MUX32  
    PORT (
	 I0 : in std_logic;
	 I1 : in std_logic;
	 I2 : in std_logic; 
	 I3 : in std_logic;
	 I4 : in std_logic;
	 I5 : in std_logic;
	 I6 : in std_logic;
	 I7 : in std_logic;
	 I8 : in std_logic;
	 I9 : in std_logic;
	 I10 : in std_logic;
	 I11 : in std_logic;
	 I12 : in std_logic;
	 I13 : in std_logic;
	 I14 : in std_logic;
	 I15 : in std_logic;
	 I16 : in std_logic;
	 I17 : in std_logic;
	 I18 : in std_logic;
	 I19 : in std_logic;
	 I20 : in std_logic;
	 I21 : in std_logic;
	 I22 : in std_logic;
	 I23:  in std_logic;
	 I24 : in std_logic;
	 I25 : in std_logic;
	 I26 : in std_logic;
	 I27 : in std_logic;
	 I28 : in std_logic;
	 I29 : in std_logic;
	 I30 : in std_logic;
	 I31 : in std_logic; 
	 S0 : in std_logic;
	 S1 : in std_logic;
	 S2 : in std_logic;
	 S3 : in std_logic;
	 S4 : in std_logic;
	 O : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of MUX32 : Component is true;
--------------------------ALU-------------------------------

COMPONENT ALU 
    GENERIC (  
         ADD : INTEGER := 0 ;
       	 SUB : INTEGER := 1 ;
         ADDSUB : INTEGER := 2 ;
       	 NE : INTEGER := 3 ;
       	 GE : INTEGER := 4 ;
	     LE : INTEGER := 5;
       	 CUP : INTEGER := 6 ;
         CDN : INTEGER := 7 ;
         CUPCDN : INTEGER := 8;
	     MULT : INTEGER := 9;
	     ALU_MODE : INTEGER := 0
    );	
    PORT (
	 SUM : OUT std_logic;
	 COUT : OUT std_logic;	
	 I0 : IN std_logic;
	 I1: IN std_logic;
	 I3: IN std_logic;
	 CIN: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of ALU : Component is true;
----------------------------DFF ------------------------------

COMPONENT DFF 
    GENERIC ( INIT : bit := '0');	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFF : Component is true;
---------------------------DFFE ---------------------------------

COMPONENT DFFE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CE : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFE : Component is true;
-------------------------DFFS ---------------------------------

COMPONENT DFFS 
    GENERIC ( INIT: bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 SET : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFS : Component is true;
----------------------------DFFSE--------------------------------------

COMPONENT DFFSE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 SET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFSE : Component is true;
------------------------DFFR ----------------------------------

COMPONENT DFFR 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 RESET : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFR : Component is true;
---------------------------DFFRE -------------------------------------

COMPONENT DFFRE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 RESET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFRE : Component is true;
----------------------------DFFP---------------------------------------

COMPONENT DFFP 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFP : Component is true;
--------------------------DFFPE ---------------------------------------------

COMPONENT DFFPE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFPE : Component is true;
-----------------------------DFFC --------------------------------

COMPONENT DFFC 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFC : Component is true;
-----------------------------DFFCE -------------------------------------------

COMPONENT DFFCE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFCE : Component is true;
--------------------------DFFN ------------------------------

COMPONENT DFFN 
    GENERIC ( INIT : bit := '0');	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFN : Component is true;
------------------DFFNE ---------------------------------

COMPONENT DFFNE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CE : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNE : Component is true;
-------------------------DFFNS ---------------------------------

COMPONENT DFFNS 
    GENERIC ( INIT: bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 SET : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNS : Component is true;
----------------------------DFFNSE--------------------------------------

COMPONENT DFFNSE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 SET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNSE : Component is true;
-----------------------------DFFNR----------------------------------

COMPONENT DFFNR 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 RESET : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNR : Component is true;
---------------------------DFFNRE -------------------------------------

COMPONENT DFFNRE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 RESET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNRE : Component is true;
----------------------------DFFNP---------------------------------------

COMPONENT DFFNP 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNP : Component is true;
--------------------------DFFNPE ---------------------------------------------

COMPONENT DFFNPE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNPE : Component is true;
-----------------------------DFFNC --------------------------------------------

COMPONENT DFFNC 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNC : Component is true;
-----------------------------DFFNCE -------------------------------------------

COMPONENT DFFNCE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;
	 CE: IN std_logic;	
	 CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DFFNCE : Component is true;
--------------------------------DL ------------------------------------------

COMPONENT DL 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DL : Component is true;
-------------------------DLE-----------------------------------

COMPONENT DLE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CE: IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLE : Component is true;
-----------------------------DLC ------------------------------------

COMPONENT DLC 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLC : Component is true;
-----------------------------DLCE ------------------------------------

COMPONENT DLCE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 G : IN std_logic;
	 CE: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLCE : Component is true;
-----------------------------DLP ------------------------------------

COMPONENT DLP 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : std_logic;
	 PRESET : IN std_logic;	
	 G: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLP : Component is true;
-----------------------------DLPE ------------------------------------

COMPONENT DLPE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET : IN std_logic;	
	 G : IN std_logic;
	 CE: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLPE : Component is true;
------------------------DLN ------------------------------------------

COMPONENT DLN 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLN : Component is true;
-----------------------------DLNE-----------------------------------

COMPONENT DLNE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CE: IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLNE : Component is true;
-----------------------------DLNC ------------------------------------

COMPONENT DLNC 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 G : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLNC : Component is true;
-----------------------------DLNCE ------------------------------------

COMPONENT DLNCE 
    GENERIC ( INIT : bit := '0' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR : IN std_logic;	
	 G : IN std_logic;
	 CE: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLNCE : Component is true;
-----------------------------DLNP ------------------------------------

COMPONENT DLNP 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : std_logic;
	 PRESET : IN std_logic;	
	 G: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLNP : Component is true;
-----------------------------DLNPE ------------------------------------

COMPONENT DLNPE 
    GENERIC ( INIT : bit := '1' );	
    PORT (
	 Q : OUT std_logic;	
	 D : IN std_logic;
	 PRESET : IN std_logic;	
	 G : IN std_logic;
	 CE: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DLNPE : Component is true;
----------------------IBUF-------------------------------------

COMPONENT IBUF 
    PORT (
    	 O : OUT std_logic;
    	 I : IN std_logic
    );
end COMPONENT;
	attribute syn_black_box of IBUF : Component is true;
------------------------------OBUF---------------------------------------

COMPONENT OBUF 
    PORT (
    	 O : OUT std_logic;
    	 I : IN std_logic
    );
end COMPONENT;
	attribute syn_black_box of OBUF : Component is true;
------------------------------------TBUF---------------------------

COMPONENT TBUF 
    PORT (
    	O : OUT std_logic;
    	I : IN std_logic;
    	OEN : IN std_logic
    );
end COMPONENT;
	attribute syn_black_box of TBUF : Component is true;
    attribute black_box_pad_pin of TBUF : Component is "O";

----------------------------IOBUF--------------------------------

COMPONENT IOBUF 
    PORT (
    	O  : OUT   std_logic;
    	IO : INOUT std_logic;
     	I  : IN    std_logic;
	    OEN : IN    std_logic
    );
end COMPONENT;
	attribute syn_black_box of IOBUF : Component is true;
---------------------------------IDDR--------------------------

COMPONENT IDDR 
    GENERIC (
	Q0_INIT : bit := '0';
	Q1_INIT : bit := '0'
    );	
    PORT (
	 Q0 : OUT std_logic;
	 Q1 : OUT std_logic;	
	 D : IN std_logic;
	 CLK: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of IDDR : Component is true;
---------------------------------IDDRC---------------------------

COMPONENT IDDRC 
    GENERIC ( 
	Q0_INIT : bit := '0';
	Q1_INIT : bit := '0'
    );	
    PORT (
	 Q0 : OUT std_logic;
	 Q1 : OUT std_logic;	
	 D : IN std_logic;
	 CLEAR: IN std_logic;	
	 CLK: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of IDDRC : Component is true;

-------------------------------ODDR----------------------

COMPONENT ODDR 
    GENERIC ( 
        TXCLK_POL : bit := '0'; --'0':Rising edge output; '1':Falling edge output        
        CONSTANT INIT : std_logic := '0' 
    );	
    PORT ( 
	    Q0 : OUT std_logic;	
	    Q1 : OUT std_logic;	
	    D0 : IN std_logic;
	    D1 : IN std_logic;
	    TX : IN std_logic;
	    CLK : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of ODDR : Component is true;

-------------------------------ODDRC----------------------
COMPONENT ODDRC 
    GENERIC ( 
        TXCLK_POL : bit := '0'; --'0':Rising edge output; '1':Falling edge output        
        CONSTANT INIT : std_logic := '0' 
    );	
    PORT (
	 Q0 : OUT std_logic;	
	 Q1 : OUT std_logic;	
	 D0 : IN std_logic;
	 D1 : IN std_logic;	
	 TX : IN std_logic;	
	 CLK : IN std_logic;
	 CLEAR: IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of ODDRC : Component is true;

-----------------------------------------IDES4-------------------------------

COMPONENT IDES4 
	GENERIC (
		GSREN : string := "false";
		LSREN : string := "true"
	);
	PORT (
		D : IN std_logic;
		CALIB : IN std_logic;
		RESET : IN std_logic;
		FCLK : IN std_logic;
		PCLK : IN std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic;
		Q2 : OUT std_logic;
		Q3 : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of IDES4 : Component is true;

------------------------------------IVIDEO-------------------------------

COMPONENT IVIDEO 
    GENERIC (
		GSREN : string := "false";
		LSREN : string := "true"
    );
    PORT (
		D : IN std_logic;
		RESET : IN std_logic;
		CALIB : IN std_logic;
		FCLK : IN std_logic;
		PCLK : IN std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic;
		Q2 : OUT std_logic;
		Q3 : OUT std_logic;
		Q4 : OUT std_logic;
		Q5 : OUT std_logic;
		Q6 : OUT std_logic
    );
end COMPONENT;
	attribute syn_black_box of IVIDEO : Component is true;
-----------------------------------IDES8-------------------------------------

COMPONENT IDES8 
    GENERIC (
	    GSREN : string := "false";
		LSREN : string := "true"
    );
    PORT (
		D,RESET : IN std_logic;
		CALIB : IN std_logic;
		FCLK,PCLK : IN std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic;
		Q2 : OUT std_logic;
		Q3 : OUT std_logic;
		Q4 : OUT std_logic;
		Q5 : OUT std_logic;
		Q6 : OUT std_logic;
		Q7 : OUT std_logic
    );
end COMPONENT;
	attribute syn_black_box of IDES8 : Component is true;

--------------------------------------IDES10----------------------------------

COMPONENT IDES10 
    GENERIC (
	    GSREN : string := "false";
		LSREN : string := "true"
    );
    PORT (
		D,RESET : IN std_logic;
		CALIB : IN std_logic;
		FCLK,PCLK : IN std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic;
		Q2 : OUT std_logic;
		Q3 : OUT std_logic;
		Q4 : OUT std_logic;
		Q5 : OUT std_logic;
		Q6 : OUT std_logic;
		Q7 : OUT std_logic;
		Q8 : OUT std_logic;
		Q9 : OUT std_logic
    );
end COMPONENT;
	attribute syn_black_box of IDES10 : Component is true;

-----------------------------------IDES16-------------------------------------
COMPONENT IDES16
    GENERIC (
	    GSREN : string := "false";
		LSREN : string := "true"
    );
    PORT (
		D,RESET : IN std_logic;
		CALIB : IN std_logic;
		FCLK,PCLK : IN std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic;
		Q2 : OUT std_logic;
		Q3 : OUT std_logic;
		Q4 : OUT std_logic;
		Q5 : OUT std_logic;
		Q6 : OUT std_logic;
		Q7 : OUT std_logic;
        Q8 : OUT std_logic;
		Q9 : OUT std_logic;
		Q10 : OUT std_logic;
		Q11 : OUT std_logic;
		Q12 : OUT std_logic;
		Q13 : OUT std_logic;
		Q14 : OUT std_logic;
		Q15 : OUT std_logic
    );
end COMPONENT;
	attribute syn_black_box of IDES16 : Component is true;


-------------------------OSER4------------------------------

COMPONENT OSER4 
	GENERIC (
		GSREN : string := "false";
		LSREN : string := "true";
        HWL : string := "false"; --"true"; "false"
        TXCLK_POL : bit := '0' --'0':Rising edge output; '1':Falling edge output
	);
	PORT (
		D0 : in std_logic;
		D1 : in std_logic;
		D2 : in std_logic;
		D3 : in std_logic;
		TX0 : in std_logic;
		TX1 : in std_logic;
		PCLK : in std_logic;
		RESET : in std_logic;
		FCLK : in std_logic;
		Q0 : OUT std_logic;
		Q1 : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of OSER4 : Component is true;

--------------------OVIDEO----------------------------------

COMPONENT OVIDEO 
	GENERIC(
		GSREN : string := "false";
		LSREN : string := "true"
	);
	PORT (
		D0 : in std_logic;
		D1 : in std_logic;
		D2 : in std_logic;
		D3 : in std_logic;
		D4 : in std_logic;
		D5 : in std_logic;
		D6 : in std_logic;
		PCLK : in std_logic;
		RESET : in std_logic;
		FCLK : in std_logic;
		Q : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of OVIDEO : Component is true;
--------------------OSER8-----------------------------------

COMPONENT OSER8 
    GENERIC (
    	GSREN : string := "false";
    	LSREN : string := "true";
        HWL : string := "false";
        TXCLK_POL : bit := '0' --'0':Rising edge output; '1':Falling edge output
    );
    PORT (
       	D0 : in std_logic;
       	D1 : in std_logic;
       	D2 : in std_logic;
       	D3 : in std_logic;
       	D4 : in std_logic;
      	D5 : in std_logic;
       	D6 : in std_logic;
       	D7 : in std_logic;
       	TX0 : in std_logic;
    	TX1 : in std_logic;
	    TX2 : in std_logic;
	    TX3 : in std_logic;
	    PCLK : in std_logic;
	    RESET : in std_logic;
	    FCLK : in std_logic;
       	Q0 : OUT std_logic;
       	Q1 : OUT std_logic
    );
end COMPONENT;
	attribute syn_black_box of OSER8 : Component is true;

--------------------OSER10-----------------------------------
COMPONENT OSER10 
	GENERIC (
		GSREN : string := "false";
		LSREN : string := "true"
	);
	PORT (
		D0 : in std_logic;
		D1 : in std_logic;
		D2 : in std_logic;
		D3 : in std_logic;
		D4 : in std_logic;
		D5 : in std_logic;
		D6 : in std_logic;
		D7 : in std_logic;
		D8 : in std_logic;
		D9 : in std_logic;
		PCLK : in std_logic;
		RESET : in std_logic;
		FCLK : in std_logic;
		Q : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of OSER10 : Component is true;


--------------------OSER16-----------------------------------
COMPONENT OSER16 
	GENERIC (
		GSREN : string := "false";
		LSREN : string := "true"
	);
	PORT (
		D0 : in std_logic;
		D1 : in std_logic;
		D2 : in std_logic;
		D3 : in std_logic;
		D4 : in std_logic;
		D5 : in std_logic;
		D6 : in std_logic;
		D7 : in std_logic;
		D8 : in std_logic;
		D9 : in std_logic;
        D10 : in std_logic;
		D11 : in std_logic;
		D12 : in std_logic;
		D13 : in std_logic;
		D14 : in std_logic;
		D15 : in std_logic;
		PCLK : in std_logic;
		RESET : in std_logic;
		FCLK : in std_logic;
		Q : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of OSER16 : Component is true;


--------------------IODELAY-----------------------------------

COMPONENT IODELAY 
	GENERIC (  C_STATIC_DLY : integer := 0); -- 0~127
	PORT (
		DI : IN std_logic;
		SDTAP : IN std_logic;
		SETN : IN std_logic;
		VALUE : IN std_logic;
		DO : OUT std_logic;
		DF : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of IODELAY : Component is true;
--------------------IEM----------------------------------

COMPONENT IEM 
	GENERIC(
		WINSIZE : string := "SMALL";
		GSREN : string := "false";
		LSREN : string := "true"
	);
	PORT (
		D : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		MCLK: in std_logic;
		LAG : out std_logic;
		LEAD : out std_logic
	);
end COMPONENT;
	attribute syn_black_box of IEM : Component is true;


-----------------------ROM----------------------------

COMPONENT ROM 
    GENERIC ( 
	    BIT_WIDTH : integer :=1;	
	    READ_MODE : bit := '0';
        BLK_SEL : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"        
    );
    PORT (
	    DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	    CLK, CE,OCE,RESET,WRE : in std_logic;
    	BLKSEL : in std_logic_vector(2 downto 0);
	    AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of ROM : Component is true;
------------------------------ROMX9 ---------------------------------------------

COMPONENT ROMX9 
    GENERIC ( 
        BIT_WIDTH : integer :=9;
        READ_MODE : bit :='0';
        BLK_SEL : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
        INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"     

    );
    PORT (
	 DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	 CLK, CE,OCE,RESET,WRE : in std_logic;
     BLKSEL : in std_logic_vector(2 downto 0);
	 AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of ROMX9 : Component is true;

-------------------SP---------------------------------------
COMPONENT SP 
    GENERIC (
	 BIT_WIDTH : integer :=32; -- 1, 2, 4, 8, 16, 32
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 WRITE_MODE : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
     BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"    
    );
    PORT (
	 DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	 CLK, CE,OCE,RESET,WRE : in std_logic;
	 AD : in std_logic_vector(13 downto 0);
    	 BLKSEL : in std_logic_vector(2 downto 0);
	 DI : in std_logic_vector(31 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of SP : Component is true;

----------------------------SPX9---------------------------------------
COMPONENT SPX9 
    GENERIC ( 
	 BIT_WIDTH : integer :=9;
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 WRITE_MODE : bit_vector :="00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
	 BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"    
    );
    PORT (
	 DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	 CLK, CE,OCE,RESET,WRE : in std_logic;
	 AD : in std_logic_vector(13 downto 0);
	 DI : in std_logic_vector(35 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0)

    );
end COMPONENT;
	attribute syn_black_box of SPX9 : Component is true;
----------------------------------SDP---------------------------------------

COMPONENT SDP 
    GENERIC ( 
	 BIT_WIDTH_0 : integer :=16; -- 1, 2, 4, 8, 16, 32
	 BIT_WIDTH_1 : integer :=16; -- 1, 2, 4, 8, 16, 32
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
     BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"    
    );
    PORT (
	 DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	 CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB,WREA,WREB : in std_logic;
	 ADA,ADB : in std_logic_vector(13 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0);
	 DI : in std_logic_vector(31 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of SDP : Component is true;

----------------------------------SDPX9---------------------------------------
COMPONENT SDPX9 
    GENERIC ( 
	 BIT_WIDTH_0 : integer :=18; -- 9, 18, 36
	 BIT_WIDTH_1 : integer :=18; -- 9, 18, 36
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"      
    );
    PORT (
	 DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	 CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB,WREA,WREB : in std_logic;
	 ADA,ADB : in std_logic_vector(13 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0);
	 DI : in std_logic_vector(35 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of SDPX9 : Component is true;

----------------------------DP---------------------------------------

COMPONENT DP 
    GENERIC (
		 BIT_WIDTH_0 : integer :=16; 
		 BIT_WIDTH_1 : integer :=16; 
		 READ_MODE0 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
		 READ_MODE1 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
		 WRITE_MODE0 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
		 WRITE_MODE1 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
    	 BLK_SEL : bit_vector := "000";
         RESET_MODE : string := "SYNC"; --SYNC, ASYNC
		 INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"
    );
    PORT (
		 DOA,DOB : out std_logic_vector(15 downto 0):=conv_std_logic_vector(0,16);
		 CLKA,CLKB, CEA,CEB,OCEA,OCEB,RESETA,RESETB,WREA,WREB : in std_logic;
		 ADA,ADB : in std_logic_vector(13 downto 0);
    	 BLKSEL : in std_logic_vector(2 downto 0);
		 DIA,DIB : in std_logic_vector(15 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of DP : Component is true;

----------------------------DPX9---------------------------------------
COMPONENT DPX9 
    GENERIC ( 
	 BIT_WIDTH_0 : integer :=18; -- 9, 18
	 BIT_WIDTH_1 : integer :=18; -- 9, 18
	 READ_MODE0 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 READ_MODE1 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 WRITE_MODE0 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
	 WRITE_MODE1 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
     BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"     
    );
    PORT (
	 DOA,DOB : out std_logic_vector(17 downto 0):=conv_std_logic_vector(0,18);
	 CLKA,CLKB, CEA,CEB,OCEA,OCEB,RESETA,RESETB,WREA,WREB : in std_logic;
	 ADA,ADB : in std_logic_vector(13 downto 0);
	 DIA : in std_logic_vector(17 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0);
	 DIB : in std_logic_vector(17 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of DPX9 : Component is true;

----------------------------------rSDP---------------------------------------
COMPONENT rSDP 
    GENERIC ( 
	 BIT_WIDTH_0 : integer :=16; -- 1, 2, 4, 8, 16, 32
	 BIT_WIDTH_1 : integer :=16; -- 1, 2, 4, 8, 16, 32
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
     BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"    
    );
    PORT (
	 DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	 CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB : in std_logic;
	 ADA,ADB : in std_logic_vector(13 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0);
	 DI : in std_logic_vector(31 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of rSDP : Component is true;

----------------------------------rSDPX9---------------------------------------
COMPONENT rSDPX9 
    GENERIC ( 
	 BIT_WIDTH_0 : integer :=18; -- 9, 18, 36
	 BIT_WIDTH_1 : integer :=18; -- 9, 18, 36
	 READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	 BLK_SEL : bit_vector := "000";
     RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	 INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	 INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"      
    );
    PORT (
	 DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	 CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB : in std_logic;
	 ADA,ADB : in std_logic_vector(13 downto 0);
     BLKSEL : in std_logic_vector(2 downto 0);
	 DI : in std_logic_vector(35 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of rSDPX9 : Component is true;

-----------------------rROM----------------------------
COMPONENT rROM 
    GENERIC ( 
	    BIT_WIDTH : integer :=1;	
	    READ_MODE : bit := '0';
        BLK_SEL : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"        
    );
    PORT (
	    DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	    CLK, CE, OCE, RESET : in std_logic;
    	BLKSEL : in std_logic_vector(2 downto 0);
	    AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of rROM : Component is true;

------------------------------rROMX9 ---------------------------------------------
COMPONENT rROMX9 
    GENERIC ( 
        BIT_WIDTH : integer :=9;
        READ_MODE : bit :='0';
        BLK_SEL : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
        INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"     

    );
    PORT (
	 DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	 CLK, CE, OCE, RESET : in std_logic;
     BLKSEL : in std_logic_vector(2 downto 0);
	 AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of rROMX9 : Component is true;

-----------------------pROM----------------------------
COMPONENT pROM 
    GENERIC ( 
	    BIT_WIDTH : integer :=1;	
	    READ_MODE : bit := '0';
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"        
    );
    PORT (
	    DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	    CLK, CE, OCE, RESET : in std_logic;
	    AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of pROM : Component is true;

------------------------------pROMX9 ---------------------------------------------
COMPONENT pROMX9 
    GENERIC ( 
        BIT_WIDTH : integer :=9;
        READ_MODE : bit :='0';
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
        INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"     

    );
    PORT (
	    DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	    CLK, CE, OCE, RESET : in std_logic;
	    AD : in std_logic_vector(13 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of pROMX9 : Component is true;

----------------------------------SDPB---------------------------------------
COMPONENT SDPB 
    GENERIC ( 
	    BIT_WIDTH_0 : integer :=16; -- 1, 2, 4, 8, 16, 32
	    BIT_WIDTH_1 : integer :=16; -- 1, 2, 4, 8, 16, 32
	    READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
        BLK_SEL_0 : bit_vector := "000";
        BLK_SEL_1 : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"    
    );
    PORT (
	    DO : out std_logic_vector(31 downto 0):=conv_std_logic_vector(0,32);
	    CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB : in std_logic;
	    ADA,ADB : in std_logic_vector(13 downto 0);
        BLKSELA,BLKSELB : in std_logic_vector(2 downto 0);
	    DI : in std_logic_vector(31 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of SDPB : Component is true;

----------------------------------SDPX9B---------------------------------------
COMPONENT SDPX9B 
    GENERIC ( 
	    BIT_WIDTH_0 : integer :=18; -- 9, 18, 36
	    BIT_WIDTH_1 : integer :=18; -- 9, 18, 36
	    READ_MODE : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	    BLK_SEL_0 : bit_vector := "000";
	    BLK_SEL_1 : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"      
    );
    PORT (
	    DO : out std_logic_vector(35 downto 0):=conv_std_logic_vector(0,36);
	    CLKA,CLKB, CEA,CEB,OCE,RESETA,RESETB : in std_logic;
	    ADA,ADB : in std_logic_vector(13 downto 0);
        BLKSELA,BLKSELB : in std_logic_vector(2 downto 0);
	    DI : in std_logic_vector(35 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of SDPX9B : Component is true;

----------------------------DPB---------------------------------------

COMPONENT DPB 
    GENERIC (
		 BIT_WIDTH_0 : integer :=16; 
		 BIT_WIDTH_1 : integer :=16; 
		 READ_MODE0 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
		 READ_MODE1 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
		 WRITE_MODE0 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
		 WRITE_MODE1 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
    	 BLK_SEL_0 : bit_vector := "000";
    	 BLK_SEL_1 : bit_vector := "000";
         RESET_MODE : string := "SYNC"; --SYNC, ASYNC
		 INIT_RAM_00 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_01 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_02 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_03 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_04 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_05 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_06 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_07 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_08 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_09 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_0F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_10 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_11 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_12 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_13 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_14 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_15 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_16 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_17 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_18 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_19 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_1F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_20 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_21 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_22 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_23 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_24 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_25 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_26 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_27 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_28 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_29 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_2F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_30 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_31 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_32 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_33 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_34 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_35 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_36 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_37 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_38 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_39 : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3A : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3B : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3C : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3D : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3E : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000";
		 INIT_RAM_3F : bit_vector := X"0000000000000000000000000000000000000000000000000000000000000000"
    );
    PORT (
		 DOA,DOB : out std_logic_vector(15 downto 0):=conv_std_logic_vector(0,16);
		 CLKA,CLKB, CEA,CEB,OCEA,OCEB,RESETA,RESETB,WREA,WREB : in std_logic;
		 ADA,ADB : in std_logic_vector(13 downto 0);
    	 BLKSELA,BLKSELB : in std_logic_vector(2 downto 0);
		 DIA,DIB : in std_logic_vector(15 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of DPB : Component is true;

----------------------------DPX9B---------------------------------------
COMPONENT DPX9B 
    GENERIC ( 
	    BIT_WIDTH_0 : integer :=18; -- 9, 18
	    BIT_WIDTH_1 : integer :=18; -- 9, 18
	    READ_MODE0 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	    READ_MODE1 : bit := '0'; -- 0: bypass mode; 1: pipeline mode
	    WRITE_MODE0 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
	    WRITE_MODE1 : bit_vector := "00"; -- 00: normal mode; 01: write-through mode; 10: read-before-write mode
        BLK_SEL_0 : bit_vector := "000";
        BLK_SEL_1 : bit_vector := "000";
        RESET_MODE : string := "SYNC"; --SYNC, ASYNC
	    INIT_RAM_00 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_01 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_02 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_03 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_04 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_05 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_06 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_07 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_08 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_09 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_0F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_10 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_11 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_12 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_13 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_14 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_15 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_16 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_17 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_18 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_19 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_1F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_20 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_21 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_22 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_23 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_24 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_25 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_26 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_27 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_28 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_29 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_2F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_30 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_31 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_32 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_33 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_34 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_35 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_36 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_37 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_38 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_39 : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3A : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3B : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3C : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3D : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3E : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000";
	    INIT_RAM_3F : bit_vector := X"000000000000000000000000000000000000000000000000000000000000000000000000"     
    );
    PORT (
	    DOA,DOB : out std_logic_vector(17 downto 0) := conv_std_logic_vector(0,18);
	    CLKA,CLKB, CEA,CEB,OCEA,OCEB,RESETA,RESETB,WREA,WREB : in std_logic;
	    ADA,ADB : in std_logic_vector(13 downto 0);
	    DIA : in std_logic_vector(17 downto 0);
        BLKSELA,BLKSELB : in std_logic_vector(2 downto 0);
	    DIB : in std_logic_vector(17 downto 0)
    );
end COMPONENT;
	attribute syn_black_box of DPX9B : Component is true;


---------------------BUFG---------------------------

COMPONENT BUFG 
  PORT(
    	O : out std_logic;
    	I : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of BUFG : Component is true;
-----------------BUFS--------------------

COMPONENT BUFS 
    PORT (
         O : out std_logic;
         I : in std_logic
    );
end COMPONENT;
	attribute syn_black_box of BUFS : Component is true;
----------------------GND-----------------

COMPONENT GND 
    PORT (
    	 G : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of GND : Component is true;
---------------------VCC------------------------

COMPONENT VCC 
    PORT (
    	 V : out std_logic
    );
end COMPONENT;
	attribute syn_black_box of VCC : Component is true;


------------------OSCZ----------------------------

COMPONENT OSCZ 
    GENERIC (
        FREQ_DIV : integer := 100  --2~128,only even num
    );
    PORT (
	    OSCOUT : out std_logic;
        OSCEN : IN STD_LOGIC
    );
end COMPONENT;
	attribute syn_black_box of OSCZ : Component is true;

-----------------INV---------------------------------

COMPONENT INV 
    PORT (
    	 O : OUT std_logic;
    	 I : IN std_logic
    );
end COMPONENT;
	attribute syn_black_box of INV : Component is true;


-----------------TLVDS_IBUF---------------------------------

COMPONENT TLVDS_IBUF
    PORT(
        O : OUT std_logic;
        I : IN std_logic;
        IB : IN std_logic
        );
end COMPONENT;
    attribute syn_black_box of TLVDS_IBUF : Component is true;
    attribute black_box_pad_pin of TLVDS_IBUF : Component is "I, IB";

-----------------TLVDS_OBUF---------------------------------

COMPONENT TLVDS_OBUF
    PORT(
        O : OUT std_logic;
        OB : OUT std_logic;
        I : IN std_logic
        );
end COMPONENT;
    attribute syn_black_box of TLVDS_OBUF : Component is true;
    attribute black_box_pad_pin of TLVDS_OBUF : Component is "O, OB";

-----------------TLVDS_TBUF---------------------------------

COMPONENT TLVDS_TBUF
    PORT (
    	 O  : OUT   std_logic;
    	 OB : OUT std_logic;
     	 I  : IN    std_logic;
	     OEN : IN    std_logic
    );
end COMPONENT;
    attribute syn_black_box of TLVDS_TBUF : Component is true;
    attribute black_box_pad_pin of TLVDS_TBUF : Component is "O, OB";


-----------------ELVDS_IBUF---------------------------------

COMPONENT ELVDS_IBUF
    PORT(
        O : OUT std_logic;
        I : IN std_logic;
        IB : IN std_logic
        );
end COMPONENT;
    attribute syn_black_box of ELVDS_IBUF : Component is true;
    attribute black_box_pad_pin of ELVDS_IBUF : Component is "I, IB";

-----------------ELVDS_OBUF---------------------------------

COMPONENT ELVDS_OBUF
    PORT(
        O : OUT std_logic;
        OB : OUT std_logic;
        I : IN std_logic
        );
end COMPONENT;
    attribute syn_black_box of ELVDS_OBUF : Component is true;
    attribute black_box_pad_pin of ELVDS_OBUF : Component is "O, OB";

-----------------ELVDS_TBUF---------------------------------

COMPONENT ELVDS_TBUF
    PORT (
    	 O  : OUT   std_logic;
    	 OB : OUT std_logic;
     	 I  : IN    std_logic;
	     OEN : IN    std_logic
    );
end COMPONENT;
    attribute syn_black_box of ELVDS_TBUF : Component is true;
    attribute black_box_pad_pin of ELVDS_TBUF : Component is "O, OB";

-----------------ELVDS_IOBUF---------------------------------

COMPONENT ELVDS_IOBUF
    PORT (
    	 O  : OUT   std_logic;
    	 IOB : INOUT std_logic;
         IO : INOUT std_logic;
     	 I  : IN    std_logic;
	     OEN : IN    std_logic
    );
end COMPONENT;
    attribute syn_black_box of ELVDS_IOBUF : Component is true;
    attribute black_box_pad_pin of ELVDS_IOBUF : Component is "IO, IOB";

------------------------MIPI_IBUF------------------------------------
COMPONENT MIPI_IBUF
    PORT (
         OH, OL, OB : OUT std_logic;
         IO, IOB : INOUT std_logic;
         I, IB : IN std_logic;
         OEN, OENB, HSREN : IN std_logic
    );
end COMPONENT;
    attribute syn_black_box of MIPI_IBUF : Component is true;
    attribute black_box_pad_pin of MIPI_IBUF : Component is "IO, IOB";

------------------------MIPI_IBUF_HS------------------------------------
COMPONENT MIPI_IBUF_HS
    PORT (
    	 OH : OUT std_logic;
         I : IN std_logic;
         IB : IN std_logic
    );
end COMPONENT;
    attribute syn_black_box of MIPI_IBUF_HS : Component is true;
    attribute black_box_pad_pin of MIPI_IBUF_HS : Component is "I, IB";

----------------------MIPI_IBUF_LP-------------------------------------
COMPONENT MIPI_IBUF_LP
    PORT (
    	 OL : OUT std_logic;
         OB : OUT std_logic;
    	 IB : IN std_logic;
         I : IN std_logic
    );
end COMPONENT;
    attribute syn_black_box of MIPI_IBUF_LP : Component is true;
    attribute black_box_pad_pin of MIPI_IBUF_LP : Component is "I, IB";

------------------------MIPI_OBUF---------------------------------------
COMPONENT MIPI_OBUF
    PORT (
    	 O : OUT std_logic;
         OB : OUT std_logic;
         I : IN std_logic;
         IB : IN std_logic;
         MODESEL : IN std_logic
    );
end COMPONENT;
    attribute syn_black_box of MIPI_OBUF : Component is true;
    attribute black_box_pad_pin of MIPI_OBUF : Component is "O, OB";

----------------------------I3C_IOBUF--------------------------------

COMPONENT I3C_IOBUF 
    PORT (
    	O  : OUT   std_logic;
    	IO : INOUT std_logic;
     	I  : IN    std_logic;
	    MODESEL : IN    std_logic
    );
end COMPONENT;
	attribute syn_black_box of I3C_IOBUF : Component is true;
    attribute black_box_pad_pin of I3C_IOBUF : Component is "IO";


----------------------------PADD18---------------------------------------
COMPONENT PADD18
		generic(
		    AREG : bit := '0';-- '0': bypass mode; '1': registered mode
		    BREG : bit := '0'; 
		    SOREG : bit := '0';
		    ADD_SUB : bit := '0';
            PADD_RESET_MODE : string := "SYNC"; -- SYNC,ASYNC
            BSEL_MODE : bit := '1' -- "1": shift, "0": parallel input B.
	    );
       
        port(
		    A : in std_logic_vector(17 downto 0);
		    B : in std_logic_vector(17 downto 0);
		    ASEL : in std_logic;
		    CE,CLK,RESET : in std_logic;
		    SI,SBI : in std_logic_vector(17 downto 0);
		    SO,SBO : out std_logic_vector(17 downto 0);
		    DOUT : out std_logic_vector(17 downto 0)
    	);
END COMPONENT;
	attribute syn_black_box of PADD18 : Component is true;

----------------------------PADD9---------------------------------------
COMPONENT PADD9
        generic(
		    AREG : bit := '0';-- '0': bypass mode; '1': registered mode
		    BREG : bit := '0'; 
		    SOREG : bit := '0';
		    ADD_SUB : bit := '0';
            PADD_RESET_MODE : string := "SYNC"; -- SYNC,ASYNC
            BSEL_MODE : bit := '1' -- "1": shift, "0": parallel input B.
	    );
        
    	port (
		    A : in std_logic_vector(8 downto 0);
		    B : in std_logic_vector(8 downto 0);
		    ASEL : in std_logic;
		    CE,CLK,RESET : in std_logic;
		    SI,SBI : in std_logic_vector(8 downto 0);
		    SO,SBO : out std_logic_vector(8 downto 0);
		    DOUT : out std_logic_vector(8 downto 0)
	    );
end COMPONENT;
	attribute syn_black_box of PADD9 : Component is true;

----------------------------MULT9X9-------------------------------------
COMPONENT MULT9X9
		generic(
		    AREG :  bit := '0'; --  '0': bypass mode; '1': registered mode
		    BREG :  bit := '0';
		    OUT_REG :  bit := '0';
		    PIPE_REG :  bit := '0';
		    ASIGN_REG :  bit := '0';
		    BSIGN_REG :  bit := '0';
            SOA_REG :  bit := '0'; 
		    MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	    );

    	port (
		    A,SIA : in std_logic_vector(8 downto 0);
		    B,SIB : in std_logic_vector(8 downto 0);
		    ASIGN, BSIGN : in std_logic;
            ASEL,BSEL : in std_logic;
		    CE : in std_logic;
		    CLK : in std_logic;
		    RESET : in std_logic;
		    DOUT : out std_logic_vector(17 downto 0);
            SOA,SOB : out std_logic_vector(8 downto 0)
	    );
END COMPONENT;
	attribute syn_black_box of MULT9X9 : Component is true;

----------------------------MULT18X18---------------------------------------

COMPONENT MULT18X18
    generic(
		AREG :  bit := '0'; --  '0': bypass mode; '1': registered mode
		BREG :  bit := '0';
		OUT_REG :  bit := '0';
		PIPE_REG :  bit := '0';
		ASIGN_REG :  bit := '0';
		BSIGN_REG :  bit := '0';
        SOA_REG :  bit := '0';
		MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	);

	port (
		A,SIA : in std_logic_vector(17 downto 0);
		B,SIB : in std_logic_vector(17 downto 0);
		ASIGN, BSIGN : in std_logic;
        ASEL,BSEL : in std_logic;
		CE : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		DOUT : out std_logic_vector(35 downto 0);
        SOA,SOB : out std_logic_vector(17 downto 0)
	);
end COMPONENT;
	attribute syn_black_box of MULT18X18 : Component is true;

----------------------------MULT36X36---------------------------------------
COMPONENT MULT36X36
	generic(
		AREG :  bit := '0'; --  '0': bypass mode; '1': registered mode
		BREG :  bit := '0';
		OUT0_REG :  bit := '0';
		OUT1_REG :  bit := '0';
		PIPE_REG :  bit := '0';
		ASIGN_REG :  bit := '0';
		BSIGN_REG :  bit := '0';
		MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	);

	port (
		A : in std_logic_vector(35 downto 0);
		B : in std_logic_vector(35 downto 0);
		ASIGN, BSIGN : in std_logic;
		CE : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		DOUT : out std_logic_vector(71 downto 0)
	);
END COMPONENT;
	attribute syn_black_box of MULT36X36 : Component is true;

----------------------------MULTALU36X18---------------------------------------
COMPONENT MULTALU36X18
	generic(
		AREG :  bit := '0'; --  '0': bypass mode; '1': registered mode
		BREG :  bit := '0';
		CREG :  bit := '0';
		OUT_REG :  bit := '0';
		PIPE_REG :  bit := '0';
		ASIGN_REG :  bit := '0';
		BSIGN_REG :  bit := '0';
        ACCLOAD_REG0 : bit := '0';
        ACCLOAD_REG1 : bit := '0';
        SOA_REG : bit := '0';
        MULTALU36X18_MODE : integer := 0;--0:36x18 +/- C; 1:ACC/0 + 36x18; 2: 36x18 + CASI
        C_ADD_SUB : bit := '0';-- '0': add;  '1': sub
		MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	);
    
	port (
		A : in std_logic_vector(17 downto 0);
		B : in std_logic_vector(35 downto 0);
		C : in std_logic_vector(53 downto 0);
		ASIGN, BSIGN, ACCLOAD : in std_logic;
		CE : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		CASI : in std_logic_vector(54 downto 0);
		DOUT : out std_logic_vector(53 downto 0);
		CASO : out std_logic_vector(54 downto 0)
	);
END COMPONENT;
	attribute syn_black_box of MULTALU36X18 : Component is true;
    
----------------------------MULTADDALU18X18---------------------------------------
COMPONENT MULTADDALU18X18
	generic(
		A0REG : bit := '0';-- '0': bypass mode; '1': registered mode
		B0REG : bit := '0'; 
		A1REG : bit := '0';
		B1REG : bit := '0';
		CREG : bit := '0';
		OUT_REG : bit := '0';
		PIPE0_REG : bit := '0';
		PIPE1_REG : bit := '0';
		ASIGN0_REG : bit := '0';
		BSIGN0_REG : bit := '0';
		ASIGN1_REG : bit := '0';
		BSIGN1_REG : bit := '0';
		ACCLOAD_REG0 : bit := '0';
		ACCLOAD_REG1 : bit := '0';
        SOA_REG : bit := '0';
		B_ADD_SUB : bit := '0';  -- '0': add; '1': sub
		C_ADD_SUB : bit := '0';
		MULTADDALU18X18_MODE : integer := 0;--0:18x18 +/- 18x18 +/- C;  1: ACC/0 + 18x18 +/- 18x18; 2:18x18 +/- 18x18 + CASI
		MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	);

	port (
		A0,A1 : in std_logic_vector(17 downto 0);
		B0,B1 : in std_logic_vector(17 downto 0);
		SIA,SIB : in std_logic_vector(17 downto 0);
		C : in std_logic_vector(53 downto 0);
        ASIGN,BSIGN : in std_logic_vector(1 downto 0);
        ASEL,BSEL : in std_logic_vector(1 downto 0);
        CASI : in std_logic_vector(54 downto 0);
        ACCLOAD : in std_logic;
		CE : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		DOUT : out std_logic_vector(53 downto 0);
        SOA,SOB : out std_logic_vector(17 downto 0);
		CASO : out std_logic_vector(54 downto 0)
	);
END COMPONENT;
	attribute syn_black_box of MULTADDALU18X18 : Component is true;

----------------------------MULTALU18X18---------------------------------------
COMPONENT MULTALU18X18
	generic(
		AREG : bit := '0';-- '0': bypass mode; '1': registered mode
		BREG : bit := '0'; 
		CREG : bit := '0';
		DREG : bit := '0';
        OUT_REG : bit := '0';
		PIPE_REG : bit := '0';
		ASIGN_REG : bit := '0';
		BSIGN_REG : bit := '0';
		DSIGN_REG : bit := '0';
		ACCLOAD_REG0 : bit := '0';
		ACCLOAD_REG1 : bit := '0';
		B_ADD_SUB : bit := '0';  -- '0': add; '1': sub
		C_ADD_SUB : bit := '0';
		MULTALU18X18_MODE : integer := 0;--0:ACC/0 +/- 18x18 +/- C; 1:ACC/0 +/- 18x18 + CASI; 2: 18x18 +/- D + CASI;
		MULT_RESET_MODE : string := "SYNC" -- SYNC, ASYNC
	);

	port (
		A : in std_logic_vector(17 downto 0);
		B : in std_logic_vector(17 downto 0);
		C, D : in std_logic_vector(53 downto 0);
        ASIGN, BSIGN : in std_logic;
        CASI : in std_logic_vector(54 downto 0);
        ACCLOAD,DSIGN : in std_logic;
		CE : in std_logic;
		CLK : in std_logic;
		RESET : in std_logic;
		DOUT : out std_logic_vector(53 downto 0);
		CASO : out std_logic_vector(54 downto 0)
	);
END COMPONENT;
	attribute syn_black_box of MULTALU18X18 : Component is true;

----------------------------ALU54D--------------------------------------
COMPONENT ALU54D
	generic(
	    AREG : bit := '0'; --'0': bypass mode; '1': registered mode
        BREG : bit := '0';
	    ASIGN_REG : bit := '0';
	    BSIGN_REG : bit := '0';
	    ACCLOAD_REG : bit := '0';
	    OUT_REG : bit := '0';
	    B_ADD_SUB : bit := '0';--'0':add; '1':sub
	    C_ADD_SUB : bit := '0';
        ALUD_MODE : integer := 0;--0:ACC/0 +/- B +/- A; 1:ACC/0 +/- B + CASI; 2:A +/- B + CASI;
		ALU_RESET_MODE : string := "SYNC" --SYNC, ASYNC
    );
    port (
	    A : in std_logic_vector (53 downto 0);
	    B : in std_logic_vector (53 downto 0);
	    CE : in std_logic;
	    CLK : in std_logic;
	    RESET : in std_logic;
	    ASIGN,BSIGN : in std_logic;
	    ACCLOAD : in std_logic;
	    CASI : in std_logic_vector (54 downto 0);
	    DOUT : out std_logic_vector (53 downto 0);
	    CASO : out std_logic_vector (54 downto 0)
    );
END COMPONENT;
	attribute syn_black_box of ALU54D : Component is true;


-----------------DLL---------------------------------
COMPONENT DLL
    GENERIC(
        DLL_FORCE : integer := 0;--1: force lock and code; 0: code/lock generated from DLL loop
        DIV_SEL : bit := '1';--0,normal lock mode; 1,fast lock mode
	    CODESCAL : STRING := "000";--001 010 011 100 101 110 111
        SCAL_EN : STRING := "true"--true,false
    );
    PORT(
        CLKIN:IN std_logic:='0';
        STOP: In std_logic:='0';
        RESET : In std_logic:='0';
        UPDNCNTL : In std_logic:='0';
        LOCK : OUT std_logic;
        STEP : OUT std_logic_vector(7 downto 0)
     );
end COMPONENT;
	attribute syn_black_box of DLL : Component is true;

--------------------------------CLKDIV--------------------------
COMPONENT CLKDIV
    GENERIC(
	     DIV_MODE : STRING := "2"; -- "2", "3.5", "4", "5", "8"
	     GSREN : STRING := "false" -- "false", "true"
	    );
    PORT(
         HCLKIN : IN std_logic;
	     RESETN : IN std_logic;
	     CALIB : In std_logic;
	     CLKOUT : OUT std_logic
        );
end COMPONENT;
	attribute syn_black_box of CLKDIV : Component is true;

--------------------------------DHCEN-------------------------------------
COMPONENT DHCEN
    PORT (
	 CLKOUT : OUT std_logic;	
	 CE : IN std_logic;	
	 CLKIN : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DHCEN : Component is true;    

------------------------------DLLDLY---------------------------------------
COMPONENT DLLDLY
    GENERIC(
        DLL_INSEL : bit := '0'; --'0':bypass mode, '1': use dll_delay cell
        DLY_SIGN : bit := '0'; -- '0':'+',  '1': '-'
        DLY_ADJ : integer := 0 --0~255, dly_sign=0 :dly_adj; dly_sign=1: -256+dly_adj
    );
    PORT(
        DLLSTEP : IN std_logic_vector(7 downto 0);
        CLKIN:IN std_logic;
        DIR,LOADN,MOVE: In std_logic;
        CLKOUT : OUT std_logic;
        FLAG : OUT std_logic
     );
end COMPONENT;
	attribute syn_black_box of DLLDLY : Component is true;


---------------------------------DCS-------------------------------------
COMPONENT DCS
    GENERIC (
		DCS_MODE : string := "RISING"   --CLK0,CLK1,CLK2,CLK3,GND,VCC,RISING,FALLING,CLK0_GND,CLK0_VCC,CLK1_GND,CLK1_VCC,CLK2_GND,CLK2_VCC,CLK3_GND,CLK3_VCC
	);
	PORT (
		CLK0 : IN std_logic;
		CLK1 : IN std_logic;
		CLK2 : IN std_logic;
		CLK3 : IN std_logic;
		CLKSEL : IN std_logic_vector(3 downto 0);
		SELFORCE : IN std_logic;
		CLKOUT : OUT std_logic
	);
end COMPONENT;
	attribute syn_black_box of DCS : Component is true;    

--------------------------------DQCE-------------------------------------
COMPONENT DQCE
    PORT (
	 CLKOUT : OUT std_logic;	
	 CE : IN std_logic;	
	 CLKIN : IN std_logic
    );	
end COMPONENT;
	attribute syn_black_box of DQCE : Component is true;    

------------------------------FLASH256K---------------------------------------
COMPONENT FLASH256K
    PORT(
        XADR : IN std_logic_vector(6 downto 0);
        YADR : IN std_logic_vector(5 downto 0);
        XE,YE,SE:IN std_logic;
        DIN : IN std_logic_vector(31 downto 0);
        ERASE,PROG,NVSTR: IN std_logic;
        DOUT : OUT std_logic_vector(31 downto 0)
     );
end COMPONENT;
	attribute syn_black_box of FLASH256K : Component is true;    



end components;


