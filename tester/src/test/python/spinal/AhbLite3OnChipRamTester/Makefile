include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/AhbLite3OnChipRamTester.v
	TOPLEVEL=AhbLite3OnChipRamTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/AhbLite3OnChipRamTester.vhd
	TOPLEVEL=ahblite3onchipramtester
endif

MODULE=AhbLite3OnChipRamTester

include ../common/Makefile.sim
