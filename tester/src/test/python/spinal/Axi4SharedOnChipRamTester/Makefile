include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Axi4SharedOnChipRamTester.v
	TOPLEVEL=Axi4SharedOnChipRamTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Axi4SharedOnChipRamTester.vhd
	TOPLEVEL=axi4sharedonchipramtester
endif

MODULE=Axi4SharedOnChipRamTester

include ../common/Makefile.sim
