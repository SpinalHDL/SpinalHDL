include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/RomTester.v
	TOPLEVEL=RomTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/RomTester.vhd
	TOPLEVEL=romtester
endif

MODULE=RomTester

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
