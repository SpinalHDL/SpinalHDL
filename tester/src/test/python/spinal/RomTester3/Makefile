include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/RomTester3.v
	TOPLEVEL=RomTester3
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/RomTester3.vhd
	TOPLEVEL=romtester3
endif

MODULE=RomTester3

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
