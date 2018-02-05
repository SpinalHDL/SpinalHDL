include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/RomTester2.v
	TOPLEVEL=RomTester2
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/RomTester2.vhd
	TOPLEVEL=romtester2
endif

MODULE=RomTester2

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
