include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/PDMTester.v
	TOPLEVEL=PDMTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/PDMTester.vhd
	TOPLEVEL=pdmtester
endif

MODULE=PDMTester

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
