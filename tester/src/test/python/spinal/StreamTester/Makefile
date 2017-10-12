include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/StreamTester.v
	TOPLEVEL=StreamTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/StreamTester.vhd
	TOPLEVEL=streamtester
endif

MODULE=StreamTester

#SIM_ARGS += --vcd=ghdl.vcd
#RANDOM_SEED=1500899963 COCOTB_ANSI_OUTPUT=true

include ../common/Makefile.sim
