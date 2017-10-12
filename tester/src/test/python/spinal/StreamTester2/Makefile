include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/StreamTester2.v
	TOPLEVEL=StreamTester2
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/StreamTester2.vhd
	TOPLEVEL=streamtester2
endif

MODULE=StreamTester2

#SIM_ARGS += --vcd=ghdl.vcd
#RANDOM_SEED=1500899963 COCOTB_ANSI_OUTPUT=true

include ../common/Makefile.sim
