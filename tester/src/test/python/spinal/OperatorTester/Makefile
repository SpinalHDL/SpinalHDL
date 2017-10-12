include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/OperatorTester.v
	TOPLEVEL=OperatorTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/OperatorTester.vhd
	TOPLEVEL=operatortester
endif

MODULE=OperatorTester

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
