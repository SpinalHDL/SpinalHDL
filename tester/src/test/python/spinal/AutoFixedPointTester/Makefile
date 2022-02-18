include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/AutoFixedPointTester.v
	TOPLEVEL=AutoFixedPointTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/AutoFixedPointTester.vhd
	TOPLEVEL=autofixedpointtester
endif

MODULE=AutoFixedPointTester

include ../common/Makefile.sim
