include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/FixedPointTester.v
	TOPLEVEL=FixedPointTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/FixedPointTester.vhd
	TOPLEVEL=fixedpointtester
endif

MODULE=FixedPointTester

include ../common/Makefile.sim
