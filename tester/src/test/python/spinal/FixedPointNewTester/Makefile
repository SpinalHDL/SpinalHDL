include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/FixedPointNewTester.v
	TOPLEVEL=FixedPointNewTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/FixedPointNewTester.vhd
	TOPLEVEL=fixedpointnewtester
endif

MODULE=FixedPointNewTester

include ../common/Makefile.sim
