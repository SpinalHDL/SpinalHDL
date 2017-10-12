include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/FloatingTester.v
	TOPLEVEL=FloatingTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/FloatingTester.vhd
	TOPLEVEL=floatingtester
endif

MODULE=float_test

include ../common/Makefile.sim
