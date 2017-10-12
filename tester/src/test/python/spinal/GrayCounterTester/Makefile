include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/GrayCounterTester.v
	TOPLEVEL=GrayCounterTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/GrayCounterTester.vhd
	TOPLEVEL=graycountertester
endif

MODULE=GrayCounterTester

include ../common/Makefile.sim
