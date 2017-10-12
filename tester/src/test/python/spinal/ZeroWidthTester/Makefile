include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/ZeroWidthTester.v
	TOPLEVEL=ZeroWidthTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/ZeroWidthTester.vhd
	TOPLEVEL=zerowidthtester
endif

MODULE=ZeroWidthTester

include ../common/Makefile.sim
