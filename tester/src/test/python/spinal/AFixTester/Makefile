include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/AFixTester.v
	TOPLEVEL=AFixTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/AFixTester.vhd
	TOPLEVEL=afixtester
endif

MODULE=AFixTester

include ../common/Makefile.sim
