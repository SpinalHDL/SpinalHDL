include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/MultiClockTester.v
	TOPLEVEL=MultiClockTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/MultiClockTester.vhd
	TOPLEVEL=multiclocktester
endif

MODULE=MultiClockTester

include ../common/Makefile.sim
