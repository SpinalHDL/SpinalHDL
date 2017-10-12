include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/InternalClockTester.v
	TOPLEVEL=InternalClockTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/InternalClockTester.vhd
	TOPLEVEL=internalclocktester
endif

MODULE=InternalClockTester

include ../common/Makefile.sim
