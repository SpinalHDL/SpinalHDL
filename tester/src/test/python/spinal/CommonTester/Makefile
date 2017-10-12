include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/CommonTester.v
	TOPLEVEL=CommonTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/CommonTester.vhd
	TOPLEVEL=commontester
endif

MODULE=CommonTester

include ../common/Makefile.sim
