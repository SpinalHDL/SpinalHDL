include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/WhenTester.v
	TOPLEVEL=WhenTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/WhenTester.vhd
	TOPLEVEL=whentester
endif

MODULE=WhenTester

include ../common/Makefile.sim
