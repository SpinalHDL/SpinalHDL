include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/LibTester.v
	TOPLEVEL=LibTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/LibTester.vhd
	TOPLEVEL=libtester
endif

MODULE=LibTester

include ../common/Makefile.sim
