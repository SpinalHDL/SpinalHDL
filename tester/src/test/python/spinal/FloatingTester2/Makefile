include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/FloatingTester2.v
	TOPLEVEL=FloatingTester2
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/FloatingTester2.vhd
	TOPLEVEL=floatingtester2
endif

MODULE=FloatingTester2

include ../common/Makefile.sim
