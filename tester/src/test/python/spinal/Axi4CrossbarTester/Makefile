include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Axi4CrossbarTester.v
	TOPLEVEL=Axi4CrossbarTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Axi4CrossbarTester.vhd
	TOPLEVEL=axi4crossbartester
endif

MODULE=Axi4CrossbarTester

include ../common/Makefile.sim
