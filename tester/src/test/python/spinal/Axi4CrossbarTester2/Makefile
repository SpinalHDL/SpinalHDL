include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Axi4CrossbarTester2.v
	TOPLEVEL=Axi4CrossbarTester2
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Axi4CrossbarTester2.vhd
	TOPLEVEL=axi4crossbartester2
endif

MODULE=Axi4CrossbarTester2

include ../common/Makefile.sim
