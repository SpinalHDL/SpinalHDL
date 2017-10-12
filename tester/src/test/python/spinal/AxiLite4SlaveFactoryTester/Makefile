include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/AxiLite4SlaveFactoryTester.v
	TOPLEVEL=AxiLite4SlaveFactoryTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/AxiLite4SlaveFactoryTester.vhd
	TOPLEVEL=axilite4slavefactorytester
endif

MODULE=AxiLite4SlaveFactoryTester

include ../common/Makefile.sim
