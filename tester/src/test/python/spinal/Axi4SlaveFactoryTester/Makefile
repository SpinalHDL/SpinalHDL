include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Axi4SlaveFactoryTester.v
	TOPLEVEL=Axi4SlaveFactoryTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Axi4SlaveFactoryTester.vhd
	TOPLEVEL=axi4slavefactorytester
endif

MODULE=Axi4SlaveFactoryTester

include ../common/Makefile.sim
