include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += BlackBoxToTest.v $(SPINALROOT)/BlackBoxTester.v
	TOPLEVEL=BlackBoxTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += ${CURDIR}/BlackBoxToTest.vhd $(SPINALROOT)/BlackBoxTester.vhd
	TOPLEVEL=blackboxtester
endif

MODULE=BlackBoxTester

include ../common/Makefile.sim
