include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += BlackBoxed.v $(SPINALROOT)/InOutTester.v
	TOPLEVEL=InOutTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += ${CURDIR}/BlackBoxed.vhd $(SPINALROOT)/InOutTester.vhd
	TOPLEVEL=inouttester
endif

MODULE=InOutTester

include ../common/Makefile.sim
