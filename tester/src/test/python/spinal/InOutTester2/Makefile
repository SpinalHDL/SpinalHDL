include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += BlackBoxed.v $(SPINALROOT)/InOutTester2.v
	TOPLEVEL=InOutTester2
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += ${CURDIR}/BlackBoxed.vhd $(SPINALROOT)/InOutTester2.vhd
	TOPLEVEL=inouttester2
endif

MODULE=InOutTester2

include ../common/Makefile.sim
