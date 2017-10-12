include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/AhbLite3CrossbarTester.v
	TOPLEVEL=AhbLite3CrossbarTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/AhbLite3CrossbarTester.vhd
	TOPLEVEL=ahblite3crossbartester
endif

MODULE=AhbLite3CrossbarTester

include ../common/Makefile.sim
