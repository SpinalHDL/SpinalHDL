include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/InOutTester3.v
	TOPLEVEL=InOutTester3
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/InOutTester3.vhd
	TOPLEVEL=inouttester3
endif

MODULE=InOutTester3

include ../common/Makefile.sim
