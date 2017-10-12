include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/BundleTester.v
	TOPLEVEL=BundleTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/BundleTester.vhd
	TOPLEVEL=bundletester
endif

MODULE=BundleTester

include ../common/Makefile.sim
