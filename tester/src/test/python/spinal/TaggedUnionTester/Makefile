include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/TaggedUnionTester.v
	TOPLEVEL=TaggedUnionTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/TaggedUnionTester.vhd
	TOPLEVEL=TaggedUnionTester
endif

MODULE=TaggedUnionTester

include ../common/Makefile.sim
