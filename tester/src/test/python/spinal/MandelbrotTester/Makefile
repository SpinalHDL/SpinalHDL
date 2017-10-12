include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/MandelbrotTester.v
	TOPLEVEL=MandelbrotTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/MandelbrotTester.vhd
	TOPLEVEL=mandelbrottester
endif

MODULE=MandelbrotTester

include ../common/Makefile.sim
