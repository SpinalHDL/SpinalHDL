include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/UartTester.v
	TOPLEVEL=UartTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/UartTester.vhd
	TOPLEVEL=uarttester
endif

MODULE=UartTester

include ../common/Makefile.sim
