include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/MsgSeqTester.v
	TOPLEVEL=MsgSeqTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/MsgSeqTester.vhd
	TOPLEVEL=msgseqtester
endif

COCOTB_LOG_LEVEL=TRACE
MODULE=MsgSeqTester
include ../common/Makefile.sim
