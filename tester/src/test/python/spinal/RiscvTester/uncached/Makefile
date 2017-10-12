include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/RiscvTesterUncached.v
	TOPLEVEL=RiscvTesterUncached
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/RiscvTesterUncached.vhd
	TOPLEVEL=riscvtesteruncached
endif

MODULE=RiscvTesterUncached

#SIM_ARGS += --vcd=ghdl.vcd

include ../../common/Makefile.sim
