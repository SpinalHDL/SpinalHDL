include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/RiscvTesterCached.v
	TOPLEVEL=RiscvTesterCached
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/RiscvTesterCached.vhd
	TOPLEVEL=riscvtestercached
endif

MODULE=RiscvTesterCached

#SIM_ARGS += --vcd=ghdl.vcd

include ../../common/Makefile.sim
