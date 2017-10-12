include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Apb3SpiSlaveCtrl.v
	TOPLEVEL=Apb3SpiSlaveCtrl
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Apb3SpiSlaveCtrl.vhd
	TOPLEVEL=apb3spislavectrl
endif

MODULE=Apb3SpiSlaveCtrlTester

#SIM_ARGS += --vcd=ghdl.vcd
#RANDOM_SEED=1500899963 COCOTB_ANSI_OUTPUT=true

include ../../common/Makefile.sim
