include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Apb3SpiMasterCtrl.v
	TOPLEVEL=Apb3SpiMasterCtrl
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Apb3SpiMasterCtrl.vhd
	TOPLEVEL=apb3spimasterctrl
endif

MODULE=Apb3SpiMasterCtrlTester

#SIM_ARGS += --vcd=ghdl.vcd
#RANDOM_SEED=1500899963 COCOTB_ANSI_OUTPUT=true

include ../../common/Makefile.sim
