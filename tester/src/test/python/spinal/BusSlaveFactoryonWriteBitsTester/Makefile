include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/BusSlaveFactoryonWriteBitsTester.v
	TOPLEVEL=BusSlaveFactoryonWriteBitsTester
endif
# ifeq ($(TOPLEVEL_LANG),vhdl)
# 	VHDL_SOURCES += $(SPINALROOT)/BusSlaveFactoryonWriteBitsTester.vhd
# 	TOPLEVEL=BusSlaveFactoryonWriteBitsTester
# endif
MODULE=BusSlaveFactoryonWriteBitsTester

include ../common/Makefile.sim
