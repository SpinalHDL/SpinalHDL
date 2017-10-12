include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/Apb3I2cSlave.v
	TOPLEVEL=Apb3I2cSlave
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/Apb3I2cSlave.vhd
	TOPLEVEL=apb3i2cslave
endif

MODULE=Apb3I2cSlaveTester

include ../../common/Makefile.sim
