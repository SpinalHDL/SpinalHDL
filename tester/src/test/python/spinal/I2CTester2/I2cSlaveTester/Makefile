include ../../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/I2cSlave.v
	TOPLEVEL=I2cSlave
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/I2cSlave.vhd
	TOPLEVEL=i2cslave
endif

MODULE=I2cSlaveTester

include ../../common/Makefile.sim
