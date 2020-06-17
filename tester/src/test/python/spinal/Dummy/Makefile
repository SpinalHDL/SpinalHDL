include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += Dummy.v
	TOPLEVEL=Dummy
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += ${CURDIR}/Dummy.vhd
	TOPLEVEL=dummy
	SIM=ghdl
endif

MODULE=Dummy

include $(shell cocotb-config --makefiles)/Makefile.inc
include $(shell cocotb-config --makefiles)/Makefile.sim
