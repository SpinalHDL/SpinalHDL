include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += PllAAssertSDeassertTesterBB.v $(SPINALROOT)/PllAAssertSDeassertTester.v
	TOPLEVEL=PllAAssertSDeassertTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES +=  ${CURDIR}/PllAAssertSDeassertTesterBB.vhd $(SPINALROOT)/PllAAssertSDeassertTester.vhd
	TOPLEVEL=pllaassertsdeasserttester
endif

MODULE=PllAAssertSDeassertTester

#SIM_ARGS += --vcd=ghdl.vcd

include ../common/Makefile.sim
