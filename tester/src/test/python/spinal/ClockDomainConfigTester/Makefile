include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
	VERILOG_SOURCES += $(SPINALROOT)/ClockDomainConfigTester.v
	TOPLEVEL=ClockDomainConfigTester
endif

ifeq ($(TOPLEVEL_LANG),vhdl)
	VHDL_SOURCES += $(SPINALROOT)/ClockDomainConfigTester.vhd
	TOPLEVEL=clockdomainconfigtester
endif

MODULE=ClockDomainConfigTester

include ../common/Makefile.sim
