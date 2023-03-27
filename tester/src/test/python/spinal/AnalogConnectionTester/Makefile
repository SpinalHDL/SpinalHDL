include ../common/Makefile.def

ifeq ($(TOPLEVEL_LANG),verilog)
    VERILOG_SOURCES += $(SPINALROOT)/AnalogConnectionTester.v PortBlackBox.v
    TOPLEVEL=AnalogConnectionTester
endif
ifeq ($(TOPLEVEL_LANG),vhdl)
    VHDL_SOURCES += $(SPINALROOT)/AnalogConnectionTester.vhd ${CURDIR}/PortBlackBox.vhd
    TOPLEVEL=analogconnectiontester
endif
MODULE=AnalogConnectionTester

include ../common/Makefile.sim