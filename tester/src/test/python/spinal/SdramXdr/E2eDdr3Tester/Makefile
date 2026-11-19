# E2eDdr3Tester -- SpinalHDL DFI encoder vs original Micron ddr3.v (cocotb, Icarus)
#
# Entry:  make SIM=icarus          (or ./run.sh, which adds the PASS gate)
#
# Requires the SpinalHDL-generated encoder; it is auto-coded if missing:
#   <repo>/tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e/build/DfiLegacySimPhy.v

REPO_ROOT   := $(abspath ../../../../../../..)
E2E_SCALA   := $(REPO_ROOT)/tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e
ENCODER     := $(E2E_SCALA)/build/DfiLegacySimPhy.v
MODEL_DIR   := $(abspath ../Ddr3ModelTester)

# The $(ENCODER) rule below would otherwise become the default goal (first
# target in the file); pin the default to the cocotb sim entry.
.DEFAULT_GOAL := sim

# defines_ddr3.v FIRST: its `define den2048Mb must precede ddr3.v so the
# model's top-of-file `ifdef selects the 2048Mb parameter header. The include
# dir goes to VERILOG_INCLUDE_DIRS (iverilog-only), NOT EXTRA_ARGS (which
# cocotb 1.8 also forwards to vvp).
VERILOG_SOURCES += defines_ddr3.v e2e_top_ddr3.v $(ENCODER) $(MODEL_DIR)/ddr3.v
VERILOG_INCLUDE_DIRS += $(MODEL_DIR)
TOPLEVEL=e2e_top_ddr3
MODULE=e2e_ddr3_test

.PHONY: codegen
codegen:
	cd "$(REPO_ROOT)" && DFI_E2E_GEN_DIR="$(E2E_SCALA)/build" \
		sbt "tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.DfiE2eGen"

$(ENCODER):
	@test -f "$(ENCODER)" || $(MAKE) --no-print-directory codegen

# Ensure the encoder exists before the cocotb compile step.
sim: $(ENCODER)

include $(shell cocotb-config --makefiles)/Makefile.sim
