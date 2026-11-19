# E2eDdr2Tester -- SpinalHDL DFI encoder vs original Micron ddr2.v (cocotb, Icarus)
#
# Entry:  make SIM=icarus          (or ./run.sh, which adds the PASS gate)
#
# Requires the SpinalHDL-generated encoder; it is auto-coded if missing:
#   <repo>/tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e/build_ddr2/DfiLegacySimPhy.v

REPO_ROOT   := $(abspath ../../../../../../..)
E2E_SCALA   := $(REPO_ROOT)/tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e
ENCODER     := $(E2E_SCALA)/build_ddr2/DfiLegacySimPhy.v
MODEL_DIR   := $(abspath ../Ddr2ModelTester)

# The $(ENCODER) rule below would otherwise become the default goal (first
# target in the file); pin the default to the cocotb sim entry.
.DEFAULT_GOAL := sim

VERILOG_SOURCES += e2e_top_ddr2.v $(ENCODER) $(MODEL_DIR)/ddr2.v
VERILOG_INCLUDE_DIRS += $(MODEL_DIR)
TOPLEVEL=e2e_top_ddr2
MODULE=e2e_ddr2_test
# NOTE: no EXTRA_ARGS -- cocotb 1.8 forwards them to BOTH iverilog and
# vvp, and vvp rejects -g2012. All sources are Verilog-2001 compatible.

.PHONY: codegen
codegen:
	cd "$(REPO_ROOT)" && DFI_E2E_GEN_DIR="$(E2E_SCALA)/build_ddr2" \
		sbt "tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.Ddr2E2eGen"

$(ENCODER):
	@test -f "$(ENCODER)" || $(MAKE) --no-print-directory codegen

# Ensure the encoder exists before the cocotb compile step.
sim: $(ENCODER)

include $(shell cocotb-config --makefiles)/Makefile.sim
