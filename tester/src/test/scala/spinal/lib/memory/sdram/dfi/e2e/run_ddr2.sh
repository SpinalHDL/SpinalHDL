#!/usr/bin/env bash
#=============================================================================
# run_ddr2.sh -- build + run the DFI DDR2 e2e co-simulation.
#
# Pipeline:
#   1. SpinalVerilog codegen: export the SpinalHDL command encoder
#      (DfiLegacySimPhy, DDR2-configured) via `sbt runMain Ddr2E2eGen`.
#   2. iverilog: compile the generated encoder + the ORIGINAL Micron ddr2.v
#      + the SystemVerilog testbench into one vvp (no Verilator, no patched
#      model derivative).
#   3. vvp: run the testbench; expect "[e2e] PASS" and no model ERROR lines.
#
# Requires: sbt, iverilog + vvp (Icarus Verilog) on PATH.
#
# The proprietary Micron model is referenced BY PATH only (never copied into
# this tree). Its location is overridable via $MICRON_DDR2_DIR; it defaults to
# the in-repo copy.
#=============================================================================
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

E2E=tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e
PARA="${MICRON_DDR2_DIR:-$ROOT/tester/src/test/python/spinal/SdramXdr/Ddr2ModelTester}"
MODEL="$PARA/ddr2.v"
PARAM_H="$PARA/ddr2_parameters.vh"
BUILD="$E2E/build_ddr2"
GEN_V="$BUILD/DfiLegacySimPhy.v"
VVP="$BUILD/e2e_ddr2.vvp"

if [ ! -f "$MODEL" ]; then
  echo "ERROR: Micron ddr2.v not found at $MODEL" >&2
  echo "       Point MICRON_DDR2_DIR at a directory containing ddr2.v and" >&2
  echo "       ddr2_parameters.vh." >&2
  exit 1
fi
if [ ! -f "$PARAM_H" ]; then
  echo "ERROR: ddr2_parameters.vh not found at $PARAM_H" >&2
  exit 1
fi

mkdir -p "$BUILD"
export DFI_E2E_GEN_DIR="$BUILD"

echo "== [1/3] SpinalVerilog codegen ($GEN_V) =="
sbt -batch "tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.Ddr2E2eGen"
if [ ! -f "$GEN_V" ]; then
  echo "ERROR: codegen did not produce $GEN_V" >&2
  exit 1
fi

echo "== [2/3] iverilog compile (encoder + original ddr2.v + tb) =="
iverilog -g2012 -I "$PARA" -s e2e_tb \
  "$GEN_V" "$MODEL" "$E2E/tb/e2e_tb_ddr2.sv" \
  -o "$VVP"

echo "== [3/3] vvp run =="
LOG="$BUILD/run.log"
vvp "$VVP" | tee "$LOG"

# The model self-dumps wave.vcd into the cwd; keep the tree clean.
rm -f "$ROOT/wave.vcd"

if grep -q "\[e2e\] PASS" "$LOG" && ! grep -q "ERROR:" "$LOG"; then
  echo "== DDR2 e2e: PASS (no model timing errors) =="
elif grep -q "\[e2e\] PASS" "$LOG"; then
  echo "== DDR2 e2e: PASS with model warnings/errors above =="
  exit 1
else
  echo "== DDR2 e2e: FAIL =="
  exit 1
fi
