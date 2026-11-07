#!/usr/bin/env bash
#=============================================================================
# run.sh -- build + run the DFI DDR3 e2e co-simulation.
#
# Pipeline:
#   1. SpinalVerilog codegen: export the SpinalHDL command encoder
#      (DfiLegacySimPhy) to Verilog via `sbt runMain DfiE2eGen`.
#   2. iverilog: compile the generated encoder + the ORIGINAL Micron ddr3.v
#      + the SystemVerilog testbench into one vvp (no Verilator, no patched
#      model derivative).
#   3. vvp: run the testbench; expect "[e2e] PASS".
#
# Requires: sbt, iverilog + vvp (Icarus Verilog) on PATH.
#
# The proprietary Micron model is referenced BY PATH only (never copied into
# this tree). Its location is overridable via $MICRON_DDR3_DIR; it defaults to
# the in-repo copy. Set MICRON_DDR3_DIR to a pristine vendor directory if the
# in-repo copy is unavailable.
#=============================================================================
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

E2E=tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e
PARA="${MICRON_DDR3_DIR:-$ROOT/tester/src/test/python/spinal/SdramXdr/Ddr3ModelTester}"
MODEL="$PARA/ddr3.v"
PARAM_H="$PARA/2048Mb_ddr3_parameters.vh"
BUILD="$E2E/build"
GEN_V="$BUILD/DfiLegacySimPhy.v"
VVP="$BUILD/e2e.vvp"

if [ ! -f "$MODEL" ]; then
  echo "ERROR: Micron ddr3.v not found at $MODEL" >&2
  echo "       Point MICRON_DDR3_DIR at a directory containing ddr3.v and the" >&2
  echo "       2048Mb_ddr3_parameters.vh param file." >&2
  exit 1
fi
if [ ! -f "$PARAM_H" ]; then
  echo "WARN: param file not found at $PARAM_H (needed for -D den2048Mb)" >&2
fi

mkdir -p "$BUILD"
export DFI_E2E_GEN_DIR="$BUILD"

echo "== [1/3] SpinalVerilog codegen ($GEN_V) =="
sbt -batch "tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.DfiE2eGen"
# Belt-and-suspenders: if the generator defaulted to the cwd, pick it up.
if [ ! -f "$GEN_V" ] && [ -f "$ROOT/DfiLegacySimPhy.v" ]; then
  mv -f "$ROOT/DfiLegacySimPhy.v" "$GEN_V"
fi

echo "== [2/3] iverilog compile (encoder + original ddr3.v + tb) =="
iverilog -g2012 -D den2048Mb -I "$PARA" -s e2e_tb \
  "$GEN_V" "$MODEL" "$E2E/tb/e2e_tb.sv" \
  -o "$VVP"

echo "== [3/3] vvp run =="
vvp "$VVP"
