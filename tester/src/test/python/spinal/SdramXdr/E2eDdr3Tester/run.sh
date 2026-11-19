#!/usr/bin/env bash
#
# DDR3 e2e via the repo's cocotb facility (SIM=icarus).
# PASS gate: cocotb test passes AND zero model timing-error lines
# (the pristine Micron ddr3.v prints "<hier>.u_mem... ERROR: ..." via
# its built-in live checker).
#
set -u
cd "$(dirname "$0")"

LOG=./e2e_run.log
rm -f "$LOG" result.xml wave.vcd

make SIM=icarus TOPLEVEL_LANG=verilog 2>&1 | tee "$LOG"
rc=${PIPESTATUS[0]}

rm -f wave.vcd sim_build/wave.vcd

pass=0
if [ "$rc" -eq 0 ] && grep -q '\[e2e\] PASS' "$LOG" && ! grep -q 'u_mem.*ERROR:' "$LOG"; then
  pass=1
fi

if [ "$pass" -eq 1 ]; then
  echo "== DDR3 e2e (cocotb/icarus): PASS (no model timing errors) =="
else
  echo "== DDR3 e2e (cocotb/icarus): FAIL (make_rc=$rc) =="
  grep -E 'u_mem.*ERROR:|\[e2e\]|FAILED|Traceback|Error' "$LOG" | tail -30 || true
  exit 1
fi
