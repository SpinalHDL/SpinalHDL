# DFI DDR3 end-to-end co-simulation (SpinalHDL encoder ⇄ Micron ddr3.v)

Real-time co-simulation of the SpinalHDL **DFI command encoder**
(`DfiLegacySimPhy`) against the **original, unmodified Micron `ddr3.v`
behavioral model**, driven as a live `VerilogBlackBox`-style instance inside a
single [Icarus Verilog](https://steveicarus.github.io/iverilog/) bench.

This proves the DFI → DDR3 command/data path end to end: reset, CKE/power-up,
ZQ calibration, MRS init, write (DQ/DQS burst), read (DQ capture) and a
location-specific read-back — all checked by the vendor model's **own live
timing checker** (no timing relaxations in the model).

## What is in the loop

| File | Role |
|------|------|
| `DfiLegacySimPhy.scala` | SpinalHDL component: DFI phase-mux → DDR3 command pins + `ck`/`ck_n`. The unit under test. |
| `DfiE2eGen.scala` | `sbt` codegen main — emits `build/DfiLegacySimPhy.v` via `SpinalConfig(..., mode = Verilog)`. |
| `tb/e2e_tb.sv` | SystemVerilog testbench: clocks, DQ/DQS/DM tri-state bus, init sequence, ACT→WR→RD round-trip + location check. |
| `run.sh` | Drives the 3-step pipeline below. |

The Micron model (`ddr3.v` + `2048Mb_ddr3_parameters.vh`) is referenced **by
path only** and is **never copied into this tree**.

## Prerequisites

- `sbt` (the SpinalHDL build), `iverilog` + `vvp` (Icarus Verilog ≥ 11) on `PATH`.
- The Micron DDR3 model. By default `run.sh` uses the in-repo copy at
  `tester/src/test/python/spinal/SdramXdr/Ddr3ModelTester/`. To use a pristine
  vendor download elsewhere, point `MICRON_DDR3_DIR` at a directory containing
  `ddr3.v` and `2048Mb_ddr3_parameters.vh`:

  ```bash
  export MICRON_DDR3_DIR=/path/to/micron/ddr3_model
  ```

## Run

```bash
bash tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e/run.sh
```

Pipeline:

1. **Codegen** — `sbt "tester/Test/runMain spinal.lib.memory.sdram.dfi.e2e.DfiE2eGen"`
   writes `build/DfiLegacySimPhy.v` (our own regenerable RTL).
2. **Compile** — `iverilog -g2012 -D den2048Mb -I "$PARA" -s e2e_tb`
   the generated encoder **+ the original `ddr3.v` + the testbench** into one `.vvp`.
3. **Run** — `vvp build/e2e.vvp`.

Expected tail of the run:

```
[e2e] col0 ok=1  col8 ok=1  col0-still-A000 ok=1
[e2e] PASS: WR/RD round-trip vs original ddr3.v (2 locations)
```

with **no `ERROR:` timing violations** from the model.

## Why Icarus, not Verilator

The Micron model is written in behavioral Verilog that leans on 4-state values
(`z`/`x`), `#delay`, `bufif1`, `$dist_uniform`, and an **advancing `$time`** for
its built-in timing checks. Verilator (2-state, cycle-based, no native
`--timing` at 4.x) needs a patched model derivative — which is a derivative of
proprietary Micron code and therefore **cannot be committed** to this
open-source repo. Icarus runs the pristine model unmodified, so this test
bundles **no proprietary bytes** and exercises the model's real timing checker.

## Key model interactions (the gotchas, documented for maintainers)

- **Pin settling.** The model mirrors every input into an `_in` copy via
  `always @(sig) _in <= #0 sig`, so each `_in` stays `x` until that pin first
  *changes*. The testbench control regs therefore start `x` (no static init) and
  are settled procedurally at `t=0`; that one change event latches all `_in`
  copies to defined values before the model's reset / CKE-active checks run.
- **Data mask.** For the x16 config `dm_tdqs` is an *input* mask; the model masks
  each write byte by `~dm_in`. A floating `dm_tdqs` (`zz` → `~zz = x`) makes every
  write store `x`. It is tied to `2'b00` so writes land.
- **`mpr_en` is global.** `ddr3.v` sets `mpr_en = addr[2]` on **every** MRS, not
  only MR3. MR0's CL=5 field drives `addr[2]`, so a final MR3 write with
  `addr[2]=0` is issued to clear it — otherwise the model rejects ACT/WR.
- **Init timing.** Gaps sized to the live checks: `tXPR` (170 ns) after CKE
  active, `tZQinit` (~1024 tCK) after ZQCL, `tDLLK` (512 tCK) after the MR0 DLL
  reset, `tMRD`/`tMOD` between MRS. `tCK = 3.0 ns` sits in the SG25 window.

## Licensing

This directory adds **no proprietary files**. The Micron model remains a
pre-existing in-repo asset (vendored by the project founder) and is consumed
here strictly by path. Regenerable artifacts under `build/` are not committed.
