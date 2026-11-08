# DFI DDR3 / DDR2 end-to-end co-simulation (SpinalHDL encoder ⇄ Micron models)

Real-time co-simulation of the SpinalHDL **DFI command encoder**
(`DfiLegacySimPhy`) against the **original, unmodified Micron `ddr3.v`
behavioral model**, driven as a live `VerilogBlackBox`-style instance inside a
single [Icarus Verilog](https://steveicarus.github.io/iverilog/) bench. A
second bench runs the same encoder against the original **`ddr2.v`** model
(see the DDR2 section below).

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

## DDR2 variant (⇄ Micron ddr2.v)

Same methodology, mirrored files (kept separate because the init sequences,
MR encodings, and timing constraints differ substantially):

| File | Role |
|------|------|
| `Ddr2E2eGen.scala` | codegen — emits `build_ddr2/DfiLegacySimPhy.v` with the DDR2 `SdramConfig` (x16, 13-bit row, BA=3). |
| `tb/e2e_tb_ddr2.sv` | pin-level TB against the pristine `ddr2.v` (`tester/src/test/python/spinal/SdramXdr/Ddr2ModelTester/`). |
| `run_ddr2.sh` | `sbt` codegen → `iverilog -g2012` → `vvp`; PASS = TB PASS line **and** zero model `ERROR:` lines. |

```bash
bash tester/src/test/scala/spinal/lib/memory/sdram/dfi/e2e/run_ddr2.sh
```

DDR2 specifics (vs DDR3):

- **`tCK = 5.0 ns`** — the model's default speed bin (no `` `define ``) is
  `sg5E` with `TCK_MIN = 5000 ps`.
- **Width defaults to x16**: `dq[15:0]`, `dqs[1:0]`, `dm_rdqs[1:0]` (tie DM to 0).
- **Command encoding** `{ras_n, cas_n, we_n}`: ACT=`{0,1,1}`, WR=`{1,0,0}`,
  RD=`{1,0,1}`, PRE=`{0,1,0}`, REF=`{0,0,1}`, MRS=`{0,0,0}`; PRE-ALL via `addr[10]`.
- **MR values cross-checked against the model's MRS decode-echo log**:
  MR0 = `0x953` (BL8 seq, CL5, WR5, DLL reset) → `0x853` (normal);
  MR1 = `0x380` (OCD=111) → `0x000` (final). Manual bit-packing is error-
  prone — verify each constant against the echo immediately after MRS.
- **Timing traps**: `tRFC_MIN = 127.5 ns` (space REFs and the following
  command ≥ 150 ns), and command pins must be released to NOP on the CK
  **negedge** after decode — changing a pin at the sampling posedge trips
  the 350 ps `tIS` setup check.
- Init mirrors the model's own `initialize` task: PRE-ALL, MRS MR3/MR2/MR1/
  MR0(reset), PRE-ALL, 2× REF, MRS MR0(norm), MR1 OCD 111→000, 250 tCK DLL lock.

## Licensing

This directory adds **no proprietary files**. The Micron model remains a
pre-existing in-repo asset (vendored by the project founder) and is consumed
here strictly by path. Regenerable artifacts under `build/` are not committed.
