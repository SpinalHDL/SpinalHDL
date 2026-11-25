# DFI DDR3 / DDR2 end-to-end co-simulation (SpinalHDL encoder ⇄ Micron models)

End-to-end co-simulation of the SpinalHDL **DFI command encoder**
(`DfiLegacySimPhy`) against the **original, unmodified Micron `ddr3.v` /
`ddr2.v` behavioral models, run through the repo's standard
[cocotb](https://www.cocotb.org)/Icarus Verilog test facility
(`make SIM=icarus`). Two independent testers cover the two SDRAM families:

| Test | Directory | Model |
|------|-----------|-------|
| DDR3 | `tester/src/test/python/spinal/SdramXdr/E2eDdr3Tester/` | `../Ddr3ModelTester/ddr3.v` |
| DDR2 | `tester/src/test/python/spinal/SdramXdr/E2eDdr2Tester/` | `../Ddr2ModelTester/ddr2.v` |

Each proves the DFI → SDRAM command/data path end to end: power-up/init
sequence, write (DQ/DQS burst), read (DQ capture) and a location-specific
read-back — all checked by the vendor model's **own live timing checker**
(no timing relaxations in the model).

## What is in the loop

Shared Scala sources (this directory):

| File | Role |
|------|------|
| `DfiLegacySimPhy.scala` | SpinalHDL component: DFI phase-mux → SDRAM command pins + `ck`/`ck_n`. The unit under test. |
| `DfiE2eGen.scala` | `sbt` codegen main — emits `build/DfiLegacySimPhy.v` (DDR3 `SdramConfig`). |
| `Ddr2E2eGen.scala` | `sbt` codegen main — emits `build_ddr2/DfiLegacySimPhy.v` (DDR2 `SdramConfig`). |

Per-tester files (e.g. `E2eDdr3Tester/`, mirrored in `E2eDdr2Tester/`):

| File | Role |
|------|------|
| `e2e_top_ddr3.v` / `e2e_top_ddr2.v` | Structural wrapper: generated encoder ⇄ pristine Micron model, over tri-state DQ/DQS buses with `dq_obs`/`dq_valid` observation ports. |
| `e2e_ddr3_test.py` / `e2e_ddr2_test.py` | cocotb test: phase-locked CK, init sequence, WR/RD round-trip at 2 columns + location-specificity re-read. |
| `Makefile` | cocotb makefile (`.DEFAULT_GOAL := sim`); auto-runs `sbt` codegen if the generated encoder is missing. |
| `run.sh` | Entry: `make SIM=icarus`, then PASS gate = cocotb pass **and** `[e2e] PASS` line **and** zero model `ERROR:` lines. |
| `defines_ddr3.v` (DDR3 only) | `` `define den2048Mb `` placed first in `VERILOG_SOURCES` so the model selects its 2048 Mb parameter header. |

The Micron models are vendored in-repo and referenced **by path only**;
nothing proprietary is ever copied into these trees.

## Prerequisites

- `sbt` (SpinalHDL codegen), `iverilog` + `vvp` on `PATH`.
- Python with `cocotb` 1.8.1 (the repo-standard version).
- Models: in-repo by default. To use a pristine vendor download elsewhere,
  point `MICRON_DDR3_DIR` at a directory containing `ddr3.v` and
  `2048Mb_ddr3_parameters.vh`.

## Run

```bash
bash tester/src/test/python/spinal/SdramXdr/E2eDdr3Tester/run.sh
bash tester/src/test/python/spinal/SdramXdr/E2eDdr2Tester/run.sh
```

Pipeline (identical shape for both): **codegen** (only if the generated
encoder `.v` is missing) → **compile** (iverilog: wrapper + generated encoder
+ pristine model) → **run** (cocotb on vvp). Wall time ≈ 40 s.

Expected tail:

```
[e2e] col0 ok=1  col8 ok=1  col0-still-A000 ok=1
[e2e] PASS: WR/RD round-trip vs original ddr3.v (2 locations)
```

with **no `ERROR:` timing violations** from the model.

## Why Icarus, not Verilator

The Micron models are written in behavioral Verilog that leans on 4-state
values (`z`/`x`), `#delay`, `bufif1`, `$dist_uniform`, and an **advancing
`$time`** for their built-in timing checks. Verilator (2-state, cycle-based,
no native `--timing` at 4.x) needs a patched model derivative — which is a
derivative of proprietary Micron code and therefore **cannot be committed**
to this open-source repo. Icarus runs the pristine models unmodified, so this
test bundles **no proprietary bytes** and exercises the models' real timing
checkers.

## Key model interactions (gotchas baked into the tests — do not "simplify")

Common to both models:

- **Pin settling.** The models mirror every input into an `_in` copy via
  `always @(sig) _in <= #0 sig`, so each `_in` stays `x` until that pin first
  *changes*. Control regs therefore start undefined and are settled
  procedurally at `t=0`; that one change event latches all `_in` copies to
  defined values before the models' reset / CKE-active checks run.
- **Commands released on the CK negedge.** Command pins must return to NOP on
  the CK **negedge** after decode — changing a pin at the sampling posedge
  trips the model's `tIS` input-setup check.
- **Both DQS bits must toggle.** Drive `dqs` and `dqs_n` for all byte lanes
  in phase (`_dup(dqs)`). The models check each `dqs[i]` bit independently;
  driving only bit 0 trips `DQS bit 1 latching edge required`.
- **DQS period must equal one tCK** (half-CK high, half-CK low), phase-locked
  to CK. Driving DQS at 2× tCK trips `tDQSH`/`tDQSL`/`tDSS`/`tWPRE`
  violations in bursts.
- **Write→READ gap (tWTR).** A 20-tCK gap separates the write burst from the
  following READ; releasing the write too close to the READ produces spurious
  `tWTR violation during Read`.
- **DM tie-off.** For the x16 configs `dm_tdqs`/`dm_rdqs` are *inputs*; a
  floating value makes every write store `x`. Tied to 0.

cocotb/Icarus plumbing:

- cocotb 1.8.1 `Timer` unit string is `'sec'` (not `'s'`) — a wrong unit
  raises at construction and cascades into a scheduler crash + vvp segfault.
- cocotb 1.8.1 forwards `EXTRA_ARGS` to **both** `iverilog` and `vvp`; `vvp`
  rejects compiler flags like `-g2012`/`-D…`. Keep sources Verilog-2001 clean
  and put defines in a `.v` file at the front of `VERILOG_SOURCES` instead.
- Pin `.DEFAULT_GOAL := sim` in the Makefile — a custom `$(ENCODER):` rule as
  the first target would otherwise steal the default goal.

DDR3 specifics:

- **`mpr_en` is global.** `ddr3.v` sets `mpr_en = addr[2]` on **every** MRS,
  not only MR3. MR0's CL=5 field drives `addr[2]`, so a final MR3 write with
  `addr[2]=0` is issued to clear it — otherwise the model rejects ACT/WR.
- **Init timing** (live-checked): `tXPR` (170 ns) after CKE active,
  `tZQinit` (~1024 tCK) after ZQCL, `tDLLK` (512 tCK) after the MR0 DLL
  reset, `tMRD`/`tMOD` between MRS. `tCK = 3.0 ns` sits in the SG25 window.
- **Parameter header:** the unmodified `ddr3.v` needs `` `den2048Mb `` defined
  *before* inclusion to pick the 2048 Mb parameter header (see
  `defines_ddr3.v`).

DDR2 specifics:

- **`tCK = 5.0 ns`** — the model's default speed bin (no `` `define ``) is
  `sg5E` with `TCK_MIN = 5000 ps`. Width defaults to x16 (`dq[15:0]`,
  `dqs[1:0]`, `dm_rdqs[1:0]`).
- **Command encoding** `{ras_n, cas_n, we_n}`: ACT=`{0,1,1}`, WR=`{1,0,0}`,
  RD=`{1,0,1}`, PRE=`{0,1,0}`, REF=`{0,0,1}`, MRS=`{0,0,0}`; PRE-ALL via
  `addr[10]`.
- **MR values cross-checked against the model's MRS decode-echo log**:
  MR0 = `0x953` (BL8 seq, CL5, WR5, DLL reset) → `0x853` (normal);
  MR1 = `0x380` (OCD=111) → `0x000` (final). Manual bit-packing is error-
  prone — verify each constant against the echo immediately after MRS.
- **Timing traps:** `tRFC_MIN = 127.5 ns` (space REFs and the following
  command ≥ 150 ns). Init mirrors the model's own `initialize` task: PRE-ALL,
  MRS MR3/MR2/MR1/MR0(reset), PRE-ALL, 2× REF, MRS MR0(norm), MR1 OCD
  111→000, then 250 tCK of DLL lock.

## Artifacts

Regenerable outputs are gitignored and never committed:
`build/` + `build_ddr2/` (codegen caches) and each tester's `sim_build/`,
`results.xml`, `e2e_run.log`.

## Licensing

These directories add **no proprietary files**; the Micron models remain
pre-existing in-repo assets consumed by path only.
