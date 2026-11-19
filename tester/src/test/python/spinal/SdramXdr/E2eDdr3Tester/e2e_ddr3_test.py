"""
e2e_ddr3_test.py -- cocotb e2e test (repo's standard test facility).

SpinalHDL DFI command encoder (DfiLegacySimPhy, DDR3 config, generated to
plain Verilog) wired LIVE to the original, unmodified Micron ddr3.v
behavioral model, simulated by Icarus Verilog (SIM=icarus). The model's own
live timing checker runs for real; the run script (run.sh) fails the test if
any `u_mem ... ERROR:` line appears.

Mirrors the validated standalone SV bench (dfi/e2e/tb/e2e_tb.sv) step for
step. Protocol essentials:
  * tCK = 3.0 ns (SG25 / DDR3-800E window); DQS period = tCK (1.5 ns high,
    1.5 ns low), edges 0.75 ns after each CK negedge (phase-locked to the
    SV TB's `cnt[0]` grid: posedges at 0.75 + 1.5k ns)
  * Commands driven on CK negedge, released to NOP on the next negedge
  * MR0 = 0x110 (CL=5 + DLL reset), MR1 = 0x004 (DLL en/ODT), MR2 = 0x000
    (CWL=5); a FINAL MR3 write (addr[2]=0) clears the model's GLOBAL mpr_en,
    which MR0's CL field would otherwise leave latched (ddr3.v sets
    mpr_en = addr[2] on EVERY MRS) and make it reject ACT/WR
  * ZQCAL-long then gap(1100) for tZQinit; gap(512) after MR0 for tDLLK
  * WR data: 24 DQS toggle edges at 1.5 ns spacing (lead+data+trail, ~12 tCK);
    a 20-CK gap separates the write burst from the following READ (tWTR)
"""
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge

HALF_PS = 1500          # half CK period (CK period = 3.0 ns)
DQS_PS = 750            # DQS strobe grid (0.75 ns -> 400 MHz)
BL = 8                  # burst length
ROW = 0x010             # test row
PREA = 0x400            # PRECHARGE-ALL / ZQCAL-long (addr[10]=1)
FILL0 = 0xA000
FILL1 = 0xB000

# Mode registers (cross-checked against the model's MRS decode-echo log)
MR2_CWL5 = 0x000        # bank2: CWL = 5
MR1_DLL  = 0x004        # bank1: DLL enable, ODT
MR0_CL5  = 0x110        # bank0: CL = 5 + DLL reset
MR3_CLR  = 0x000        # bank3: clears the global mpr_en latch


def _dup(bit):
    return (bit << 1) | bit


def _safe_int(val, default=None):
    """int(...) raises ValueError on x/z bits; return default instead."""
    try:
        return int(val)
    except (ValueError, TypeError):
        return default


def _nop(dut, cke=1):
    dut.ctl_rasN.value = 0b11
    dut.ctl_casN.value = 0b11
    dut.ctl_weN.value = 0b11
    dut.ctl_cke.value = _dup(cke)
    dut.ctl_csN.value = 0b00
    dut.ctl_odt.value = 0b11
    dut.ctl_bank.value = 0
    dut.ctl_addr.value = 0


def _cmd(dut, rasN, casN, weN, bank, addr):
    # Both DFI phase slots driven identically (encoder output phase-independent).
    dut.ctl_rasN.value = _dup(rasN)
    dut.ctl_casN.value = _dup(casN)
    dut.ctl_weN.value = _dup(weN)
    dut.ctl_cke.value = 0b11
    dut.ctl_csN.value = 0b00
    dut.ctl_odt.value = 0b11
    dut.ctl_bank.value = (bank << 3) | bank
    dut.ctl_addr.value = (addr << 13) | addr


async def issue(dut, rasN, casN, weN, bank, addr):
    """Single-cycle command: drive at CK negedge, decode at posedge,
    release NOP at the following negedge."""
    await FallingEdge(dut.ck)
    _cmd(dut, rasN, casN, weN, bank, addr)
    await RisingEdge(dut.ck)       # model decodes here
    await FallingEdge(dut.ck)
    _nop(dut)


async def gap(dut, n):
    for _ in range(n):
        await RisingEdge(dut.ck)


async def write_fill(dut, col, fill):
    """WRITE `col` with constant `fill`: 24 DQS toggle edges at 1.5 ns
    (half-CK) spacing, first edge 0.75 ns after the CK negedge (phase-locked
    to the SV TB), then release the bus. DQS period must equal tCK — a
    faster grid trips the model's tDQSH/tDQSL checks."""
    await issue(dut, 1, 0, 0, 0, col)            # WRITE (returns at CK negedge)
    dut.wr_en.value = 1
    dut.wb_dq.value = fill
    dut.wb_dqs.value = 0b00
    await Timer(750, 'ps')                       # onto the DQS grid
    dqs = 0
    for i in range(24):
        dqs ^= 1
        dut.wb_dqs.value = _dup(dqs)             # BOTH groups in phase
        if i < 23:
            await Timer(HALF_PS, 'ps')
    dut.wr_en.value = 0


async def read_verify(dut, col, exp):
    """READ `col`, capture non-high-Z DQ beats on the 0.75 ns grid, verify a
    contiguous run of BL beats == exp."""
    await issue(dut, 1, 0, 1, 0, col)            # READ
    caps = []
    for _ in range(64):
        await Timer(DQS_PS, 'ps')
        if _safe_int(dut.dq_valid.value, 0) == 1:
            v = _safe_int(dut.dq_obs.value)
            if v is not None:
                caps.append(v)
    ok, run = False, 0
    for v in caps:
        run = run + 1 if v == exp else 0
        if run >= BL:
            ok = True
    print(f"[e2e] col={col:03x} captured={len(caps)} ok={'1' if ok else '0'}")
    return ok


async def round_trip(dut, col, fill):
    await issue(dut, 0, 1, 1, 0, ROW)            # ACTIVATE bank0
    await gap(dut, 20)                           # tRCD margin
    await write_fill(dut, col, fill)
    await gap(dut, 20)                           # burst + tWTR margin
    ok = await read_verify(dut, col, fill)
    await issue(dut, 0, 1, 0, 0, PREA)           # PRECHARGE ALL
    await gap(dut, 20)                           # tRP margin
    return ok


async def _watchdog():
    # NOTE: cocotb 1.8.1 Timer unit is 'sec', NOT 's' ('s' raises at construction)
    await Timer(1, 'sec')
    raise RuntimeError('[e2e] TIMEOUT (1 s sim) -- aborting')


async def _clk(dut):
    """Free-running CK, posedges at 1.5 + 3k ns (phase-locked to t=0)."""
    while True:
        await Timer(HALF_PS, 'ps')
        dut.ck.value = not dut.ck.value


@cocotb.test()
async def e2e_ddr3_roundtrip(dut):
    wd = cocotb.start_soon(_watchdog())
    clk_task = None
    try:
        # ---- t=0 settle: one change event latches the model's `_in`
        #      input mirrors (CKE inactive, command pins NOP, reset held). ----
        dut.mcRst.value = 1
        dut.ck.value = 0
        _nop(dut, cke=0)
        dut.wr_en.value = 0
        dut.wb_dq.value = 0
        dut.wb_dqs.value = 0

        clk_task = cocotb.start_soon(_clk(dut))

        # Hold CKE inactive + reset asserted for > 10 ns, then release reset.
        await gap(dut, 20)
        dut.mcRst.value = 0
        await gap(dut, 8)                        # CKE low post-reset

        # ---- CKE active with a clean NOP, then tXPR ----
        await FallingEdge(dut.ck)
        _nop(dut, cke=1)
        await gap(dut, 64)                       # tXPR = 170 ns (~57 tCK)

        # ---- init: ZQCAL-long, then MRS (MR2, MR1, MR0) + MR3 housekeeping ----
        await issue(dut, 1, 1, 0, 0, PREA)       # ZQCAL long (A10=1)
        await gap(dut, 1100)                     # tZQinit ~ 1024 tCK
        await issue(dut, 0, 0, 0, 2, MR2_CWL5)   # MRS MR2: CWL = 5
        await gap(dut, 16)                       # tMRD/tMOD margin
        await issue(dut, 0, 0, 0, 1, MR1_DLL)    # MRS MR1: DLL en, ODT
        await gap(dut, 16)
        await issue(dut, 0, 0, 0, 0, MR0_CL5)    # MRS MR0: CL = 5 + DLL reset
        await gap(dut, 16)
        # MR0's CL bit (addr[2]=1) latches the model's GLOBAL mpr_en (ddr3.v
        # sets mpr_en = addr[2] on EVERY MRS). Clear it via a final MR3 write
        # with addr[2]=0, then wait tDLLK for the DLL to re-lock.
        await issue(dut, 0, 0, 0, 3, MR3_CLR)    # MRS MR3: clear mpr_en
        await gap(dut, 512)                      # tDLLK = 512 tCK

        # ---- round-trip column 0, then column 8 (disjoint bursts) ----
        ok0 = await round_trip(dut, 0x000, FILL0)
        ok1 = await round_trip(dut, 0x008, FILL1)

        # Location-specificity: re-read col0 -> must STILL be FILL0.
        await issue(dut, 0, 1, 1, 0, ROW)
        await gap(dut, 20)
        okc = await read_verify(dut, 0x000, FILL0)
        await issue(dut, 0, 1, 0, 0, PREA)
        await gap(dut, 20)

        print(f"[e2e] col0 ok={'1' if ok0 else '0'}  col8 ok={'1' if ok1 else '0'}  "
              f"col0-still-A000 ok={'1' if okc else '0'}")
        assert ok0 and ok1 and okc, 'round-trip incomplete'
        print("[e2e] PASS: WR/RD round-trip vs original ddr3.v (2 locations)")
    finally:
        wd.kill()
        if clk_task is not None:
            clk_task.kill()
