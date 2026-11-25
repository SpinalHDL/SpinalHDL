"""
e2e_ddr2_test.py -- cocotb e2e test (repo's standard test facility).

SpinalHDL DFI command encoder (DfiLegacySimPhy, DDR2 config, generated to
plain Verilog) wired LIVE to the original, unmodified Micron ddr2.v
behavioral model, simulated by Icarus Verilog (SIM=icarus). The model's own
built-in $time-based timing checker runs for real; the run script (run.sh)
fails the test if any `u_mem ... ERROR:` line appears.

Protocol (validated against the model's timing checker):
  * tCK = 5.0 ns (model default speed bin sg5E, TCK_MIN = 5000 ps)
  * Commands driven on CK negedge, released to NOP on the NEXT negedge
    (a pin change AT the sampling posedge trips the 350 ps tIS check)
  * WR DQS: 50% duty, 2.5 ns period, DQS posedges coincident with CK
    posedges; 16 toggle edges ending at T1+45.0 ns (postamble covered)
  * REF spacing >= 150 ns (TRFC_MIN = 127.5 ns)
  * MR0 = 0x953 (BL8 seq, CL5, WR5, DLL reset) -> 0x853 (normal);
    MR1 = 0x380 (OCD=111) -> 0x000 (final). Cross-check each constant
    against the model's MRS decode-echo log, not manual bit packing.
"""
import cocotb
from cocotb.triggers import Timer, RisingEdge, FallingEdge

HALF_PS = 2500          # half CK period (CK period = 5.0 ns)
BL = 8                  # burst length
ROW = 0x010             # test row
PREA = 0x400            # PRECHARGE-ALL (addr[10]=1)
FILL0 = 0xA000
FILL1 = 0xB000

MR0_RST = 0x953         # BL8 seq, CL5, WR5, DLL reset
MR0_NORM = 0x853        # BL8 seq, CL5, WR5
MR1_OCD = 0x380         # OCD = 111
MR_ZERO = 0x000


def _dup(bit):
    return (bit << 1) | bit


def _safe_int(val, default=None):
    """int(...) raises ValueError on x/z bits; return default instead."""
    try:
        return int(val)
    except (ValueError, TypeError):
        return default


def _nop(dut):
    dut.ctl_rasN.value = 0b11
    dut.ctl_casN.value = 0b11
    dut.ctl_weN.value = 0b11
    dut.ctl_cke.value = 0b11
    dut.ctl_csN.value = 0b00
    dut.ctl_odt.value = 0b11
    dut.ctl_bank.value = 0
    dut.ctl_addr.value = 0


def _cmd(dut, rasN, casN, weN, bank, addr):
    # Both DFI phase slots driven identically (encoder slices slot 0).
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
    """WRITE `col` with constant `fill`. T1 = WR decode posedge.
    Command released at T1+2.5 (negedge, tIS-safe), DQS toggles at
    T1+5.0..T1+45.0, bus released at T1+45.0."""
    await FallingEdge(dut.ck)                     # T1 - 2.5
    _cmd(dut, 1, 0, 0, 0, col)                    # WRITE
    dut.wr_en.value = 1
    dut.wb_dq.value = fill
    dut.wb_dqs.value = 0b00                      # both DQS groups, in phase
    await Timer(HALF_PS, 'ps')                    # T1: decode + DQS posedge
    await Timer(HALF_PS, 'ps')                    # T1+2.5: CK negedge
    _nop(dut)                                     # release command pins
    dqs = 0
    # 17 toggle edges T1+5.0..T1+45.0 (matches the validated SV TB exactly:
    # 16 toggles then one final hold-release at T1+45.0, late enough for the
    # model's write-end bookkeeping to precede the following READ by TWTR).
    for _ in range(17):
        await Timer(HALF_PS, 'ps')
        dqs ^= 1
        dut.wb_dqs.value = _dup(dqs)              # BOTH groups must toggle (model checks each bit)
    dut.wr_en.value = 0                           # T1 + 45.0


async def read_verify(dut, col, exp):
    """READ `col`, sample the databus on a free-running half-CK grid
    (beat-center phase), verify a contiguous run of BL beats == exp."""
    await issue(dut, 1, 0, 1, 0, col)             # READ
    caps = []
    for _ in range(40):
        await Timer(HALF_PS, 'ps')
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
    await issue(dut, 0, 1, 1, 0, ROW)             # ACTIVATE bank0
    await gap(dut, 10)                            # tRCD margin
    await write_fill(dut, col, fill)
    ok = await read_verify(dut, col, fill)
    await issue(dut, 0, 1, 0, 0, PREA)            # PRECHARGE ALL
    await gap(dut, 10)                            # tRP margin
    return ok


async def _watchdog():
    # NOTE: cocotb 1.8.1 Timer unit is 'sec', NOT 's' ('s' raises at construction)
    await Timer(180, 'sec')
    raise RuntimeError('[e2e] TIMEOUT (180 s sim) -- aborting')


async def _clk(dut):
    """Free-running CK, posedges at 2.5 + 5k ns (phase-locked to t=0)."""
    while True:
        await Timer(HALF_PS, 'ps')
        dut.ck.value = not dut.ck.value


@cocotb.test()
async def e2e_ddr2_roundtrip(dut):
    wd = cocotb.start_soon(_watchdog())
    clk_task = None
    try:
        # ---- t=0 settle: one change event latches the model's `_in`
        #      input mirrors (CKE inactive, command pins NOP). ----
        dut.ck.value = 0
        dut.ctl_rasN.value = 0b11
        dut.ctl_casN.value = 0b11
        dut.ctl_weN.value = 0b11
        dut.ctl_cke.value = 0b00
        dut.ctl_csN.value = 0b00
        dut.ctl_odt.value = 0b11
        dut.ctl_bank.value = 0
        dut.ctl_addr.value = 0
        dut.wr_en.value = 0
        dut.wb_dq.value = 0
        dut.wb_dqs.value = 0

        clk_task = cocotb.start_soon(_clk(dut))

        # ---- wait out the 200 us post-power-up requirement ----
        await Timer(201_000_000, 'ps')

        # ---- CKE active with a clean NOP ----
        await FallingEdge(dut.ck)
        dut.ctl_cke.value = 0b11
        await gap(dut, 4)

        # ---- init sequence (mirrors the model's `initialize` task) ----
        await issue(dut, 0, 1, 0, 0, PREA)        # PRECHARGE ALL
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 3, MR_ZERO)     # MRS MR3
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 2, MR_ZERO)     # MRS MR2
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 1, MR_ZERO)     # MRS MR1 (DLL enable)
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 0, MR0_RST)     # MRS MR0 (CL5 WR5 DLL reset)
        await gap(dut, 4)
        await issue(dut, 0, 1, 0, 0, PREA)        # PRECHARGE ALL
        await gap(dut, 30)                        # >= TRFC_MIN (127.5 ns)
        await issue(dut, 0, 0, 1, 0, 0)           # REFRESH 1
        await gap(dut, 30)
        await issue(dut, 0, 0, 1, 0, 0)           # REFRESH 2
        await gap(dut, 30)
        await issue(dut, 0, 0, 0, 0, MR0_NORM)    # MRS MR0 (no DLL reset)
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 1, MR1_OCD)     # MRS MR1 (OCD = 111)
        await gap(dut, 4)
        await issue(dut, 0, 0, 0, 1, MR_ZERO)     # MRS MR1 (OCD = 000) -> done
        await gap(dut, 250)                       # > TDLLK (200 tCK)

        # ---- round-trip column 0, then column 8 (disjoint bursts) ----
        ok0 = await round_trip(dut, 0x000, FILL0)
        ok1 = await round_trip(dut, 0x008, FILL1)

        # Location-specificity: re-read col0 -> must STILL be FILL0.
        await issue(dut, 0, 1, 1, 0, ROW)
        await gap(dut, 10)
        okc = await read_verify(dut, 0x000, FILL0)
        await issue(dut, 0, 1, 0, 0, PREA)
        await gap(dut, 10)

        print(f"[e2e] col0 ok={'1' if ok0 else '0'}  col8 ok={'1' if ok1 else '0'}  "
              f"col0-still-A000 ok={'1' if okc else '0'}")
        assert ok0 and ok1 and okc, 'round-trip incomplete'
        print("[e2e] PASS: WR/RD round-trip vs original ddr2.v (2 locations)")
    finally:
        wd.kill()
        if clk_task is not None:
            clk_task.kill()
