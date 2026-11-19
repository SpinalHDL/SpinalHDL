# I2cAuto — Fully-Automatic I2C Controller

Self-contained I2C master and slave controllers for SpinalHDL. Coexists with the
legacy `I2cSlave` + `driveI2cSlaveIo` stack (see ADR-0001). The controllers are
**register-bus agnostic**: the register map is bound to the SoC bus via
`driveFrom` (see ADR-0004), so the cores themselves carry no dependency on any
particular bus type.

## Components

| Component | File | Role |
|-----------|------|------|
| `I2cAutoMaster` | `I2cAutoMaster.scala` | CSR host adapter: register map, TX/RX byte FIFOs, interrupt controller |
| `I2cAutoController` | `I2cAutoController.scala` | Bus-facing engine: bus conditioning, watchdog, transfer FSM (START/addr/data/RESTART/STOP/recovery) |
| `I2cAutoSlave` | `I2cAutoSlave.scala` | Slave with sparse RAM, dual-port access |

> Example top-levels (SpinalVerilog generation demos, 7-bit and 10-bit variants) live outside the library:
> `tester/src/test/scala/spinal/lib/com/i2c/demo/I2cAutoDemo.scala`.

## I2cAutoMaster

### Parameters

```scala
case class Config(
  fifoDepth  : Int = 64,   // TX and RX FIFO depth (power of 2)
  timerWidth : Int = 12,   // Width of timing counters
  sampDivWidth: Int = 10,  // Width of input filter divider
  support10bit: Boolean = false // Enable 10-bit address mode
)
```

### Port Summary

```
io.i2c      : master(I2c())   // Open-drain I2C pins
io.irq      : Bool            // Maskable interrupt output
io.cmd      : {addr, rw, phase2, nackLast, byteCount, p2ByteCount, trigger}
io.tx       : Flow(Bits(8))  // valid/payload — one-cycle push per TX_DATA write
io.rx       : Stream(Bits(8)) // valid/payload/ready — pop-on-read
io.status   : {busy, errCode, aborted, txAvail, rxAvail}
io.intr     : {en, flags, clr}
io.cfg      : {sampDiv, timeout, tLow, tHigh, tBuf, abort}
```

(`trigger`/`tx.valid`/`abort` are one-cycle pulses produced by the register
map binding; `rx` pops on the accepted register read.)

The controller exposes **no register-bus port of its own**. Instead,
`io.driveFrom(busCtrl, baseAddress)` binds the register map below to a
caller-supplied `BusSlaveFactory` elaborated at the SoC top: the factory's
registers live in the parent scope and the core consumes only the resulting
values and event pulses. Any bus family whose slave factory extends
`BusSlaveFactory` (Bmb, APB3, TileLink, ...) can serve the same register map.

### Register Map

| Offset | Name | Access | Description |
|--------|------|--------|-------------|
| 0x00 | CMD | WO | `{nackLast[addrW+2], phase2[addrW+1], rw[addrW], addr[addrW-1:0]}` — fields + trigger land atomically on the write handshake |
| 0x04 | BYTE_COUNT | WO | Phase-1 data byte count |
| 0x08 | CMD_P2 | RW | Phase-2 address for cross-slave RESTART composites: `{valid[addrW], p2Addr[addrW-1:0]}`. Writing never triggers a transfer. `valid=1`: phase 2 addresses `p2Addr`; `valid=0`: legacy behavior (phase 2 repeats the phase-1 address) |
| 0x0C | P2_BYTE_COUNT | WO | Phase-2 data byte count |
| 0x10 | TX_DATA | WO | Push one byte into TX FIFO (`data[7:0]`) |
| 0x14 | RX_DATA | RO | `{notEmpty[8], data[7:0]}` — pop-on-read |
| 0x18 | STATUS | RO | `{aborted[6], errCode[5:3], rxAvail[2], txAvail[1], busy[0]}` |
| 0x1C | INT_EN | RW | Interrupt enable mask [2:0] |
| 0x20 | INT_FLAG | RO | Interrupt flags |
| 0x24 | INT_CLR | W1C | Interrupt clear [2:0] |
| 0x28 | CFG_SAMPDIV | WO | Input filter sample divider |
| 0x2C | CFG_TIMEOUT | WO | Bus-hang timeout (0 = disabled) |
| 0x30 | CFG_TLOW | WO | SCL Low period |
| 0x34 | CFG_THIGH | WO | SCL High period |
| 0x38 | CFG_TBUF | WO | Post-STOP idle time |
| 0x3C | CTRL | WO | `{abrt[0]}` — abort active transfer |

### Interrupt Sources

| Bit | Name | Trigger |
|-----|------|---------|
| 0 | COMPLETE | Transfer finished normally |
| 1 | ERROR | NACK, timeout, or abort |
| 2 | TIMEOUT | Bus hang detected (reserved) |

### Error Codes (STATUS.errCode)

| Code | Meaning |
|------|---------|
| 0 | None / cleared |
| 1 | Address NACK |
| 2 | Data NACK |
| 3 | Timeout |
| 4 | Arbitration loss |

### Usage Pattern (firmware)

```
1. Configure timing:  CFG_T_LOW, CFG_T_HIGH, CFG_T_BUF
2. Load TX data:      Write bytes to TX_DATA (for write transfers)
3. Issue command:     Write CMD register (sets addr, rw, len, trigger=1)
4. Wait for IRQ:      Poll STATUS.busy or wait for INTR
5. Check result:      Read STATUS (errCode, aborted)
6. Collect RX data:   Pop bytes from RX_DATA (for read transfers)
```

> **Firmware contract (TX/RX FIFO sizing).**
> - **TX:** before issuing a write command, preload **at least `byteCount`** (and `p2ByteCount` for the phase-2 write portion) bytes into the TX FIFO. The RTL pops one byte per transmitted bit-group and does **not** stall on an empty FIFO — an under-run shifts out undefined data with no `errCode`. Preloading is the responsibility of the driver (there is no hardware NAK for TX under-run).
> - **RX:** drain RX_DATA within `fifoDepth` (default 64) bytes of a read, or the RX FIFO fills and the controller stops clocking (slave clock-stretch equivalent). Reads longer than `fifoDepth` must be drained incrementally.

---

### Composite Transfers (RESTART)

Set `phase2=1` in CMD to perform an atomic write→RESTART→read:
- `rw` + `byteCount`: Phase 1 (typically write address/pointer)
- `p2ByteCount`: Phase 2 (read data)
- `nackLast`: NACK the last byte of phase 2 (standard I2C read termination)

Example: Write 1-byte pointer, then read 4 bytes:
```
CMD = {p2ByteCount=4, byteCount=1, nackLast=1, phase2=1, rw=0, addr=0x28, trigger=1}
TX FIFO: [pointer_byte]
```

Direction of phase 2 is fixed to the **inverse** of phase 1 (`curRw := !latRw`);
same-direction composites (write→write or read→read across a Repeated START)
are not supported by this design.

### Timing Compliance (firmware sizing)

The START/STOP setup/hold minima of UM10204 (Table 25) are realized from the
runtime-configured bit timing, not from fixed hardware constants:

| Spec parameter | Realized by | Segment |
|---|---|---|
| `tSU;STA` (SCL-High before SDA falls) | `tSegQ = max(tLow/4, 1)` filter ticks | START1 |
| `tHD;STA` (SDA-Low after SCL falls) | `tLow` | START3 |
| `tSU;STO` (SCL-High before SDA rises) | `tSegQ` | STOP1 |
| `tHD;STO` (SDA-Low after SCL falls) | `tSegFull + tSegQ` | STOP0+STOP1 |

`TSU:STA` / `TSU:STO` are therefore governed by `tSegQ`. A START preceded by a
STOP additionally enjoys the `tBuf` bus-free window, but a Repeated START and a
fresh START right after reset or bus recovery only guarantee `≥ tSegQ` of
SCL-High setup. Firmware must size `tLow` so that
`tSegQ × sampDiv × clk_period ≥ max(TSU:STA, TSU:STO)` of the target speed mode
(e.g. Standard Mode: `TSU:STA ≥ 4.7 µs` → `tLow ≥ ~19 µs`; Fast Mode:
`≥ 0.6 µs` → `tLow ≥ ~2.4 µs`; consult UM10204 Table 25 for authoritative
values). The functional test suite uses short tick counts for speed and does
NOT validate these electrical budgets.

---

## I2cAutoSlave

### Parameters

```scala
case class Config(
  ramDepth    : Int = 64,  // Data RAM depth (power of 2)
  validAddrs  : Seq[Int] = (0 until 64).toSeq, // Pointer values that are ACKed
  support10bit: Boolean = false,               // Enable 10-bit address mode
  sampWinSize : Int = 3,
  sampDivWidth: Int = 10
)
```

### Port Summary

```
io.i2c        : slave(I2c())   // Open-drain I2C pins
io.irq        : Bool           // Maskable interrupt output
io.sclStretch : Bool           // Active while the slave is stretching SCL
io.cfg        : {slaveAddr, addrMatchEn, ptrVal, ptrLoad, ramWrAddr, ramWrData, ramWrValid, sampDiv, timeout, stretchReq}
io.intr       : {en, flags, clr}
io.status     : {inFrame, errNack, ptr, stretchClr}
```

As with the master, the slave exposes **no register-bus port**; bind the map
below with `io.driveFrom(busCtrl, baseAddress)`.

### Register Map

| Offset | Name | Access | Description |
|--------|------|--------|-------------|
| 0x00 | ADDR | RW | `{matchEn[addrW], addr[addrW-1:0]}` |
| 0x04 | PTR | RW | Data pointer — a write loads the HW counter; a read returns the live (HW-incremented) value |
| 0x08 | RAM_IND | WO | Indirect RAM write: `addr[ramAddrW-1:0]` @ bits[ramAddrW-1:0], `data[7:0]` @ bits[ramAddrW+7:ramAddrW] |
| 0x0C | STATUS | RO | `{errNack[1], inFrame[0]}` |
| 0x10 | INT_EN | RW | Interrupt enable [1:0] |
| 0x14 | INT_FLAG | RO | Interrupt flags |
| 0x18 | INT_CLR | W1C | Interrupt clear [1:0] |
| 0x1C | CFG_SAMPDIV | WO | Sampling window divider |
| 0x20 | CFG_TIMEOUT | WO | Reserved (unused in RTL) |
| 0x24 | STRETCH_REQ | RW | SCL stretch request (level; auto-cleared on STOP) |

### Behavior

- **Sparse addressing**: Only responds when `addrMatchEn=1` AND received address matches
- **Auto-increment**: `dataPtr` increments after each byte; NACK when pointer exceeds valid range
- **Dual-port RAM**: Firmware can pre-load or inspect RAM via register bus while I2C is idle
- **Read mode**: Drives data from `RAM[dataPtr]`, MSB first; releases SDA at the 8th SCL fall and samples the master's ACK/NACK on the following SCL rise (so the sample is uncontaminated by the slave's own last data bit)
- **Write mode**: Stores received bytes into `RAM[dataPtr]`
- **SCL stretching**: `STRETCH_REQ` (level) is deferred to the SCL-Low phase (`stretchActive`), so a mid-byte stretch resumes cleanly without truncating an SCL-High window and desynchronizing the master's timer-based bit completion. Auto-cleared on STOP.

### Interrupt Sources

| Bit | Name | Trigger |
|-----|------|---------|
| 0 | TRANSFER_COMPLETE | STOP detected during active frame |
| 1 | NACK_SENT | Slave NACKed (address mismatch or pointer overflow) |

### 10-Bit Address Mode

Set `support10bit = true` in both master and slave Config. The address field
widens to 10 bits and the master transmits two address bytes in the standard
I2C format (UM10204 §4.2.2 / IEC 60197):

```
Byte 0: 1 1 1 1 0 A[9] A[8] R/W
Byte 1: A[7] A[6] A[5] A[4] A[3] A[2] A[1] A[0]
```

The slave validates the `11110` prefix on the first byte before ACKing; frames
without it (e.g. 7-bit traffic on a shared bus) are NACKed silently (no
error flag, no interrupt).

Example: write 4 bytes to 10-bit address 0x345 (register writes through the
bound bus):

```
// Master
WRITE 0x04 <- 4                    // BYTE_COUNT
WRITE 0x00 <- (0 << 10) | 0x345    // CMD: rw=0, addr=0x345 (write triggers)
TX FIFO: push 4 bytes via 0x10

// Slave
WRITE 0x00 <- (1 << 10) | 0x345    // ADDR: matchEn=1, addr=0x345
```

---

## Interconnect (Open-Drain Emulation)

In simulation or fabric integration, emulate the open-drain I2C bus. Note the
direction convention: the master drives on `.write` and senses on `.read`;
the slave drives on `.read` (its SDA output and its SCL stretch driver) and
senses on `.write`:

```scala
// SCL: AND of the master driver and the slave stretch driver
// (slave.io.i2c.scl.read is !stretchActive — High unless stretching)
val sclWire = master.io.i2c.scl.write && slave.io.i2c.scl.read
master.io.i2c.scl.read := sclWire
slave.io.i2c.scl.write := sclWire

// SDA: AND of both drivers (open-drain)
val sdaWire = master.io.i2c.sda.write && slave.io.i2c.sda.read
master.io.i2c.sda.read := sdaWire
slave.io.i2c.sda.write := sdaWire
```

## Register-Bus Integration (driveFrom)

The controllers are bus-agnostic: they expose no bus port. The SoC top binds
the register map with `io.driveFrom(busCtrl, baseAddress)`, where `busCtrl`
is any `BusSlaveFactory` (Bmb, APB3, TileLink, ...). Example with a Bmb port
(the topology used by the demo top and the sim tests):

```scala
// Top level
val regBusParam = BmbAccessParameter(8, 32)
  .addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2))
  .toBmbParameter()
val masterRegBus = slave(Bmb(regBusParam))
master.io.driveFrom(BmbSlaveFactory(masterRegBus), 0)
```

Port-parameter notes:
- **Single-beat only**: `BmbSlaveFactory` hardcodes `rsp.last := True` (no
  beat iteration), so keep `lengthWidth <= 2` (`allowBurst = false`) — a wider
  setting would advertise burst capability that silently truncates after beat 1.
- **`canMask = true`** (the `BmbSourceParameter` default) is required:
  `BmbSlaveFactory.writeByteEnable()` references `bus.cmd.mask` unconditionally.

Key properties (guaranteed by the factory-side binding):
- **Atomic CMD trigger**: CMD field registers and the trigger pulse are both
  derived from the same write handshake — the FSM latches stable fields, no
  separate GO bit, no race window.
- **Pop-on-read RX_DATA**: the returned `data` is the FIFO head *before* the
  pop takes effect.
- **Live pointer readback (slave)**: reading PTR returns the HW counter as
  incremented during transfers, not just the loaded value.
- **Error reporting**: unmapped addresses return `rsp.opcode = ERROR` (Bmb).

## Known Limitations

| Item | Status |
|------|--------|
| DMA interface | **Not implemented in this branch** (explicit scope decision, 2026-11-02): the data path is manual FIFO push/pop via `TX_DATA`/`RX_DATA` only; the 64B-FIFO + sparse-RAM model is the ceiling of this branch's data path. Supersedes the earlier "deferred" wording in ADR-0002 |
| SCL stretching (slave) | Supported: `STRETCH_REQ` register (level, auto-cleared on STOP) defers to the SCL-Low phase (`stretchActive`), so mid-byte stretching resumes cleanly without master/slave bit-counter desync (tested both directions) |
| Multi-slave isolation | Read + write data-phase both isolated (gated by `wasAddressed`); non-matched slaves release SDA in both directions |
| Read-end STOP masking | Fixed: `masterNack` + `readAckPending` make the slave release SDA after a master NACK before the STOP edge, so it can no longer hold SDA Low through the STOP and desynchronize (regression-tested: "read NACK then immediate next transfer") |
| Composite phase-switch TX prefetch | Fixed: removed the phase-switch FIFO pop entirely — a write phase 2 prefetches its first byte at its own address ACK, and a read phase 2 needs no TX bytes. The old `!latRw` gating did a phantom pop on write→read composites (counter underflow, next write shifted). Related slave fix: `dataPtr` now advances on a NACKed read byte too (the byte was transferred either way), so a read→write composite writes to the correct slots. Regression-tested in both composite directions |
| Verilog generation | Working (7-bit and 10-bit `SpinalVerilog` generate cleanly) |
| `readAsync` write-first warning | Benign: FIFO/RAM reads and writes never target the same address in the same cycle (see code comments at the `Mem` declarations) |
| FIFO bounds | Simulation asserts catch TX/RX FIFO underflow (pop on empty) and overflow (push on full); unsynthesizable guards, active in sim only |
| Pre-START bus-free check (UM10204 §3.1.8 AR-1) | Partial: the master rejects a CMD issued while it observes the external bus busy (`errCode=5` BUSY_REJECTED, tracked from START/STOP edges seen after the first accepted transfer). A late JOINER onto an already-active bus still has no re-synchronization path — a documented deviation |
| 10-bit header acknowledgement (UM10204 §3.1.11 A1/A3) | Fixed: the slave ACKs the first header byte only when the `A9:A8` upper nibble matches its own address (previously it ACKed any `11110` prefix, letting one 10-bit slave swallow another slave's header); read-direction headers are not ACKed on byte 0 — the master proceeds and the authoritative NACK check sits on the last address byte |
| Arbitration test topology | Covered both ways: SDA-injection tests the LOSER side (passive exit, `errCode=4`); the dual-master contest topology ("0x10 vs 0x30" and mirrored "swapped addresses") verifies the on-wire content decides the winner (lower address wins, §3.1.8 AR-4/5/6/7/11) and that swapping the instances swaps the winner |
| Speed envelope | All tests run at `tLow=tHigh=20` @100 MHz (≈2.5 MHz SCL) plus the slow-rate smokes above; no test exercises the electrical (ns-voltage) budgets of Tables 9/10 — simulation is purely functional |
| START/STOP setup/hold (UM10204 Table 25) | Configuration-dependent: `TSU:STA`/`TSU:STO` = `tSegQ = max(tLow/4,1)` ticks; firmware must size `tLow` per the [Timing Compliance](#timing-compliance-firmware-sizing) section. No hardware floor is applied (spec minima are time-based, dependent on clk/sampDiv) |
| Same-direction composite (write→write / read→read across Sr) | Not supported: phase-2 direction is hardwired to the inverse of phase 1 (`curRw := !latRw`). Feature boundary of this branch |

## Testing

```bash
sbt "tester/testOnly spinal.tester.scalatest.I2cAutoSim spinal.tester.scalatest.I2cAutoFuzz"
```

40 deterministic tests in `I2cAutoSim` plus 5 fuzz tests in `I2cAutoFuzz`
covering: write, read, composite+RESTART, address NACK,
data NACK mid-burst abort, full-FIFO burst (64B), runtime abort, back-to-back
transfers, timeout with 9-pulse recovery **observed on the live wire**
(release the SCL clamp once `errCode=3` latches; assert >= 9 SCL rises, SDA
released High, and exactly one trailing STOP), multi-slave bus coexistence,
arbitration loss passive exit, SCL stretching (stall, mid-byte clean
resumption in both write and read directions with data-integrity checks),
read-NACK-then-next-transfer regression, and 10-bit address (write, read,
composite, dual-slave upper-byte isolation, `11111` reserved-prefix NACK).

Additional specification-conformance coverage:
- **Wire-level golden vectors**: the test-topology sniffer decodes the first
  two on-wire bytes and their ACK bits; basic write/read assert the canonical
  encodings (e.g. `0x50`/`0x51` for 7-bit addr `0x28` R/W, `0xF3`/`0xBC` for
  10-bit `0x1BC`), defeating the loopback self-consistency trap
- **§3.1.3 SDA-stability guard**: every test topology counts SDA edges taken
  while SCL is High (legal START/STOP shapes excluded); `assertWireClean`
  asserts zero violations plus exact START/STOP edge counts where meaningful
- **Slow-rate smokes**: basic write+read at Std (`tLow/tHigh`=130/60),
  Fast (32/16) and Fast-mode Plus (10/10 — deliberately stresses the
  6-cycle filter window) timings
- **Glitch immunity**: a 1-cycle SDA glitch during SCL-Low is absorbed by the
  3-sample filter (data intact, no spurious START/STOP)
- **Busy rejection (errCode=5)**: a CMD issued while a transfer is active or
  while the external bus is busy (including during the 9-pulse recovery tail)
  is rejected with `errCode=5` + `intr.flags(1)`, instead of being silently
  swallowed
- **Cross-slave RESTART routing**: `CMD_P2` (0x08) routes phase 2 to a second
  slave (write->Sr->read verified); `valid=0` preserves legacy address repeat

Features with dedicated test coverage:
- Arbitration loss detection (errCode=4): tested via `sdaForceLow` injection (a competing master pulling SDA Low while our master drives High)
- Slave SCL stretching: `stretchReq` is deferred to the SCL-Low phase so the
  slave can never truncate an SCL-High window (which would desynchronize the
  master's timer-based bit completion from the slave's edge-based sampling).
  Mid-byte stretch + clean resumption verified with full data integrity in
  both write and read directions.
