# I2cAuto — Fully-Automatic I2C Controller

Self-contained I2C master and slave controllers for SpinalHDL. Coexists with the
legacy `I2cSlave` + `driveI2cSlaveIo` stack (see ADR-0001). The controllers are
**register-bus agnostic**: the register map is bound to the SoC bus via
`driveFrom` (see ADR-0004), so the cores themselves carry no dependency on any
particular bus type.

## Components

| Component | File | Role |
|-----------|------|------|
| `I2cAutoMaster` | `I2cAutoMaster.scala` | FSM-driven master with TX/RX FIFOs |
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
io.cmd      : {addr, rw, phase2, nackLast, len1, len2, trigger}
io.tx       : {data, push}
io.rx       : {head, notEmpty, pop}
io.status   : {busy, errCode, aborted, txAvail, rxAvail}
io.intr     : {en, fl, clr}
io.cfg      : {sampDiv, timeout, tLow, tHigh, tBuf, abort}
```

(`trigger`/`push`/`pop`/`abort` are one-cycle pulses produced by the register
map binding; `head` is the pre-pop RX FIFO head.)

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
| 0x04 | LEN1 | WO | Phase-1 data byte count |
| 0x08 | LEN2 | WO | Phase-2 data byte count |
| 0x0C | TX_DATA | WO | Push one byte into TX FIFO (`data[7:0]`) |
| 0x10 | RX_DATA | RO | `{notEmpty[8], data[7:0]}` — pop-on-read |
| 0x14 | STATUS | RO | `{aborted[6], errCode[5:3], rxAvail[2], txAvail[1], busy[0]}` |
| 0x18 | INT_EN | RW | Interrupt enable mask [2:0] |
| 0x1C | INT_FLAG | RO | Interrupt flags |
| 0x20 | INT_CLR | W1C | Interrupt clear [2:0] |
| 0x24 | CFG_SAMPDIV | WO | Input filter sample divider |
| 0x28 | CFG_TIMEOUT | WO | Bus-hang timeout (0 = disabled) |
| 0x2C | CFG_TLOW | WO | SCL Low period |
| 0x30 | CFG_THIGH | WO | SCL High period |
| 0x34 | CFG_TBUF | WO | Post-STOP idle time |
| 0x38 | CTRL | WO | `{abrt[0]}` — abort active transfer |

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

---

### Composite Transfers (RESTART)

Set `phase2=1` in CMD to perform an atomic write→RESTART→read:
- `rw` + `len1`: Phase 1 (typically write address/pointer)
- `len2`: Phase 2 (read data)
- `nackLast`: NACK the last byte of phase 2 (standard I2C read termination)

Example: Write 1-byte pointer, then read 4 bytes:
```
CMD = {len2=4, len1=1, nackLast=1, phase2=1, rw=0, addr=0x28, trigger=1}
TX FIFO: [pointer_byte]
```

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
io.intr       : {en, fl, clr}
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
WRITE 0x04 <- 4                    // LEN1
WRITE 0x00 <- (0 << 10) | 0x345    // CMD: rw=0, addr=0x345 (write triggers)
TX FIFO: push 4 bytes via 0x0C

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

## Testing

```bash
sbt "tester/testOnly spinal.tester.scalatest.I2cAutoSim spinal.tester.scalatest.I2cAutoFuzz"
```

18 deterministic tests in `I2cAutoSim` plus 3 fuzz tests in `I2cAutoFuzz`
(100 random writes, 50 random reads, 20 random composite write+read
RESTARTs) covering: write, read, composite+RESTART, address NACK,
data NACK mid-burst abort, full-FIFO burst (64B), runtime abort, back-to-back
transfers, timeout with 9-pulse recovery, multi-slave bus coexistence,
arbitration loss passive exit, SCL stretching (stall, mid-byte clean
resumption in both write and read directions with data-integrity checks),
read-NACK-then-next-transfer regression, and 10-bit address (write, read,
composite).

Features with dedicated test coverage:
- Arbitration loss detection (errCode=4): tested via `sdaForceHigh` injection
- Slave SCL stretching: `stretchReq` is deferred to the SCL-Low phase so the
  slave can never truncate an SCL-High window (which would desynchronize the
  master's timer-based bit completion from the slave's edge-based sampling).
  Mid-byte stretch + clean resumption verified with full data integrity in
  both write and read directions.
