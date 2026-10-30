# I2cAuto — Fully-Automatic I2C Controller

Self-contained I2C master and slave controllers for SpinalHDL. Coexists with the
legacy `I2cSlave` + `driveI2cSlaveIo` stack (see ADR-0001).

## Components

| Component | File | Role |
|-----------|------|------|
| `I2cAutoMaster` | `I2cAutoMaster.scala` | FSM-driven master with TX/RX FIFOs |
| `I2cAutoSlave` | `I2cAutoSlave.scala` | Slave with sparse RAM, dual-port access |
| `I2cAutoDemo` | `I2cAutoDemo.scala` | Minimal top-level example |

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
io.i2c      : I2c()          // Standard I2C bus (scl, sda)
io.irq      : Bool           // Maskable interrupt output
```

### Register Map (via `driveFrom`)

| Offset | Name | Access | Description |
|--------|------|--------|-------------|
| 0x00 | CFG_SAMPDIV | RW | Input filter sample divider |
| 0x04 | CFG_TIMEOUT | RW | Bus hang timeout (cycles) |
| 0x08 | CFG_T_LOW | RW | SCL Low period |
| 0x0C | CFG_T_HIGH | RW | SCL High period |
| 0x10 | CFG_T_BUF | RW | Setup/hold time |
| 0x14 | CMD | WO | `{len2[7:0], len1[7:0], nackLast, phase2, rw, addr[addrW-1:0], trigger}` (addrW=7 or 10) |
| 0x18 | STATUS | RO | `{aborted, errCode[2:0], busy}` |
| 0x1C | INTR_EN | RW | Interrupt enable mask [2:0] |
| 0x20 | INTR_CLR | WO/W1C | Interrupt clear [2:0] |
| 0x24 | ABORT_REQ | WO | Request abort of active transfer |
| 0x28 | TX_FIFO_DATA | WO | Push byte to TX FIFO |
| 0x2C | TX_FIFO_CTRL | RO/WO | `{cnt[6:0], popValid}` |
| 0x30 | RX_FIFO_DATA | RO | Pop byte from RX FIFO (head) |
| 0x34 | RX_FIFO_CTRL | RO/WO | `{cnt[6:0], popValid}` |

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

### Usage Pattern (firmware)

```
1. Configure timing:  CFG_T_LOW, CFG_T_HIGH, CFG_T_BUF
2. Load TX data:      Write bytes to TX_FIFO_DATA (for write transfers)
3. Issue command:     Write CMD register (sets addr, rw, len, trigger=1)
4. Wait for IRQ:      Poll STATUS.busy or wait for INTR
5. Check result:      Read STATUS (errCode, aborted)
6. Collect RX data:   Pop bytes from RX_FIFO_DATA (for read transfers)
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
  sampDivWidth: Int = 10,
  support10bit: Boolean = false // Enable 10-bit address mode
)
```

### Port Summary

```
io.i2c      : I2c()     // Standard I2C bus
io.irq      : Bool      // Maskable interrupt output
```

### Register Map (via `driveFrom`)

| Offset | Name | Access | Description |
|--------|------|--------|-------------|
| 0x00 | ADDR | RW | Slave address (7 or 10-bit) + match enable |
| 0x04 | CFG_SAMPDIV | RW | Input filter sample divider |
| 0x08 | DATA_PTR | RW | Current data pointer (auto-increments) |
| 0x0C | INTR_EN | RW | Interrupt enable [1:0] |
| 0x10 | INTR_CLR | WO/W1C | Interrupt clear [1:0] |
| 0x14+ | RAM[n] | RW | Direct RAM access (dual-port with I2C) |

### Behavior

- **Sparse addressing**: Only responds when `addrMatchEn=1` AND received address matches
- **Auto-increment**: `dataPtr` increments after each byte; NACK when pointer exceeds valid range
- **Dual-port RAM**: Firmware can pre-load or inspect RAM via register bus while I2C is idle
- **Read mode**: Drives data from `RAM[dataPtr]`, MSB first
- **Write mode**: Stores received bytes into `RAM[dataPtr]`

### Interrupt Sources

| Bit | Name | Trigger |
|-----|------|---------|
| 0 | TRANSFER_COMPLETE | STOP detected during active frame |
| 1 | NACK_SENT | Slave NACKed (address mismatch or pointer overflow) |

### 10-Bit Address Mode

Set `support10bit = true` in both master and slave Config. The address field
widens to 10 bits and the master transmits two address bytes:

```
Byte 0: 1 1 1 1 0 A[9] A[8] A[7]
Byte 1: A[6] A[5] A[4] A[3] A[2] A[1] A[0] R/W
```

Example: Write 4 bytes to 10-bit address 0x345:
```
// Master
master = new I2cAutoMaster(Config(support10bit = true))
master.cmd.addr  #= 0x345   // 10-bit address
master.cmd.rw    #= false
master.cmd.len1  #= 4

// Slave
slave = new I2cAutoSlave(Config(support10bit = true))
slave.slaveAddr  #= 0x345
```

---

## Interconnect (Open-Drain Emulation)

In simulation or fabric integration, emulate the open-drain I2C bus:

```scala
// SCL: master-only drive
master.io.i2c.scl.read := master.io.i2c.scl.write
slave.io.i2c.scl.write := master.io.i2c.scl.write

// SDA: AND of both contributions (open-drain)
val sdaWire = master.io.i2c.sda.write && slave.io.i2c.sda.read
master.io.i2c.sda.read := sdaWire
slave.io.i2c.sda.write := sdaWire
```

## Known Limitations

| Item | Status |
|------|--------|
| DMA interface | Deferred (manual FIFO push/pop only) |
| SCL stretching (slave) | Partial (stretchReq stalls master; clean mid-byte resume TBD) |
| Multi-slave isolation | Limited (non-matched slave still enters data phase; SpinalHDL constraint) |
| I2cAutoDemo Verilog gen | Broken (pre-existing BusSlaveFactory addr conflict at 0x08) |

## Testing

```bash
sbt "tester/testOnly spinal.tester.scalatest.I2cAutoSim"
```

15 active testcases covering: write, read, composite+RESTART, address NACK,
full FIFO burst (64B), runtime abort, back-to-back transfers, timeout with
9-pulse recovery, data NACK mid-burst abort, multi-slave bus coexistence,
arbitration loss passive exit, SCL stretching pause verification, and
10-bit address (write, read, composite).

Features with dedicated test coverage:
- Arbitration loss detection (errCode=4): tested via `sdaForceHigh` injection
- Slave SCL stretching: tested via `stretchReq` assertion, validates master stall
  (clean mid-byte resume requires byte-aligned stretching - future enhancement)
