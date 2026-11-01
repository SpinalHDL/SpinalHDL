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
io.i2c      : I2c()                    // Standard I2C bus (scl, sda)
io.irq      : Bool                     // Maskable interrupt output
io.regBus   : slave(Bmb(8, 32))        // 32-bit data / 8-bit address register bus
```

The register bus is a standard **Bmb** slave port. Internally the controller
instantiates a `BmbSlaveFactory` and maps the register file onto it, so a SoC
integrator simply connects a Bmb master (e.g. APB3/AXI4-Lite bridge) to
`io.regBus`. Unmapped addresses return `rsp.opcode = ERROR`.

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
  validAddrs  : Seq[Int] = (0 until 64).toSeq, // Pointer values that are ACKed
  support10bit: Boolean = false,               // Enable 10-bit address mode
  sampWinSize : Int = 10,
  sampDivWidth: Int = 10
)
```

### Port Summary

```
io.i2c        : I2c()                    // Standard I2C bus (scl, sda)
io.irq        : Bool                     // Maskable interrupt output
io.sclStretch : Bool                     // Active while SCL is being stretched
io.regBus     : slave(Bmb(8, 32))        // 32-bit data / 8-bit address register bus
```

### Register Map

| Offset | Name | Access | Description |
|--------|------|--------|-------------|
| 0x00 | ADDR | RW | `{matchEn[addrW], addr[addrW-1:0]}` |
| 0x04 | PTR | RW | Data pointer |
| 0x08 | RAM_IND | WO | Indirect RAM write: `addr[ramAddrW-1:0]` @ bits[ramAddrW-1:0], `data[7:0]` @ bits[ramAddrW+7:ramAddrW] |
| 0x0C | STATUS | RO | `{errNack[1], inFrame[0]}` |
| 0x10 | INT_EN | RW | Interrupt enable [1:0] |
| 0x14 | INT_FLAG | RO | Interrupt flags |
| 0x18 | INT_CLR | W1C | Interrupt clear [1:0] |
| 0x1C | CFG_SAMPDIV | WO | Sampling window divider |
| 0x20 | CFG_TIMEOUT | WO | Reserved (unused in RTL) |

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

## Register-Bus Integration (Bmb)

Both controllers expose `io.regBus` as a Bmb **slave** port (8-bit address,
32-bit data, single source, masked writes). Connect any Bmb master from the
fabric. The demo top ties the port off (`cmd.valid = 0`, `rsp.ready = 0`);
in a real SoC, e.g.:

```scala
val apb = new Apb3(32, 8)
apb <> bmbBridge.cmd   // or any Bmb master source
master.io.regBus <> bmbBridge.slaves(0)
slave.io.regBus  <> bmbBridge.slaves(1)
```

Key properties (guaranteed by the `BmbSlaveFactory` implementation):
- **Atomic CMD trigger**: CMD fields and the implicit trigger pulse are both
  applied at the same handshake edge — no separate GO bit, no race window.
- **Pop-on-read RX_DATA**: the returned `data` is the FIFO head *before* the
  pop takes effect.
- **Error reporting**: unmapped addresses return `rsp.opcode = ERROR`.

## Known Limitations

| Item | Status |
|------|--------|
| DMA interface | Deferred (manual FIFO push/pop only) |
| SCL stretching (slave) | Supported: `stretchReq` defers to the SCL-Low phase (`stretchActive`), so mid-byte stretching resumes cleanly without master/slave bit-counter desync (tested both directions) |
| Multi-slave isolation | Read + write data-phase both isolated (gated by `wasAddressed`); non-matched slaves release SDA in both directions |
| Read-end STOP masking | Fixed: `masterNack` flag makes the slave release SDA after a master NACK, so it can no longer hold SDA Low through the STOP edge and desynchronize |
| Verilog generation | Working (7-bit and 10-bit `SpinalVerilog` generate cleanly) |

## Testing

```bash
sbt "tester/testOnly spinal.tester.scalatest.I2cAutoSim"
```

17 deterministic tests in `I2cAutoSim` plus 2 fuzz tests in `I2cAutoFuzz`
(100 random writes, 50 random reads) covering: write, read, composite+RESTART,
address NACK, full FIFO burst (64B), runtime abort, back-to-back transfers,
timeout with 9-pulse recovery, data NACK mid-burst abort, multi-slave bus
coexistence, arbitration loss passive exit, SCL stretching (stall, mid-byte
clean resumption in both write and read directions with data-integrity checks),
and 10-bit address (write, read, composite).

Features with dedicated test coverage:
- Arbitration loss detection (errCode=4): tested via `sdaForceHigh` injection
- Slave SCL stretching: `stretchReq` is deferred to the SCL-Low phase so the
  slave can never truncate an SCL-High window (which would desynchronize the
  master's timer-based bit completion from the slave's edge-based sampling).
  Mid-byte stretch + clean resumption verified with full data integrity in
  both write and read directions.
