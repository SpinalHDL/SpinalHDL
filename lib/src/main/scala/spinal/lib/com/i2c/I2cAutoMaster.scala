package spinal.lib.com.i2c

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

/**
  * Fully-automatic I2C master controller.
  *
  * Software writes the CMD register to initiate a transfer. The hardware
  * autonomously generates START, address, data bytes, optional RESTART +
  * phase-2, and STOP. Completion or error is signalled via the IRQ output.
  *
  * The controller core is register-bus agnostic: it exposes the signals it
  * needs ([[I2cAutoMasterIo]]) and leaves register-map instantiation to
  * [[I2cAutoMaster.driveFrom]], which binds a caller-supplied
  * [[BusSlaveFactory]] to parent-side registers wired into the io. Any bus
  * family whose slave factory extends BusSlaveFactory (Bmb, Apb3, TileLink, ...)
  * can serve the same register map.
  *
  * Register map (byte addresses, 32-bit data width; bit offsets shift with addrW=7/10):
  *   0x00  CMD          W: {nackLast[addrW+2], hasPhase2[addrW+1], rw[addrW], addr[addrW-1:0]}
  *                       (fields + trigger land atomically on the bus write handshake)
  *   0x04  LEN1         W: phase-1 data byte count
  *   0x08  LEN2         W: phase-2 data byte count
  *   0x0C  TX_DATA      W: push one byte into TX FIFO
  *   0x10  RX_DATA      R: {valid[8], data[7:0]}
  *   0x14  STATUS       R: {aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
  *   0x18  INT_EN       RW: {rxNotEmpty[2], error[1], complete[0]}
  *   0x1C  INT_FLAG     R:  interrupt flags
  *   0x20  INT_CLR      W:  W1C interrupt flags
  *   0x24  CFG_SAMPDIV  W: sampling clock divider
  *   0x28  CFG_TIMEOUT  W: timeout counter (0 = disabled)
  *   0x2C  CFG_TLOW     W: SCL low period
  *   0x30  CFG_THIGH    W: SCL high period
  *   0x34  CFG_TBUF     W: post-STOP idle time
  *   0x38  CTRL         W: {abrt[0]}
  */
object I2cAutoMaster {

  /** Parameter for a Bmb register bus serving this controller (8-bit address, 32-bit data). */
  def getBmbParameter(): BmbParameter = BmbAccessParameter(8, 32)
    .addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 3))
    .toBmbParameter()

  case class Config(
    fifoDepth    : Int     = 64,
    support10bit : Boolean = false,
    timerWidth   : Int     = 12,
    sampWinSize  : Int     = 3,
    sampDivWidth : Int     = 10
  )

  def apply(c: Config = Config()): I2cAutoMaster = new I2cAutoMaster(c)

  /**
   * Instantiate the register map on a caller-supplied bus factory and wire it
   * to the controller's io. Must be elaborated in a scope that can see both
   * the factory's bus and the io (typically the parent component).
   *
   * Event registers (CMD trigger, TX_DATA push, RX_DATA pop, CTRL abort) are
   * one-cycle pulses derived from bus write/read handshakes; value registers
   * are plain registers. Pulse timing matches the legacy internal
   * BmbSlaveFactory implementation (pulse high one cycle after the bus
   * handshake).
   */
  def driveFrom(io: I2cAutoMasterIo, busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit = new Area {
    import io._
    val factory = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val addrW   = cmd.addr.getWidth

    // CMD (0x00): fields + trigger. Field regs update at the write edge; the
    // trigger pulse goes high the following cycle, so the FSM latches stable
    // field values.
    val cmdAddr     = Reg(Bits(addrW bits)) init(0)
    val cmdRw       = RegInit(False)
    val cmdPhase2   = RegInit(False)
    val cmdNackLast = RegInit(False)
    val cmdTrigger  = RegInit(False)
    factory.write(cmdAddr, 0x00, 0)
    factory.write(cmdRw, 0x00, addrW)
    factory.write(cmdPhase2, 0x00, addrW + 1)
    factory.write(cmdNackLast, 0x00, addrW + 2)
    cmdTrigger := False
    factory.onWrite(0x00) { cmdTrigger := True }
    cmd.addr     := cmdAddr
    cmd.rw       := cmdRw
    cmd.phase2   := cmdPhase2
    cmd.nackLast := cmdNackLast
    cmd.trigger  := cmdTrigger

    // LEN1 / LEN2
    val len1 = Reg(UInt(8 bits)) init(0)
    val len2 = Reg(UInt(8 bits)) init(0)
    factory.write(len1, 0x04)
    factory.write(len2, 0x08)
    cmd.len1 := len1
    cmd.len2 := len2

    // TX_DATA (0x0C): push one byte into the TX FIFO
    val txData = Reg(Bits(8 bits)) init(0)
    val txPush = RegInit(False)
    factory.write(txData, 0x0C)
    txPush := False
    factory.onWrite(0x0C) { txPush := True }
    tx.data := txData
    tx.push := txPush

    // RX_DATA (0x10): pop-on-read. The read returns the pre-pop head
    // combinationally; the pop pulse fires one cycle after the read.
    factory.read(rx.head, 0x10, 0)
    factory.read(rx.notEmpty, 0x10, 8)
    val rxPop = RegInit(False)
    rxPop := False
    factory.onRead(0x10) { rxPop := True }
    rx.pop := rxPop

    // STATUS (0x14): {aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
    factory.read(0x14, 0 -> status.busy, 1 -> status.txAvail, 2 -> status.rxAvail,
                 3 -> status.errCode, 6 -> status.aborted)

    // Interrupts
    val intEn  = RegInit(B(0, 3 bits))
    val intClr = RegInit(B(0, 3 bits))
    factory.write(intEn, 0x18)
    factory.read(intr.fl, 0x1C)
    factory.write(intClr, 0x20)
    intr.en  := intEn
    intr.clr := intClr

    // Timing configuration
    val sampDiv = Reg(UInt(cfg.sampDiv.getWidth bits)) init(0)
    val timeout = Reg(UInt(20 bits)) init(0)
    val tLow    = Reg(UInt(cfg.tLow.getWidth bits)) init(10)
    val tHigh   = Reg(UInt(cfg.tHigh.getWidth bits)) init(10)
    val tBuf    = Reg(UInt(cfg.tBuf.getWidth bits)) init(10)
    factory.write(sampDiv, 0x24)
    factory.write(timeout, 0x28)
    factory.write(tLow, 0x2C)
    factory.write(tHigh, 0x30)
    factory.write(tBuf, 0x34)
    cfg.sampDiv := sampDiv
    cfg.timeout := timeout
    cfg.tLow    := tLow
    cfg.tHigh   := tHigh
    cfg.tBuf    := tBuf

    // CTRL (0x38): abort pulse
    val abort = RegInit(False)
    abort := False
    factory.onWrite(0x38) { abort := True }
    cfg.abort := abort
  }
}

/** Controller io: I2C pins, IRQ, and the firmware-facing control/status signals bound by driveFrom. */
case class I2cAutoMasterIo(c: I2cAutoMaster.Config) extends Bundle {
  val i2c = master(I2c())
  val irq = out Bool()

  val cmd = new Bundle {
    val addr     = in Bits((if (c.support10bit) 10 else 7) bits)
    val rw       = in Bool()
    val phase2   = in Bool()
    val nackLast = in Bool()
    val len1     = in UInt(8 bits)
    val len2     = in UInt(8 bits)
    val trigger  = in Bool()  // one-cycle pulse from the CMD register write
  }

  val tx = new Bundle {
    val data = in Bits(8 bits)
    val push = in Bool()  // one-cycle pulse from the TX_DATA register write
  }

  val rx = new Bundle {
    val head     = out Bits(8 bits)  // pre-pop head
    val notEmpty = out Bool()
    val pop      = in Bool()  // one-cycle pulse from the RX_DATA register read
  }

  val status = new Bundle {
    val busy    = out Bool()
    val errCode = out UInt(3 bits)
    val aborted = out Bool()
    val txAvail = out Bool()
    val rxAvail = out Bool()
  }

  val intr = new Bundle {
    val en  = in Bits(3 bits)
    val fl  = out Bits(3 bits)
    val clr = in Bits(3 bits)  // W1C
  }

  val cfg = new Bundle {
    val sampDiv = in UInt(c.sampDivWidth bits)
    val timeout = in UInt(20 bits)
    val tLow    = in UInt(c.timerWidth bits)
    val tHigh   = in UInt(c.timerWidth bits)
    val tBuf    = in UInt(c.timerWidth bits)
    val abort   = in Bool()  // one-cycle pulse from the CTRL register write
  }

  /** Bind the register map to a bus factory (see [[I2cAutoMaster.driveFrom]]). */
  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit =
    I2cAutoMaster.driveFrom(this, busCtrl, baseAddress)
}

class I2cAutoMaster(c: I2cAutoMaster.Config) extends Component {

  import I2cAutoMaster.Config

  val io = I2cAutoMasterIo(c)

  val fifoPtrW = log2Up(c.fifoDepth)

  // === Input filtering ===
  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, io.cfg.sampDiv, counter - U(1))
  }

  val sclFilt = RegInit(True)
  val sdaFilt = RegInit(True)
  val sclHist = RegInit(B(1, c.sampWinSize bits))
  val sdaHist = RegInit(B(1, c.sampWinSize bits))

  when(filterTick.tick) {
    sclHist := Cat(io.i2c.scl.read, sclHist(c.sampWinSize - 2 downto 0))
    sdaHist := Cat(io.i2c.sda.read, sdaHist(c.sampWinSize - 2 downto 0))
    when(sclHist.andR || sclHist.orR) { sclFilt := io.i2c.scl.read }
    when(sdaHist.andR || sdaHist.orR) { sdaFilt := io.i2c.sda.read }
  }

  // === Timeout watchdog ===
  val timeoutFired = RegInit(False)
  val timeoutCtr   = Reg(UInt(20 bits)) init(0)
  when(timeoutCtr =/= U(0)) {
    timeoutCtr := timeoutCtr - U(1)
    timeoutFired := timeoutCtr === U(1)
  } otherwise {
    timeoutFired := False
  }


  // === TX FIFO ===
  val txFifo = new Area {
    val mem  = Mem(Bits(8 bits), c.fifoDepth)
    val wPtr = Reg(UInt(fifoPtrW bits)) init(0)
    val rPtr = Reg(UInt(fifoPtrW bits)) init(0)
    val cnt  = Reg(UInt((fifoPtrW + 1) bits)) init(0)
    val pushValid = RegInit(False)
    val pushData  = RegInit(B(0, 8 bits))
    val popValid  = RegInit(False)
    val head      = mem.readAsync(rPtr)  // TX: combinational read
    val notEmpty  = cnt =/= U(0)

    when(pushValid) {
      mem.write(wPtr, pushData)
      wPtr := wPtr + 1
      cnt  := cnt + 1
    }
    when(popValid) {
      rPtr := rPtr + 1
      cnt  := cnt - 1
    }
  }

  // === RX FIFO ===
  val rxFifo = new Area {
    val mem  = Mem(Bits(8 bits), c.fifoDepth)
    val wPtr = Reg(UInt(fifoPtrW bits)) init(0)
    val rPtr = Reg(UInt(fifoPtrW bits)) init(0)
    val cnt  = Reg(UInt((fifoPtrW + 1) bits)) init(0)
    val pushValid = RegInit(False)
    val pushData  = RegInit(B(0, 8 bits))
    val popValid  = RegInit(False)
    val head      = mem.readAsync(rPtr)  // RX: combinational read
    val notEmpty  = cnt =/= U(0)

    when(pushValid) {
      mem.write(wPtr, pushData)
      wPtr := wPtr + 1
      cnt  := cnt + 1
    }
    when(popValid) {
      rPtr := rPtr + 1
      cnt  := cnt - 1
    }
  }

  // Wire the firmware-facing FIFO interface (driven by driveFrom-bound registers)
  txFifo.pushValid := io.tx.push
  txFifo.pushData  := io.tx.data
  txFifo.popValid  := False
  rxFifo.pushValid := False
  rxFifo.popValid  := io.rx.pop
  io.rx.head       := rxFifo.head
  io.rx.notEmpty   := rxFifo.notEmpty

  /** Width of the address field in bits (7 or 10). */
  val addrW: Int = if (c.support10bit) 10 else 7
  /** Number of address bytes on the bus (1 for 7-bit, 2 for 10-bit). */
  val addrBytes: Int = if (c.support10bit) 2 else 1

  // === Status ===
  val status = new Area {
    val busy    = RegInit(False)
    val errCode = RegInit(U(0, 3 bits))
    val aborted = RegInit(False)
  }

  // Timeout reload on SCL edge or when not busy
  val sclPrev = RegInit(True)
  val sclEdge = sclFilt =/= sclPrev
  sclPrev := sclFilt
  when(sclEdge || !status.busy) {
    timeoutCtr := io.cfg.timeout
  }

  // === Interrupt controller ===
  val intr = new Area {
    val fl = RegInit(B(0, 3 bits))
    for (i <- 0 until 3) { fl(i) clearWhen(io.intr.clr(i)) }
  }
  io.irq := (io.intr.en & intr.fl).orR

  // Forward internal status to the io
  io.status.busy    := status.busy
  io.status.errCode := status.errCode
  io.status.aborted := status.aborted
  io.status.txAvail := txFifo.notEmpty
  io.status.rxAvail := rxFifo.notEmpty
  io.intr.fl        := intr.fl

  // === Transfer FSM ===
  val fsm = new StateMachine {

    // Timer with armed flag to avoid spurious timerDone on entry cycle
    val timer      = Reg(UInt(c.timerWidth bits)) init(0)
    val timerArmed = RegInit(False)
    val timerDone  = timerArmed && timer === U(0)

    // 9-pulse recovery counter
    val recovCnt   = Reg(UInt(4 bits)) init(0)

    // Global timer decrement
    when(timerArmed && timer =/= U(0)) { timer := timer - U(1) }

    val phase   = RegInit(U(0, 1 bits))
    val byteNum = RegInit(U(0, 8 bits))
    val bitPos  = RegInit(U(0, 4 bits))
    val curLen  = RegInit(U(0, 8 bits))
    val curRw   = RegInit(False)

    val txByte = Reg(Bits(8 bits)) init(0)
    val rxByte = Reg(Bits(8 bits)) init(0)
    val ackBit = RegInit(False)

    // Drive signals
    val sdaDrive = RegInit(True)
    val sdaEn    = RegInit(False)
    val sclDrive = RegInit(True)

    // Latched params
    val latAddr     = Reg(Bits(addrW bits)) init(0)
    val latRw       = RegInit(False)
    val latHasP2    = RegInit(False)
    val latNackLast = RegInit(False)
    val latLen2     = Reg(UInt(8 bits)) init(0)

    // Derived
    val dataStart    = U(addrBytes, 8 bits)
    val isAddrByte   = byteNum < dataStart
    val isLastAddrByte = byteNum === U(addrBytes - 1, 8 bits)
    val isAckBit     = bitPos === U(8)
    val isLastByte   = byteNum === (dataStart - U(1)) + curLen

    val driveSda: Bool = Mux(isAckBit, Mux(isAddrByte, False, curRw), isAddrByte || !curRw)

    // Address drive logic (compile-time selected to avoid out-of-bounds in 7-bit mode)
    val addrDrive: Bool = if (c.support10bit) {
      val tenBitByte0: Bool = Mux(bitPos(2 downto 0) < U(4), True,
                                    Mux(bitPos(2 downto 0) === U(4), False,
                                      Mux(bitPos(2 downto 0) === U(5), latAddr(9),
                                        Mux(bitPos(2 downto 0) === U(6), latAddr(8), latAddr(7)))))
      val tenBitByte1: Bool = Mux(bitPos(2 downto 0) === U(7), curRw, latAddr(6 - bitPos(2 downto 0)))
      Mux(byteNum === U(0), tenBitByte0, tenBitByte1)
    } else {
      Mux(bitPos(2 downto 0) === U(7), curRw, latAddr(6 - bitPos(2 downto 0)))
    }

    val sdaValue: Bool = Mux(isAckBit,
      Mux(latNackLast && isLastByte && !(latHasP2 && !curRw), True, False),
      Mux(isAddrByte, addrDrive, txByte(7 - bitPos(2 downto 0)))
    )

    // States
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        when(io.cmd.trigger) {
          latAddr     := io.cmd.addr
          latRw       := io.cmd.rw
          latHasP2    := io.cmd.phase2
          latNackLast := io.cmd.nackLast
          latLen2     := io.cmd.len2
          phase   := U(0)
          byteNum := U(0)
          bitPos  := U(0)
          curLen  := io.cmd.len1
          curRw   := io.cmd.rw
          rxByte  := B(0)
          status.busy    := True
          status.errCode := U(0, 3 bits)
          status.aborted := False
          timeoutCtr     := io.cfg.timeout
          goto(START1)
        }
      }
    }

    val START1: State = new State {
      onEntry { timer := U(2); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True
        when(timerDone) { goto(START2) }
      }
    }

    val START2: State = new State {
      onEntry { timer := U(2); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(START3) }
      }
    }

    val START3: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(BIT_LOW) }
      }
    }

    val BIT_LOW: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        when(driveSda) {
          sdaEn    := True
          sdaDrive := sdaValue
        } otherwise {
          sdaEn := False
        }
        when(timerDone) { goto(BIT_HIGH) }
      }
    }

    val BIT_HIGH: State = new State {
      onEntry { timer := io.cfg.tHigh; timerArmed := True }
      whenIsActive {
        sclDrive := True
        when(driveSda) {
          sdaEn    := True
          sdaDrive := sdaValue
        } otherwise {
          sdaEn := False
        }
        // Clock stretching: reload timer if SCL held low
        when(!sclFilt) { timer := io.cfg.tHigh }

        when(timerDone && sclFilt) {
          when(!driveSda) {
            when(isAckBit) {
              ackBit := sdaFilt  // keep for observability
            } otherwise {
              rxByte(7 - bitPos(2 downto 0)) := sdaFilt
            }
          }
          when(isAckBit) {
            when(isAddrByte) {
              when(isLastAddrByte) {
                when(sdaFilt) {  // NACK on last address byte
                  status.errCode := U(1, 3 bits)
                  status.busy    := False
                  intr.fl(1)     := True
                  goto(STOP0)
                } otherwise {
                  byteNum := dataStart
                  bitPos  := U(0)
                  when(!curRw && curLen > U(0)) {
                    txByte := txFifo.head
                    txFifo.popValid := True
                  }
                  goto(BIT_LOW)
                }
              } otherwise {
                // Intermediate address byte (10-bit mode): ACK and continue
                when(sdaFilt) {  // NACK on first addr byte
                  status.errCode := U(1, 3 bits)
                  status.busy    := False
                  intr.fl(1)     := True
                  goto(STOP0)
                } otherwise {
                  byteNum := byteNum + U(1)
                  bitPos  := U(0)
                  goto(BIT_LOW)
                }
              }
            } otherwise {
              when(curRw) {
                rxFifo.pushValid := True
                rxFifo.pushData  := rxByte
              }
              when(!curRw && sdaFilt) {  // Data NACK: use direct signal
                status.errCode := U(2, 3 bits)
                status.busy    := False
                intr.fl(1)     := True
                goto(STOP0)
              } otherwise {
                when(isLastByte) {
                  when(latHasP2 && phase === U(0)) {
                    phase   := U(1)
                    byteNum := U(0)
                    bitPos  := U(0)
                    curLen  := latLen2
                    curRw   := !latRw
                    rxByte  := B(0)
                    when(!latRw) {
                      txByte := txFifo.head
                      txFifo.popValid := True
                    }
                    goto(RESTART0)
                  } otherwise {
                    status.busy  := False
                    intr.fl(0)   := True
                    goto(STOP0)
                  }
                } otherwise {
                  byteNum := byteNum + U(1)
                  bitPos  := U(0)
                  rxByte  := B(0)
                  when(!curRw) {
                    txByte := txFifo.head
                    txFifo.popValid := True
                  }
                  goto(BIT_LOW)
                }
              }
            }
          } otherwise {
            bitPos := bitPos + U(1)
            goto(BIT_LOW)
          }
        }
      }
    }

    val RESTART0: State = new State {
      onEntry { timer := U(5); timerArmed := True }
      whenIsActive {
        sclDrive := False  // drop SCL to let slave release SDA
        sdaEn    := True
        sdaDrive := True
        when(timerDone) { goto(RESTART1) }
      }
    }

    val RESTART1: State = new State {
      onEntry { timer := U(5); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True
        when(timerDone) { goto(RESTART2) }
      }
    }

    val RESTART2: State = new State {
      onEntry { timer := U(5); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(RESTART3) }
      }
    }

    val RESTART3: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(BIT_LOW) }
      }
    }

    // 9-pulse clock recovery (I2C spec §3.4.2: recover from bus hang)
    val RECOV_LO: State = new State {
      onEntry { timer := U(10); timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := False
        when(timerDone) { goto(RECOV_HI) }
      }
    }

    val RECOV_HI: State = new State {
      onEntry { timer := U(10); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := False
        when(timerDone) {
          when(recovCnt === U(1)) {
            recovCnt := U(0)
            goto(STOP0)  // after 9 pulses, issue STOP
          } otherwise {
            recovCnt := recovCnt - U(1)
            goto(RECOV_LO)
          }
        }
      }
    }

    val STOP0: State = new State {
      onEntry { timer := U(5); timerArmed := True }
      whenIsActive {
        sclDrive := False  // drop SCL to let slave release SDA
        sdaEn    := True   // drive SDA Low (open-drain), NOT release:
        sdaDrive := False  // SDA must be Low before SCL rises in STOP1, else the
                           // SCL-rise + SDA-fall looks like a spurious START to the slave.
        when(timerDone) { goto(STOP1) }
      }
    }

    val STOP1: State = new State {
      onEntry { timer := U(2); timerArmed := True }
      whenIsActive {
        sclDrive := True   // SCL rises, master drives SDA Low
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(STOP2) }
      }
    }

    val STOP2: State = new State {
      onEntry { timer := U(2); timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True   // SDA rises while SCL High = STOP
        when(timerDone) { goto(TBUF) }
      }
    }

    val TBUF: State = new State {
      onEntry { timer := io.cfg.tBuf; timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := False
        when(timerDone) { goto(IDLE) }
      }
    }

    // Arbitration loss: passive exit (no STOP - we're no longer bus master)
    val ARB_LOSS: State = new State {
      onEntry { timer := U(5); timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := False  // release SDA immediately
        when(timerDone) { goto(IDLE) }
      }
    }

    always {
      // Arbitration loss: actively driving SDA Low but raw bus reads High
      // Require sdaEn=True to avoid false positive on entry cycle (sdaEn not yet updated)
      when(status.busy && driveSda && sdaEn && !sdaDrive && io.i2c.sda.read) {
        status.errCode := U(4, 3 bits)
        status.busy    := False
        intr.fl(1)     := True
        goto(ARB_LOSS)  // passive exit: no STOP
      }
      when(timeoutFired && status.busy) {
        status.errCode := U(3, 3 bits)
        status.busy    := False
        intr.fl(1)     := True
        recovCnt       := U(9)  // initiate 9-pulse recovery
        goto(RECOV_LO)
      }
      when(io.cfg.abort) {
        status.busy    := False
        status.aborted := True
        intr.fl(1)     := True
        goto(STOP0)  // abort: no recovery needed (local request)
      }
    }

    this.build()
  }

  // Drive pins
  io.i2c.scl.write := fsm.sclDrive
  io.i2c.sda.write := Mux(fsm.sdaEn, fsm.sdaDrive, True)

  // Expose internals for simulation
  status.busy.simPublic(); status.errCode.simPublic(); status.aborted.simPublic()
  txFifo.pushValid.simPublic(); txFifo.pushData.simPublic(); txFifo.popValid.simPublic()
  txFifo.head.simPublic(); txFifo.notEmpty.simPublic(); txFifo.cnt.simPublic()
  rxFifo.popValid.simPublic(); rxFifo.head.simPublic(); rxFifo.notEmpty.simPublic()
  fsm.bitPos.simPublic(); fsm.byteNum.simPublic()
}
