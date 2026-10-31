package spinal.lib.com.i2c

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

/**
  * Fully-automatic I2C master controller.
  *
  * Software writes the CMD register to initiate a transfer. The hardware
  * autonomously generates START, address, data bytes, optional RESTART +
  * phase-2, and STOP. Completion or error is signalled via the IRQ output.
  *
  * Register map (byte addresses, 32-bit data width):
  *   0x00  CMD          W: {nackLast[9], hasPhase2[8], rw[7], addr[6:0]}
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

  class RegBus extends Bundle {
    val addr  = in Bits(8 bits)
    val wdata = in Bits(32 bits)
    val wen   = in Bool()
    val rdata = out Bits(32 bits)
    val re    = in Bool()
  }

  case class Config(
    fifoDepth    : Int     = 64,
    support10bit : Boolean = false,
    timerWidth   : Int     = 12,
    sampWinSize  : Int     = 3,
    sampDivWidth : Int     = 10
  )

  def apply(c: Config = Config()): I2cAutoMaster = new I2cAutoMaster(c)
}

class I2cAutoMaster(c: I2cAutoMaster.Config) extends Component {

  import I2cAutoMaster.Config

  val io = new Bundle {
    val i2c = master(I2c())
    val irq = out Bool()
    val regBus = new I2cAutoMaster.RegBus()
  }

  val fifoPtrW = log2Up(c.fifoDepth)

  // === Configuration registers ===
  val cfg = new Area {
    val sampDiv = Reg(UInt(c.sampDivWidth bits)) init(0)
    val timeout = Reg(UInt(20 bits)) init(0)
    val tLow    = Reg(UInt(c.timerWidth bits)) init(10)
    val tHigh   = Reg(UInt(c.timerWidth bits)) init(10)
    val tBuf    = Reg(UInt(c.timerWidth bits)) init(10)
    List(sampDiv, timeout, tLow, tHigh, tBuf).foreach(_.allowUnsetRegToAvoidLatch)
  }

  // === Input filtering ===
  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, cfg.sampDiv, counter - U(1))
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
    pushData.allowUnsetRegToAvoidLatch
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
    pushData.allowUnsetRegToAvoidLatch
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

  // Default: clear pulse signals (they are set to True for 1 cycle by FSM/bus)
  txFifo.popValid  := False
  txFifo.pushValid := False
  rxFifo.popValid  := False
  rxFifo.pushValid := False

  /** Width of the address field in bits (7 or 10). */
  val addrW: Int = if (c.support10bit) 10 else 7
  /** Number of address bytes on the bus (1 for 7-bit, 2 for 10-bit). */
  val addrBytes: Int = if (c.support10bit) 2 else 1

  // === Command / Status ===
  val cmd = new Area {
    val trigger  = RegInit(False)
    val addr     = Reg(Bits(addrW bits)) init(0)
    val rw       = RegInit(False)
    val phase2   = RegInit(False)
    val nackLast = RegInit(False)
    val len1     = Reg(UInt(8 bits)) init(0)
    val len2     = Reg(UInt(8 bits)) init(0)
    List(trigger, addr, rw, phase2, nackLast, len1, len2).foreach(_.allowUnsetRegToAvoidLatch)
  }

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
    timeoutCtr := cfg.timeout
  }

  // === Interrupt controller ===
  val intr = new Area {
    val en = RegInit(B(0, 3 bits))
    val fl = RegInit(B(0, 3 bits))
    val clrIn = RegInit(B(0, 3 bits))
    val irq = (en & fl).orR
    for (i <- 0 until 3) { fl(i) clearWhen(clrIn(i)) }
    en.allowUnsetRegToAvoidLatch
    clrIn.allowUnsetRegToAvoidLatch
  }

  val abortReq = RegInit(False)
  abortReq.allowUnsetRegToAvoidLatch

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
        when(cmd.trigger) {
          latAddr     := cmd.addr
          latRw       := cmd.rw
          latHasP2    := cmd.phase2
          latNackLast := cmd.nackLast
          latLen2     := cmd.len2
          phase   := U(0)
          byteNum := U(0)
          bitPos  := U(0)
          curLen  := cmd.len1
          curRw   := cmd.rw
          rxByte  := B(0)
          status.busy    := True
          status.errCode := U(0, 3 bits)
          status.aborted := False
          timeoutCtr     := cfg.timeout
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
      onEntry { timer := cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(BIT_LOW) }
      }
    }

    val BIT_LOW: State = new State {
      onEntry { timer := cfg.tLow; timerArmed := True }
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
      onEntry { timer := cfg.tHigh; timerArmed := True }
      whenIsActive {
        sclDrive := True
        when(driveSda) {
          sdaEn    := True
          sdaDrive := sdaValue
        } otherwise {
          sdaEn := False
        }
        // Clock stretching: reload timer if SCL held low
        when(!sclFilt) { timer := cfg.tHigh }

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
      onEntry { timer := cfg.tLow; timerArmed := True }
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
        sdaEn    := False
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
      onEntry { timer := cfg.tBuf; timerArmed := True }
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
      when(abortReq) {
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

  io.irq := intr.irq

  // Expose internals for simulation
  cfg.sampDiv.simPublic(); cfg.tLow.simPublic(); cfg.tHigh.simPublic(); cfg.tBuf.simPublic(); cfg.timeout.simPublic()
  cmd.addr.simPublic(); cmd.rw.simPublic(); cmd.phase2.simPublic(); cmd.nackLast.simPublic()
  cmd.len1.simPublic(); cmd.len2.simPublic(); cmd.trigger.simPublic()
  status.busy.simPublic(); status.errCode.simPublic(); status.aborted.simPublic()
  abortReq.simPublic()
  txFifo.pushValid.simPublic(); txFifo.pushData.simPublic(); txFifo.popValid.simPublic()
  txFifo.head.simPublic(); txFifo.notEmpty.simPublic(); txFifo.cnt.simPublic()
  rxFifo.popValid.simPublic(); rxFifo.head.simPublic(); rxFifo.notEmpty.simPublic()
  fsm.bitPos.simPublic(); fsm.byteNum.simPublic()

  // === Register map (internal, flat bus decoding) ===
  io.regBus.rdata := B(0, 32 bits)
  when(io.regBus.wen) {
    switch(io.regBus.addr) {
      is(B("h00")) {
        cmd.addr     := io.regBus.wdata(addrW - 1 downto 0)
        cmd.rw       := io.regBus.wdata(addrW)
        cmd.phase2   := io.regBus.wdata(addrW + 1)
        cmd.nackLast := io.regBus.wdata(addrW + 2)
        cmd.trigger  := True
      }
      is(B("h04")) { cmd.len1 := io.regBus.wdata(7 downto 0).asUInt }
      is(B("h08")) { cmd.len2 := io.regBus.wdata(7 downto 0).asUInt }
      is(B("h0C")) {
        txFifo.pushValid := True
        txFifo.pushData  := io.regBus.wdata(7 downto 0)
      }
      is(B("h18")) { intr.en := io.regBus.wdata(2 downto 0) }
      is(B("h20")) { intr.clrIn := io.regBus.wdata(2 downto 0) }
      is(B("h24")) { cfg.sampDiv := io.regBus.wdata(c.sampDivWidth - 1 downto 0).asUInt }
      is(B("h28")) { cfg.timeout := io.regBus.wdata(19 downto 0).asUInt }
      is(B("h2C")) { cfg.tLow    := io.regBus.wdata(c.timerWidth - 1 downto 0).asUInt }
      is(B("h30")) { cfg.tHigh   := io.regBus.wdata(c.timerWidth - 1 downto 0).asUInt }
      is(B("h34")) { cfg.tBuf    := io.regBus.wdata(c.timerWidth - 1 downto 0).asUInt }
      is(B("h38")) { abortReq := io.regBus.wdata(0) }
    }
  }
  when(io.regBus.re) {
    switch(io.regBus.addr) {
      is(B("h10")) {
        io.regBus.rdata(7 downto 0) := rxFifo.head
        io.regBus.rdata(8) := rxFifo.notEmpty
        rxFifo.popValid := True
      }
      is(B("h14")) {
        io.regBus.rdata(0) := status.busy
        io.regBus.rdata(1) := txFifo.notEmpty
        io.regBus.rdata(2) := rxFifo.notEmpty
        io.regBus.rdata(5 downto 3) := status.errCode.asBits
        io.regBus.rdata(6) := status.aborted
      }
      is(B("h1C")) { io.regBus.rdata(2 downto 0) := intr.fl }
    }
  }
}
