package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._

/**
 * SWD transport for the RISC-V debug module.
 */

case class SwdDpCmd() extends Bundle {
  val apNdp = Bool()          // request APnDP bit
  val rnw   = Bool()          // request RnW bit
  val addr  = Bits(2 bits)    // A[3:2] (addr(0) = A[2])
}

case class SwdDpRsp() extends Bundle {
  val ack   = Bits(3 bits)    // OK=001, WAIT=010, FAULT=100 — sent LSB first
  val rdata = Bits(32 bits)   // used when cmd.rnw and ack == OK
}

case class SwdDpWrite() extends Bundle {
  val data     = Bits(32 bits)
  val parityOk = Bool()       // false => WDATAERR material for the DP (Phase 2B)
}

object SwdAck {
  def OK    = B"3'b001"
  def WAIT  = B"3'b010"
  def FAULT = B"3'b100"
}

case class SwdPhy() extends Component {
  val io = new Bundle {
    val swdio = new Bundle {
      val i  = in  Bool()
      val o  = out Bool()
      val oe = out Bool()
    }
    val dp = new Bundle {
      val cmd = master Flow(SwdDpCmd())
      val rsp = slave  Flow(SwdDpRsp())
      val wr  = master Flow(SwdDpWrite())
    }
  }

  val dio = io.swdio.i

  val oData  = Reg(Bool()) init(False)
  val oDrive = Reg(Bool()) init(False)
  io.swdio.o  := oData
  io.swdio.oe := oDrive

  // Response capture: rsp may be presented combinationally off cmd (same cycle the
  // turnaround starts) or held valid for one cycle; latch it for the rest of the frame.
  val rspHold = Reg(SwdDpRsp())
  when(io.dp.rsp.valid){ rspHold := io.dp.rsp.payload }
  val ackNow   = Mux(io.dp.rsp.valid, io.dp.rsp.payload.ack,   rspHold.ack)

  val cmdValid   = Reg(Bool()) init(False)
  val cmdPayload = Reg(SwdDpCmd())
  cmdValid := False
  io.dp.cmd.valid   := cmdValid
  io.dp.cmd.payload := cmdPayload

  val wrValid   = Reg(Bool()) init(False)
  val wrPayload = Reg(SwdDpWrite())
  wrValid := False
  io.dp.wr.valid   := wrValid
  io.dp.wr.payload := wrPayload

  // Line reset: 50+ SWCLK cycles with SWDIO high while the host owns the line
  // (ADIv6.0 B4.3.3). Recovers from any state, including protocol error.
  val lineReset = new Area {
    val counter = Reg(UInt(6 bits)) init(0)
    val hit = counter === 50
    when(oDrive || !dio) {
      counter := 0
    } elsewhen(!hit) {
      counter := counter + 1
    }
  }

  object EState extends SpinalEnum {
    val IDLE, HEADER, ACK, READ_DATA, WR_TRN, WRITE_DATA, RELEASE, ERROR, RESET_WAIT = newElement()
  }
  val state = Reg(EState()) init(EState.RESET_WAIT)
  val cnt   = Reg(UInt(6 bits)) init(0)
  val hdr   = Reg(Bits(6 bits))          // apNdp, rnw, a2, a3, parity, stop (LSB first)
  val wShift = Reg(Bits(32 bits))

  switch(state) {
    is(EState.IDLE) {
      oDrive := False
      when(dio) {                        // start bit
        state := EState.HEADER
        cnt := 0
      }
    }
    is(EState.HEADER) {
      hdr := dio ## hdr(5 downto 1)      // LSB-first shift-in
      cnt := cnt + 1
      when(cnt === 6) {                  // dio is the park bit; hdr holds apNdp..stop
        val apNdp = hdr(0)
        val rnw   = hdr(1)
        val a2    = hdr(2)
        val a3    = hdr(3)
        val parityOk = (apNdp ^ rnw ^ a2 ^ a3) === hdr(4)
        val stopOk   = !hdr(5)
        when(parityOk && stopOk && dio) {
          cmdValid := True
          cmdPayload.apNdp := apNdp
          cmdPayload.rnw   := rnw
          cmdPayload.addr  := a3 ## a2
          state := EState.ACK            // next cycle is the turnaround
          cnt := 0
        } otherwise {
          state := EState.ERROR          // silent until line reset (B4.2.5)
        }
      }
    }
    is(EState.ACK) {                     // drives ACK[0..2], first bit lands after the turnaround
      oDrive := True
      oData  := ackNow(cnt(1 downto 0))
      cnt := cnt + 1
      when(cnt === 2) {
        cnt := 0
        when(ackNow === SwdAck.OK) {
          when(cmdPayload.rnw) {
            state := EState.READ_DATA    // target keeps the line: RDATA follows ACK directly
          } otherwise {
            state := EState.WR_TRN       // turnaround back to host before WDATA
          }
        } otherwise {
          state := EState.RELEASE        // WAIT/FAULT: no data phase (no ORUNDETECT)
        }
      }
    }
    is(EState.READ_DATA) {               // 32 data bits LSB first + even parity
      oDrive := True
      oData  := Mux(cnt === 32, rspHold.rdata.xorR, rspHold.rdata(cnt(4 downto 0)))
      cnt := cnt + 1
      when(cnt === 32) {
        state := EState.RELEASE
        cnt := 0
      }
    }
    is(EState.WR_TRN) {                  // release the line, then one turnaround bit period
      oDrive := False                    // (a write has a 2nd turnaround after ACK; the host
      cnt := cnt + 1                     //  drives its first WDATA bit only after it)
      when(cnt === 1) {
        state := EState.WRITE_DATA
        cnt := 0
      }
    }
    is(EState.WRITE_DATA) {              // sample 32 data bits LSB first, then parity
      wShift := dio ## wShift(31 downto 1)
      cnt := cnt + 1
      when(cnt === 32) {                 // dio is the parity bit; wShift holds the data
        wrValid := True
        wrPayload.data     := wShift
        wrPayload.parityOk := wShift.xorR === dio
        state := EState.IDLE
      }
    }
    is(EState.RELEASE) {                 // turnaround back to host after a target-driven phase
      oDrive := False
      cnt := cnt + 1
      when(cnt === 1) {
        state := EState.IDLE
        cnt := 0
      }
    }
    is(EState.ERROR) {                   // protocol error: line released, headers ignored
      oDrive := False
    }
    is(EState.RESET_WAIT) {              // line reset seen; leave once the host drives low
      oDrive := False
      when(!dio) {
        state := EState.IDLE
      }
    }
  }

  when(lineReset.hit) {                  // overrides everything, including ERROR
    state := EState.RESET_WAIT
  }
}

case class SwdApCmd() extends Bundle {
  val rnw   = Bool()          // true = AP read, false = AP write
  val addr  = Bits(2 bits)    // AP register A[3:2]
  val apSel = Bits(8 bits)    // SELECT[31:24]; the Phase 2C gateway decodes ADDR == 0
  val wdata = Bits(32 bits)   // valid for writes only
}

case class SwdApRsp() extends Bundle {
  val error = Bool()          // completion error -> STICKYERR
  val data  = Bits(32 bits)   // read result (ignored for write completions)
}

case class SwdDp(dpidr : BigInt = BigInt("0BA11AAB", 16)) extends Component {
  val io = new Bundle {
    val dp = new Bundle {
      val cmd = slave  Flow(SwdDpCmd())
      val rsp = master Flow(SwdDpRsp())
      val wr  = slave  Flow(SwdDpWrite())
    }
    val ap = new Bundle {
      val cmd = master Flow(SwdApCmd())
      val rsp = slave  Flow(SwdApRsp())
    }
  }

  // CTRL/STAT state
  val orunDetect   = Reg(Bool()) init(False)   // stored; overrun detection NOT implemented
  val stickyOrun   = Reg(Bool()) init(False)   // never set by hardware, ABORT-clearable
  val stickyCmp    = Reg(Bool()) init(False)   // never set by hardware, ABORT-clearable
  val stickyErr    = Reg(Bool()) init(False)   // set on AP completion error
  val wdataErr     = Reg(Bool()) init(False)   // set on SWD write-data parity error
  val cdbgPwrUpReq = Reg(Bool()) init(False)
  val csysPwrUpReq = Reg(Bool()) init(False)

  val select    = Reg(Bits(32 bits)) init(0)   // APSEL[31:24] APBANKSEL[7:4] DPBANKSEL[3:0]
  val dpBankSel = select(3 downto 0)

  val rdBuffer  = Reg(Bits(32 bits)) init(0)   // posted AP read result (RDBUFF / RESEND)
  val apBusy    = Reg(Bool()) init(False)
  val apWasRead = Reg(Bool()) init(False)      // outstanding transaction is a read
  val apDiscard = Reg(Bool()) init(False)      // DAPABORT: drop the in-flight completion

  val anySticky = stickyOrun || stickyCmp || stickyErr || wdataErr

  val ctrlStat = Bits(32 bits)
  ctrlStat := 0
  ctrlStat(0)  := orunDetect
  ctrlStat(1)  := stickyOrun
  ctrlStat(4)  := stickyCmp
  ctrlStat(5)  := stickyErr
  ctrlStat(7)  := wdataErr
  ctrlStat(28) := cdbgPwrUpReq
  ctrlStat(29) := cdbgPwrUpReq                 // CDBGPWRUPACK mirrors REQ
  ctrlStat(30) := csysPwrUpReq
  ctrlStat(31) := csysPwrUpReq                 // CSYSPWRUPACK mirrors REQ

  val cmd  = io.dp.cmd
  val isAp = cmd.payload.apNdp
  val addr = cmd.payload.addr
  val isRdbuffRead = !isAp && cmd.payload.rnw && addr === B"11"
  val gated = isAp || isRdbuffRead             // accesses subject to sticky/busy gating

  val ack = Bits(3 bits)
  when(anySticky && gated) {
    ack := SwdAck.FAULT
  } elsewhen(apBusy && gated) {
    ack := SwdAck.WAIT
  } otherwise {
    ack := SwdAck.OK
  }

  val dpReadData = Bits(32 bits)
  switch(addr) {
    is(B"00") { dpReadData := B(dpidr, 32 bits) }                            // DPIDR
    is(B"01") { dpReadData := (dpBankSel === 0) ? ctrlStat | B(0, 32 bits) } // banked
    is(B"10") { dpReadData := rdBuffer }                                     // RESEND
    is(B"11") { dpReadData := rdBuffer }                                     // RDBUFF
  }

  // Combinational response off the (registered) cmd pulse -> within the turnaround.
  io.dp.rsp.valid := cmd.valid
  io.dp.rsp.payload.ack   := ack
  io.dp.rsp.payload.rdata := isAp ? rdBuffer | dpReadData

  // A write's data arrives via wr after its ACK - remember the acked target.
  val last = new Area {
    val pendingWrite = Reg(Bool()) init(False)
    val isApReg      = Reg(Bool())
    val addrReg      = Reg(Bits(2 bits))
  }
  when(cmd.valid) {
    last.pendingWrite := !cmd.payload.rnw && ack === SwdAck.OK
    last.isApReg      := isAp
    last.addrReg      := addr
  }

  // AP completion. Only a completion we are actually waiting for is honored — an
  // unsolicited rsp event must not set touch the read buffer.
  when(io.ap.rsp.valid) {
    when(apDiscard) {
      apDiscard := False                         // DAPABORT'd access: swallow its completion
      apBusy := False
    } elsewhen(apBusy) {
      apBusy := False
      when(io.ap.rsp.payload.error) {
        stickyErr := True
      } elsewhen(apWasRead) {
        rdBuffer := io.ap.rsp.payload.data
      }
    }                                            // otherwise: unsolicited — ignore
  }

  // Write commit (fires after the WDATA phase; absent on WAIT/FAULT frames)
  val wr = io.dp.wr
  val apWrFire = False
  when(wr.valid) {
    last.pendingWrite := False
    when(!wr.payload.parityOk) {
      wdataErr := True                         // WDATAERR; the write is dropped
    } elsewhen(last.pendingWrite) {
      when(last.isApReg) {
        apWrFire := True
      } otherwise {
        switch(last.addrReg) {
          is(B"00") {                          // ABORT
            when(wr.payload.data(1)) { stickyCmp  := False }
            when(wr.payload.data(2)) { stickyErr  := False }
            when(wr.payload.data(3)) { wdataErr   := False }
            when(wr.payload.data(4)) { stickyOrun := False }
            when(wr.payload.data(0)) {         // DAPABORT
              when(apBusy) { apDiscard := True }
              apBusy := False
            }
          }
          is(B"01") {                          // CTRL/STAT (bank 0 only; others WI)
            when(dpBankSel === 0) {
              orunDetect   := wr.payload.data(0)
              cdbgPwrUpReq := wr.payload.data(28)
              csysPwrUpReq := wr.payload.data(30)
            }
          }
          is(B"10") { select := wr.payload.data }
          is(B"11") { }                        // TARGETSEL (SWD v2) - ignored in v1
        }
      }
    }
  }

  // AP launch: reads fire at the request (posted), writes fire at the data commit.
  val apRdFire = cmd.valid && isAp && cmd.payload.rnw && ack === SwdAck.OK
  io.ap.cmd.valid := apRdFire || apWrFire
  io.ap.cmd.payload.rnw   := apRdFire
  io.ap.cmd.payload.addr  := apRdFire ? addr | last.addrReg
  io.ap.cmd.payload.apSel := select(31 downto 24)
  io.ap.cmd.payload.wdata := wr.payload.data
  when(io.ap.cmd.valid) {                      // last assignment wins over the rsp clear
    apBusy    := True
    apWasRead := apRdFire
  }
}

case class SwdPhyDp(dpidr : BigInt = BigInt("0BA11AAB", 16)) extends Component {
  val io = new Bundle {
    val swdio = new Bundle {
      val i  = in  Bool()
      val o  = out Bool()
      val oe = out Bool()
    }
    val ap = new Bundle {
      val cmd = master Flow(SwdApCmd())
      val rsp = slave  Flow(SwdApRsp())
    }
  }
  val phy = SwdPhy()
  val dp  = SwdDp(dpidr)
  phy.io.swdio.i := io.swdio.i
  io.swdio.o  := phy.io.swdio.o
  io.swdio.oe := phy.io.swdio.oe
  dp.io.dp.cmd  << phy.io.dp.cmd
  phy.io.dp.rsp << dp.io.dp.rsp
  dp.io.dp.wr   << phy.io.dp.wr
  io.ap.cmd     << dp.io.ap.cmd
  dp.io.ap.rsp  << io.ap.rsp
}

class SwdDmiGateway(p : DebugTransportModuleParameter,
                    swdCd : ClockDomain,
                    debugCd : ClockDomain,
                    apIdr : BigInt) extends Area {
  import p._

  val swdLogic = swdCd on new Area {
    val apCmd = Flow(SwdApCmd())             // driven by the parent from SwdPhyDp
    val apRsp = Flow(SwdApRsp())

    val dmiAddr  = Reg(UInt(addressWidth bits)) init(0)
    val lastRead = Reg(Bits(32 bits)) init(0)

    val dmiCmd = Flow(DebugCmd(addressWidth))
    val dmiRsp = Flow(DebugRsp())            // driven by systemLogic (crossed back)

    val isDmiData = apCmd.payload.addr === B"10"
    val localHit  = apCmd.valid && !isDmiData

    // AP_IDR / DMI_ADDR / POSTED_READ complete locally one cycle after the launch.
    val local = new Area {
      val valid = RegNext(localHit) init(False)
      val data  = Reg(Bits(32 bits))
      when(localHit) {
        data := 0
        switch(apCmd.payload.addr) {
          is(B"00") { data := B(apIdr, 32 bits) }
          is(B"01") {
            when(apCmd.payload.rnw) {
              data := B(0, (32 - addressWidth) bits) ## dmiAddr.asBits
            } otherwise {
              dmiAddr := apCmd.payload.wdata(addressWidth - 1 downto 0).asUInt
            }
          }
          is(B"11") { data := lastRead }     // writes to RO registers: OK, no effect
        }
      }
    }

    val dmiWasRead = Reg(Bool())
    val dmiPending = Reg(Bool()) init(False)     // a DebugBus access is in flight
    dmiCmd.valid   := apCmd.valid && isDmiData
    dmiCmd.write   := !apCmd.payload.rnw
    dmiCmd.address := dmiAddr
    dmiCmd.data    := apCmd.payload.wdata
    when(dmiCmd.valid) {
      dmiWasRead := apCmd.payload.rnw
      dmiPending := True
    }

    // Only honor a response we are waiting for — a spurious rsp event out of the
    // clock crossing (e.g. boot-time toggle mismatch) must not complete anything.
    val dmiRspHit = dmiRsp.valid && dmiPending
    when(dmiRspHit) {
      dmiPending := False
      when(dmiWasRead && !dmiRsp.error) { lastRead := dmiRsp.data }
    }

    apRsp.valid         := local.valid || dmiRspHit
    apRsp.payload.error := dmiRspHit && dmiRsp.error
    apRsp.payload.data  := local.valid ? local.data | dmiRsp.data
  }

  val systemLogic = debugCd on new Area {
    val bus = DebugBus(addressWidth)
    val cmd = swdLogic.dmiCmd.ccToggle(
      pushClock = swdCd,
      popClock = debugCd,
      withOutputM2sPipe = false
    ).toStream.m2sPipe(crossClockData = true, holdPayload = true)
    bus.cmd << cmd
    swdLogic.dmiRsp << bus.rsp.ccToggle(
      pushClock = debugCd,
      popClock = swdCd,
      // The SWCLK domain is BOOT-reset (no reset wire exists on the 2-wire interface);
      // a buffered reset cannot be synthesized into it — pop-side regs boot-init instead.
      withOutputBufferedReset = false
    )
  }
}

/**
 * The complete SWD transport: SWD pins -> SwdPhy -> SwdDp -> DMI gateway -> DebugBus.
 * SWD-side logic runs on the probe-driven SWCLK (BOOT reset: no reset wire on the pins —
 * line reset is the protocol-level reset); the DebugBus side runs on debugCd.
 * Counterpart of DebugTransportModuleJtagTap for the SWD wire protocol.
 */
case class DebugTransportModuleSwd(p : DebugTransportModuleParameter,
                                   debugCd : ClockDomain,
                                   dpidr : BigInt = BigInt("0BA11AAB", 16),
                                   apIdr : BigInt = BigInt("74726976", 16)) extends Component {
  val io = new Bundle {
    val swd = new Bundle {
      val swclk = in Bool()
      val swdio = new Bundle {
        val i  = in  Bool()
        val o  = out Bool()
        val oe = out Bool()
      }
    }
    val bus = master(DebugBus(p.addressWidth))
  }

  val swdCd = ClockDomain(clock = io.swd.swclk, config = ClockDomainConfig(resetKind = BOOT))

  val core = swdCd on SwdPhyDp(dpidr)
  core.io.swdio.i := io.swd.swdio.i
  io.swd.swdio.o  := core.io.swdio.o
  io.swd.swdio.oe := core.io.swdio.oe

  val gateway = new SwdDmiGateway(p, swdCd, debugCd, apIdr)
  gateway.swdLogic.apCmd << core.io.ap.cmd
  core.io.ap.rsp << gateway.swdLogic.apRsp

  io.bus <> gateway.systemLogic.bus
}
