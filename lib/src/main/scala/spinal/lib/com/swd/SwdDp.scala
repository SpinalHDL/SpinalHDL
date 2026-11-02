package spinal.lib.com.swd

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

/**
 * ADI SW-DP: DPIDR, CTRL/STAT, SELECT, RDBUFF/RESEND, ABORT, sticky-error and WAIT/FAULT
 * gating, posted AP reads. Access ports hang off ap.cmd / ap.rsp — one outstanding AP
 * transaction at a time, completed by a single ap.rsp pulse. Runs in the SWCLK domain.
 *
 * dpidr defaults to a placeholder: pick your own designer/part encoding, and do not reuse
 * an ARM Cortex SW-DP value (debuggers key target detection off it).
 */

case class SwdApCmd() extends Bundle {
  val rnw   = Bool()          // true = AP read, false = AP write
  val addr  = Bits(2 bits)    // AP register A[3:2]
  val apSel = Bits(8 bits)    // SELECT[31:24]; decoding it is up to the AP side
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
  // unsolicited rsp event must not touch the read buffer.
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
    val swdio = master(TriState(Bool()))
    val ap = new Bundle {
      val cmd = master Flow(SwdApCmd())
      val rsp = slave  Flow(SwdApRsp())
    }
  }
  val phy = SwdPhy()
  val dp  = SwdDp(dpidr)
  io.swdio << phy.io.swdio
  dp.io.dp.cmd  << phy.io.dp.cmd
  phy.io.dp.rsp << dp.io.dp.rsp
  dp.io.dp.wr   << phy.io.dp.wr
  io.ap.cmd     << dp.io.ap.cmd
  dp.io.ap.rsp  << io.ap.rsp
}
