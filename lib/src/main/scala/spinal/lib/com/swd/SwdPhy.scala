package spinal.lib.com.swd

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

/**
 * SWD wire protocol (ADIv6 B4): bit-level framing of one packet — request, turnaround, ACK,
 * data phase, line reset. Runs in the SWCLK domain (see Swd.clockDomain) and knows nothing
 * about DP/AP registers: it hands each request to a DP over dp.cmd / dp.rsp / dp.wr.
 *
 * The DP must answer dp.cmd combinationally (rsp.valid in the same cycle): that cycle is the
 * turnaround, and ACK[0] goes on the wire on the next SWCLK. The PHY cannot stall the host.
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
  val parityOk = Bool()       // false => the DP flags WDATAERR and drops the write
}

object SwdAck {
  def OK    = B"3'b001"
  def WAIT  = B"3'b010"
  def FAULT = B"3'b100"
}

case class SwdPhy() extends Component {
  val io = new Bundle {
    val swdio = master(TriState(Bool()))
    val dp = new Bundle {
      val cmd = master Flow(SwdDpCmd())
      val rsp = slave  Flow(SwdDpRsp())
      val wr  = master Flow(SwdDpWrite())
    }
  }

  val dio = io.swdio.read

  val oData  = Reg(Bool()) init(False)
  val oDrive = Reg(Bool()) init(False)
  io.swdio.write       := oData
  io.swdio.writeEnable := oDrive

  // Hold the last valid rsp for the rest of the frame. Bypass ACK combinationally:
  // Flow.stage()/m2sPipe without a mux would delay ACK[0] by a cycle past the turnaround.
  val rspHold = io.dp.rsp.m2sPipe(holdPayload = true)
  val ackNow  = Mux(io.dp.rsp.valid, io.dp.rsp.payload.ack, rspHold.payload.ack)

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
    val counter = Counter(51) // [0, 50]
    when(oDrive || !dio) {
      counter.clear()
    } elsewhen(!counter.willOverflowIfInc) {
      counter.increment()
    }
    val hit = counter.willOverflowIfInc // value === 50
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
      oData  := Mux(cnt === 32, rspHold.payload.rdata.xorR, rspHold.payload.rdata(cnt(4 downto 0)))
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
