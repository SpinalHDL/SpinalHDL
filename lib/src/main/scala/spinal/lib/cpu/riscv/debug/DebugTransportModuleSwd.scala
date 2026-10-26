package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.swd._

/**
 * SWD transport for the RISC-V debug module.
 *
 * The wire side (Swd pins, SwdPhy, SwdDp) is generic ADI and lives in spinal.lib.com.swd,
 * like the JTAG TAP in spinal.lib.com.jtag. What is RISC-V specific stays here: the
 * designer-AP that maps AP registers onto the DMI, and the DTM that assembles the stack.
 */

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
 *
 * This is a RISC-V Debug Spec Ch. 6 custom DTM, not a ratified SWD chapter.
 * The pin/AP map may change if the spec later defines one.
 */
case class DebugTransportModuleSwd(p : DebugTransportModuleParameter,
                                   debugCd : ClockDomain,
                                   dpidr : BigInt = BigInt("0BA11AAB", 16),
                                   apIdr : BigInt = BigInt("74726976", 16)) extends Component {
  val io = new Bundle {
    val swd = slave(Swd())
    val bus = master(DebugBus(p.addressWidth))
  }

  val swdCd = io.swd.clockDomain

  val core = swdCd on SwdPhyDp(dpidr)
  io.swd.swdio << core.io.swdio

  val gateway = new SwdDmiGateway(p, swdCd, debugCd, apIdr)
  gateway.swdLogic.apCmd << core.io.ap.cmd
  core.io.ap.rsp << gateway.swdLogic.apRsp

  io.bus <> gateway.systemLogic.bus
}
