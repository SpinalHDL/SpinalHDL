package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTapFactory, JtagTapFunctions, JtagTapInstructionCtrl}

case class DebugTransportModuleParameter(addressWidth : Int,
                                         version : Int,
                                         idle : Int)

class DebugTransportModuleJtag(p : DebugTransportModuleParameter,
                               tap : JtagTapFunctions,
                               jtagCd : ClockDomain,
                               debugCd : ClockDomain) extends Area{
  import p._

  val jtagLogic = jtagCd on new Area {
    val dmiCmd = Flow(DebugCmd(addressWidth))
    val dmiRsp = Flow(DebugRsp())

    val dtmcs = tap.readAndWriteWithEvents(Bits(32 bits), Bits(32 bits))(0x10)
    val dmi   = tap.readAndWriteWithEvents(DebugCapture(addressWidth), DebugUpdate(addressWidth))(0x11)

    val dmiStat = new Area{
      val value = Reg(DebugCaptureOp())
      val failure = False
      val busy = False
      val clear = False

      when(value === DebugCaptureOp.SUCCESS){
        when(failure){ value := DebugCaptureOp.FAILED }
        when(busy){ value := DebugCaptureOp.OVERRUN }
      }
      when(clear) { value := DebugCaptureOp.SUCCESS }
    }


    val pending = Reg(Bool()) setWhen (dmiCmd.valid) clearWhen (dmiRsp.fire)

    val trigger = new Area {
      val dmiHardReset = dtmcs.updateData(17) && dtmcs.updateValid || tap.isReseting()
      val dmiReset     = dtmcs.updateData(16) && dtmcs.updateValid || tap.isReseting()
      val dmiCmd       = False
      when(dmi.updateValid){
        switch(dmi.updateData.op){
          is(DebugUpdateOp.NOP){ }
          is(DebugUpdateOp.READ){ dmiCmd := True }
          is(DebugUpdateOp.WRITE){ dmiCmd := True }
          default{ dmiStat.failure := True }
        }
      }
      when(dmiReset) { dmiStat.clear := True }
      when(dmiHardReset) {
        pending := False
        dmiStat.clear := True
      }
    }
    dtmcs.captureData := B(0, 17 bits) ## B(idle, 3 bits) ## dmiStat.value ## B(addressWidth, 6 bits) ## B(version, 4 bits)


    val cmdLogic = new Area {
      dmiCmd.valid := trigger.dmiCmd
      dmiCmd.write := dmi.updateData.op === DebugUpdateOp.WRITE
      dmiCmd.address := dmi.updateData.address
      dmiCmd.data := dmi.updateData.data
    }

    val rspLogic = new Area{
      val buffer = Reg(Bits(32 bits))
      when(dmiRsp.fire){
        buffer := dmiRsp.data
        when(dmiRsp.error){ dmiStat.failure := True }
      }

      dmi.captureData.op   := dmiStat.value.getAheadValue()
      dmi.captureData.data := buffer
      dmi.captureData.padding := 0
      when(dmi.captureValid && pending){
        dmiStat.busy := True
      }
    }
  }

  val systemLogic = debugCd on new Area{
    val bus = DebugBus(addressWidth)
    val cmd = jtagLogic.dmiCmd.ccToggle(
      pushClock = jtagCd,
      popClock = debugCd,
      withOutputM2sPipe = false
    ).toStream.m2sPipe(crossClockData = true, holdPayload = true)

    bus.cmd << cmd

    jtagLogic.dmiRsp << bus.rsp.ccToggle(
      pushClock = debugCd,
      popClock = jtagCd
    )
  }
}

case class DebugTransportModuleJtagTap(p : DebugTransportModuleParameter,
                                       debugCd : ClockDomain,
                                       jtagId : Int = 0x10002FFF) extends Component{
  val io = new Bundle {
    val jtag = slave(Jtag())
    val bus = master(DebugBus(p.addressWidth))
  }

  val jtagCd = ClockDomain(io.jtag.tck)

  val tap = jtagCd on JtagTapFactory(io.jtag, instructionWidth = 5)
  val idcodeArea = jtagCd on tap.idcode(B(jtagId, 32 bits))(1)
  val logic = new DebugTransportModuleJtag(
    p       = p,
    tap     = tap,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )

  io.bus <> logic.systemLogic.bus
}

case class DebugTransportModuleTunneled(p : DebugTransportModuleParameter,
                                        jtagCd : ClockDomain,
                                        debugCd : ClockDomain) extends Component{
  val io = new Bundle {
    val instruction = slave(JtagTapInstructionCtrl())
    val bus = master(DebugBus(p.addressWidth))
  }

  val tap = jtagCd on new JtagTunnel(io.instruction, instructionWidth = 6)
//  val idcodeArea = tap.idcode(B"x10002FFF")(1)
  val logic = new DebugTransportModuleJtag(
    p       = p,
    tap     = tap,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )

  io.bus <> logic.systemLogic.bus
}

case class DebugTransportModuleJtagTapWithTunnel(p : DebugTransportModuleParameter,
                                                 debugCd : ClockDomain) extends Component{
  val io = new Bundle {
    val jtag = slave(Jtag())
    val bus = master(DebugBus(p.addressWidth))
  }

  val jtagCd = ClockDomain(io.jtag.tck)

  val tap = jtagCd on JtagTapFactory(io.jtag, instructionWidth = 6)
  val idcodeArea = jtagCd on tap.idcode(B"x10003FFF")(1)
  val tunnel = DebugTransportModuleTunneled(
    p       = p,
    jtagCd  = jtagCd,
    debugCd = debugCd
  )
  tap.map(tunnel.io.instruction, 0x23)


  io.bus <> tunnel.io.bus
}