package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.core.fiber._
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug._

import scala.collection.mutable.ArrayBuffer

class DebugModuleFiber() extends Area{
  var harts = ArrayBuffer[RiscvHart]()
  def bindHart(cpu: RiscvHart) = {
    harts += cpu
  }

  val ndmreset = Bool()

  val thread = Fiber build new Area{
    val p = DebugTransportModuleParameter(
      addressWidth = 7,
      version = 1,
      idle = 7
    )

    val logic = DebugModule(
      DebugModuleParameter(
        version = p.version + 1,
        harts = harts.size,
        progBufSize = 2,
        datacount = harts.map(_.getXlen()).max/32,
        hartsConfig = List(DebugModuleCpuConfig(
          xlen = harts.map(_.getXlen()).max,
          flen = harts.map(_.getFlen()).max,
          withFpuRegAccess = false
        ))
      )
    )

    ndmreset := logic.io.ndmreset

    for(i <- harts.indices){
      val from = logic.io.harts(i)
      val to = harts(i).getDebugBus
//      from <> to
//      to.dmToHart.removeAssignments() <-< from.dmToHart

      from.halted := RegNext(to.halted) init(False)
      from.running := RegNext(to.running) init(False)
      from.unavailable := RegNext(to.unavailable) init(False)
      from.haveReset := RegNext(to.haveReset) init(False)
      from.exception := RegNext(to.exception) init(False)
      from.commit := RegNext(to.commit) init(False)
      from.ebreak := RegNext(to.ebreak) init(False)
      from.redo := RegNext(to.redo) init (False)
      from.regSuccess := RegNext(to.regSuccess) init (False)
      to.haltReq := RegNext(from.haltReq) init (False)
      to.ackReset := RegNext(from.ackReset) init (False)
      to.hartToDm >-> from.hartToDm
      to.dmToHart <-< from.dmToHart
      to.resume.cmd <-< from.resume.cmd
      to.resume.rsp >-> from.resume.rsp
    }
  }

  def withJtagTap() = Fiber build new Area {
    val logic = DebugTransportModuleJtagTap(
      thread.p,
      debugCd = thread.logic.clockDomain
    )
    thread.logic.io.ctrl <> logic.io.bus
    val jtag = logic.io.jtag.toIo
  }

  def withJtagInstruction() = Fiber build new Area {
    val logic = DebugTransportModuleTunneled(
      p = thread.p,
      jtagCd = ClockDomain.current,
      debugCd = thread.logic.clockDomain
    )
    thread.logic.io.ctrl <> logic.io.bus
    val instruction = logic.io.instruction.toIo
  }
}
